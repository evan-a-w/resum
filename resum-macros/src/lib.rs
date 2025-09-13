use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{parse::Parse, parse::ParseStream, parse_macro_input, Block, Expr, ItemFn, Pat, Stmt, Type};

struct ResumArgs {
    yield_ty: Option<Type>,
    resume_ty: Option<Type>,
}

impl Parse for ResumArgs {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        if input.is_empty() {
            return Ok(Self { yield_ty: None, resume_ty: None });
        }
        let mut yield_ty = None;
        let mut resume_ty = None;
        while !input.is_empty() {
            let ident: syn::Ident = input.parse()?;
            let _eq: syn::Token![=] = input.parse()?;
            let ty: Type = input.parse()?;
            let _ = input.parse::<syn::Token![,]>().ok();
            match &*ident.to_string() {
                "yield_ty" => yield_ty = Some(ty),
                "resume_ty" => resume_ty = Some(ty),
                _ => return Err(syn::Error::new_spanned(ident, "unknown argument; expected `yield_ty` or `resume_ty`")),
            }
        }
        Ok(Self { yield_ty, resume_ty })
    }
}

/// Attribute macro to transform a function with `coyield!` suspension points
/// into a resumable coroutine implementing `resum::Resum`.
///
/// Limitations (v0):
/// - `coyield!` must appear at the top level of the function body, either as
///   a statement `coyield!(expr);` or as a `let` initializer `let pat = coyield!(expr);`.
/// - `return` statements are not specially handled; prefer tail expressions.
/// - Control-flow constructs with `coyield!` nested deep are not supported yet.
#[proc_macro_attribute]
pub fn resum(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = parse_macro_input!(attr as ResumArgs);
    let func = parse_macro_input!(item as ItemFn);

    // Capture the original signature details.
    let vis = &func.vis;
    let sig = &func.sig;
    let ident = &sig.ident;
    let generics = &sig.generics;
    let inputs = &sig.inputs;
    let output = &sig.output; // return type as declared by user

    // We will rewrite the function body into a `Coroutine::from_start(move || { ... })`.
    let body = &func.block;

    // Prepare identifiers for Yield and Resume type parameters we add.
    let y_ident = syn::Ident::new("__ResumYield", proc_macro2::Span::call_site());
    let r_ident = syn::Ident::new("__ResumResume", proc_macro2::Span::call_site());

    let resume_ty_tokens: proc_macro2::TokenStream = if let Some(ref rt) = args.resume_ty {
        rt.to_token_stream()
    } else {
        r_ident.to_token_stream()
    };
    let cont_expr = compile_block_to_continuation(body, &y_ident, resume_ty_tokens.clone());

    // New function signature returning impl Resum (we only bind Output; others inferred)
    // Keep original generics and where-clause.
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    // Extract the user-declared output type to thread into `impl Resum<Output = T>`.
    let output_ty: proc_macro2::TokenStream = match &sig.output {
        syn::ReturnType::Default => syn::parse_quote!(()),
        syn::ReturnType::Type(_, ty) => ty.to_token_stream(),
    };

    // Reconstruct function item with modified signature.
    let vis2 = vis.clone();
    let block2 = func.block.clone();
    let inputs2 = inputs.clone();
    let attrs2 = func.attrs.clone();

    // Instead of relying on above partial patching, directly emit the full function with explicit signature.
    let output_ts = output_ty;
    // Build augmented generics: append type params for Yield and Resume only if not provided.
    let mut gen_ts = generics.clone();
    let use_generics = args.yield_ty.is_none() || args.resume_ty.is_none();
    if use_generics {
        gen_ts.params.push(syn::parse_quote!(#y_ident));
        gen_ts.params.push(syn::parse_quote!(#r_ident));
    }

    let lifetime_bounds: Vec<_> = generics
        .lifetimes()
        .map(|lt| lt.lifetime.clone())
        .collect();
    let expanded2 = if let (Some(y_ty), Some(r_ty)) = (args.yield_ty, args.resume_ty) {
        quote! {
            #(#attrs2)*
            #vis2 fn #ident #gen_ts (#inputs2) -> impl ::resum::Resum<Yield = #y_ty, Resume = #r_ty, Output = #output_ts> + ::resum::ResumBranch<Yield = #y_ty, Resume = #r_ty, Output = #output_ts> #(+ #lifetime_bounds)* {
                let __start = move || { #cont_expr };
                ::resum::Coroutine::from_start(__start)
            }
        }
    } else {
        quote! {
            #(#attrs2)*
            #vis2 fn #ident #gen_ts (#inputs2) -> impl ::resum::Resum<Yield = #y_ident, Resume = #r_ident, Output = #output_ts> + ::resum::ResumBranch<Yield = #y_ident, Resume = #r_ident, Output = #output_ts> #(+ #lifetime_bounds)* {
                let __start = move || { #cont_expr };
                ::resum::Coroutine::from_start(__start)
            }
        }
    };

    expanded2.into()
}

/// Represents a single suspension point encountered while scanning the block.
struct YieldPoint {
    pre: Vec<Stmt>,
    binder: Option<Pat>,
    yield_expr: Expr,
}

fn is_coyield_macro_expr(expr: &Expr) -> Option<&syn::Macro> {
    if let Expr::Macro(m) = expr {
        let path = &m.mac.path;
        if path.segments.last().map(|s| s.ident == "coyield").unwrap_or(false) {
            return Some(&m.mac);
        }
    }
    None
}

fn extract_coyield_arg(m: &syn::Macro) -> Expr {
    // Accept exactly one expression argument.
    syn::parse2::<Expr>(m.tokens.clone()).expect("coyield! expects an expression as its argument")
}

fn compile_block_to_continuation(
    block: &Block,
    _y_ident: &syn::Ident,
    r_ty_tokens: proc_macro2::TokenStream,
) -> proc_macro2::TokenStream {
    // Linear scan of top-level statements, splitting on `coyield!` occurrences.
    let mut segments: Vec<YieldPoint> = Vec::new();
    let mut current: Vec<Stmt> = Vec::new();

    for stmt in &block.stmts {
        match stmt {
            Stmt::Local(local) => {
                if let Some(init) = &local.init {
                    let init_expr = &init.expr;
                    if let Some(mac) = is_coyield_macro_expr(init_expr) {
                        // `let <pat> = coyield!(<expr>);`
                        let binder = Some(local.pat.clone());
                        let y_expr = extract_coyield_arg(mac);
                        segments.push(YieldPoint { pre: current, binder, yield_expr: y_expr });
                        current = Vec::new();
                        continue;
                    }
                }
                current.push(stmt.clone());
            }
            Stmt::Expr(Expr::Macro(m), _semi) => {
                // `coyield!(<expr>)` possibly with a semicolon
                if m.mac.path.segments.last().map(|s| s.ident == "coyield").unwrap_or(false) {
                    let y_expr = extract_coyield_arg(&m.mac);
                    segments.push(YieldPoint { pre: current, binder: None, yield_expr: y_expr });
                    current = Vec::new();
                    continue;
                }
                current.push(stmt.clone());
            }
            _ => current.push(stmt.clone()),
        }
    }

    // Remaining tail statements form the final Done block.
    let tail = current;

    // Build Continuation expression from the tail backwards through the yields.
    let mut acc: proc_macro2::TokenStream = quote! {
        ::resum::__rt::Continuation::Done({ #(#tail)* })
    };

    for yp in segments.into_iter().rev() {
        let pre = yp.pre;
        let y = yp.yield_expr;
        let binder_stmt = yp.binder.map(|pat| quote! { let #pat = __resume; });

        let r_ty = r_ty_tokens.clone();
        acc = quote! {{
            #(#pre)*
            ::resum::__rt::Continuation::Yield {
                value: (#y),
                next: Box::new(move |__resume: #r_ty| {
                    #binder_stmt
                    #acc
                }),
            }
        }};
    }

    acc
}
