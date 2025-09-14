//! Resumable coroutine runtime and public API.
//!
//! This crate provides the `Resum` trait, a `Coroutine` runtime,
//! and re-exports the `#[resum]` attribute macro from the companion
//! `resum-macros` crate.

/// The result of stepping a coroutine: either a yielded value or a ready output.
#[derive(Debug, PartialEq)]
pub enum ResumPoll<Y, O> {
    /// The coroutine yielded a value and is suspended, waiting to be resumed.
    Yield(Y),
    /// The coroutine completed and returned a final output.
    Ready(O),
}

/// Trait for resumable coroutines.
///
/// - `Yield`: value type produced when suspending.
/// - `Resume`: value type the coroutine expects on resume after a yield.
/// - `Output`: final result when the coroutine completes.
pub trait Resum<'a> {
    type Yield;
    type Resume;
    type Output;

    /// Starts execution from the beginning until the first yield or completion.
    fn start(&mut self) -> ResumPoll<Self::Yield, Self::Output>;

    /// Resumes from the last suspension point with a value.
    ///
    /// Accepts any type that can convert into `Self::Resume`.
    fn resume(&mut self, value: Self::Resume) -> ResumPoll<Self::Yield, Self::Output>;
}

/// Internal runtime types used by the proc-macro expansion.
pub mod __rt {
    /// Continuation produced by executing a coroutine segment.
    ///
    /// - `Yield { value, next }` suspends and carries a continuation `next`
    ///   to resume with the given resume value.
    /// - `Done(output)` completes the coroutine.
    pub enum Continuation<'a, Y, R, O> {
        Yield {
            value: Y,
            next: Box<dyn Fn(R) -> Continuation<'a, Y, R, O> + 'a>,
        },
        Done(O),
    }

    /// State machine powering a coroutine instance.
    pub enum State<'a, Y, R, O> {
        Entry(std::rc::Rc<dyn Fn() -> Continuation<'a, Y, R, O> + 'a>),
        Next(std::rc::Rc<dyn Fn(R) -> Continuation<'a, Y, R, O> + 'a>),
        Done,
    }

    impl<'a, Y, R, O> std::fmt::Debug for State<'a, Y, R, O> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                State::Entry(_) => f.write_str("State::Entry"),
                State::Next(_) => f.write_str("State::Next"),
                State::Done => f.write_str("State::Done"),
            }
        }
    }

    /// Helper to build a `State::Entry` from a start closure.
    pub fn entry<'a, Y, R, O, F>(f: F) -> State<'a, Y, R, O>
    where
        F: Fn() -> Continuation<'a, Y, R, O> + 'a,
    {
        State::Entry(std::rc::Rc::new(f))
    }
}

/// A concrete coroutine runtime implementing `Resum`.
pub struct Coroutine<'a, Y, R, O> {
    state: __rt::State<'a, Y, R, O>,
}

impl<'a, Y, R, O> Coroutine<'a, Y, R, O> {
    /// Builds a coroutine from a start closure returning a `Continuation`.
    pub fn from_start<F>(start: F) -> Self
    where
        F: Fn() -> __rt::Continuation<'a, Y, R, O> + 'a,
    {
        Self {
            state: __rt::entry(start),
        }
    }
}

impl<'a, Y: 'a, R: 'a, O: 'a> Resum<'a> for Coroutine<'a, Y, R, O> {
    type Yield = Y;
    type Resume = R;
    type Output = O;

    fn start(&mut self) -> ResumPoll<Self::Yield, Self::Output> {
        use __rt::{Continuation, State};
        match &self.state {
            State::Entry(f) => {
                match f() {
                    Continuation::Done(output) => {
                        // Do not alter state; allow multi-shot start
                        ResumPoll::Ready(output)
                    }
                    Continuation::Yield { value, next } => {
                        let next_rc = std::rc::Rc::new(move |r: R| (next)(r));
                        self.state = State::Next(next_rc);
                        ResumPoll::Yield(value)
                    }
                }
            }
            State::Next(_) => panic!(
                "start() called while coroutine is suspended; call resume(...) instead"
            ),
            State::Done => panic!("start() called after completion"),
        }
    }

    fn resume(&mut self, value: Self::Resume) -> ResumPoll<Self::Yield, Self::Output>
    {
        use __rt::{Continuation, State};
        // Call the stored continuation without consuming it to allow multi-shot.
        let cont = match &self.state {
            State::Next(next) => next(value),
            State::Entry(_) => panic!("resume() called before start()"),
            State::Done => panic!("resume() called after completion"),
        };

        match cont {
            Continuation::Done(output) => {
                // Do NOT advance state; keep the last suspension point alive
                // for multi-shot resumption from the same point.
                ResumPoll::Ready(output)
            }
            Continuation::Yield { value, next } => {
                // Advance to deeper suspension point.
                let next_rc = std::rc::Rc::new(move |r: R| (next)(r));
                self.state = State::Next(next_rc);
                ResumPoll::Yield(value)
            }
        }
    }
}

/// Branching API: persistent, non-mutating operations returning a new coroutine.
pub trait ResumBranch<'a>: Sized {
    type Yield;
    type Resume;
    type Output;

    fn start_new(&self) -> (ResumPoll<Self::Yield, Self::Output>, Self);

    fn resume_new<V>(&self, value: V) -> (ResumPoll<Self::Yield, Self::Output>, Self)
    where
        Self::Resume: From<V>;
}

impl<'a, Y: 'a, R: 'a, O: 'a> ResumBranch<'a> for Coroutine<'a, Y, R, O> {
    type Yield = Y;
    type Resume = R;
    type Output = O;

    fn start_new(&self) -> (ResumPoll<Self::Yield, Self::Output>, Self) {
        use __rt::{Continuation, State};
        match &self.state {
            State::Entry(f) => match f() {
                Continuation::Done(output) => (
                    ResumPoll::Ready(output),
                    Coroutine {
                        state: State::Entry(f.clone()),
                    },
                ),
                Continuation::Yield { value, next } => {
                    let next_rc = std::rc::Rc::new(move |r: R| (next)(r));
                    (
                        ResumPoll::Yield(value),
                        Coroutine {
                            state: State::Next(next_rc),
                        },
                    )
                }
            },
            State::Next(_) => panic!("start_new() called while suspended"),
            State::Done => panic!("start_new() called after completion"),
        }
    }

    fn resume_new<V>(&self, value: V) -> (ResumPoll<Self::Yield, Self::Output>, Self)
    where
        Self::Resume: From<V>,
    {
        use __rt::{Continuation, State};
        match &self.state {
            State::Next(next) => match next(<Self::Resume as From<V>>::from(value)) {
                Continuation::Done(output) => (
                    ResumPoll::Ready(output),
                    Coroutine {
                        state: State::Next(next.clone()),
                    },
                ),
                Continuation::Yield { value, next } => {
                    let next_rc = std::rc::Rc::new(move |r: R| (next)(r));
                    (
                        ResumPoll::Yield(value),
                        Coroutine {
                            state: State::Next(next_rc),
                        },
                    )
                }
            },
            State::Entry(_) => panic!("resume_new() called before start"),
            State::Done => panic!("resume_new() called after completion"),
        }
    }
}

// Re-export the attribute macro for convenience.
pub use resum_macros::resum;
