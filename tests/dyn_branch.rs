use resum::{resum, ResumBranch, ResumPoll};

#[resum(yield_ty = i32, resume_ty = i32)]
fn brancher() -> i32 {
    let r = coyield!(1);
    r + 10
}

#[test]
fn dyn_resumbranch_works() {
    // Get a concrete coroutine and use it via dyn ResumBranch
    let c = brancher();
    let b: Box<dyn ResumBranch<'static, Yield = i32, Resume = i32, Output = i32>> = Box::new(c);

    // Start a branch; we should see the yield value
    let (p, b1) = b.start_new();
    assert!(matches!(p, ResumPoll::Yield(1)));

    // Resume the branched coroutine to completion
    let (p2, _b2) = b1.resume_new(7);
    assert!(matches!(p2, ResumPoll::Ready(17)));

    // Original boxed coroutine remains usable and unchanged (persistent branching)
    let (p3, _b3) = b.start_new();
    assert!(matches!(p3, ResumPoll::Yield(1)));
}

