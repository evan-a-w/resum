use resum::{resum, ResumPoll, ResumBranch};

#[resum(yield_ty = i32, resume_ty = i32)]
fn two_yields(a: i32) -> i32 {
    let x: i32 = coyield!(a + 1);
    let y: i32 = coyield!(a + x);
    a + x + y
}

#[test]
fn branch_two_paths() {
    let c0 = two_yields(2);

    let (p1, c1) = c0.start_new();
    assert!(matches!(p1, ResumPoll::Yield(3)));

    // Branch A from first suspension point
    let (p2a, c2a) = c1.resume_new(5);
    assert!(matches!(p2a, ResumPoll::Yield(7)));
    let (done_a, _next_a) = c2a.resume_new(11);
    assert!(matches!(done_a, ResumPoll::Ready(18)));

    // Branch B reusing c1 again (persistent state)
    let (p2b, c2b) = c1.resume_new(10);
    assert!(matches!(p2b, ResumPoll::Yield(12)));
    let (done_b, _next_b) = c2b.resume_new(100);
    assert!(matches!(done_b, ResumPoll::Ready(112)));
}
