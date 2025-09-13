use resum::{resum, Resum, ResumPoll};

#[resum(yield_ty = (), resume_ty = usize)]
fn do_something(a: usize, b: usize) -> usize {
    let random: usize = coyield!(());
    a ^ b ^ random
}

#[test]
fn basic_example() {
    let mut c = do_something(1, 2);
    assert!(matches!(c.start(), ResumPoll::Yield(_)));
    let r = c.resume(5usize);
    assert!(matches!(r, ResumPoll::Ready(6)));
    let r = c.resume(6usize);
    assert!(matches!(r, ResumPoll::Ready(5)));
}
