use resum::{resum, Resum, ResumPoll};

#[resum(yield_ty = i32, resume_ty = i32)]
fn two_yields(a: i32) -> i32 {
    let x: i32 = coyield!(a + 1);
    let y: i32 = coyield!(a + x);
    a + x + y
}

#[resum(yield_ty = T, resume_ty = T)]
fn generic<T: Clone + 'static>(a: T) -> T {
    let x = coyield!(a.clone());
    x
}

#[test]
fn multi_yield() {
    let mut c = two_yields(2);
    let y1 = c.start();
    assert!(matches!(y1, ResumPoll::Yield(_)));
    let y2 = c.resume(5i32);
    assert!(matches!(y2, ResumPoll::Yield(_)));
    let done = c.resume(11i32);
    assert!(matches!(done, ResumPoll::Ready(_)));
    if let ResumPoll::Ready(v) = done { assert_eq!(v, 18); }
}

#[resum(yield_ty = usize, resume_ty = usize)]
fn with_ref<'a>(s: &'a str) -> usize {
    let i: usize = coyield!(s.len());
    s.as_bytes()[i] as usize
}

#[test]
fn generic_test() {
    let mut c = generic(1u32);
    let _ = c.start();
    assert!(matches!(c.resume(1u32), ResumPoll::Ready(1)));
}

#[test]
fn capture_reference() {
    let s = "hello";
    let mut c = with_ref(s);
    assert!(matches!(c.start(), ResumPoll::Yield(_)));
    let done = c.resume(1usize);
    assert!(matches!(done, ResumPoll::Ready(_)));
    if let ResumPoll::Ready(v) = done { assert_eq!(v, b'e' as usize); }
}
