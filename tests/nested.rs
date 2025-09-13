use resum::{resum, Resum, ResumPoll};

#[resum(yield_ty = i32, resume_ty = i32)]
fn if_example(cond: bool) -> i32 {
    if cond {
        let v = coyield!(1);
        v
    } else {
        0
    }
}

#[test]
fn nested_if() {
    let mut c = if_example(true);
    assert!(matches!(c.start(), ResumPoll::Yield(1)));
    assert!(matches!(c.resume(5), ResumPoll::Ready(5)));
}

#[resum(yield_ty = i32, resume_ty = i32)]
fn match_example(opt: Option<i32>) -> i32 {
    match opt {
        Some(n) => {
            let v = if n >= 0 {
                coyield!(n)
            } else {
                5
            };
            v
        }
        None => 0,
    }
}

#[test]
fn nested_match() {
    let mut c = match_example(Some(2));
    assert!(matches!(c.start(), ResumPoll::Yield(2)));
    assert!(matches!(c.resume(3), ResumPoll::Ready(3)));
}
