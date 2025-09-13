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
pub trait Resum {
    type Yield;
    type Resume;
    type Output;

    /// Starts execution from the beginning until the first yield or completion.
    fn start(&mut self) -> ResumPoll<Self::Yield, Self::Output>;

    /// Resumes from the last suspension point with a value.
    ///
    /// Accepts any type that can convert into `Self::Resume`.
    fn resume<V>(&mut self, value: V) -> ResumPoll<Self::Yield, Self::Output>
    where
        Self::Resume: From<V>;
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
        Entry(Option<Box<dyn FnMut() -> Continuation<'a, Y, R, O> + 'a>>),
        Next(Box<dyn Fn(R) -> Continuation<'a, Y, R, O> + 'a>),
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
        F: FnMut() -> Continuation<'a, Y, R, O> + 'a,
    {
        State::Entry(Some(Box::new(f)))
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
        F: FnMut() -> __rt::Continuation<'a, Y, R, O> + 'a,
    {
        Self {
            state: __rt::entry(start),
        }
    }
}

impl<'a, Y, R, O> Resum for Coroutine<'a, Y, R, O> {
    type Yield = Y;
    type Resume = R;
    type Output = O;

    fn start(&mut self) -> ResumPoll<Self::Yield, Self::Output> {
        use __rt::{Continuation, State};
        match &mut self.state {
            State::Entry(f_opt) => {
                let mut f = f_opt
                    .take()
                    .expect("start() called more than once without resuming");
                match f() {
                    Continuation::Done(output) => {
                        self.state = State::Done;
                        ResumPoll::Ready(output)
                    }
                    Continuation::Yield { value, next } => {
                        self.state = State::Next(next);
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

    fn resume<V>(&mut self, value: V) -> ResumPoll<Self::Yield, Self::Output>
    where
        Self::Resume: From<V>,
    {
        use __rt::{Continuation, State};
        // Call the stored continuation without consuming it to allow multi-shot.
        let cont = match &self.state {
            State::Next(next) => next(<Self::Resume as From<V>>::from(value)),
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
                // We reached a deeper suspension point; advance the stored continuation
                // to this new point so further resumes continue from here.
                self.state = State::Next(next);
                ResumPoll::Yield(value)
            }
        }
    }
}

/// Convenience free function to resume a coroutine with a value whose type
/// determines the coroutine's `Resume` associated type via inference.
pub fn resume<T, R>(co: &mut T, value: R) -> ResumPoll<T::Yield, T::Output>
where
    T: Resum,
    T::Resume: From<R>,
{
    Resum::resume(co, value)
}

// Re-export the attribute macro for convenience.
pub use resum_macros::resum;
