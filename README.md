# resum

A library for multi shot coroutines in rust, implemented basically by passing around Rc<Fn(_) -> _>.

Have a look at tests for examples.

Limitations:
- `return` is borked
- `coyield!` only works toplevel in functions, and in if/matches
- Probably there are other things borked, I haven't used it much
