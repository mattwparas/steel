use crate::env::VOID;
use crate::gc::Gc;
use crate::rvals::SteelVal;

#[derive(Clone)]
pub struct LazyStream {
    pub initial_value: Gc<SteelVal>, // argument to stream
    pub stream_thunk: Gc<SteelVal>,  // function to get the next value
    pub empty_stream: bool,
}

impl LazyStream {
    // Perhaps do some error checking here in order to determine
    // if the arguments passed are actually valid
    pub fn new(initial_value: Gc<SteelVal>, stream_thunk: Gc<SteelVal>) -> Self {
        LazyStream {
            initial_value,
            stream_thunk,
            empty_stream: false,
        }
    }

    pub fn new_empty_stream() -> Self {
        LazyStream {
            initial_value: VOID.with(|x| Gc::clone(x)),
            stream_thunk: VOID.with(|x| Gc::clone(x)),
            empty_stream: true,
        }
    }

    // Should return the value in the `initial_value` field
    // is equivalent to calling (stream-first stream)
    pub fn stream_first(&self) -> Gc<SteelVal> {
        Gc::clone(&self.initial_value)
    }

    // `stream_thunk` should be a thunk that return the next `LazyStream`
    //  this should just return a new `LazyStream`
    pub fn stream_thunk(&self) -> Gc<SteelVal> {
        Gc::clone(&self.stream_thunk)
    }

    pub fn empty_stream(&self) -> Gc<SteelVal> {
        Gc::new(SteelVal::BoolV(self.empty_stream))
    }
}
