use serde::{Deserialize, Serialize};

use crate::rvals::{SerializableSteelVal, SteelVal};

#[derive(Clone)]
pub struct LazyStream {
    pub initial_value: SteelVal, // argument to stream
    pub stream_thunk: SteelVal,  // function to get the next value
    pub empty_stream: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SerializableStream {
    pub initial_value: SerializableSteelVal,
    pub stream_thunk: SerializableSteelVal,
    pub empty_stream: bool,
}

impl LazyStream {
    // Perhaps do some error checking here in order to determine
    // if the arguments passed are actually valid
    pub fn new(initial_value: SteelVal, stream_thunk: SteelVal) -> Self {
        LazyStream {
            initial_value,
            stream_thunk,
            empty_stream: false,
        }
    }

    pub fn new_empty_stream() -> Self {
        LazyStream {
            initial_value: SteelVal::Void,
            stream_thunk: SteelVal::Void,
            empty_stream: true,
        }
    }

    // Should return the value in the `initial_value` field
    // is equivalent to calling (stream-first stream)
    pub fn stream_first(&self) -> SteelVal {
        self.initial_value.clone()
    }

    // `stream_thunk` should be a thunk that return the next `LazyStream`
    //  this should just return a new `LazyStream`
    pub fn stream_thunk(&self) -> SteelVal {
        self.stream_thunk.clone()
    }

    pub fn empty_stream(&self) -> SteelVal {
        SteelVal::BoolV(self.empty_stream)
    }
}
