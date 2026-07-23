use serde::{Deserialize, Serialize};

use crate::rvals::{SerializableSteelVal, SteelVal, SteelValGeneric};

pub struct LazyStream<A: crate::gc::Allocator + Clone + Send + Sync + 'static = crate::gc::Global> {
    pub initial_value: SteelValGeneric<A>, // argument to stream
    pub stream_thunk: SteelValGeneric<A>,  // function to get the next value
    pub empty_stream: bool,
}

impl<A: crate::gc::Allocator + Clone + Send + Sync + 'static> Clone for LazyStream<A> {
    fn clone(&self) -> Self {
        LazyStream {
            initial_value: self.initial_value.clone(),
            stream_thunk: self.stream_thunk.clone(),
            empty_stream: self.empty_stream,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SerializableStream {
    pub initial_value: SerializableSteelVal,
    pub stream_thunk: SerializableSteelVal,
    pub empty_stream: bool,
}

impl<A: crate::gc::Allocator + Clone + Send + Sync + 'static> LazyStream<A> {
    // Perhaps do some error checking here in order to determine
    // if the arguments passed are actually valid
    pub fn new(initial_value: SteelValGeneric<A>, stream_thunk: SteelValGeneric<A>) -> Self {
        LazyStream {
            initial_value,
            stream_thunk,
            empty_stream: false,
        }
    }

    pub fn new_empty_stream() -> Self {
        LazyStream {
            initial_value: SteelValGeneric::Void,
            stream_thunk: SteelValGeneric::Void,
            empty_stream: true,
        }
    }

    // Should return the value in the `initial_value` field
    // is equivalent to calling (stream-first stream)
    pub fn stream_first(&self) -> SteelValGeneric<A> {
        self.initial_value.clone()
    }

    // `stream_thunk` should be a thunk that return the next `LazyStream`
    //  this should just return a new `LazyStream`
    pub fn stream_thunk(&self) -> SteelValGeneric<A> {
        self.stream_thunk.clone()
    }

    pub fn empty_stream(&self) -> SteelVal {
        SteelVal::BoolV(self.empty_stream)
    }
}
