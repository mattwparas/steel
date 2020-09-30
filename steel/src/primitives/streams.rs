use crate::gc::Gc;
use crate::rerrs::SteelErr;
use crate::rvals::{Result, SteelVal};
use crate::stop;

// use crate::rvals::Transducer;
// use crate::rvals::Transducers;

use crate::lazy_stream::LazyStream;

pub struct StreamOperations {}
impl StreamOperations {
    pub fn stream_cons() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() != 2 {
                stop!(ArityMismatch => "stream-cons requires 2 argments")
            }

            if let SteelVal::Closure(_) = &args[1].as_ref() {
                let initial_value = Gc::clone(&args[0]);
                let stream_thunk = Gc::clone(&args[1]);
                Ok(Gc::new(SteelVal::StreamV(LazyStream::new(
                    initial_value,
                    stream_thunk,
                ))))
            } else {
                stop!(TypeMismatch => "stream-cons takes a function in the second position")
            }
        })
    }
}
