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

    #[inline(always)]
    pub fn empty_stream() -> SteelVal {
        SteelVal::StreamV(LazyStream::new_empty_stream())
    }

    pub fn stream_empty_huh() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() != 1 {
                stop!(ArityMismatch => "stream-empty takes 1 argument")
            }
            if let SteelVal::StreamV(s) = &args[0].as_ref() {
                Ok(s.empty_stream())
            } else {
                stop!(TypeMismatch => "stream-empty? takes a stream")
            }
        })
    }

    pub fn stream_car() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() != 1 {
                stop!(ArityMismatch => "stream-car takes 1 argument")
            }
            if let SteelVal::StreamV(s) = &args[0].as_ref() {
                Ok(s.stream_first())
            } else {
                stop!(TypeMismatch => "stream-car takes a stream")
            }
        })
    }

    pub fn stream_cdr() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() != 1 {
                stop!(ArityMismatch => "stream-cdr takes 1 argument")
            }
            if let SteelVal::StreamV(s) = &args[0].as_ref() {
                Ok(s.stream_thunk())
            } else {
                stop!(TypeMismatch => "stream-cdr takes a stream")
            }
        })
    }
}
