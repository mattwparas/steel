use crate::gc::Gc;
use crate::rerrs::SteelErr;
use crate::rvals::SteelVal::*;
use crate::rvals::{Result, SteelVal};
use crate::stop;

use crate::rvals::Transducer;
use crate::rvals::Transducers;

pub struct TransducerOperations {}
impl TransducerOperations {
    pub fn compose() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            let mut transformers = Transducer::new();
            for transducer in args {
                if let IterV(t) = transducer.as_ref() {
                    transformers.append(t.clone());
                } else {
                    stop!(TypeMismatch => "compose only accepts transducers")
                }
            }

            Ok(Gc::new(SteelVal::IterV(transformers)))

            // unimplemented!()
        })
    }

    pub fn map() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() != 1 {
                stop!(ArityMismatch => "mapping takes one argument");
            }

            match &args[0].as_ref() {
                Closure(_) | FuncV(_) | StructClosureV(_, _) => {
                    let mut transducer = Transducer::new();
                    transducer.push(Transducers::Map(Gc::clone(&args[0])));
                    Ok(Gc::new(SteelVal::IterV(transducer)))
                }
                _ => stop!(TypeMismatch => "mapping expects a function"),
            }
        })
    }

    pub fn filter() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() != 1 {
                stop!(ArityMismatch => "filtering takes one argument");
            }

            match &args[0].as_ref() {
                Closure(_) | FuncV(_) | StructClosureV(_, _) => {
                    let mut transducer = Transducer::new();
                    transducer.push(Transducers::Filter(Gc::clone(&args[0])));
                    Ok(Gc::new(SteelVal::IterV(transducer)))
                }
                _ => stop!(TypeMismatch => "filtering expects a function"),
            }
        })
    }

    pub fn take() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() != 1 {
                stop!(ArityMismatch => "taking takes one argument");
            }

            if let IntV(_) = &args[0].as_ref() {
                let mut transducer = Transducer::new();
                transducer.push(Transducers::Take(Gc::clone(&args[0])));
                Ok(Gc::new(SteelVal::IterV(transducer)))
            } else {
                stop!(TypeMismatch => "taking expects an integer")
            }
        })
    }

    pub fn transducer_construct() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            Ok(Gc::new(SteelVal::VectorV(
                args.into_iter().map(Gc::clone).collect(),
            )))
        })
    }
}
