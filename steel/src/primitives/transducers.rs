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
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            let mut transformers = Transducer::new();
            for transducer in args {
                if let IterV(t) = transducer {
                    transformers.append(t.unwrap());
                } else {
                    stop!(TypeMismatch => "compose only accepts transducers")
                }
            }

            Ok(SteelVal::IterV(Gc::new(transformers)))
        })
    }

    pub fn map() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 1 {
                stop!(ArityMismatch => "mapping takes one argument");
            }

            match &args[0] {
                Closure(_) | FuncV(_) | StructClosureV(_) => {
                    let mut transducer = Transducer::new();
                    transducer.push(Transducers::Map(args[0].clone()));
                    Ok(SteelVal::IterV(Gc::new(transducer)))
                }
                _ => stop!(TypeMismatch => "mapping expects a function"),
            }
        })
    }

    pub fn filter() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 1 {
                stop!(ArityMismatch => "filtering takes one argument");
            }

            match &args[0] {
                Closure(_) | FuncV(_) | StructClosureV(_) => {
                    let mut transducer = Transducer::new();
                    transducer.push(Transducers::Filter(args[0].clone()));
                    Ok(SteelVal::IterV(Gc::new(transducer)))
                }
                _ => stop!(TypeMismatch => "filtering expects a function"),
            }
        })
    }

    pub fn take() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 1 {
                stop!(ArityMismatch => "taking takes one argument");
            }

            if let IntV(_) = &args[0] {
                let mut transducer = Transducer::new();
                transducer.push(Transducers::Take(args[0].clone()));
                Ok(SteelVal::IterV(Gc::new(transducer)))
            } else {
                stop!(TypeMismatch => "taking expects an integer")
            }
        })
    }

    pub fn transducer_construct() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            Ok(SteelVal::VectorV(Gc::new(
                args.into_iter().cloned().collect(),
            )))
        })
    }
}
