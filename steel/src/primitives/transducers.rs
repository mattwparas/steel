use crate::gc::Gc;
use crate::rvals::SteelVal::*;
use crate::rvals::{Result, SteelVal};
use crate::stop;

use crate::values::transducers::Transducer;
use crate::values::transducers::Transducers;

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
                Closure(_)
                | FuncV(_)
                | BoxedFunction(_)
                | ContractedFunction(_)
                | BuiltIn(_)
                | MutFunc(_) => {
                    let mut transducer = Transducer::new();
                    transducer.push(Transducers::Map(args[0].clone()));
                    Ok(SteelVal::IterV(Gc::new(transducer)))
                }
                v => stop!(TypeMismatch => format!("mapping expects a function, found: {:?}", v)),
            }
        })
    }

    pub fn extending() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 1 {
                stop!(ArityMismatch => "extending takes one argument");
            }

            match &args[0] {
                VectorV(_) | StreamV(_) | StringV(_) | ListV(_) | StructV(_) | HashSetV(_)
                | HashMapV(_) => {
                    let mut transducer = Transducer::new();
                    transducer.push(Transducers::Extend(args[0].clone()));
                    Ok(SteelVal::IterV(Gc::new(transducer)))
                }
                v => {
                    stop!(TypeMismatch => format!("extending expects an iterable, found: {:?}", v))
                }
            }
        })
    }

    pub fn flat_map() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 1 {
                stop!(ArityMismatch => "mapping takes one argument");
            }

            match &args[0] {
                Closure(_)
                | FuncV(_)
                | BoxedFunction(_)
                | ContractedFunction(_)
                | BuiltIn(_)
                | MutFunc(_) => {
                    let mut transducer = Transducer::new();
                    transducer.push(Transducers::FlatMap(args[0].clone()));
                    Ok(SteelVal::IterV(Gc::new(transducer)))
                }
                v => {
                    stop!(TypeMismatch => format!("flat-mapping expects a function, found: {:?}", v))
                }
            }
        })
    }

    pub fn flatten() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 0 {
                stop!(ArityMismatch => "flattening takes no arguments");
            }

            let mut transducer = Transducer::new();
            transducer.push(Transducers::Flatten);
            Ok(SteelVal::IterV(Gc::new(transducer)))
        })
    }

    pub fn filter() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 1 {
                stop!(ArityMismatch => "filtering takes one argument");
            }

            match &args[0] {
                Closure(_)
                | FuncV(_)
                | BoxedFunction(_)
                | ContractedFunction(_)
                | BuiltIn(_)
                | MutFunc(_) => {
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

    pub fn dropping() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 1 {
                stop!(ArityMismatch => "dropping takes one argument");
            }

            if let IntV(_) = &args[0] {
                let mut transducer = Transducer::new();
                transducer.push(Transducers::Drop(args[0].clone()));
                Ok(SteelVal::IterV(Gc::new(transducer)))
            } else {
                stop!(TypeMismatch => "dropping expects an integer")
            }
        })
    }
}
