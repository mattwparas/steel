use crate::rvals::SteelVal::*;
use crate::rvals::{Result, SteelVal};
use crate::stop;
use crate::{core::utils::declare_const_ref_functions, gc::Gc};

use crate::values::transducers::Transducer;
use crate::values::transducers::Transducers;

declare_const_ref_functions!(
    COMPOSE => compose,
    ENUMERATING => enumerating,
    ZIPPING => zipping,
    INTERLEAVING => interleaving,
    MAPPING => map,
    EXTENDING => extending,
    FLAT_MAPPING => flat_map,
    FLATTENING => flatten,
    FILTERING => filter,
    TAKING => take,
    DROPPING => dropping,
);

pub fn compose(args: &[SteelVal]) -> Result<SteelVal> {
    let mut transformers = Transducer::new();
    for transducer in args {
        if let IterV(t) = transducer {
            transformers.append(t.unwrap());
        } else {
            stop!(TypeMismatch => "compose only accepts transducers")
        }
    }

    Ok(SteelVal::IterV(Gc::new(transformers)))
}

pub fn enumerating(args: &[SteelVal]) -> Result<SteelVal> {
    if args.len() != 0 {
        stop!(ArityMismatch => "enumerating takes no arguments");
    }

    let mut transducer = Transducer::new();
    transducer.push(Transducers::Enumerating);
    Ok(SteelVal::IterV(Gc::new(transducer)))
}

pub fn zipping(args: &[SteelVal]) -> Result<SteelVal> {
    if args.len() != 1 {
        stop!(ArityMismatch => "zipping takes one argument");
    }

    match &args[0] {
        VectorV(_) | StreamV(_) | StringV(_) | ListV(_) | StructV(_) | HashSetV(_)
        | HashMapV(_) => {
            let mut transducer = Transducer::new();
            transducer.push(Transducers::Zipping(args[0].clone()));
            Ok(SteelVal::IterV(Gc::new(transducer)))
        }
        v => {
            stop!(TypeMismatch => format!("zipping expects an iterable, found: {:?}", v))
        }
    }
}

pub fn interleaving(args: &[SteelVal]) -> Result<SteelVal> {
    if args.len() != 1 {
        stop!(ArityMismatch => "interleaving takes one argument");
    }

    match &args[0] {
        VectorV(_) | StreamV(_) | StringV(_) | ListV(_) | StructV(_) | HashSetV(_)
        | HashMapV(_) => {
            let mut transducer = Transducer::new();
            transducer.push(Transducers::Interleaving(args[0].clone()));
            Ok(SteelVal::IterV(Gc::new(transducer)))
        }
        v => {
            stop!(TypeMismatch => format!("interleaving expects an iterable, found: {:?}", v))
        }
    }
}

pub fn map(args: &[SteelVal]) -> Result<SteelVal> {
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
}

pub fn extending(args: &[SteelVal]) -> Result<SteelVal> {
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
}

pub fn flat_map(args: &[SteelVal]) -> Result<SteelVal> {
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
}

pub fn flatten(args: &[SteelVal]) -> Result<SteelVal> {
    if args.len() != 0 {
        stop!(ArityMismatch => "flattening takes no arguments");
    }

    let mut transducer = Transducer::new();
    transducer.push(Transducers::Flatten);
    Ok(SteelVal::IterV(Gc::new(transducer)))
}

pub fn filter(args: &[SteelVal]) -> Result<SteelVal> {
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
}

pub fn take(args: &[SteelVal]) -> Result<SteelVal> {
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
}

pub fn dropping(args: &[SteelVal]) -> Result<SteelVal> {
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
}
// }
