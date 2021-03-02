// use std::convert::TryFrom;

use crate::rerrs::{ErrorKind, SteelErr};
use crate::rvals::TryCast;
use crate::rvals::{Result, SteelVal};
use crate::stop;
use std::any::Any;
use std::rc::Rc;

pub type BoxedFunctionSignature = Rc<dyn Fn(&[SteelVal]) -> Result<SteelVal>>;

// #[derive(Clone)]
// pub struct BoxedFunction<T: Fn(&[SteelVal]) -> Result<SteelVal>>(T);

pub trait Embeddable {
    fn to_function(self, name: &'static str) -> BoxedFunctionSignature;
}

// pub type UnboxedFunctionSignature = dyn Fn(&[SteelVal]) -> Result<SteelVal>;

impl<T> Embeddable for fn(T) -> T
where
    T: TryCast<SteelVal, Error = SteelErr> + Into<SteelVal> + Clone + std::fmt::Debug + Any,
{
    fn to_function(self, name: &'static str) -> BoxedFunctionSignature {
        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 1 {
                stop!(ArityMismatch => format!("{} expected 1 argument, got {}", name, args.len()));
            }

            let res = self(T::try_cast(args[0].clone())?);

            Ok(res.into())
        };
        Rc::new(f)
    }
}

// impl<T> Embeddable for fn(T, T) -> T
// where
//     T: TryFrom<SteelVal, Error = SteelErr>
//         + Into<SteelVal>
//         + From<T>
//         + Clone
//         + std::fmt::Debug
//         + 'static,
// {
//     fn to_function(self, name: &'static str) -> BoxedFunctionSignature {
//         let f = move |args: &[SteelVal]| -> Result<SteelVal> {
//             if args.len() != 1 {
//                 stop!(ArityMismatch => format!("{} expected 2 arguments, got {}", name, args.len()));
//             }

//             let res = self(T::try_from(args[0].clone())?, T::try_from(args[1].clone())?);

//             Ok(SteelVal::try_from(res)?)
//         };
//         Box::new(f)
//     }
// }
