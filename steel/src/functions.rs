use crate::rerrs::{ErrorKind, SteelErr};
// use crate::rvals::TryCast;
use crate::rvals::{Result, SteelVal};
use crate::stop;
use std::any::Any;
use std::rc::Rc;

pub type BoxedFunctionSignature = Rc<dyn Fn(&[SteelVal]) -> Result<SteelVal>>;
