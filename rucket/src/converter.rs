use crate::rerrs::RucketErr;
use crate::rvals::RucketVal;
use std::convert::TryFrom;
// the conversion layer works like
// Vec<RucketVal> -> your struct -> call the function -> output -> Rucketval output
// maybe TryFrom Error type should be something else?

pub trait RucketFunctor<U, V>
where
    U: TryFrom<Vec<RucketVal>, Error = RucketErr>,
    V: Into<RucketVal>,
{
    fn new_func() -> fn(args: Vec<RucketVal>) -> Result<RucketVal, RucketErr> {
        |args: Vec<RucketVal>| {
            let input = Self::in_convert(args)?;
            let res = Self::call(input)?;
            Ok(res.into())
        }
    }
    fn call(input: U) -> Result<V, RucketErr>;
    fn in_convert(input: Vec<RucketVal>) -> Result<U, RucketErr> {
        U::try_from(input)
    }
}
