use crate::rerrs::RucketErr;
use crate::rvals::RucketVal;
use std::convert::TryFrom;
use thiserror::Error;
// the conversion layer works like
// Vec<RucketVal> -> your struct -> call the function -> output -> Rucketval output
// maybe TryFrom Error type should be something else?
#[derive(Debug, Error)]
pub enum ConversionError {
    #[error("Infallible")]
    Infallible(#[from] std::convert::Infallible),
    #[error("{0}")]
    Generic(String),
}
pub trait RucketFunctor<U, V>
where
    U: TryFrom<Vec<RucketVal>, Error = ConversionError>,
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
        U::try_from(input).map_err(|e| RucketErr::ConversionError(e.to_string()))
    }
}
