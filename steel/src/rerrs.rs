use crate::parser::ParseError;
use std::convert::Infallible;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum SteelErr {
    #[error("Error: Arity Mismatch: {0}")]
    ArityMismatch(String),
    #[error("Error: Free Identifier: {0}")]
    FreeIdentifier(String),
    #[error("Error: Expected {0}")]
    TypeMismatch(String),
    #[error("Error: Unexpected Token {0}")]
    UnexpectedToken(String),
    #[error("Error: Contract Violation: {0}")]
    ContractViolation(String),
    #[error("Error: Bad Syntax: {0}")]
    BadSyntax(String),
    #[error("Error: Conversion Error: {0}")]
    ConversionError(String),
    #[error("Error: IO error")]
    Io(#[from] std::io::Error),
    #[error("Error: Parse error")]
    Parse(#[from] ParseError),
    #[error("Error: Infallible")]
    Infallible(#[from] Infallible),
}

#[macro_export]
macro_rules! stop {
    ($type:ident) => {
        return Err(SteelErr::$type);
    };
    ($type:ident => $fmt:expr, $($arg:tt)+) => {
        return Err(SteelErr::$type(format!($fmt, $($arg)+)));
    };
    ($type:ident => $thing:expr) => {
        return Err(SteelErr::$type(($thing).to_string()));
    };
}
