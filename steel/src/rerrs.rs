use crate::parser::ParseError;
use std::convert::Infallible;
use thiserror::Error;

pub struct Span {
    expr: String,
}

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
    #[error("Error: Parse error: {0}")]
    Parse(#[from] ParseError),
    #[error("Error: Infallible")]
    Infallible(#[from] Infallible),
    #[error("Error: Generic Error: {0}")]
    Generic(String),
}

impl PartialEq for SteelErr {
    fn eq(&self, other: &Self) -> bool {
        use SteelErr::*;
        match (self, other) {
            (ArityMismatch(l), ArityMismatch(r)) => l == r,
            (FreeIdentifier(l), FreeIdentifier(r)) => l == r,
            (TypeMismatch(l), TypeMismatch(r)) => l == r,
            (UnexpectedToken(l), UnexpectedToken(r)) => l == r,
            (ContractViolation(l), ContractViolation(r)) => l == r,
            (BadSyntax(l), BadSyntax(r)) => l == r,
            (ConversionError(l), ConversionError(r)) => l == r,
            (Generic(l), Generic(r)) => l == r,
            _ => false,
        }
    }
}

// // TODO add tests
// impl PartialEq for SteelVal {
//     fn eq(&self, other: &Self) -> bool {
//         match (self, other) {
//             (BoolV(l), BoolV(r)) => l == r,
//             (NumV(l), NumV(r)) => l == r,
//             (StringV(l), StringV(r)) => l == r,
//             (VectorV(l), VectorV(r)) => l == r,
//             (SymbolV(l), SymbolV(r)) => l == r,
//             (CharV(l), CharV(r)) => l == r,
//             //TODO
//             (_, _) => false, // (l, r) => {
//                              //     let left = unwrap!(l, usize);
//                              //     let right = unwrap!(r, usize);
//                              //     match (left, right) {
//                              //         (Ok(l), Ok(r)) => l == r,
//                              //         (_, _) => false,
//                              //     }
//                              // }
//         }
//     }
// }

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

#[macro_export]
macro_rules! throw {
    ($type:ident) => {
        || SteelErr::$type
    };
    ($type:ident => $fmt:expr, $($arg:tt)+) => {
        || SteelErr::$type(format!($fmt, $($arg)+))
    };
    ($type:ident => $thing:expr) => {
        || SteelErr::$type(($thing).to_string())
    };
}
