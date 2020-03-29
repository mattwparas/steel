use crate::parser::ParseError;
use colored::*;
use std::convert::Infallible;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum SteelErr {
    #[error("{}: Arity Mismatch: {0}", "Error".bright_red())]
    ArityMismatch(String),
    #[error("{}: Free Identifier: {}", "Error".bright_red().bold(), .0.bright_red())]
    FreeIdentifier(String),
    #[error("{}: Expected {0}", "Error".bright_red())]
    TypeMismatch(String),
    #[error("{}: Unexpected Token {0}", "Error".bright_red())]
    UnexpectedToken(String),
    #[error("{}: Contract Violation: {0}", "Error".bright_red())]
    ContractViolation(String),
    #[error("{}: Bad Syntax: {0}", "Error".bright_red())]
    BadSyntax(String),
    #[error("{}: Conversion Error: {0}", "Error".bright_red())]
    ConversionError(String),
    #[error("{}: IO error", "Error".bright_red())]
    Io(#[from] std::io::Error),
    #[error("{}: Parse error", "Error".bright_red())]
    Parse(#[from] ParseError),
    #[error("{}: Infallible", "Error".bright_red())]
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
