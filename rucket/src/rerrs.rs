use thiserror::Error;

#[derive(Debug, Error)]
pub enum RucketErr {
    #[error("Arity Mismatch: {0}")]
    ArityMismatch(String),
    #[error("Free Identifier: {0}")]
    FreeIdentifier(String),
    #[error("Expected {0}")]
    TypeMismatch(String),
    #[error("Unexpected Token {0}")]
    UnexpectedToken(String),
    #[error("Contract Violation: {0}")]
    ContractViolation(String),
    #[error("Bad Syntax: {0}")]
    BadSyntax(String),
    #[error("Conversion Error: {0}")]
    ConversionError(String),
    #[error("IO error")]
    Io(#[from] std::io::Error),
}

#[macro_export]
macro_rules! stop {
    ($type:ident) => {
        return Err(RucketErr::$type);
    };
    ($type:ident => $fmt:expr, $($arg:tt)+) => {
        return Err(RucketErr::$type(format!($fmt, $($arg)+)));
    };

    ($type:ident => $thing:expr) => {
        return Err(RucketErr::$type(($thing).to_string()));
    };
}
