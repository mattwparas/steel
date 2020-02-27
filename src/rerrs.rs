use thiserror::Error;

#[derive(Clone, Debug, Error)]
pub enum RucketErr {
    #[error("Arity Mismatch: {0}")]
    ArityMismatch(String),
    #[error("Expected Number, got {0}")]
    ExpectedNumber(String),
    #[error("Free Identifier: {0}")]
    FreeIdentifier(String),
    #[error("Expected Function: {0}")]
    ExpectedFunction(String),
    #[error("Expected identifier: {0}")]
    ExpectedIdentifier(String),
    #[error("Expected arguments to lambda: {0}")]
    ExpectedArgumentsToLambda(String),
    #[error("Expected List: {0}")]
    ExpectedList(String),
    #[error("Environment Not Found")]
    EnvironmentNotFound,
    #[error("Malformed Let statement")]
    MalformedLet,
    #[error("Unexpected Token {0}")]
    UnexpectedToken(String),
    #[error("Contract Violation: {0}")]
    ContractViolation(String),
    #[error("Bad Syntax: {0}")]
    BadSyntax(String),
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
    }; //     ($type:ident => $($thing:expr),+) => {
       //         return Err(SErr::$type($(($thing).into()),+));
       //     };
}
