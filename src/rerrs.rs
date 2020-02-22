use thiserror::Error;

#[derive(Clone, Debug, Error)]
pub enum RucketErr {
    #[error("Arity Mismatch")]
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
    #[error("Environment Not Found")]
    EnvironmentNotFound,
    #[error("Malformed Let statement")]
    MalformedLet,
}
