use std::fmt;
use thiserror::Error;
use Token::*;

#[derive(Clone, Debug, PartialEq, Error)]
pub enum TokenError {
    #[error("Unexpected char, {0}")]
    UnexpectedChar(char),
    #[error("Incomplete String")]
    IncompleteString,
    #[error("Invalid Escape")]
    InvalidEscape,
    #[error("Invalid Character")]
    InvalidCharacter,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    OpenParen,
    CloseParen,
    QuoteTick,
    CharacterLiteral(char),
    BooleanLiteral(bool),
    Identifier(String),
    NumberLiteral(f64),
    StringLiteral(String),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            OpenParen => write!(f, "("),
            CloseParen => write!(f, "("),
            CharacterLiteral(x) => write!(f, "#\\{}", x),
            BooleanLiteral(x) => write!(f, "#{}", x),
            Identifier(x) => write!(f, "{}", x),
            NumberLiteral(x) => write!(f, "{}", x),
            StringLiteral(x) => write!(f, "\"{}\"", x),
            QuoteTick => write!(f, "'"),
        }
    }
}
