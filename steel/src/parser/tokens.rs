use std::fmt;
use thiserror::Error;
use Token::*;

#[derive(Clone, Debug, PartialEq, Error)]
pub enum TokenError {
    #[error("Unexpected char, {0} on line: {1}")]
    UnexpectedChar(char, usize),
    #[error("Incomplete String on line {0}")]
    IncompleteString(usize),
    #[error("Invalid Escape on line {0}")]
    InvalidEscape(usize),
    #[error("Invalid Character on line {0}")]
    InvalidCharacter(usize),
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
    IntegerLiteral(isize),
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
            NumberLiteral(x) => write!(f, "{:?}", x),
            IntegerLiteral(x) => write!(f, "{}", x),
            StringLiteral(x) => write!(f, "\"{}\"", x),
            QuoteTick => write!(f, "'"),
        }
    }
}
