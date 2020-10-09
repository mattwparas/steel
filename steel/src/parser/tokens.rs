use core::ops;
use std::fmt;
use thiserror::Error;
use TokenType::*;

use logos::{Lexer, Logos};

use crate::parser::span::Span;

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

fn gen_bool(lex: &mut Lexer<TokenType>) -> Option<bool> {
    let slice = lex.slice();
    match slice {
        "#t" | "#true" => Some(true),
        "#f" | "#false" => Some(false),
        _ => None,
    }
}

fn parse_char(lex: &mut Lexer<TokenType>) -> Option<char> {
    let slice = lex.slice();
    match slice {
        "#\\SPACE" => Some(' '),
        character if character.starts_with("#\\") => match slice.len() {
            3 | 4 | 5 => slice.chars().last(),
            _ => None,
        },
        _ => None,
    }
}

fn parse_str(lex: &mut Lexer<TokenType>) -> Option<String> {
    let slice = lex.slice();
    // println!("Slice: {:?}", slice);

    // Trim off the start and end of the string
    // We don't need that inside the lexer at all
    let end = slice.trim_end_matches("\"");
    let new = end.trim_start_matches("\"");

    // Some(unescape(slice))
    Some(new.to_string())
}

// TODO the character parsing is not quite right
// need to make sure that we can handle cases like "#\SPACE" or "#\a" but not "#\applesauce"
#[derive(Logos, Clone, Debug, PartialEq)]
pub enum TokenType {
    #[token("(")]
    #[token("[")]
    #[token("{")]
    OpenParen,
    #[token(")")]
    #[token("]")]
    #[token("}")]
    CloseParen,
    #[token("'")]
    QuoteTick,
    #[token("`")]
    QuasiQuote,
    #[token(",")]
    Unquote,
    #[token(",@")]
    UnquoteSplice,
    #[token("#")]
    Hash,
    #[token("#\\SPACE", |_| Some(' '))]
    #[regex(r"#\\\p{L}", parse_char)]
    CharacterLiteral(char),

    #[regex(";[^\r\n]*", priority = 2)] // "
    #[regex(";[^\n]*", priority = 1)] // "
    Comment,

    #[token("#true", gen_bool)]
    #[token("#false", gen_bool)]
    #[token("#t", gen_bool)]
    #[token("#f", gen_bool)]
    BooleanLiteral(bool),

    // /// An identifier literal.
    // #[regex(r#"(?&ident)"#)]
    // Identifier(String),
    #[regex(r#"[_\+\-\*\x2F%\&\|!?\~<>=@\.\p{XID_Start}\p{Emoji_Presentation}]['_\+\-\*\x2F%\&\|!?\~<>=@\.\p{XID_Continue}\p{Emoji_Presentation}]*"#, callback = |lex| lex.slice().parse())]
    Identifier(String),

    // #[token("inf")]
    // #[token("NaN")]
    #[regex(r#"[+-]?[0-9][0-9_]*\.[0-9][0-9_]*([eE][+-]?[0-9][0-9_]*)?"#, |lex| lex.slice().parse())] // "
    #[regex(
        r#"[+-]?0x[0-9a-fA-F][0-9a-fA-F_]*\.[0-9a-fA-F][0-9a-fA-F_]*([pP][+-]?[0-9][0-9_]?)?"#, |lex| lex.slice().parse()
    )]
    #[regex(r#"[+-]?[0-9][0-9_]*\."#, |lex| lex.slice().parse())]
    NumberLiteral(f64),

    #[regex("[+-]?[0-9][0-9_]*", priority = 2, callback = |lex| lex.slice().parse())] // "
    #[regex("[+-]?0b[0-1][0-1_]*", |lex| lex.slice().parse())] // "
    #[regex("[+-]?0x[0-9a-fA-F][0-9a-fA-F_]*", |lex| lex.slice().parse())] // "
    IntegerLiteral(isize),

    // #[regex(r#"b?"(\\.|[^\\"])*""#, parse_str)] // "
    // #[regex(r#"(?:[^"]|\\")*", parse_str)] // "
    #[regex(r#""([^"\\]|\\t|\\u|\\n|\\")*""#, parse_str)]
    StringLiteral(String),

    #[error]
    #[regex(r"[ \t\n\f]+", logos::skip)] // "
    Error,
}

impl fmt::Display for TokenType {
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
            Unquote => write!(f, ","),
            QuasiQuote => write!(f, "`"),
            UnquoteSplice => write!(f, ",@"),
            Error => write!(f, "error"),
            Comment => write!(f, ""),
            Hash => write!(f, "#"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token<'a> {
    pub(crate) ty: TokenType,
    pub(crate) source: &'a str,
    pub(crate) span: Span,
}

impl<'a> Token<'a> {
    pub const fn new(ty: TokenType, source: &'a str, range: ops::Range<usize>) -> Self {
        Self {
            ty,
            source,
            span: Span::new(range.start, range.end),
        }
    }

    // pub const fn ty(&self) -> TokenType {
    //     self.ty
    // }

    pub const fn span(&self) -> Span {
        self.span
    }

    pub const fn range(&self) -> ops::Range<usize> {
        self.span.start()..self.span.end()
    }

    pub const fn source(&self) -> &'a str {
        self.source
    }
}

impl Into<Span> for Token<'_> {
    fn into(self) -> Span {
        self.span()
    }
}

impl Into<Span> for &Token<'_> {
    fn into(self) -> Span {
        self.span()
    }
}

impl<'a> Into<ops::Range<usize>> for Token<'a> {
    fn into(self) -> ops::Range<usize> {
        self.span.into()
    }
}

impl<'a> Into<ops::Range<usize>> for &Token<'a> {
    fn into(self) -> ops::Range<usize> {
        self.span.into()
    }
}

impl<'a> Into<(usize, usize)> for Token<'a> {
    fn into(self) -> (usize, usize) {
        self.span.into()
    }
}

impl<'a> Into<(usize, usize)> for &Token<'a> {
    fn into(self) -> (usize, usize) {
        self.span.into()
    }
}

impl<'a> Into<[usize; 2]> for Token<'a> {
    fn into(self) -> [usize; 2] {
        self.span.into()
    }
}

impl<'a> Into<[usize; 2]> for &Token<'a> {
    fn into(self) -> [usize; 2] {
        self.span.into()
    }
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} @ {:?}", self.source, self.span)
    }
}
