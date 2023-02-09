use core::ops;
use std::fmt;
use TokenType::*;

use logos::{Lexer, Logos};

use crate::parser::span::Span;

use serde::{Deserialize, Serialize};
use std::str::FromStr;

use std::convert::TryFrom;
use std::num::ParseIntError;

use crate::parser::parser::SourceId;

fn gen_bool(lex: &mut Lexer<TokenType>) -> Option<bool> {
    let slice = lex.slice();
    match slice {
        "#t" | "#true" => Some(true),
        "#f" | "#false" => Some(false),
        _ => None,
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DecodeHexError {
    OddLength,
    ParseInt(ParseIntError),
}

impl From<ParseIntError> for DecodeHexError {
    fn from(e: ParseIntError) -> Self {
        DecodeHexError::ParseInt(e)
    }
}

impl fmt::Display for DecodeHexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DecodeHexError::OddLength => "input string has an odd number of bytes".fmt(f),
            DecodeHexError::ParseInt(e) => e.fmt(f),
        }
    }
}

impl std::error::Error for DecodeHexError {}

pub fn decode_hex(s: &str) -> Result<Vec<u8>, DecodeHexError> {
    if s.len() % 2 != 0 {
        Err(DecodeHexError::OddLength)
    } else {
        (0..s.len())
            .step_by(2)
            .map(|i| u8::from_str_radix(&s[i..i + 2], 16).map_err(|e| e.into()))
            .collect()
    }
}

fn parse_unicode_str(slice: &str) -> Option<char> {
    if slice.starts_with("#\\\\u") && slice.contains('{') && slice.contains('}') {
        let rest = slice
            .trim_start_matches("#\\\\u")
            .trim_start_matches('{')
            .trim_end_matches('}')
            .to_lowercase();

        let rest = match rest.len() {
            0 => panic!("length of 0"),
            1 => "000".to_string() + &rest,
            2 => "00".to_string() + &rest,
            3 => "0".to_string() + &rest,
            _ => return None,
        };

        let decoded: u8 = decode_hex(&rest).ok()?.into_iter().sum();
        let uinitial: u32 = decoded.into();
        let result = char::try_from(uinitial).ok();

        println!("{result:?}");
        result
    } else {
        None
    }
}

fn parse_char(lex: &mut Lexer<TokenType>) -> Option<char> {
    let slice = lex.slice();

    match slice {
        "#\\SPACE" => Some(' '),
        character if character.starts_with("#\\") => {
            let parsed_unicode = parse_unicode_str(character);

            if parsed_unicode.is_some() {
                return parsed_unicode;
            }
            char::from_str(character.trim_start_matches("#\\")).ok()
        }
        _ => None,
    }
}

fn parse_str(lex: &mut Lexer<TokenType>) -> Option<String> {
    let slice = lex.slice();
    // println!("Slice: {:?}", slice);

    // Trim off the start and end of the string
    // We don't need that inside the lexer at all
    slice
        .strip_suffix('\"')
        .or(Some(slice))
        .and_then(|x| x.strip_prefix('\"'))
        .or(Some(slice))
        .map(|x| x.replace("\\\"", "\""))
    // .map(|x| x.to_string())
}

// TODO the character parsing is not quite right
// need to make sure that we can handle cases like "#\SPACE" or "#\a" but not "#\applesauce"
#[derive(Logos, Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum TokenType {
    #[token("(")]
    #[token("[")]
    // #[token("{")]
    OpenParen,
    #[token(")")]
    #[token("]")] // "
    // #[token("}")]
    CloseParen,
    #[token("'")]
    QuoteTick,
    #[token("`")]
    QuasiQuote,
    #[token(",")]
    Unquote,
    #[token(",@")]
    UnquoteSplice,

    #[token("if")]
    If,
    #[regex("(define)|(defn)")]
    Define,
    #[token("let")]
    Let,

    #[token("%plain-let")]
    TestLet,
    // #[token("transduce")]
    // Transduce,
    // #[token("execute")]
    // Execute,
    #[token("return!")]
    Return,
    #[token("begin")]
    Begin,
    // #[token("panic!")]
    // Panic,
    #[regex("(lambda)|(fn)|(λ)|(#%plain-lambda)")]
    Lambda,
    #[token("quote")]
    Quote,

    #[token("syntax-rules")]
    SyntaxRules,
    #[token("define-syntax")]
    DefineSyntax,
    #[token("...")]
    Ellipses,

    // #[token("apply")]
    // Apply,
    #[token("set!")]
    Set,

    #[token("require")]
    Require,

    #[token("#\\SPACE", |_| Some(' '))]
    #[token("#\\space", |_| Some(' '))]
    #[token("#\\\\", |_| Some('\\'))]
    #[token("#\\tab", |_| Some('\t'))]
    #[token("#\\TAB", |_| Some('\t'))]
    #[token("#\\NEWLINE", |_| Some('\n'))]
    #[token("#\\newline", |_| Some('\n'))]
    #[token("#\\return", |_| Some('\r'))]
    #[token("#\\RETURN", |_| Some('\r'))]
    #[token("#\\)", |_| Some(')'))]
    #[token("#\\]", |_| Some(']'))] // "
    #[token("#\\^", |_| Some('^'))]
    #[regex(r"#\\[^\s^\)^\]]+", parse_char, priority = 1)] // "
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
    #[regex(r#"[_:\+\-\*\x2F%\&\|!?\~<>=@\.\p{XID_Start}\p{Emoji_Presentation}]['_:\+\-\*\x2F%\&\|!?\~<>=@\.\p{XID_Continue}\p{Emoji_Presentation}]*"#, callback = |lex| lex.slice().parse())]
    // "
    Identifier(String),

    #[regex(r#"#:[_:\+\-\*\x2F%\&\|!?\~<>=@\.\p{XID_Start}\p{Emoji_Presentation}]['_:\+\-\*\x2F%\&\|!?\~<>=@\.\p{XID_Continue}\p{Emoji_Presentation}]*"#, callback = |lex| lex.slice().parse())]
    Keyword(String),

    // #[token("inf")]
    // #[token("NaN")]
    #[regex(r#"[+-]?[0-9][0-9_]*\.[0-9][0-9_]*([eE][+-]?[0-9][0-9_]*)?"#, |lex| lex.slice().parse())] // "
    #[regex(
        r#"[+-]?0x[0-9a-fA-F][0-9a-fA-F_]*\.[0-9a-fA-F][0-9a-fA-F_]*([pP][+-]?[0-9][0-9_]?)?"#, |lex| lex.slice().parse()
    )]
    #[regex(r#"[+-]?[0-9][0-9_]*\."#, |lex| lex.slice().parse())] // "
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

fn character_special_display(c: char, f: &mut fmt::Formatter) -> fmt::Result {
    match c {
        ' ' => write!(f, "#\\SPACE"),
        '\t' => write!(f, "#\\TAB"),
        '\n' => write!(f, "#\\NEWLINE"),
        '\r' => write!(f, "#\\RETURN"),
        c if c.is_control() || c.is_whitespace() => {
            write!(f, "#\\{}", c.escape_unicode())
        }
        // '\"' => write!(f, "#\\DOUBLE-QUOTE"),
        // '\'' => write!(f, "#\\QUOTE"),
        // '\\' => write!(f, "#\\BACKSLASH"),
        _ => write!(f, "#\\{c}"),
    }
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            OpenParen => write!(f, "("),
            CloseParen => write!(f, "("),
            CharacterLiteral(x) => character_special_display(*x, f),
            BooleanLiteral(x) => write!(f, "#{x}"),
            Identifier(x) => write!(f, "{x}"),
            NumberLiteral(x) => write!(f, "{x:?}"),
            IntegerLiteral(x) => write!(f, "{x}"),
            StringLiteral(x) => write!(f, "\"{x}\""),
            Keyword(x) => write!(f, "{x}"),
            QuoteTick => write!(f, "'"),
            Unquote => write!(f, ","),
            QuasiQuote => write!(f, "`"),
            UnquoteSplice => write!(f, ",@"),
            Error => write!(f, "error"),
            Comment => write!(f, ""),
            If => write!(f, "if"),
            Define => write!(f, "define"),
            Let => write!(f, "let"),
            TestLet => write!(f, "test-let"),
            Return => write!(f, "return!"),
            Begin => write!(f, "begin"),
            Lambda => write!(f, "lambda"),
            Quote => write!(f, "quote"),
            DefineSyntax => write!(f, "define-syntax"),
            SyntaxRules => write!(f, "syntax-rules"),
            Ellipses => write!(f, "..."),
            Set => write!(f, "set!"),
            Require => write!(f, "require"),
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
    pub const fn new(
        ty: TokenType,
        source: &'a str,
        range: ops::Range<usize>,
        source_id: Option<SourceId>,
    ) -> Self {
        Self {
            ty,
            source,
            span: Span::new(range.start, range.end, source_id),
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

impl From<Token<'_>> for Span {
    fn from(token: Token<'_>) -> Self {
        token.span()
    }
}

impl From<&Token<'_>> for Span {
    fn from(token: &Token<'_>) -> Self {
        token.span()
    }
}

impl From<Token<'_>> for ops::Range<usize> {
    fn from(token: Token<'_>) -> Self {
        token.span().into()
    }
}

impl From<&Token<'_>> for ops::Range<usize> {
    fn from(token: &Token<'_>) -> Self {
        token.span().into()
    }
}

impl From<Token<'_>> for (usize, usize) {
    fn from(token: Token<'_>) -> Self {
        token.span().into()
    }
}

impl From<&Token<'_>> for (usize, usize) {
    fn from(token: &Token<'_>) -> Self {
        token.span().into()
    }
}

impl From<Token<'_>> for [usize; 2] {
    fn from(token: Token<'_>) -> Self {
        token.span().into()
    }
}

impl From<&Token<'_>> for [usize; 2] {
    fn from(token: &Token<'_>) -> Self {
        token.span().into()
    }
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} @ {:?}", self.source, self.span)
    }
}