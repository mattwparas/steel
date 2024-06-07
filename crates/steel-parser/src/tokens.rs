use crate::lexer;
use crate::parser::SourceId;
use crate::span::Span;
use core::ops;
use num::{BigInt, Rational32, Signed};
use serde::{Deserialize, Serialize};
use std::convert::TryFrom;
use std::fmt::{self, Display};
use std::num::ParseIntError;
use std::str::FromStr;
use TokenType::*;

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

impl Display for DecodeHexError {
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

pub fn parse_unicode_str(slice: &str) -> Option<char> {
    if slice.starts_with("#\\u") && slice.contains('{') && slice.contains('}') {
        let rest = slice
            .trim_start_matches("#\\u")
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
        char::try_from(uinitial).ok()
    } else if slice.starts_with("#\\u") {
        let rest = slice.trim_start_matches("#\\u").to_lowercase();

        let rest = match rest.len() {
            1 => "000".to_string() + &rest,
            2 => "00".to_string() + &rest,
            3 => "0".to_string() + &rest,
            4 => rest,
            _ => return None,
        };

        let decoded: u8 = decode_hex(&rest).ok()?.into_iter().sum();

        let uinitial: u32 = decoded.into();

        char::try_from(uinitial).ok()
    } else {
        None
    }
}

// TODO the character parsing is not quite right
// need to make sure that we can handle cases like "#\SPACE" or "#\a" but not "#\applesauce"
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum TokenType<S> {
    OpenParen,
    CloseParen,
    QuoteTick,
    QuasiQuote,
    Unquote,
    UnquoteSplice,
    QuoteSyntax,
    QuasiQuoteSyntax,
    UnquoteSyntax,
    UnquoteSpliceSyntax,
    If,
    Define,
    Let,
    TestLet,
    Return,
    Begin,
    Lambda,
    Quote,
    SyntaxRules,
    DefineSyntax,
    Ellipses,
    Set,
    Require,
    CharacterLiteral(char),
    Comment,
    BooleanLiteral(bool),
    Identifier(S),
    Keyword(S),
    Number(NumberLiteral),
    StringLiteral(String),
    Dot,
    Error,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum NumberLiteral {
    Real(RealLiteral),
    Complex(RealLiteral, RealLiteral),
}

impl Display for NumberLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NumberLiteral::Real(r) => r.fmt(f),
            NumberLiteral::Complex(re, im) => {
                if im.is_negative() {
                    write!(f, "{re}{im}i")
                } else {
                    write!(f, "{re}+{im}i")
                }
            }
        }
    }
}

impl<S> From<NumberLiteral> for TokenType<S> {
    fn from(n: NumberLiteral) -> Self {
        TokenType::Number(n)
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum RealLiteral {
    Int(IntLiteral),
    Rational(IntLiteral, IntLiteral),
    Float(f64),
}

impl RealLiteral {
    fn is_negative(&self) -> bool {
        match self {
            RealLiteral::Int(i) => i.is_negative(),
            RealLiteral::Rational(n, _) => n.is_negative(),
            RealLiteral::Float(f) => f.is_sign_negative(),
        }
    }
}

impl From<RealLiteral> for NumberLiteral {
    fn from(value: RealLiteral) -> Self {
        NumberLiteral::Real(value).into()
    }
}

impl<S> From<RealLiteral> for TokenType<S> {
    fn from(value: RealLiteral) -> Self {
        NumberLiteral::Real(value).into()
    }
}

impl From<f64> for RealLiteral {
    fn from(value: f64) -> RealLiteral {
        RealLiteral::Float(value)
    }
}

impl From<isize> for RealLiteral {
    fn from(value: isize) -> RealLiteral {
        RealLiteral::Int(IntLiteral::Small(value))
    }
}

impl From<Rational32> for RealLiteral {
    fn from(value: Rational32) -> RealLiteral {
        RealLiteral::Rational(
            (*value.numer() as isize).into(),
            (*value.denom() as isize).into(),
        )
    }
}

impl Display for RealLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RealLiteral::Int(i) => i.fmt(f),
            RealLiteral::Rational(n, d) => write!(f, "{n}/{d}"),
            RealLiteral::Float(x) => {
                if x.is_nan() {
                    write!(f, "{}", lexer::NAN)
                } else if x.is_infinite() && x.is_sign_negative() {
                    write!(f, "{}", lexer::NEG_INFINITY)
                } else if x.is_infinite() {
                    write!(f, "{}", lexer::INFINITY)
                } else {
                    x.fmt(f)
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum IntLiteral {
    Small(isize),
    Big(Box<BigInt>),
}

impl IntLiteral {
    fn is_negative(&self) -> bool {
        match self {
            IntLiteral::Small(i) => i.is_negative(),
            IntLiteral::Big(i) => i.is_negative(),
        }
    }
}

impl FromStr for IntLiteral {
    type Err = <num::BigInt as FromStr>::Err;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.parse::<isize>().map(IntLiteral::Small).or_else(|_| {
            s.parse::<num::BigInt>()
                .map(|b| IntLiteral::Big(Box::new(b)))
        })
    }
}

impl Display for IntLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Small(s) => write!(f, "{s}"),
            Self::Big(b) => write!(f, "{b}"),
        }
    }
}

impl<S> From<IntLiteral> for TokenType<S> {
    fn from(value: IntLiteral) -> Self {
        RealLiteral::Int(value).into()
    }
}

impl From<IntLiteral> for RealLiteral {
    fn from(value: IntLiteral) -> Self {
        RealLiteral::Int(value)
    }
}

impl From<IntLiteral> for BigInt {
    fn from(v: IntLiteral) -> BigInt {
        match v {
            IntLiteral::Small(x) => x.into(),
            IntLiteral::Big(x) => *x,
        }
    }
}

impl From<isize> for IntLiteral {
    fn from(value: isize) -> Self {
        IntLiteral::Small(value)
    }
}

impl From<BigInt> for IntLiteral {
    fn from(value: BigInt) -> Self {
        IntLiteral::Big(Box::new(value))
    }
}

impl<'a> TokenType<&'a str> {
    pub fn to_owned<T: From<&'a str>>(self) -> TokenType<T> {
        match self {
            TokenType::Identifier(i) => TokenType::Identifier(i.into()),
            TokenType::Keyword(i) => TokenType::Keyword(i.into()),
            OpenParen => OpenParen,
            CloseParen => CloseParen,
            CharacterLiteral(x) => CharacterLiteral(x),
            BooleanLiteral(x) => BooleanLiteral(x),
            Number(x) => Number(x),
            StringLiteral(x) => StringLiteral(x),
            QuoteTick => QuoteTick,
            Unquote => Unquote,
            QuasiQuote => QuasiQuote,
            UnquoteSplice => UnquoteSplice,
            Error => Error,
            Comment => Comment,
            If => If,
            Define => Define,
            Let => Let,
            TestLet => TestLet,
            Return => Return,
            Begin => Begin,
            Lambda => Lambda,
            Quote => Quote,
            DefineSyntax => DefineSyntax,
            SyntaxRules => SyntaxRules,
            Ellipses => Ellipses,
            Set => Set,
            Require => Require,
            QuasiQuoteSyntax => QuasiQuoteSyntax,
            UnquoteSyntax => UnquoteSyntax,
            QuoteSyntax => QuoteSyntax,
            UnquoteSpliceSyntax => UnquoteSpliceSyntax,
            Dot => Dot,
        }
    }

    pub fn map<T>(self, mut func: impl FnMut(&'a str) -> T) -> TokenType<T> {
        match self {
            TokenType::Identifier(i) => TokenType::Identifier(func(i)),
            TokenType::Keyword(i) => TokenType::Keyword(func(i)),
            OpenParen => OpenParen,
            CloseParen => CloseParen,
            CharacterLiteral(x) => CharacterLiteral(x),
            BooleanLiteral(x) => BooleanLiteral(x),
            Number(x) => Number(x),
            StringLiteral(x) => StringLiteral(x),
            QuoteTick => QuoteTick,
            Unquote => Unquote,
            QuasiQuote => QuasiQuote,
            UnquoteSplice => UnquoteSplice,
            Error => Error,
            Comment => Comment,
            If => If,
            Define => Define,
            Let => Let,
            TestLet => TestLet,
            Return => Return,
            Begin => Begin,
            Lambda => Lambda,
            Quote => Quote,
            DefineSyntax => DefineSyntax,
            SyntaxRules => SyntaxRules,
            Ellipses => Ellipses,
            Set => Set,
            Require => Require,
            QuasiQuoteSyntax => QuasiQuoteSyntax,
            UnquoteSyntax => UnquoteSyntax,
            QuoteSyntax => QuoteSyntax,
            UnquoteSpliceSyntax => UnquoteSpliceSyntax,
            Dot => Dot,
        }
    }
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

impl<T: Display> fmt::Display for TokenType<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            OpenParen => write!(f, "("),
            CloseParen => write!(f, "("),
            CharacterLiteral(x) => character_special_display(*x, f),
            BooleanLiteral(x) => write!(f, "#{x}"),
            Identifier(x) => write!(f, "{x}"),
            Number(x) => write!(f, "{x}"),
            StringLiteral(x) => write!(f, "\"{x}\""),
            Keyword(x) => write!(f, "{x}"),
            QuoteTick => write!(f, "'"),
            Unquote => write!(f, ","),
            QuasiQuote => write!(f, "`"),
            UnquoteSplice => write!(f, ",@"),
            QuoteSyntax => write!(f, "#'"),
            QuasiQuoteSyntax => write!(f, "#`"),
            UnquoteSyntax => write!(f, "#,"),
            UnquoteSpliceSyntax => write!(f, "#,@"),
            Error => write!(f, "error"),
            Comment => write!(f, ""),
            If => write!(f, "if"),
            Define => write!(f, "define"),
            Let => write!(f, "let"),
            TestLet => write!(f, "%plain-let"),
            Return => write!(f, "return!"),
            Begin => write!(f, "begin"),
            Lambda => write!(f, "lambda"),
            Quote => write!(f, "quote"),
            DefineSyntax => write!(f, "define-syntax"),
            SyntaxRules => write!(f, "syntax-rules"),
            Ellipses => write!(f, "..."),
            Set => write!(f, "set!"),
            Require => write!(f, "require"),
            Dot => write!(f, "."),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token<'a, T> {
    pub ty: TokenType<T>,
    pub source: &'a str,
    pub span: Span,
}

impl<'a, T> Token<'a, T> {
    pub const fn new(
        ty: TokenType<T>,
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

    pub fn typ(&self) -> &TokenType<T> {
        &self.ty
    }

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

impl<T> From<Token<'_, T>> for Span {
    fn from(token: Token<'_, T>) -> Self {
        token.span()
    }
}

impl<T> From<&Token<'_, T>> for Span {
    fn from(token: &Token<'_, T>) -> Self {
        token.span()
    }
}

impl<T> From<Token<'_, T>> for ops::Range<usize> {
    fn from(token: Token<'_, T>) -> Self {
        token.span().into()
    }
}

impl<T> From<&Token<'_, T>> for ops::Range<usize> {
    fn from(token: &Token<'_, T>) -> Self {
        token.span().into()
    }
}

impl<T> From<Token<'_, T>> for (usize, usize) {
    fn from(token: Token<'_, T>) -> Self {
        token.span().into()
    }
}

impl<T> From<&Token<'_, T>> for (usize, usize) {
    fn from(token: &Token<'_, T>) -> Self {
        token.span().into()
    }
}

impl<T> From<Token<'_, T>> for [usize; 2] {
    fn from(token: Token<'_, T>) -> Self {
        token.span().into()
    }
}

impl<T> From<&Token<'_, T>> for [usize; 2] {
    fn from(token: &Token<'_, T>) -> Self {
        token.span().into()
    }
}

impl<T> Display for Token<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} @ {:?}", self.source, self.span)
    }
}
