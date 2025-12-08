use crate::lexer;
use crate::parser::SourceId;
use crate::span::Span;
use alloc::borrow::Cow;
use alloc::sync::Arc;
use core::fmt::{self, Display};
use core::ops;
use core::str::FromStr;
use num_bigint::{BigInt, ParseBigIntError};
use num_rational::Rational32;
use num_traits::{Num, Signed};
use serde::{Deserialize, Serialize};
use TokenType::*;

#[derive(Copy, Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Paren {
    Round,
    Square,
    Curly,
}

#[derive(Copy, Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum ParenMod {
    Vector,
    Bytes,
}

impl ParenMod {
    pub(crate) fn as_str(&self) -> &'static str {
        match self {
            ParenMod::Vector => "#",
            ParenMod::Bytes => "#u8",
        }
    }
}

impl Display for ParenMod {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_str().fmt(f)
    }
}

impl Paren {
    pub fn open(&self) -> char {
        match self {
            Paren::Round => '(',
            Paren::Square => '[',
            Paren::Curly => '{',
        }
    }

    pub fn close(&self) -> char {
        match self {
            Paren::Round => ')',
            Paren::Square => ']',
            Paren::Curly => '}',
        }
    }
}

// TODO the character parsing is not quite right
// need to make sure that we can handle cases like "#\SPACE" or "#\a" but not "#\applesauce"
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum TokenType<S> {
    OpenParen(Paren, Option<ParenMod>),
    CloseParen(Paren),
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
    DatumComment,
    Comment,
    BooleanLiteral(bool),
    Identifier(S),
    Keyword(S),
    Number(Box<NumberLiteral>),
    StringLiteral(Arc<String>),
    Dot,
}

impl<T> TokenType<T> {
    pub fn identifier_mut(&mut self) -> Option<&mut T> {
        if let Self::Identifier(i) = self {
            Some(i)
        } else {
            None
        }
    }

    pub fn identifier(&self) -> Option<&T> {
        if let Self::Identifier(i) = self {
            Some(i)
        } else {
            None
        }
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum NumberLiteral {
    Real(RealLiteral),
    Complex(RealLiteral, RealLiteral),
    Polar(RealLiteral, RealLiteral),
}

impl Display for NumberLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NumberLiteral::Real(r) => r.fmt(f),
            NumberLiteral::Complex(re, im) => {
                if im.is_negative() || !im.is_finite() {
                    write!(f, "{re}{im}i")
                } else {
                    write!(f, "{re}+{im}i")
                }
            }
            NumberLiteral::Polar(r, theta) => {
                write!(f, "{r}@{theta}")
            }
        }
    }
}

impl<S> From<NumberLiteral> for TokenType<S> {
    fn from(n: NumberLiteral) -> Self {
        TokenType::Number(Box::new(n))
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

    fn is_finite(&self) -> bool {
        match self {
            RealLiteral::Int(_) => true,
            RealLiteral::Rational(_, _) => true,
            RealLiteral::Float(f) => f.is_finite(),
        }
    }
}

impl From<RealLiteral> for NumberLiteral {
    fn from(value: RealLiteral) -> Self {
        NumberLiteral::Real(value)
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
                    write!(f, "{x:?}")
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
    pub fn from_str_radix(src: &str, radix: u32) -> Result<IntLiteral, ParseBigIntError> {
        isize::from_str_radix(src, radix)
            .map(IntLiteral::Small)
            .or_else(|_| {
                BigInt::from_str_radix(src, radix)
                    .map(Box::new)
                    .map(IntLiteral::Big)
            })
    }

    fn is_negative(&self) -> bool {
        match self {
            IntLiteral::Small(i) => i.is_negative(),
            IntLiteral::Big(i) => i.is_negative(),
        }
    }
}

impl FromStr for IntLiteral {
    type Err = <num_bigint::BigInt as FromStr>::Err;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.parse::<isize>().map(IntLiteral::Small).or_else(|_| {
            s.parse::<num_bigint::BigInt>()
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

impl<'a> TokenType<Cow<'a, str>> {
    pub fn open_span(mut span: Span, paren_mod: Option<ParenMod>) -> Span {
        let offset = match paren_mod {
            Some(ParenMod::Vector) => 1,
            Some(ParenMod::Bytes) => 3,
            None => 0,
        };

        span.start += offset;

        span
    }

    pub fn to_owned<T: From<Cow<'a, str>>>(self) -> TokenType<T> {
        match self {
            TokenType::Identifier(i) => TokenType::Identifier(i.into()),
            TokenType::Keyword(i) => TokenType::Keyword(i.into()),
            OpenParen(p, m) => OpenParen(p, m),
            CloseParen(p) => CloseParen(p),
            CharacterLiteral(x) => CharacterLiteral(x),
            BooleanLiteral(x) => BooleanLiteral(x),
            Number(x) => Number(x),
            StringLiteral(x) => StringLiteral(x),
            QuoteTick => QuoteTick,
            Unquote => Unquote,
            QuasiQuote => QuasiQuote,
            UnquoteSplice => UnquoteSplice,
            Comment => Comment,
            DatumComment => DatumComment,
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

    pub fn map<T>(self, mut func: impl FnMut(Cow<'a, str>) -> T) -> TokenType<T> {
        match self {
            TokenType::Identifier(i) => TokenType::Identifier(func(i)),
            TokenType::Keyword(i) => TokenType::Keyword(func(i)),
            OpenParen(p, m) => OpenParen(p, m),
            CloseParen(p) => CloseParen(p),
            CharacterLiteral(x) => CharacterLiteral(x),
            BooleanLiteral(x) => BooleanLiteral(x),
            Number(x) => Number(x),
            StringLiteral(x) => StringLiteral(x),
            QuoteTick => QuoteTick,
            Unquote => Unquote,
            QuasiQuote => QuasiQuote,
            UnquoteSplice => UnquoteSplice,
            Comment => Comment,
            DatumComment => DatumComment,
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
        ' ' => write!(f, "#\\space"),
        '\0' => write!(f, "#\\null"),
        '\t' => write!(f, "#\\tab"),
        '\n' => write!(f, "#\\newline"),
        '\r' => write!(f, "#\\return"),
        _ => {
            let escape = c.escape_debug();
            if escape.len() <= 2 {
                // char does not need escaping
                write!(f, "#\\{}", c)
            } else {
                // escape char as #\uNNNN
                write!(f, "#\\u{:04x}", c as u32)
            }
        }
    }
}

impl<T: Display> fmt::Display for TokenType<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            OpenParen(p, m) => {
                if let Some(m) = m {
                    m.fmt(f)?;
                }

                write!(f, "{}", p.open())
            }
            CloseParen(p) => write!(f, "{}", p.close()),
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
            DatumComment => write!(f, "#;"),
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
pub struct TokenLike<'a, TY> {
    pub ty: TY,
    pub source: &'a str,
    pub span: Span,
}

impl<'a, TY> TokenLike<'a, TY> {
    pub const fn new(
        ty: TY,
        source: &'a str,
        range: ops::Range<u32>,
        source_id: Option<SourceId>,
    ) -> Self {
        Self {
            ty,
            source,
            span: Span::new(range.start, range.end, source_id),
        }
    }
}

pub type Token<'a, T> = TokenLike<'a, TokenType<T>>;

impl<'a, T> Token<'a, T> {
    pub fn typ(&self) -> &TokenType<T> {
        &self.ty
    }

    pub const fn span(&self) -> Span {
        self.span
    }

    pub const fn range(&self) -> ops::Range<u32> {
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

impl<T> From<Token<'_, T>> for ops::Range<u32> {
    fn from(token: Token<'_, T>) -> Self {
        token.span().into()
    }
}

impl<T> From<&Token<'_, T>> for ops::Range<u32> {
    fn from(token: &Token<'_, T>) -> Self {
        token.span().into()
    }
}

impl<T> From<Token<'_, T>> for (u32, u32) {
    fn from(token: Token<'_, T>) -> Self {
        token.span().into()
    }
}

impl<T> From<&Token<'_, T>> for (u32, u32) {
    fn from(token: &Token<'_, T>) -> Self {
        token.span().into()
    }
}

impl<T> From<Token<'_, T>> for [u32; 2] {
    fn from(token: Token<'_, T>) -> Self {
        token.span().into()
    }
}

impl<T> From<&Token<'_, T>> for [u32; 2] {
    fn from(token: &Token<'_, T>) -> Self {
        token.span().into()
    }
}

impl<T> Display for Token<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} @ {:?}", self.source, self.span)
    }
}
