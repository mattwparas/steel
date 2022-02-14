// This is the datum in the source code
// generic over the type used to reference identifiers or strings
// this way we can intern them

use super::interner::InternedString;

pub type InternedDatum = Datum<InternedString>;

// #[derive(Clone, Debug, PartialEq)]
pub enum Datum<T> {
    QuoteTick,
    QuasiQuote,
    Unquote,
    UnquoteSplice,
    Hash,
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
    Struct,
    Set,
    Read,
    Eval,
    Require,
    CallCC,
    CharacterLiteral(char),
    BooleanLiteral(bool),
    Identifier(T),
    NumberLiteral(f64),
    IntegerLiteral(isize),
    StringLiteral(T),
}

impl<T: Clone> Clone for Datum<T> {
    fn clone(&self) -> Self {
        match self {
            Datum::QuoteTick => Datum::QuoteTick,
            Datum::QuasiQuote => Datum::QuasiQuote,
            Datum::Unquote => Datum::Unquote,
            Datum::UnquoteSplice => Datum::UnquoteSplice,
            Datum::Hash => Datum::Hash,
            Datum::If => Datum::If,
            Datum::Define => Datum::Define,
            Datum::Let => Datum::Let,
            Datum::TestLet => Datum::TestLet,
            Datum::Return => Datum::Return,
            Datum::Begin => Datum::Begin,
            Datum::Lambda => Datum::Lambda,
            Datum::Quote => Datum::Quote,
            Datum::SyntaxRules => Datum::SyntaxRules,
            Datum::DefineSyntax => Datum::DefineSyntax,
            Datum::Ellipses => Datum::Ellipses,
            Datum::Struct => Datum::Struct,
            Datum::Set => Datum::Set,
            Datum::Read => Datum::Read,
            Datum::Eval => Datum::Eval,
            Datum::Require => Datum::Require,
            Datum::CallCC => Datum::CallCC,
            Datum::CharacterLiteral(c) => Datum::CharacterLiteral(*c),
            Datum::BooleanLiteral(b) => Datum::BooleanLiteral(*b),
            Datum::Identifier(i) => Datum::Identifier(i.clone()),
            Datum::NumberLiteral(n) => Datum::NumberLiteral(*n),
            Datum::IntegerLiteral(i) => Datum::IntegerLiteral(*i),
            Datum::StringLiteral(s) => Datum::StringLiteral(s.clone()),
        }
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for Datum<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::QuoteTick => write!(f, "QuoteTick"),
            Self::QuasiQuote => write!(f, "QuasiQuote"),
            Self::Unquote => write!(f, "Unquote"),
            Self::UnquoteSplice => write!(f, "UnquoteSplice"),
            Self::Hash => write!(f, "Hash"),
            Self::If => write!(f, "If"),
            Self::Define => write!(f, "Define"),
            Self::Let => write!(f, "Let"),
            Self::TestLet => write!(f, "TestLet"),
            Self::Return => write!(f, "Return"),
            Self::Begin => write!(f, "Begin"),
            Self::Lambda => write!(f, "Lambda"),
            Self::Quote => write!(f, "Quote"),
            Self::SyntaxRules => write!(f, "SyntaxRules"),
            Self::DefineSyntax => write!(f, "DefineSyntax"),
            Self::Ellipses => write!(f, "Ellipses"),
            Self::Struct => write!(f, "Struct"),
            Self::Set => write!(f, "Set"),
            Self::Read => write!(f, "Read"),
            Self::Eval => write!(f, "Eval"),
            Self::Require => write!(f, "Require"),
            Self::CallCC => write!(f, "CallCC"),
            Self::CharacterLiteral(arg0) => f.debug_tuple("CharacterLiteral").field(arg0).finish(),
            Self::BooleanLiteral(arg0) => f.debug_tuple("BooleanLiteral").field(arg0).finish(),
            Self::Identifier(arg0) => f.debug_tuple("Identifier").field(arg0).finish(),
            Self::NumberLiteral(arg0) => f.debug_tuple("NumberLiteral").field(arg0).finish(),
            Self::IntegerLiteral(arg0) => f.debug_tuple("IntegerLiteral").field(arg0).finish(),
            Self::StringLiteral(arg0) => f.debug_tuple("StringLiteral").field(arg0).finish(),
        }
    }
}
