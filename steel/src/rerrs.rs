use crate::parser::parser::ParseError;
use std::{convert::Infallible, fmt::Formatter};
use thiserror::Error;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

use crate::parser::span::Span;

use std::fmt;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
struct Repr {
    pub kind: ErrorKind,
    pub message: String,
    pub span: Option<Span>,
    pub source: Option<Rc<str>>,
}

impl Repr {
    pub fn set_span(&mut self, span: Span) {
        self.span = Some(span);
    }
}

impl fmt::Display for Repr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Error: {:?}: {}", self.kind, self.message)
    }
}

#[derive(Clone, Debug, PartialEq, Copy)]
pub enum ErrorKind {
    ArityMismatch,
    FreeIdentifier,
    TypeMismatch,
    UnexpectedToken,
    ContractViolation,
    BadSyntax,
    ConversionError,
    Io,
    Parse,
    Infallible,
    Generic,
}

impl ErrorKind {
    fn to_error_code(&self) -> &str {
        use ErrorKind::*;
        match self {
            ArityMismatch => "E01",
            FreeIdentifier => "E02",
            TypeMismatch => "E03",
            UnexpectedToken => "E04",
            ContractViolation => "E05",
            BadSyntax => "E06",
            ConversionError => "E07",
            Io => "E08",
            Parse => "E09",
            Infallible => "E10",
            Generic => "E11",
        }
    }
}

impl From<std::io::Error> for SteelErr {
    fn from(v: std::io::Error) -> Self {
        SteelErr::_new(v.into())
    }
}

impl From<std::io::Error> for Repr {
    fn from(v: std::io::Error) -> Self {
        Repr {
            kind: ErrorKind::Io,
            message: v.to_string(),
            span: None,
            source: None,
        }
    }
}

impl From<Infallible> for SteelErr {
    fn from(v: Infallible) -> Self {
        SteelErr::_new(v.into())
    }
}

impl From<ParseError> for SteelErr {
    fn from(v: ParseError) -> Self {
        SteelErr::_new(v.into())
    }
}

// mod inner {
//     use super::*;
//     impl<T> From<T> for SteelErr
//     where
//         T: Into<Repr>,
//     {
//         fn from(v: T) -> Self {
//             SteelErr::_new(v.into())
//         }
//     }
// }

impl From<Infallible> for Repr {
    fn from(v: Infallible) -> Self {
        Repr {
            kind: ErrorKind::Infallible,
            message: v.to_string(),
            span: None,
            source: None,
        }
    }
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl From<ParseError> for Repr {
    fn from(v: ParseError) -> Self {
        // unimplemented!()
        let (span, source) = match &v {
            ParseError::Unexpected(_, source) | ParseError::UnexpectedEOF(source) => (None, source),
            ParseError::UnexpectedChar(_, s, source) => (Some(*s), source),
            ParseError::IncompleteString(_, s, source) => (Some(*s), source),
            ParseError::SyntaxError(_, s, source) => (Some(*s), source),
            ParseError::ArityMismatch(_, s, source) => (Some(*s), source),
        };

        Repr {
            kind: ErrorKind::Parse,
            message: v.to_string(),
            span,
            source: source.clone(),
        }
    }
}

#[derive(Debug, Error, Clone, PartialEq)]
pub struct SteelErr {
    repr: Repr,
}

impl fmt::Display for SteelErr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.repr)
    }
}

// fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//     // at the top level, print a ' if we are
//     // trying to print a symbol or list
//     match self {
//         SymbolV(_) | Pair(_) => write!(f, "'")?,
//         VectorV(_) => write!(f, "'#")?,
//         _ => (),
//     };
//     display_helper(self, f)
// }

// #[derive(Debug, Error)]
// pub enum SteelErr {
//     #[error("Error: Arity Mismatch: {0}")]
//     ArityMismatch(SError),
//     #[error("Error: Free Identifier: {0}")]
//     FreeIdentifier(SError),
//     #[error("Error: Expected {0}")]
//     TypeMismatch(SError),
//     #[error("Error: Unexpected Token {0}")]
//     UnexpectedToken(SError),
//     #[error("Error: Contract Violation: {0}")]
//     ContractViolation(SError),
//     #[error("Error: Bad Syntax: {0}")]
//     BadSyntax(SError),
//     #[error("Error: Conversion Error: {0}")]
//     ConversionError(SError),
//     #[error("Error: IO error")]
//     Io(#[from] std::io::Error),
//     #[error("Error: Parse error: {0}")]
//     Parse(#[from] ParseError),
//     #[error("Error: Infallible")]
//     Infallible(#[from] Infallible),
//     #[error("Error: Generic Error: {0}")]
//     Generic(SError),
// }

// impl Clone for SteelErr {
//     fn clone(&self) -> SteelErr {
//         match self {
//             SteelErr::ArityMismatch(s) => SteelErr::ArityMismatch(s.clone()),
//             SteelErr::FreeIdentifier(s) => SteelErr::FreeIdentifier(s.clone()),
//             SteelErr::TypeMismatch(s) => SteelErr::TypeMismatch(s.clone()),
//             SteelErr::UnexpectedToken(s) => SteelErr::UnexpectedToken(s.clone()),
//             SteelErr::ContractViolation(s) => SteelErr::ContractViolation(s.clone()),
//             SteelErr::BadSyntax(s) => SteelErr::BadSyntax(s.clone()),
//             SteelErr::ConversionError(s) => SteelErr::ConversionError(s.clone()),
//             SteelErr::Io(_) => {
//                 SteelErr::Io(std::io::Error::new(std::io::ErrorKind::Other, "io error"))
//             }
//             SteelErr::Parse(p) => SteelErr::Parse(p.clone()),
//             SteelErr::Infallible(s) => SteelErr::Infallible(*s),
//             SteelErr::Generic(s) => SteelErr::Generic(s.clone()),
//         }
//     }
// }

// impl PartialEq for SteelErr {
//     fn eq(&self, other: &Self) -> bool {
//         use SteelErr::*;
//         match (self, other) {
//             (ArityMismatch(l), ArityMismatch(r)) => l == r,
//             (FreeIdentifier(l), FreeIdentifier(r)) => l == r,
//             (TypeMismatch(l), TypeMismatch(r)) => l == r,
//             (UnexpectedToken(l), UnexpectedToken(r)) => l == r,
//             (ContractViolation(l), ContractViolation(r)) => l == r,
//             (BadSyntax(l), BadSyntax(r)) => l == r,
//             (ConversionError(l), ConversionError(r)) => l == r,
//             (Generic(l), Generic(r)) => l == r,
//             _ => false,
//         }
//     }
// }

// #[derive(Debug, StructOpt)]
// #[structopt(name = "emit")]
// struct Opts {
//     #[structopt(long = "color",
//         parse(try_from_str),
//         default_value = "auto",
//         possible_values = ColorArg::VARIANTS,
//         case_insensitive = true
//     )]
//     color: ColorArg,
// }

impl SteelErr {
    // pub fn set_span(self, span: Span) -> Self {
    //     match self {
    //         Self::ArityMismatch(m) => Self::ArityMismatch(m.with_span(span)),
    //         Self::FreeIdentifier(m) => Self::FreeIdentifier(m.with_span(span)),
    //         Self::TypeMismatch(m) => Self::TypeMismatch(m.with_span(span)),
    //         Self::UnexpectedToken(m) => Self::UnexpectedToken(m.with_span(span)),
    //         Self::ContractViolation(m) => Self::ContractViolation(m.with_span(span)),
    //         Self::BadSyntax(m) => Self::BadSyntax(m.with_span(span)),
    //         Self::ConversionError(m) => Self::ConversionError(m.with_span(span)),
    //         Self::Generic(m) => Self::Generic(m.with_span(span)),
    //         Self::Io(m) => Self::Io(m),
    //         Self::Parse(m) => Self::Parse(m),
    //         Self::Infallible(m) => Self::Infallible(m),
    //     }
    // }

    fn _new(repr: Repr) -> Self {
        SteelErr { repr }
    }

    pub fn kind(&self) -> ErrorKind {
        self.repr.kind
    }

    pub fn new(kind: ErrorKind, message: String) -> Self {
        SteelErr {
            repr: Repr {
                kind,
                message,
                span: None,
                source: None,
            },
        }
    }

    pub fn set_span(mut self, span: Span) -> Self {
        self.repr.set_span(span);
        self
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.repr.span = Some(span);
        self
    }

    pub fn with_source(mut self, source: Option<Rc<str>>) -> Self {
        self.repr.source = source;
        self
    }

    pub fn emit_result(&self, file_name: &str, file_content: &str) {
        // let opts = Opts::();
        // let config = codespan_reporting::term::Config::default();
        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = codespan_reporting::term::Config::default();

        let file = SimpleFile::new(file_name, file_content);

        let error_span = Span::new(0, 0);

        let report = self.report(file_name, file_content, error_span);
        term::emit(&mut writer.lock(), &config, &file, &report).unwrap(); // TODO come back

        // for diagnostic in errors.iter().map(Error::report) {
        //     term::emit(&mut writer.lock(), &config, &file, &diagnostic)?;
        // }
    }

    fn report(&self, _file_name: &str, _file_content: &str, _error_span: Span) -> Diagnostic<()> {
        // println!("Generating error report!");

        Diagnostic::error()
            .with_code(self.repr.kind.to_error_code())
            .with_message(self.repr.kind.to_string())
            .with_labels(vec![Label::primary(
                (),
                self.repr.span.unwrap_or(_error_span),
            )
            .with_message(&self.repr.message)])

        // match self {
        //     Self::ArityMismatch(m, error_span) => Diagnostic::error()
        //         .with_code("E01")
        //         .with_message("arity mismatch")
        //         .with_labels(vec![
        //             Label::primary((), error_span.unwrap_or(_error_span)).with_message(m)
        //         ]),
        //     Self::FreeIdentifier(m, error_span) => Diagnostic::error()
        //         .with_code("E02")
        //         .with_message("free identifier")
        //         .with_labels(vec![
        //             Label::primary((), error_span.unwrap_or(_error_span)).with_message(m)
        //         ]),
        //     Self::TypeMismatch(m, error_span) => Diagnostic::error()
        //         .with_code("E03")
        //         .with_message("type mismatch")
        //         .with_labels(vec![
        //             Label::primary((), error_span.unwrap_or(_error_span)).with_message(m)
        //         ]),
        //     Self::UnexpectedToken(m, error_span) => Diagnostic::error()
        //         .with_code("E04")
        //         .with_message("unexpected token")
        //         .with_labels(vec![
        //             Label::primary((), error_span.unwrap_or(_error_span)).with_message(m)
        //         ]),
        //     Self::ContractViolation(m, error_span) => Diagnostic::error()
        //         .with_code("E05")
        //         .with_message("contract violation")
        //         .with_labels(vec![
        //             Label::primary((), error_span.unwrap_or(_error_span)).with_message(m)
        //         ]),
        //     Self::BadSyntax(m, error_span) => Diagnostic::error()
        //         .with_code("E06")
        //         .with_message("bad syntax")
        //         .with_labels(vec![
        //             Label::primary((), error_span.unwrap_or(_error_span)).with_message(m)
        //         ]),
        //     Self::ConversionError(m, error_span) => Diagnostic::error()
        //         .with_code("E07")
        //         .with_message("conversion error")
        //         .with_labels(vec![
        //             Label::primary((), error_span.unwrap_or(_error_span)).with_message(m)
        //         ]),
        //     Self::Io(m) => Diagnostic::error()
        //         .with_code("E08")
        //         .with_message("io error")
        //         .with_labels(vec![
        //             Label::primary((), _error_span).with_message(m.to_string())
        //         ]),
        //     Self::Parse(m) => {
        //         let reporting_span = if let Some(s) = m.span() {
        //             s
        //         } else {
        //             _error_span
        //         };

        //         Diagnostic::error()
        //             .with_code("E09")
        //             .with_message("parse error")
        //             .with_labels(vec![
        //                 Label::primary((), reporting_span).with_message(m.to_string())
        //             ])
        //     }
        //     Self::Infallible(m) => Diagnostic::error()
        //         .with_code("E10")
        //         .with_message("infallible")
        //         .with_labels(vec![
        //             Label::primary((), _error_span).with_message(m.to_string())
        //         ]),
        //     Self::Generic(m, error_span) => Diagnostic::error()
        //         .with_code("E11")
        //         .with_message("general")
        //         .with_labels(vec![
        //             Label::primary((), error_span.unwrap_or(_error_span)).with_message(m)
        //         ]),
        // }
    }
}

#[macro_export]
macro_rules! stop {
    // ($type:ident) => {
    //     return Err(SteelErr::new(ErrorKind::$type, None));
    // };
    ($type:ident => $fmt:expr, $($arg:tt)+) => {
        return Err(SteelErr::new(ErrorKind::$type, format!($fmt, $($arg)+)));
    };
    ($type:ident => $thing:expr) => {
        return Err(SteelErr::new(ErrorKind::$type, ($thing).to_string()));
    };
    ($type:ident => $thing:expr; $span:expr) => {
        return Err(SteelErr::new(ErrorKind::$type, ($thing).to_string()).with_span($span));
    };
    ($type:ident => $thing:expr; $span:expr; $source:expr) => {
        return Err(SteelErr::new(ErrorKind::$type, ($thing).to_string()).with_span($span).with_source($source));
    };
}

#[macro_export]
macro_rules! throw {
    // ($type:ident) => {
    //     || SteelErr::$type
    // };
    ($type:ident => $fmt:expr, $($arg:tt)+) => {
        || SteelErr::new(ErrorKind::$type, format!($fmt, $($arg)+))
    };
    ($type:ident => $thing:expr) => {
        || SteelErr::new(ErrorKind::$type, ($thing).to_string())
    };
}
