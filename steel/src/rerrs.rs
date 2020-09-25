use crate::parser::ParseError;
use std::convert::Infallible;
use thiserror::Error;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term::termcolor::StandardStream;
use codespan_reporting::term::{self, ColorArg};
// use std::ops::Range;
use structopt::StructOpt;

use crate::parser::span::Span;

// pub struct Span {
//     expr: String,
// }

#[derive(Debug, Error)]
pub enum SteelErr {
    #[error("Error: Arity Mismatch: {0}")]
    ArityMismatch(String, Option<Span>),
    #[error("Error: Free Identifier: {0}")]
    FreeIdentifier(String, Option<Span>),
    #[error("Error: Expected {0}")]
    TypeMismatch(String, Option<Span>),
    #[error("Error: Unexpected Token {0}")]
    UnexpectedToken(String, Option<Span>),
    #[error("Error: Contract Violation: {0}")]
    ContractViolation(String, Option<Span>),
    #[error("Error: Bad Syntax: {0}")]
    BadSyntax(String, Option<Span>),
    #[error("Error: Conversion Error: {0}")]
    ConversionError(String, Option<Span>),
    #[error("Error: IO error")]
    Io(#[from] std::io::Error),
    #[error("Error: Parse error: {0}")]
    Parse(#[from] ParseError),
    #[error("Error: Infallible")]
    Infallible(#[from] Infallible),
    #[error("Error: Generic Error: {0}")]
    Generic(String, Option<Span>),
}

impl Clone for SteelErr {
    fn clone(&self) -> SteelErr {
        match self {
            SteelErr::ArityMismatch(l, r) => SteelErr::ArityMismatch(l.clone(), r.clone()),
            SteelErr::FreeIdentifier(l, r) => SteelErr::FreeIdentifier(l.clone(), r.clone()),
            SteelErr::TypeMismatch(l, r) => SteelErr::TypeMismatch(l.clone(), r.clone()),
            SteelErr::UnexpectedToken(l, r) => SteelErr::UnexpectedToken(l.clone(), r.clone()),
            SteelErr::ContractViolation(l, r) => SteelErr::ContractViolation(l.clone(), r.clone()),
            SteelErr::BadSyntax(l, r) => SteelErr::BadSyntax(l.clone(), r.clone()),
            SteelErr::ConversionError(l, r) => SteelErr::ConversionError(l.clone(), r.clone()),
            SteelErr::Io(_) => {
                SteelErr::Io(std::io::Error::new(std::io::ErrorKind::Other, "io error"))
            }
            SteelErr::Parse(p) => SteelErr::Parse(p.clone()),
            SteelErr::Infallible(l) => SteelErr::Infallible(l.clone()),
            SteelErr::Generic(l, r) => SteelErr::Generic(l.clone(), r.clone()),
        }
    }
}

impl PartialEq for SteelErr {
    fn eq(&self, other: &Self) -> bool {
        use SteelErr::*;
        match (self, other) {
            (ArityMismatch(l, _), ArityMismatch(r, _)) => l == r,
            (FreeIdentifier(l, _), FreeIdentifier(r, _)) => l == r,
            (TypeMismatch(l, _), TypeMismatch(r, _)) => l == r,
            (UnexpectedToken(l, _), UnexpectedToken(r, _)) => l == r,
            (ContractViolation(l, _), ContractViolation(r, _)) => l == r,
            (BadSyntax(l, _), BadSyntax(r, _)) => l == r,
            (ConversionError(l, _), ConversionError(r, _)) => l == r,
            (Generic(l, _), Generic(r, _)) => l == r,
            _ => false,
        }
    }
}

#[derive(Debug, StructOpt)]
#[structopt(name = "emit")]
pub struct Opts {
    #[structopt(long = "color",
        parse(try_from_str),
        default_value = "auto",
        possible_values = ColorArg::VARIANTS,
        case_insensitive = true
    )]
    color: ColorArg,
}

impl SteelErr {
    pub fn set_span(self, span: Span) -> Self {
        match self {
            Self::ArityMismatch(m, _error_span) => Self::ArityMismatch(m, Some(span)),
            Self::FreeIdentifier(m, _error_span) => Self::FreeIdentifier(m, Some(span)),
            Self::TypeMismatch(m, _error_span) => Self::TypeMismatch(m, Some(span)),
            Self::UnexpectedToken(m, _error_span) => Self::UnexpectedToken(m, Some(span)),
            Self::ContractViolation(m, _error_span) => Self::ContractViolation(m, Some(span)),
            Self::BadSyntax(m, _error_span) => Self::BadSyntax(m, Some(span)),
            Self::ConversionError(m, _error_span) => Self::ConversionError(m, Some(span)),
            Self::Generic(m, _error_span) => Self::Generic(m, Some(span)),
            Self::Io(m) => Self::Io(m),
            Self::Parse(m) => Self::Parse(m),
            Self::Infallible(m) => Self::Infallible(m),
        }
    }

    pub fn emit_result(&self, file_name: &str, file_content: &str, error_span: Span) {
        let opts = Opts::from_args();
        let writer = StandardStream::stderr(opts.color.into());
        let config = codespan_reporting::term::Config::default();

        let file = SimpleFile::new(
            file_name,
            file_content
            // unindent::unindent(
            //     r#"
            //         fn main() {
            //             let foo: i32 = "hello, world";
            //             foo += 1;
            //         }
            //     "#,
            // ),
        );

        let report = self.report(file_name, file_content, error_span);
        term::emit(&mut writer.lock(), &config, &file, &report).unwrap(); // TODO come back

        // for diagnostic in errors.iter().map(Error::report) {
        //     term::emit(&mut writer.lock(), &config, &file, &diagnostic)?;
        // }
    }

    fn report(&self, _file_name: &str, _file_content: &str, _error_span: Span) -> Diagnostic<()> {
        // println!("Generating error report!");

        match self {
            Self::ArityMismatch(m, error_span) => {
                Diagnostic::error()
                .with_code("E01")
                .with_message("arity mismatch")
                .with_labels(vec![
                    Label::primary((), error_span.unwrap_or(_error_span)).with_message(m)
                ])
            }
            Self::FreeIdentifier(m, error_span) => {
                Diagnostic::error()
                .with_code("E02")
                .with_message("free identifier")
                .with_labels(vec![
                    Label::primary((), error_span.unwrap_or(_error_span)).with_message(m)
                ])
            }
            Self::TypeMismatch(m, error_span) => {
                Diagnostic::error()
                .with_code("E03")
                .with_message("type mismatch")
                .with_labels(vec![
                    Label::primary((), error_span.unwrap_or(_error_span)).with_message(m)
                ])
            }
            Self::UnexpectedToken(m, error_span) => {
                Diagnostic::error()
                .with_code("E04")
                .with_message("unexpected token")
                .with_labels(vec![
                    Label::primary((), error_span.unwrap_or(_error_span)).with_message(m)
                ])
            }
            Self::ContractViolation(m, error_span) => {
                Diagnostic::error()
                .with_code("E05")
                .with_message("contract violation")
                .with_labels(vec![
                    Label::primary((), error_span.unwrap_or(_error_span)).with_message(m)
                ])
            }
            Self::BadSyntax(m, error_span) => {
                Diagnostic::error()
                .with_code("E06")
                .with_message("bad syntax")
                .with_labels(vec![
                    Label::primary((), error_span.unwrap_or(_error_span)).with_message(m)
                ])
            }
            Self::ConversionError(m, error_span) => {
                Diagnostic::error()
                .with_code("E07")
                .with_message("conversion error")
                .with_labels(vec![
                    Label::primary((), error_span.unwrap_or(_error_span)).with_message(m)
                ])
            }
            Self::Io(m) => {
                Diagnostic::error()
                .with_code("E08")
                .with_message("io error")
                .with_labels(vec![
                    Label::primary((), _error_span).with_message(m.to_string())
                ])
            }
            Self::Parse(m) => {
                Diagnostic::error()
                .with_code("E09")
                .with_message("parse error")
                .with_labels(vec![
                    Label::primary((), _error_span).with_message(m.to_string())
                ])
            }
            Self::Infallible(m) => {
                Diagnostic::error()
                .with_code("E10")
                .with_message("infallible")
                .with_labels(vec![
                    Label::primary((), _error_span).with_message(m.to_string())
                ])
            }
            Self::Generic(m, error_span) => {
                Diagnostic::error()
                .with_code("E11")
                .with_message("general")
                .with_labels(vec![
                    Label::primary((), error_span.unwrap_or(_error_span)).with_message(m)
                ])
            }
            // SteelErr::MismatchType(left, right) => Diagnostic::error()
            //     .with_code("E0308")
            //     .with_message("mismatch types")
            //     .with_labels(vec![
            //         Label::primary((), right.range.clone()).with_message(format!(
            //             "Expected `{}`, found: `{}`",
            //             left.content, right.content,
            //         )),
            //         Label::secondary((), left.range.clone()).with_message("expected due to this"),
            //     ]),

            // Error::MutatingImmutable(original, mutating) => Diagnostic::error()
            //     .with_code("E0384")
            //     .with_message(format!(
            //         "cannot mutate immutable variable `{}`",
            //         original.content,
            //     ))
            //     .with_labels(vec![
            //         Label::secondary((), original.range.clone()).with_message(unindent::unindent(
            //             &format!(
            //                 r#"
            //                     first assignment to `{0}`
            //                     help: make this binding mutable: `mut {0}`
            //                 "#,
            //                 original.content,
            //             ),
            //         )),
            //         Label::primary((), mutating.range.clone())
            //             .with_message("cannot assign twice to immutable variable"),
            //     ]),
        }

        // Diagnostic::error()
        //     .with_code("E0308")
        //     .with_message("error")
        //     .with_labels(vec![
        //         Label::primary((), 0..0).with_message("found this message here!")
        //     ])
    }
}

// // TODO add tests
// impl PartialEq for SteelVal {
//     fn eq(&self, other: &Self) -> bool {
//         match (self, other) {
//             (BoolV(l), BoolV(r)) => l == r,
//             (NumV(l), NumV(r)) => l == r,
//             (StringV(l), StringV(r)) => l == r,
//             (VectorV(l), VectorV(r)) => l == r,
//             (SymbolV(l), SymbolV(r)) => l == r,
//             (CharV(l), CharV(r)) => l == r,
//             //TODO
//             (_, _) => false, // (l, r) => {
//                              //     let left = unwrap!(l, usize);
//                              //     let right = unwrap!(r, usize);
//                              //     match (left, right) {
//                              //         (Ok(l), Ok(r)) => l == r,
//                              //         (_, _) => false,
//                              //     }
//                              // }
//         }
//     }
// }

#[macro_export]
macro_rules! stop {
    ($type:ident) => {
        return Err(SteelErr::$type, None);
    };
    ($type:ident => $fmt:expr, $($arg:tt)+) => {
        return Err(SteelErr::$type(format!($fmt, $($arg)+), None));
    };
    ($type:ident => $thing:expr) => {
        return Err(SteelErr::$type(($thing).to_string(), None));
    };
    ($type:ident => $thing:expr; $span:expr) => {
        return Err(SteelErr::$type(($thing).to_string(), Some($span)));
    };
}

#[macro_export]
macro_rules! throw {
    ($type:ident) => {
        || SteelErr::$type
    };
    ($type:ident => $fmt:expr, $($arg:tt)+) => {
        || SteelErr::$type(format!($fmt, $($arg)+), None)
    };
    ($type:ident => $thing:expr) => {
        || SteelErr::$type(($thing).to_string(), None)
    };
}
