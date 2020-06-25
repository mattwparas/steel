use crate::parser::ParseError;
use std::convert::Infallible;
use thiserror::Error;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term::termcolor::StandardStream;
use codespan_reporting::term::{self, ColorArg};
// use std::ops::Range;
use structopt::StructOpt;

// pub struct Span {
//     expr: String,
// }

#[derive(Debug, Error)]
pub enum SteelErr {
    #[error("Error: Arity Mismatch: {0}")]
    ArityMismatch(String),
    #[error("Error: Free Identifier: {0}")]
    FreeIdentifier(String),
    #[error("Error: Expected {0}")]
    TypeMismatch(String),
    #[error("Error: Unexpected Token {0}")]
    UnexpectedToken(String),
    #[error("Error: Contract Violation: {0}")]
    ContractViolation(String),
    #[error("Error: Bad Syntax: {0}")]
    BadSyntax(String),
    #[error("Error: Conversion Error: {0}")]
    ConversionError(String),
    #[error("Error: IO error")]
    Io(#[from] std::io::Error),
    #[error("Error: Parse error: {0}")]
    Parse(#[from] ParseError),
    #[error("Error: Infallible")]
    Infallible(#[from] Infallible),
    #[error("Error: Generic Error: {0}")]
    Generic(String),
}

impl PartialEq for SteelErr {
    fn eq(&self, other: &Self) -> bool {
        use SteelErr::*;
        match (self, other) {
            (ArityMismatch(l), ArityMismatch(r)) => l == r,
            (FreeIdentifier(l), FreeIdentifier(r)) => l == r,
            (TypeMismatch(l), TypeMismatch(r)) => l == r,
            (UnexpectedToken(l), UnexpectedToken(r)) => l == r,
            (ContractViolation(l), ContractViolation(r)) => l == r,
            (BadSyntax(l), BadSyntax(r)) => l == r,
            (ConversionError(l), ConversionError(r)) => l == r,
            (Generic(l), Generic(r)) => l == r,
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
    pub fn emit_result(&self, file_name: &str, file_content: &str, error_expr: &str) {
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

        let report = self.report(file_name, file_content, error_expr);
        term::emit(&mut writer.lock(), &config, &file, &report).unwrap(); // TODO come back

        // for diagnostic in errors.iter().map(Error::report) {
        //     term::emit(&mut writer.lock(), &config, &file, &diagnostic)?;
        // }
    }

    fn report(&self, _file_name: &str, file_content: &str, error_expr: &str) -> Diagnostic<()> {
        let mut l = 0;
        let mut r = 10;

        if let Some(lp) = file_content.find(error_expr) {
            l = lp;
            r = l + error_expr.as_bytes().len();
        }

        // if let Some(rp) = file_content.rfind(error_expr) {
        //     r = rp
        // }

        println!("{}, {}", l, r);

        // let file = SimpleFile::new(
        //     file_name,
        //     file_content
        // );

        match self {
            Self::ArityMismatch(m) => {
                Diagnostic::error()
                .with_code("E0308")
                .with_message("arity mismatch")
                .with_labels(vec![
                    Label::primary((), l..r).with_message(m)
                ])
            }
            Self::FreeIdentifier(m) => {
                Diagnostic::error()
                .with_code("E0308")
                .with_message("free identifier")
                .with_labels(vec![
                    Label::primary((), l..r).with_message(m)
                ])
            }
            Self::TypeMismatch(m) => {
                Diagnostic::error()
                .with_code("E0308")
                .with_message("type mismatch")
                .with_labels(vec![
                    Label::primary((), l..r).with_message(m)
                ])
            }
            Self::UnexpectedToken(m) => {
                Diagnostic::error()
                .with_code("E0308")
                .with_message("unexpected token")
                .with_labels(vec![
                    Label::primary((), l..r).with_message(m)
                ])
            }
            Self::ContractViolation(m) => {
                Diagnostic::error()
                .with_code("E0308")
                .with_message("contract violation")
                .with_labels(vec![
                    Label::primary((), l..r).with_message(m)
                ])
            }
            Self::BadSyntax(m) => {
                Diagnostic::error()
                .with_code("E0308")
                .with_message("bad syntax")
                .with_labels(vec![
                    Label::primary((), l..r).with_message(m)
                ])
            }
            Self::ConversionError(m) => {
                Diagnostic::error()
                .with_code("E0308")
                .with_message("conversion error")
                .with_labels(vec![
                    Label::primary((), l..r).with_message(m)
                ])
            }
            Self::Io(m) => {
                Diagnostic::error()
                .with_code("E0308")
                .with_message("io error")
                .with_labels(vec![
                    Label::primary((), l..r).with_message(m.to_string())
                ])
            }
            Self::Parse(m) => {
                Diagnostic::error()
                .with_code("E0308")
                .with_message("parse error")
                .with_labels(vec![
                    Label::primary((), l..r).with_message(m.to_string())
                ])
            }
            Self::Infallible(m) => {
                Diagnostic::error()
                .with_code("E0308")
                .with_message("infallible")
                .with_labels(vec![
                    Label::primary((), l..r).with_message(m.to_string())
                ])
            }
            Self::Generic(m) => {
                Diagnostic::error()
                .with_code("E0308")
                .with_message("general")
                .with_labels(vec![
                    Label::primary((), l..r).with_message(m)
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
        return Err(SteelErr::$type);
    };
    ($type:ident => $fmt:expr, $($arg:tt)+) => {
        return Err(SteelErr::$type(format!($fmt, $($arg)+)));
    };
    ($type:ident => $thing:expr) => {
        return Err(SteelErr::$type(($thing).to_string()));
    };
}

#[macro_export]
macro_rules! throw {
    ($type:ident) => {
        || SteelErr::$type
    };
    ($type:ident => $fmt:expr, $($arg:tt)+) => {
        || SteelErr::$type(format!($fmt, $($arg)+))
    };
    ($type:ident => $thing:expr) => {
        || SteelErr::$type(($thing).to_string())
    };
}
