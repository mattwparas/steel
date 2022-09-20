use crate::parser::parser::ParseError;
use std::{convert::Infallible, fmt::Formatter, path::PathBuf};
use thiserror::Error;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, NoColor, StandardStream};

use crate::parser::span::Span;

use std::fmt;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
struct Repr {
    pub kind: ErrorKind,
    pub message: String,
    pub span: Option<Span>,
    pub source: Option<Rc<PathBuf>>,
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
    pub fn to_error_code(&self) -> &str {
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

impl SteelErr {
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

    pub fn has_span(&self) -> bool {
        self.repr.span.is_some()
    }

    pub fn set_span(mut self, span: Span) -> Self {
        self.repr.set_span(span);
        self
    }

    pub fn set_span_if_none(mut self, span: Span) -> Self {
        if self.repr.span.is_none() {
            self.repr.set_span(span);
        }

        self
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.repr.span = Some(span);
        self
    }

    pub fn with_source(mut self, source: Option<Rc<PathBuf>>) -> Self {
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

    pub fn emit_result_to_string(&self, file_name: &str, file_content: &str) -> String {
        // let writer = StandardStream::from(String::new());
        // let mut writer = String::new();
        let mut writer = NoColor::new(Vec::<u8>::new());
        let config = codespan_reporting::term::Config::default();

        let file = SimpleFile::new(file_name, file_content);

        let error_span = Span::new(0, 0);

        let report = self.report(file_name, file_content, error_span);
        term::emit(&mut writer, &config, &file, &report).unwrap(); // TODO come back
        let output = writer.into_inner();
        std::str::from_utf8(&output).unwrap().to_string()
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
    }
}

pub fn report_warning(
    error_code: &str,
    file_name: &str,
    file_content: &str,
    message: String,
    span: Span,
) {
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();

    let file = SimpleFile::new(file_name, file_content);

    let report = Diagnostic::warning()
        .with_code(error_code)
        .with_labels(vec![Label::primary((), span).with_message(message)]);

    term::emit(&mut writer.lock(), &config, &file, &report).unwrap(); // TODO come back
}

pub fn report_error(
    error_code: &str,
    file_name: &str,
    file_content: &str,
    message: String,
    span: Span,
) {
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();

    let file = SimpleFile::new(file_name, file_content);

    let report = Diagnostic::error()
        .with_code(error_code)
        .with_labels(vec![Label::primary((), span).with_message(message)]);

    term::emit(&mut writer.lock(), &config, &file, &report).unwrap(); // TODO come back
}

pub fn report_info(
    error_code: &str,
    file_name: &str,
    file_content: &str,
    message: String,
    span: Span,
) {
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();

    let file = SimpleFile::new(file_name, file_content);

    let report = Diagnostic::note()
        .with_code(error_code)
        .with_labels(vec![Label::primary((), span).with_message(message)]);

    term::emit(&mut writer.lock(), &config, &file, &report).unwrap(); // TODO come back
}

#[macro_export]
macro_rules! steelerr {
    // ($type:ident) => {
    //     return Err(SteelErr::new(ErrorKind::$type, None));
    // };
    ($type:ident => $fmt:expr, $($arg:tt)+) => {
        Err($crate::rerrs::SteelErr::new($crate::rerrs::ErrorKind::$type, format!($fmt, $($arg)+)))
    };
    ($type:ident => $thing:expr) => {
        Err($crate::rerrs::SteelErr::new($crate::rerrs::ErrorKind::$type, ($thing).to_string()))
    };
    ($type:ident => $thing:expr; $span:expr) => {
        Err($crate::rerrs::SteelErr::new($crate::rerrs::ErrorKind::$type, ($thing).to_string()).with_span($span))
    };
    ($type:ident => $thing:expr; $span:expr; $source:expr) => {
        Err($crate::rerrs::SteelErr::new($crate::rerrs::ErrorKind::$type, ($thing).to_string()).with_span($span).with_source($source))
    };
}

#[macro_export]
macro_rules! stop {
    // ($type:ident) => {
    //     return Err(SteelErr::new(ErrorKind::$type, None));
    // };
    ($type:ident => $fmt:expr, $($arg:tt)+) => {
        return Err($crate::rerrs::SteelErr::new($crate::rerrs::ErrorKind::$type, format!($fmt, $($arg)+)))
    };
    ($type:ident => $thing:expr) => {
        return Err($crate::rerrs::SteelErr::new($crate::rerrs::ErrorKind::$type, ($thing).to_string()))
    };
    ($type:ident => $thing:expr; $span:expr) => {
        return Err($crate::rerrs::SteelErr::new($crate::rerrs::ErrorKind::$type, ($thing).to_string()).with_span($span))
    };
    ($type:ident => $thing:expr; $span:expr; $source:expr) => {
        return Err($crate::rerrs::SteelErr::new($crate::rerrs::ErrorKind::$type, ($thing).to_string()).with_span($span).with_source($source))
    };
}

#[macro_export]
macro_rules! throw {
    // ($type:ident) => {
    //     || SteelErr::$type
    // };
    ($type:ident => $fmt:expr, $($arg:tt)+) => {
        || $crate::rerrs::SteelErr::new($crate::rerrs::ErrorKind::$type, format!($fmt, $($arg)+))
    };
    ($type:ident => $thing:expr) => {
        || $crate::rerrs::SteelErr::new($crate::rerrs::ErrorKind::$type, ($thing).to_string())
    };
    ($type:ident => $thing:expr; $span:expr) => {
        || $crate::rerrs::SteelErr::new($crate::rerrs::ErrorKind::$type, ($thing).to_string()).with_span($span)
    };
}
