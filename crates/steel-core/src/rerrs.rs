use crate::steel_vm::vm::DehydratedCallContext;
use crate::{parser::parser::ParseError, rvals::Custom, steel_vm::vm::DehydratedStackTrace};
use std::io::IsTerminal;
use std::{convert::Infallible, fmt::Formatter};
// use thiserror::Error;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, NoColor, StandardStream};

use crate::parser::span::Span;

use std::fmt;

#[derive(Clone, Debug, PartialEq)]
#[repr(C)]
struct Repr {
    pub kind: ErrorKind,
    pub message: String,
    pub span: Option<Span>,
    // pub source: Option<Rc<PathBuf>>,
    pub stack_trace: Option<DehydratedStackTrace>,
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

impl Custom for Repr {}

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
            // source: None,
            stack_trace: None,
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
            // source: None,
            stack_trace: None,
        }
    }
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{self:?}")
    }
}

impl From<ParseError> for Repr {
    fn from(v: ParseError) -> Self {
        // unimplemented!()
        let (span, _source) = match &v {
            ParseError::UnexpectedEOF(source) => (None, source),
            ParseError::Unexpected(_, s, source)
            | ParseError::UnexpectedChar(_, s, source)
            | ParseError::IncompleteString(_, s, source)
            | ParseError::SyntaxError(_, s, source)
            | ParseError::ArityMismatch(_, s, source) => (Some(*s), source),
        };

        Repr {
            kind: ErrorKind::Parse,
            message: v.to_string(),
            span,
            // source: source.clone(),
            stack_trace: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
#[repr(C)]
pub struct SteelErr {
    repr: Box<Repr>,
}

impl std::error::Error for SteelErr {}

impl fmt::Display for SteelErr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.repr)
    }
}

impl Custom for SteelErr {
    fn fmt(&self) -> Option<std::result::Result<String, std::fmt::Error>> {
        Some(Ok(format!("{}", self)))
    }
}

impl SteelErr {
    fn _new(repr: Repr) -> Self {
        SteelErr {
            repr: Box::new(repr),
        }
    }

    pub fn kind(&self) -> ErrorKind {
        self.repr.kind
    }

    pub fn set_kind(&mut self, kind: ErrorKind) {
        self.repr.kind = kind;
    }

    pub fn prepend_message(&mut self, message: &str) {
        self.repr.message.insert_str(0, &message)
    }

    pub fn new(kind: ErrorKind, message: String) -> Self {
        SteelErr {
            repr: Box::new(Repr {
                kind,
                message,
                span: None,
                // source: None,
                stack_trace: None,
            }),
        }
    }

    pub fn span(&self) -> Option<Span> {
        self.repr.span
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

    // pub fn with_source(mut self, source: Option<Rc<PathBuf>>) -> Self {
    //     self.repr.source = source;
    //     self
    // }

    pub fn with_stack_trace(mut self, stack_trace: DehydratedStackTrace) -> Self {
        self.repr.stack_trace = Some(stack_trace);
        self
    }

    pub fn stack_trace(&self) -> &Option<DehydratedStackTrace> {
        &self.repr.stack_trace
    }

    pub fn push_span_context_to_stack_trace_if_trace_exists(&mut self, span: Span) {
        if let Some(stacktrace) = &mut self.repr.stack_trace {
            stacktrace.push(DehydratedCallContext::new(Some(span)))
        }
    }

    pub fn emit_result(&self, file_name: &str, file_content: &str) {
        // let opts = Opts::();
        // let config = codespan_reporting::term::Config::default();
        let color = if std::io::stderr().is_terminal() {
            ColorChoice::Auto
        } else {
            ColorChoice::Never
        };

        let writer = StandardStream::stderr(color);
        let config = codespan_reporting::term::Config::default();

        let file = SimpleFile::new(file_name, file_content);

        let report = self.report();

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

        let report = self.report();
        term::emit(&mut writer, &config, &file, &report).unwrap(); // TODO come back
        let output = writer.into_inner();
        std::str::from_utf8(&output).unwrap().to_string()
    }

    fn report(&self) -> Diagnostic<()> {
        Diagnostic::error()
            .with_code(self.repr.kind.to_error_code())
            .with_message(self.repr.kind.to_string())
            .with_labels(vec![Label::primary(
                (),
                self.repr.span.unwrap_or(Span::new(0, 0, None)),
            )
            .with_message(&self.repr.message)])
    }

    pub(crate) fn message(&self) -> &str {
        &self.repr.message
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

pub fn back_trace(file_name: &str, file_content: &str, span: Span) {
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();

    let file = SimpleFile::new(file_name, file_content);

    let report = Diagnostic::note().with_labels(vec![Label::primary((), span)]);

    term::emit(&mut writer.lock(), &config, &file, &report).unwrap(); // TODO come back
}

// TODO: Abstract this to just take the writer interface optionally, otherwise just write to a string?
pub fn back_trace_to_string(file_name: &str, file_content: &str, span: Span) -> String {
    let mut writer = NoColor::new(Vec::<u8>::new());
    let config = codespan_reporting::term::Config::default();

    let file = SimpleFile::new(file_name, file_content);

    let report = Diagnostic::note().with_labels(vec![Label::primary((), span)]);

    term::emit(&mut writer, &config, &file, &report).unwrap(); // TODO come back

    let output = writer.into_inner();

    std::str::from_utf8(&output).unwrap().to_string()
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
    ($type:ident => $fmt:expr, $($arg:tt),+ ; $span:expr) => {
        return Err($crate::rerrs::SteelErr::new($crate::rerrs::ErrorKind::$type, format!($fmt, $($arg),+)).with_span($span))
    };
    ($type:ident => $fmt:expr, $($arg:tt)+) => {
        return Err($crate::rerrs::SteelErr::new($crate::rerrs::ErrorKind::$type, format!($fmt, $($arg)+)))
    };
    ($type:ident => $thing:expr) => {
        return Err($crate::rerrs::SteelErr::new($crate::rerrs::ErrorKind::$type, ($thing).to_string()))
    };
    ($type:ident => $thing:expr; $span:expr) => {
        return Err($crate::rerrs::SteelErr::new($crate::rerrs::ErrorKind::$type, ($thing).to_string()).with_span($span))
    };
    ($type:ident => $thing:expr; opt $span:expr) => {
        match $span {
            Some(span) => stop!($type => $thing; span),
            None => stop!($type => $thing)
        }
    };
    ($type:ident => $thing:expr; $span:expr; $source:expr) => {
        return Err($crate::rerrs::SteelErr::new($crate::rerrs::ErrorKind::$type, ($thing).to_string()).with_span($span))

        // .with_source($source))
    };
}

#[macro_export]
macro_rules! builtin_stop {
    // ($type:ident) => {
    //     return Err(SteelErr::new(ErrorKind::$type, None));
    // };
    ($type:ident => $fmt:expr, $($arg:tt)+) => {
        return Some(Err($crate::rerrs::SteelErr::new($crate::rerrs::ErrorKind::$type, format!($fmt, $($arg)+))))
    };
    ($type:ident => $thing:expr) => {
        return Some(Err($crate::rerrs::SteelErr::new($crate::rerrs::ErrorKind::$type, ($thing).to_string())))
    };
    ($type:ident => $thing:expr; $span:expr) => {
        return Some(Err($crate::rerrs::SteelErr::new($crate::rerrs::ErrorKind::$type, ($thing).to_string()).with_span($span)))
    };
    ($type:ident => $thing:expr; $span:expr; $source:expr) => {
        return Some(Err($crate::rerrs::SteelErr::new($crate::rerrs::ErrorKind::$type, ($thing).to_string()).with_span($span).with_source($source)))
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
