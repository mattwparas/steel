use crate::rvals::{IntoSteelVal, SteelComplex, SteelString};
use crate::{parser::tokens::TokenType::*, rvals::FromSteelVal};

use num::BigRational;
use std::borrow::Cow;
use std::str;
use std::sync::{Arc, Mutex};
use std::{collections::HashMap, path::PathBuf};
use steel_parser::tokens::{IntLiteral, NumberLiteral, RealLiteral};

use serde::{Deserialize, Serialize};
use std::convert::TryFrom;

use crate::rerrs::{ErrorKind, SteelErr};
use crate::rvals::SteelVal;
use crate::rvals::SteelVal::*;

pub use steel_parser::parser::{
    lower_entire_ast, lower_macro_and_require_definitions, lower_syntax_rules, FunctionId, ListId,
    ParseError, Parser, RawSyntaxObject, SourceId, SyntaxObject, SyntaxObjectId, SYNTAX_OBJECT_ID,
};

impl IntoSteelVal for SourceId {
    fn into_steelval(self) -> crate::rvals::Result<SteelVal> {
        self.0.into_steelval()
    }
}

impl FromSteelVal for SourceId {
    fn from_steelval(val: &SteelVal) -> crate::rvals::Result<Self> {
        if let SteelVal::IntV(v) = val {
            Ok(SourceId(*v as usize))
        } else {
            stop!(TypeMismatch => "Unable to convert steelval: {} into source id", val)
        }
    }
}

#[derive(Default, Clone, Serialize, Deserialize)]
pub(crate) struct InterierSources {
    paths: HashMap<SourceId, PathBuf>,
    reverse: HashMap<PathBuf, SourceId>,
    sources: Vec<Cow<'static, str>>,
}

impl InterierSources {
    pub fn new() -> Self {
        InterierSources {
            paths: HashMap::new(),
            reverse: HashMap::new(),
            sources: Vec::new(),
        }
    }

    pub fn size_in_bytes(&self) -> usize {
        self.sources
            .iter()
            .map(|x| std::mem::size_of_val(&*x))
            .sum()
    }

    // TODO: Source Id should probably be a weak pointer back here rather than an ID
    // that way if in the event we _do_ leak some strings or the sources are just unreachable
    // we can clean up any remaining things
    pub fn add_source(
        &mut self,
        source: impl Into<Cow<'static, str>>,
        path: Option<PathBuf>,
    ) -> SourceId {
        // We're overwriting the existing source
        if let Some(path) = &path {
            if let Some(id) = self.reverse.get(path) {
                self.sources[id.0] = source.into();
                return *id;
            }
        }

        let index = self.sources.len();
        self.sources.push(source.into());

        let id = SourceId(index);

        if let Some(path) = path {
            self.paths.insert(id, path.clone());
            self.reverse.insert(path, id);
        }

        id
    }

    pub fn get(&self, source_id: SourceId) -> Option<&Cow<'static, str>> {
        self.sources.get(source_id.0)
    }

    pub fn get_path(&self, source_id: &SourceId) -> Option<PathBuf> {
        self.paths.get(source_id).cloned()
    }

    pub fn get_id(&self, path: &PathBuf) -> Option<SourceId> {
        self.reverse.get(path).copied()
    }
}

#[derive(Clone, Serialize, Deserialize)]
pub struct Sources {
    pub(crate) sources: Arc<Mutex<InterierSources>>,
}

impl Default for Sources {
    fn default() -> Self {
        Self::new()
    }
}

impl Sources {
    pub fn new() -> Self {
        Sources {
            sources: Arc::new(Mutex::new(InterierSources::new())),
        }
    }

    pub fn add_source(
        &mut self,
        source: impl Into<Cow<'static, str>>,
        path: Option<PathBuf>,
    ) -> SourceId {
        self.sources.lock().unwrap().add_source(source, path)
    }

    pub fn get_source_id(&self, path: &PathBuf) -> Option<SourceId> {
        self.sources.lock().unwrap().get_id(path)
    }

    pub fn get_path(&self, source_id: &SourceId) -> Option<PathBuf> {
        self.sources.lock().unwrap().get_path(source_id)
    }

    pub fn size_in_bytes(&self) -> usize {
        self.sources.lock().unwrap().size_in_bytes()
    }
}

thread_local! {
    static LAMBDA_SYMBOL: SteelString = "lambda".into();
}

fn real_literal_to_steelval(r: RealLiteral) -> Result<SteelVal, SteelErr> {
    match r {
        RealLiteral::Int(IntLiteral::Small(x)) => x.into_steelval(),
        RealLiteral::Int(IntLiteral::Big(x)) => x.into_steelval(),
        RealLiteral::Rational(n, d) => BigRational::new(n.into(), d.into()).into_steelval(),
        RealLiteral::Float(f) => f.into_steelval(),
    }
}

impl TryFrom<SyntaxObject> for SteelVal {
    type Error = SteelErr;

    fn try_from(e: SyntaxObject) -> std::result::Result<Self, Self::Error> {
        let span = e.span;
        match e.ty {
            OpenParen => {
                Err(SteelErr::new(ErrorKind::UnexpectedToken, "(".to_string()).with_span(span))
            }
            CloseParen => {
                Err(SteelErr::new(ErrorKind::UnexpectedToken, ")".to_string()).with_span(span))
            }
            CharacterLiteral(x) => Ok(CharV(x)),
            BooleanLiteral(x) => Ok(BoolV(x)),
            Identifier(x) => Ok(SymbolV(x.into())),
            Number(x) => match x {
                NumberLiteral::Real(r) => real_literal_to_steelval(r),
                NumberLiteral::Complex(re, im) => SteelComplex {
                    re: real_literal_to_steelval(re)?,
                    im: real_literal_to_steelval(im)?,
                }
                .into_steelval(),
            },
            StringLiteral(x) => Ok(StringV(x.into())),
            Keyword(x) => Ok(SymbolV(x.into())),
            QuoteTick => {
                Err(SteelErr::new(ErrorKind::UnexpectedToken, "'".to_string()).with_span(span))
            }
            Unquote => {
                Err(SteelErr::new(ErrorKind::UnexpectedToken, ",".to_string()).with_span(span))
            }
            QuasiQuote => {
                Err(SteelErr::new(ErrorKind::UnexpectedToken, "`".to_string()).with_span(span))
            }
            UnquoteSplice => {
                Err(SteelErr::new(ErrorKind::UnexpectedToken, ",@".to_string()).with_span(span))
            }
            Error => {
                Err(SteelErr::new(ErrorKind::UnexpectedToken, "error".to_string()).with_span(span))
            }
            Comment => Err(
                SteelErr::new(ErrorKind::UnexpectedToken, "comment".to_string()).with_span(span),
            ),
            If => Ok(SymbolV("if".into())),
            Define => Ok(SymbolV("define".into())),
            Let => Ok(SymbolV("let".into())),
            TestLet => Ok(SymbolV("%plain-let".into())),
            Return => Ok(SymbolV("return!".into())),
            Begin => Ok(SymbolV("begin".into())),
            Lambda => Ok(SymbolV(LAMBDA_SYMBOL.with(|x| x.clone()))),
            Quote => Ok(SymbolV("quote".into())),
            DefineSyntax => Ok(SymbolV("define-syntax".into())),
            SyntaxRules => Ok(SymbolV("syntax-rules".into())),
            Ellipses => Ok(SymbolV("...".into())),
            Set => Ok(SymbolV("set!".into())),
            Require => Ok(SymbolV("require".into())),
            QuasiQuoteSyntax => {
                Err(SteelErr::new(ErrorKind::UnexpectedToken, "#`".to_string()).with_span(span))
            }
            UnquoteSyntax => {
                Err(SteelErr::new(ErrorKind::UnexpectedToken, "#,".to_string()).with_span(span))
            }
            QuoteSyntax => {
                Err(SteelErr::new(ErrorKind::UnexpectedToken, "#'".to_string()).with_span(span))
            }
            UnquoteSpliceSyntax => {
                Err(SteelErr::new(ErrorKind::UnexpectedToken, "#,@".to_string()).with_span(span))
            }
            Dot => Err(SteelErr::new(ErrorKind::UnexpectedToken, ".".to_string()).with_span(span)),
        }
    }
}
