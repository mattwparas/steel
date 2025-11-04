use crate::compiler::passes::VisitorMutUnitRef;
use crate::primitives::numbers::make_polar;
use crate::rvals::{IntoSteelVal, SteelComplex, SteelString};
use crate::HashSet;
use crate::{parser::tokens::TokenType::*, rvals::FromSteelVal};

use fxhash::FxHashMap;
use num_rational::{BigRational, Rational32};
use once_cell::sync::Lazy;
use parking_lot::Mutex;
use std::borrow::Cow;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use steel_parser::interner::InternedString;
use steel_parser::tokens::{IntLiteral, NumberLiteral, RealLiteral, TokenType};

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
            Ok(SourceId(*v as _))
        } else {
            stop!(TypeMismatch => "Unable to convert steelval: {} into source id", val)
        }
    }
}

#[derive(Debug, Default, Clone, Serialize, Deserialize)]
struct GcMetadata {
    size_in_bytes: usize,

    threshold: usize,
}

static GLOBAL_SOURCE_MAPPING: Lazy<Mutex<FxHashMap<SourceId, PathBuf>>> =
    Lazy::new(|| Mutex::new(FxHashMap::default()));

#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub(crate) struct InterierSources {
    paths: crate::HashMap<SourceId, PathBuf>,
    reverse: crate::HashMap<PathBuf, SourceId>,
    // TODO: The sources here are just ever growing.
    // Really, we shouldn't even do this. Having to index
    // into the list isn't particularly necessary, we could
    // just use a hashmap, which would allow us to shrink
    // the sources later by pruning sources that are
    // duplicate. Expressions that are just eval'd are
    // eventually going to take up a lot of space
    // in this. Those expressions should have some kind
    // of weak reference back to the program that we have.
    //
    // Every time the sources are pointing in to this and we want
    // to do some GC, we will just have to walk all of the sources
    // and collect the spans. It isn't fun, but it could be
    // a fine way to remove sources.
    sources: crate::HashMap<SourceId, Arc<Cow<'static, str>>>,

    gc_metadata: GcMetadata,
}

impl InterierSources {
    pub fn new() -> Self {
        InterierSources {
            paths: crate::HashMap::new(),
            reverse: crate::HashMap::new(),
            sources: crate::HashMap::new(),
            gc_metadata: GcMetadata {
                size_in_bytes: 0,
                // Start with 8 Mb. Which will grow to 64 if there
                // is sufficient pressure.
                threshold: 1024 * 1024 * 8,
            },
        }
    }

    pub fn size_in_bytes(&self) -> usize {
        self.sources
            .values()
            .map(|x| {
                let x: &str = x;
                std::mem::size_of_val(x)
            })
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
                let expr = source.into();
                self.gc_metadata.size_in_bytes += expr.len();
                let old = self.sources.insert(*id, Arc::new(expr));
                if let Some(old) = old {
                    self.gc_metadata.size_in_bytes -= old.len();
                }
                return *id;
            }
        }

        let id = SourceId::fresh();
        let expr = source.into();

        self.gc_metadata.size_in_bytes += expr.len();

        self.sources.insert(id, Arc::new(expr));

        if let Some(path) = path {
            self.paths.insert(id, path.clone());
            GLOBAL_SOURCE_MAPPING.lock().insert(id, path.clone());
            self.reverse.insert(path, id);
        }

        id
    }

    pub fn get(&self, source_id: SourceId) -> Option<&Arc<Cow<'static, str>>> {
        self.sources.get(&source_id)
    }

    pub fn get_path(&self, source_id: &SourceId) -> Option<PathBuf> {
        self.paths
            .get(source_id)
            .cloned()
            .or_else(|| GLOBAL_SOURCE_MAPPING.lock().get(source_id).cloned())
    }

    pub fn get_id(&self, path: &Path) -> Option<SourceId> {
        self.reverse.get(path).copied()
    }

    pub fn should_gc(&self) -> bool {
        self.gc_metadata.size_in_bytes > self.gc_metadata.threshold
    }

    pub fn gc(&mut self, roots: HashSet<SourceId>) {
        // let start = self.gc_metadata.size_in_bytes;
        // println!("Size before: {}", start);
        // println!("Threshold: {}", self.gc_metadata.threshold);
        self.sources.retain(|key, _| roots.contains(key));
        self.paths.retain(|key, _| roots.contains(key));
        self.reverse.retain(|_, value| roots.contains(value));

        let remaining = self.size_in_bytes();
        log::debug!("Sources GC: Reclaimed bytes: {}", remaining);

        self.gc_metadata.size_in_bytes = remaining;

        if remaining as f64 > (0.75 * self.gc_metadata.threshold as f64) {
            // println!("Doubling threshold");
            self.gc_metadata.threshold = remaining * 2;
        }
        // println!("Size after: {}", self.gc_metadata.size_in_bytes);
    }
}

#[derive(Default)]
pub(crate) struct SourcesCollector {
    sources: HashSet<SourceId>,
}

impl SourcesCollector {
    pub fn add(&mut self, id: SourceId) {
        self.sources.insert(id);
    }

    pub fn into_set(self) -> HashSet<SourceId> {
        self.sources
    }
}

impl<'a> VisitorMutUnitRef<'a> for SourcesCollector {
    fn visit_atom(&mut self, a: &'a steel_parser::ast::Atom) {
        if let Some(source) = a.syn.span.source_id() {
            self.sources.insert(source);
        }
    }
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct Sources {
    pub(crate) sources: InterierSources,
}

impl Default for Sources {
    fn default() -> Self {
        Self::new()
    }
}

impl Sources {
    pub fn new() -> Self {
        Sources {
            sources: InterierSources::new(),
        }
    }

    pub fn add_source(
        &mut self,
        source: impl Into<Cow<'static, str>>,
        path: Option<PathBuf>,
    ) -> SourceId {
        self.sources.add_source(source, path)
    }

    pub fn get_source_id(&self, path: &Path) -> Option<SourceId> {
        self.sources.get_id(path)
    }

    pub fn get_path(&self, source_id: &SourceId) -> Option<PathBuf> {
        self.sources.get_path(source_id)
    }

    pub fn size_in_bytes(&self) -> usize {
        self.sources.size_in_bytes()
    }

    pub(crate) fn should_gc(&self) -> bool {
        self.sources.should_gc()
    }

    pub(crate) fn gc(&mut self, roots: HashSet<SourceId>) {
        // let mut guard = self.sources.lock().unwrap();
        // Drop anything that isn't present
        self.sources.gc(roots);
    }
}

thread_local! {
    static LAMBDA_SYMBOL: SteelString = "lambda".into();
}

fn real_literal_to_steelval(r: RealLiteral) -> Result<SteelVal, SteelErr> {
    match r {
        RealLiteral::Int(IntLiteral::Small(x)) => x.into_steelval(),
        RealLiteral::Int(IntLiteral::Big(x)) => x.into_steelval(),
        RealLiteral::Rational(numer, denom) => match (&numer, &denom) {
            (IntLiteral::Small(n), IntLiteral::Small(d)) => {
                match (i32::try_from(*n), i32::try_from(*d)) {
                    (_, Ok(0)) => steelerr!(BadSyntax => "division by zero in {}/0", numer),
                    (Ok(n), Ok(d)) => Rational32::new(n, d).into_steelval(),
                    (_, _) => BigRational::new(numer.into(), denom.into()).into_steelval(),
                }
            }
            (_, _) => BigRational::new(numer.into(), denom.into()).into_steelval(),
        },
        RealLiteral::Float(f) => f.into_steelval(),
    }
}

impl IntoSteelVal for NumberLiteral {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        match self {
            NumberLiteral::Real(r) => real_literal_to_steelval(r),
            NumberLiteral::Complex(re, im) => SteelComplex {
                re: real_literal_to_steelval(re)?,
                im: real_literal_to_steelval(im)?,
            }
            .into_steelval(),
            NumberLiteral::Polar(r, theta) => {
                let r = real_literal_to_steelval(r)?;
                let theta = real_literal_to_steelval(theta)?;

                make_polar(&r, &theta)
            }
        }
    }
}

impl IntoSteelVal for &NumberLiteral {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        // as we do not have an owned `self`, we have to clone here, as
        // we need all the BigInts owned
        self.clone().into_steelval()
    }
}

impl TryFrom<TokenType<InternedString>> for SteelVal {
    type Error = SteelErr;

    fn try_from(value: TokenType<InternedString>) -> Result<Self, Self::Error> {
        match value {
            OpenParen(p, _) => Err(SteelErr::new(
                ErrorKind::UnexpectedToken,
                p.open().to_string(),
            )),
            CloseParen(p) => Err(SteelErr::new(
                ErrorKind::UnexpectedToken,
                p.close().to_string(),
            )),
            CharacterLiteral(x) => Ok(CharV(x)),
            BooleanLiteral(x) => Ok(BoolV(x)),
            Identifier(x) => Ok(SymbolV(x.into())),
            Number(x) => x.into_steelval(),
            StringLiteral(x) => Ok(StringV(x.into())),
            Keyword(x) => Ok(SymbolV(x.into())),
            QuoteTick => Err(SteelErr::new(ErrorKind::UnexpectedToken, "'".to_string())),
            Unquote => Err(SteelErr::new(ErrorKind::UnexpectedToken, ",".to_string())),
            QuasiQuote => Err(SteelErr::new(ErrorKind::UnexpectedToken, "`".to_string())),
            UnquoteSplice => Err(SteelErr::new(ErrorKind::UnexpectedToken, ",@".to_string())),
            Comment => Err(SteelErr::new(
                ErrorKind::UnexpectedToken,
                "comment".to_string(),
            )),
            DatumComment => Err(SteelErr::new(ErrorKind::UnexpectedToken, "#;".to_string())),
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
            QuasiQuoteSyntax => Err(SteelErr::new(ErrorKind::UnexpectedToken, "#`".to_string())),
            UnquoteSyntax => Err(SteelErr::new(ErrorKind::UnexpectedToken, "#,".to_string())),
            QuoteSyntax => Err(SteelErr::new(ErrorKind::UnexpectedToken, "#'".to_string())),
            UnquoteSpliceSyntax => {
                Err(SteelErr::new(ErrorKind::UnexpectedToken, "#,@".to_string()))
            }
            Dot => Err(SteelErr::new(ErrorKind::UnexpectedToken, ".".to_string())),
        }
    }
}

impl TryFrom<SyntaxObject> for SteelVal {
    type Error = SteelErr;

    fn try_from(e: SyntaxObject) -> std::result::Result<Self, Self::Error> {
        let span = e.span;
        match e.ty {
            OpenParen(..) => {
                Err(SteelErr::new(ErrorKind::UnexpectedToken, format!("{}", e.ty)).with_span(span))
            }
            CloseParen(p) => Err(
                SteelErr::new(ErrorKind::UnexpectedToken, p.close().to_string()).with_span(span),
            ),
            CharacterLiteral(x) => Ok(CharV(x)),
            BooleanLiteral(x) => Ok(BoolV(x)),
            Identifier(x) => Ok(SymbolV(x.into())),
            Number(x) => x.into_steelval(),
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
            Comment => Err(
                SteelErr::new(ErrorKind::UnexpectedToken, "comment".to_string()).with_span(span),
            ),
            DatumComment => {
                Err(SteelErr::new(ErrorKind::UnexpectedToken, "#;".to_string()).with_span(span))
            }
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
