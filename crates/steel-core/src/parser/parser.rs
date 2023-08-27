use crate::{
    compiler::program::{QUASIQUOTE, RAW_UNQUOTE, RAW_UNQUOTE_SPLICING, UNQUOTE, UNQUOTE_SPLICING},
    parser::lexer::TokenStream,
    rvals::IntoSteelVal,
};
use crate::{
    parser::tokens::{Token, TokenType, TokenType::*},
    rvals::FromSteelVal,
};

use std::result;
use std::str;
use std::{collections::HashMap, path::PathBuf};
use std::{
    rc::Rc,
    sync::{Arc, Mutex},
};
use steel_parser::{
    lexer::{OwnedTokenStream, ToOwnedString},
    tokens::MaybeBigInt,
};

use crate::parser::span::Span;

use crate::parser::ast::*;
use serde::{Deserialize, Serialize};
use std::convert::TryFrom;

use crate::rerrs::{ErrorKind, SteelErr};
use crate::rvals::SteelVal;
use crate::rvals::SteelVal::*;

use super::{ast, interner::InternedString};

use std::sync::atomic::{AtomicUsize, Ordering};

pub use steel_parser::parser::SourceId;

// #[derive(
//     Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Default, Debug, Ord, PartialOrd,
// )]
// pub struct SourceId(pub(crate) usize);

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
    sources: Vec<String>,
}

impl InterierSources {
    pub fn new() -> Self {
        InterierSources {
            paths: HashMap::new(),
            sources: Vec::new(),
        }
    }

    pub fn size_in_bytes(&self) -> usize {
        self.sources
            .iter()
            .map(|x| std::mem::size_of_val(&*x))
            .sum()
    }

    pub fn add_source(&mut self, source: String, path: Option<PathBuf>) -> SourceId {
        let index = self.sources.len();
        self.sources.push(source);

        let id = SourceId(index);

        if let Some(path) = path {
            self.paths.insert(id, path);
        }

        id
    }

    pub fn get(&self, source_id: SourceId) -> Option<&String> {
        self.sources.get(source_id.0)
    }

    pub fn get_path(&self, source_id: &SourceId) -> Option<&PathBuf> {
        self.paths.get(source_id)
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

    pub fn add_source(&mut self, source: String, path: Option<PathBuf>) -> SourceId {
        self.sources.lock().unwrap().add_source(source, path)
    }

    pub fn size_in_bytes(&self) -> usize {
        self.sources.lock().unwrap().size_in_bytes()
    }

    // pub fn get(&self, source_id: SourceId) -> MutexGuard<'_, InteriorSources> {
    //     let guard = self.sources.lock().unwrap();

    //     guard.get(source_id)
    // }

    // pub fn get(&'_ self, source_id: SourceId) -> Option<Ref<'_, String>> {
    //     Ref::filter_map(self.sources.borrow(), |x| x.get(source_id)).ok()
    // }

    // pub fn get_path(&self, source_id: &SourceId) -> Option<Ref<'_, PathBuf>> {
    //     Ref::filter_map(self.sources.borrow(), |x| x.paths.get(source_id)).ok()
    // }

    // pub fn get_path(&self, source_id: &SourceId) -> Option<&PathBuf> {
    //     self.sources.lock().unwrap().paths.get(source_id)
    // }
}

pub(crate) static SYNTAX_OBJECT_ID: AtomicUsize = AtomicUsize::new(0);

#[derive(
    Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Default, Debug, Ord, PartialOrd,
)]
pub struct SyntaxObjectId(pub usize);

impl SyntaxObjectId {
    #[inline]
    pub fn fresh() -> Self {
        SyntaxObjectId(SYNTAX_OBJECT_ID.fetch_add(1, Ordering::Relaxed))
    }
}

impl From<SyntaxObjectId> for usize {
    fn from(value: SyntaxObjectId) -> Self {
        value.0
    }
}

#[derive(
    Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Default, Debug, Ord, PartialOrd,
)]
pub struct ListId(usize);

#[derive(
    Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Default, Debug, Ord, PartialOrd,
)]
pub struct FunctionId(usize);

impl std::fmt::Display for SyntaxObjectId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

/// A syntax object that can hold anything as the syntax
/// In this case, we're using the token type emitted by logos
///
/// This should open the door to interning our strings to make
/// parsing (and optimizations later) faster
#[derive(Serialize, Deserialize)]
pub struct RawSyntaxObject<T> {
    pub(crate) ty: T,
    pub(crate) span: Span,
    // TODO: Just nuke this source here, we don't need it.
    // pub(crate) source: Option<Rc<PathBuf>>,
    // pub(crate) metadata: Option<IdentifierMetadata>,
    pub(crate) syntax_object_id: SyntaxObjectId,
}

impl<T: Clone> Clone for RawSyntaxObject<T> {
    fn clone(&self) -> Self {
        Self {
            ty: self.ty.clone(),
            span: self.span,
            // source: self.source.clone(),
            // metadata: self.metadata.clone(),
            syntax_object_id: SyntaxObjectId(SYNTAX_OBJECT_ID.fetch_add(1, Ordering::Relaxed)),
        }
    }
}

/// Denotes what kind of identifier we actually have
#[derive(Clone, Serialize, Deserialize)]
pub enum IdentifierType {
    Free,
    Global,
    Local,
    Macro,
    Module,
}

#[derive(Clone, Serialize, Deserialize, Hash, Eq, PartialOrd, Ord, PartialEq, Default)]
pub struct IdentifierMetadata {
    // kind: IdentifierType,
    // built_in: bool,
    pub(crate) depth: usize,
}

impl<T: std::fmt::Debug> std::fmt::Debug for RawSyntaxObject<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("RawSyntaxObject")
            .field("ty", &self.ty)
            .field("span", &self.span)
            // .field("source", &self.source)
            .finish()
    }
}

// Implementing hash here just on the token type - we dont want the span included
// For determining the hash here
impl<T: std::hash::Hash> std::hash::Hash for RawSyntaxObject<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.ty.hash(state);
        self.span.hash(state);
    }
}

pub type SyntaxObject = RawSyntaxObject<TokenType<InternedString>>;

// #[derive(Debug, Clone, Serialize, Deserialize)]
// pub struct SyntaxObject {
//     pub(crate) ty: TokenType,
//     pub(crate) span: Span,
//     pub(crate) source: Option<Rc<PathBuf>>,
// }

impl PartialEq for SyntaxObject {
    fn eq(&self, other: &Self) -> bool {
        self.ty == other.ty
    }
}

impl SyntaxObject {
    pub fn new(ty: TokenType<InternedString>, span: Span) -> Self {
        SyntaxObject {
            ty,
            span,
            // source: None,
            syntax_object_id: SyntaxObjectId(SYNTAX_OBJECT_ID.fetch_add(1, Ordering::Relaxed)),
        }
    }

    pub fn default(ty: TokenType<InternedString>) -> Self {
        SyntaxObject {
            ty,
            span: Span::new(0, 0, None),
            // source: None,
            syntax_object_id: SyntaxObjectId(SYNTAX_OBJECT_ID.fetch_add(1, Ordering::Relaxed)),
        }
    }

    pub fn set_span(&mut self, span: Span) {
        self.span = span
    }

    pub fn from_token_with_source(
        val: &Token<'_, InternedString>,
        _source: &Option<Rc<PathBuf>>,
    ) -> Self {
        SyntaxObject {
            ty: val.ty.clone(),
            span: val.span,
            // source: source.as_ref().map(Rc::clone),
            syntax_object_id: SyntaxObjectId(SYNTAX_OBJECT_ID.fetch_add(1, Ordering::Relaxed)),
        }
    }
}

impl From<&Token<'_, InternedString>> for SyntaxObject {
    fn from(val: &Token<'_, InternedString>) -> SyntaxObject {
        SyntaxObject::new(val.ty.clone(), val.span)
    }
}

// impl From<&Token<'_, String>> for SyntaxObject {
//     fn from(val: &Token<'_, String>) -> SyntaxObject {
//         SyntaxObject::new(val.ty.clone(), val.span)
//     }
// }

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
            NumberLiteral(x) => Ok(NumV(x)),
            IntegerLiteral(MaybeBigInt::Small(x)) => Ok(IntV(x)),

            IntegerLiteral(MaybeBigInt::Big(b)) => b.into_steelval(),

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
            TestLet => Ok(SymbolV("test-let".into())),
            Return => Ok(SymbolV("return!".into())),
            Begin => Ok(SymbolV("begin".into())),
            Lambda => Ok(SymbolV("lambda".into())),
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
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ParseError {
    Unexpected(TokenType<String>, Option<Rc<PathBuf>>),
    UnexpectedEOF(Option<Rc<PathBuf>>),
    UnexpectedChar(char, Span, Option<Rc<PathBuf>>),
    IncompleteString(String, Span, Option<Rc<PathBuf>>),
    SyntaxError(String, Span, Option<Rc<PathBuf>>),
    ArityMismatch(String, Span, Option<Rc<PathBuf>>),
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::Unexpected(l, _) => write!(f, "Parse: Unexpected token: {:?}", l),
            ParseError::UnexpectedEOF(_) => write!(f, "Parse: Unexpected EOF"),
            ParseError::UnexpectedChar(l, _, _) => {
                write!(f, "Parse: Unexpected character: {:?}", l)
            }
            ParseError::IncompleteString(l, _, _) => write!(f, "Parse: Incomplete String: {}", l),
            ParseError::SyntaxError(l, _, _) => write!(f, "Parse: Syntax Error: {}", l),
            ParseError::ArityMismatch(l, _, _) => write!(f, "Parse: Arity mismatch: {}", l),
        }
    }
}

impl std::error::Error for ParseError {}

impl ParseError {
    pub fn span(&self) -> Option<Span> {
        match self {
            // ParseError::TokenError(_) => None,
            ParseError::Unexpected(_, _) => None,
            ParseError::UnexpectedEOF(_) => None,
            ParseError::UnexpectedChar(_, s, _) => Some(*s),
            ParseError::IncompleteString(_, s, _) => Some(*s),
            ParseError::SyntaxError(_, s, _) => Some(*s),
            ParseError::ArityMismatch(_, s, _) => Some(*s),
        }
    }

    pub fn set_source(self, source: Option<Rc<PathBuf>>) -> Self {
        use ParseError::*;
        match self {
            ParseError::Unexpected(l, _) => Unexpected(l, source),
            ParseError::UnexpectedEOF(_) => UnexpectedEOF(source),
            ParseError::UnexpectedChar(l, s, _) => UnexpectedChar(l, s, source),
            ParseError::IncompleteString(l, s, _) => IncompleteString(l, s, source),
            ParseError::SyntaxError(l, s, _) => SyntaxError(l, s, source),
            ParseError::ArityMismatch(l, s, _) => ArityMismatch(l, s, source),
        }
    }
}

pub struct InternString;

impl ToOwnedString<InternedString> for InternString {
    fn own(&self, s: &str) -> InternedString {
        s.into()
    }
}

// #[derive(Debug)]
pub struct Parser<'a> {
    tokenizer: OwnedTokenStream<'a, InternedString, InternString>,
    quote_stack: Vec<usize>,
    quasiquote_depth: isize,
    quote_context: bool,
    shorthand_quote_stack: Vec<usize>,
    source_name: Option<Rc<PathBuf>>,
    context: Vec<ParsingContext>,
    comment_buffer: Vec<&'a str>,
    collecting_comments: bool,
}

#[derive(Debug, Copy, Clone)]
enum ParsingContext {
    // Inside of a quote. Expressions should be parsed without being coerced into a typed variant of the AST
    Quote(usize),
    // Shortened version of a quote
    QuoteTick(usize),
    // Inside of an unquote - expressions should actually be parsed as usual
    Unquote(usize),
    // Shortened version of unquote
    UnquoteTick(usize),
    // Treat this like a normal quote
    Quasiquote(usize),
    // Shortened version of quasiquote
    QuasiquoteTick(usize),
    // expressions should parsed as normal
    UnquoteSplicing(usize),
    // Shorted version of Unquote Splicing
    UnquoteSplicingTick(usize),
}

impl<'a> Parser<'a> {
    // #[cfg(test)]
    pub fn parse(expr: &str) -> Result<Vec<ExprKind>> {
        Parser::new(expr, None).collect()
    }
}

pub type Result<T> = result::Result<T, ParseError>;

fn tokentype_error_to_parse_error(t: &Token<'_, InternedString>) -> ParseError {
    if let TokenType::Error = t.ty {
        // println!("Found an error: {}", t);

        if t.source.starts_with('\"') {
            ParseError::IncompleteString(t.source.to_string(), t.span, None)
        } else {
            ParseError::UnexpectedChar(t.source.chars().next().unwrap(), t.span, None)
        }
    } else {
        ParseError::UnexpectedEOF(None)
    }
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str, source_id: Option<SourceId>) -> Self {
        Parser {
            tokenizer: TokenStream::new(input, false, source_id).into_owned(InternString),
            quote_stack: Vec::new(),
            quasiquote_depth: 0,
            quote_context: false,
            shorthand_quote_stack: Vec::new(),
            source_name: None,
            context: Vec::new(),
            comment_buffer: Vec::new(),
            collecting_comments: false,
        }
    }

    pub fn new_from_source(
        input: &'a str,
        source_name: PathBuf,
        source_id: Option<SourceId>,
    ) -> Self {
        Parser {
            tokenizer: TokenStream::new(input, false, source_id).into_owned(InternString),
            quote_stack: Vec::new(),
            quasiquote_depth: 0,
            quote_context: false,
            shorthand_quote_stack: Vec::new(),
            source_name: Some(Rc::from(source_name)),
            context: Vec::new(),
            comment_buffer: Vec::new(),
            collecting_comments: false,
        }
    }

    // Attach comments!
    pub fn doc_comment_parser(input: &'a str, source_id: Option<SourceId>) -> Self {
        Parser {
            tokenizer: TokenStream::new(input, false, source_id).into_owned(InternString),
            quote_stack: Vec::new(),
            quasiquote_depth: 0,
            quote_context: false,
            shorthand_quote_stack: Vec::new(),
            source_name: None,
            context: Vec::new(),
            comment_buffer: Vec::new(),
            collecting_comments: false,
        }
    }

    // TODO this is definitely wrong
    fn construct_quote(&mut self, val: ExprKind, span: Span) -> ExprKind {
        // let q = {
        //     let rc_val = TokenType::Quote;
        //     ExprKind::Atom(Atom::new(SyntaxObject::new(rc_val, span)))
        //     // let val = ExprKind::Atom(Atom::new(SyntaxObject::new(rc_val, span)));
        //     // // self.intern.insert("quote".to_string(), rc_val);
        //     // val
        // };

        // ExprKind::List(List::new(vec![q, val]))

        ExprKind::Quote(Box::new(ast::Quote::new(
            val,
            SyntaxObject::new(TokenType::Quote, span),
        )))
    }

    fn _expand_reader_macro(
        &mut self,
        token: TokenType<InternedString>,
        val: ExprKind,
        span: Span,
    ) -> ExprKind {
        let q = ExprKind::Atom(Atom::new(SyntaxObject::new(token, span)));

        ExprKind::List(List::new(vec![q, val]))
    }

    fn construct_quote_vec(&mut self, val: ExprKind, span: Span) -> Vec<ExprKind> {
        // println!("Inside construct quote vec with: {:?}", val);

        let q = {
            let rc_val = TokenType::Quote;
            ExprKind::Atom(Atom::new(SyntaxObject::new(rc_val, span)))
            // let val = ExprKind::Atom(Atom::new(SyntaxObject::new(rc_val, span)));
            // // self.intern.insert("quote".to_string(), rc_val);
            // val
        };

        vec![q, val]
    }

    // Reader macro for `
    fn construct_quasiquote(&mut self, val: ExprKind, span: Span) -> ExprKind {
        let q = {
            let rc_val = TokenType::Identifier(*QUASIQUOTE);
            ExprKind::Atom(Atom::new(SyntaxObject::new(rc_val, span)))
        };

        ExprKind::List(List::new(vec![q, val]))
    }

    // Reader macro for ,
    fn construct_unquote(&mut self, val: ExprKind, span: Span) -> ExprKind {
        let q = {
            let rc_val = TokenType::Identifier(*UNQUOTE);
            ExprKind::Atom(Atom::new(SyntaxObject::new(rc_val, span)))
        };

        ExprKind::List(List::new(vec![q, val]))
    }

    fn construct_raw_unquote(&mut self, val: ExprKind, span: Span) -> ExprKind {
        let q = {
            let rc_val = TokenType::Identifier(*RAW_UNQUOTE);
            ExprKind::Atom(Atom::new(SyntaxObject::new(rc_val, span)))
        };

        ExprKind::List(List::new(vec![q, val]))
    }
    // Reader macro for ,@
    fn construct_unquote_splicing(&mut self, val: ExprKind, span: Span) -> ExprKind {
        let q = {
            let rc_val = TokenType::Identifier(*UNQUOTE_SPLICING);
            ExprKind::Atom(Atom::new(SyntaxObject::new(rc_val, span)))
        };

        ExprKind::List(List::new(vec![q, val]))
    }

    // Reader macro for ,@
    fn construct_raw_unquote_splicing(&mut self, val: ExprKind, span: Span) -> ExprKind {
        let q = {
            let rc_val = TokenType::Identifier(*RAW_UNQUOTE_SPLICING);
            ExprKind::Atom(Atom::new(SyntaxObject::new(rc_val, span)))
        };

        ExprKind::List(List::new(vec![q, val]))
    }

    fn increment_quasiquote_context_if_not_in_quote_context(&mut self) {
        // println!("INCREMENTING");
        if !self.quote_context {
            self.quasiquote_depth += 1;
        }
    }

    fn decrement_quasiquote_context_if_not_in_quote_context(&mut self) {
        // println!("DECREMENTING");
        if !self.quote_context {
            self.quasiquote_depth -= 1;
        }
    }

    fn read_from_tokens(&mut self) -> Result<ExprKind> {
        let mut stack: Vec<Vec<ExprKind>> = Vec::new();
        let mut current_frame: Vec<ExprKind> = Vec::new();

        self.quote_stack = Vec::new();

        // println!("READING FROM TOKENS");
        // self.quasiquote_depth = 0;

        loop {
            match self.tokenizer.next() {
                Some(token) => {
                    match token.ty {
                        TokenType::Comment => {
                            // println!("Found a comment!");
                            // Internal comments, we're gonna skip for now
                            continue;
                        }
                        TokenType::Error => return Err(tokentype_error_to_parse_error(&token)), // TODO
                        TokenType::QuoteTick => {
                            // quote_count += 1;
                            // self.quote_stack.push(current_frame.len());
                            self.shorthand_quote_stack.push(current_frame.len());

                            let last_context = self.quote_context;

                            if self.quasiquote_depth == 0 {
                                self.quote_context = true;
                            }

                            // println!("Entering context: Quote Tick in read from tokens");

                            self.context
                                .push(ParsingContext::QuoteTick(current_frame.len()));

                            let quote_inner = self
                                .next()
                                .unwrap_or(Err(ParseError::UnexpectedEOF(self.source_name.clone())))
                                .map(|x| {
                                    // if self.quasiquote_depth == 0 {
                                    self.construct_quote(x, token.span)
                                    // } else {
                                    // self.construct_fake_quote(x, token.span)
                                    // }
                                });
                            // self.quote_stack.pop();
                            self.shorthand_quote_stack.pop();

                            self.quote_context = last_context;

                            // println!(
                            //     "Exiting Context: {:?} in read from tokens",
                            //     self.context.pop()
                            // );

                            // self.context.pop();

                            let popped_value = self.context.pop();

                            if let Some(popped) = popped_value {
                                debug_assert!(matches!(popped, ParsingContext::QuoteTick(_)))
                            }

                            current_frame.push(quote_inner?);
                        }
                        TokenType::Unquote => {
                            // println!("Entering context: Unquote");

                            // This could underflow and panic - if its negative then we have a problem. Maybe just use an isize and let it underflow?
                            self.decrement_quasiquote_context_if_not_in_quote_context();

                            self.context
                                .push(ParsingContext::UnquoteTick(current_frame.len()));

                            let quote_inner = self
                                .next()
                                .unwrap_or(Err(ParseError::UnexpectedEOF(self.source_name.clone())))
                                .map(|x| {
                                    // dbg!(self.quasiquote_depth);
                                    // dbg!(self.quote_context);
                                    if self.quasiquote_depth == 0 && !self.quote_context {
                                        self.construct_raw_unquote(x, token.span)
                                    } else {
                                        self.construct_unquote(x, token.span)
                                    }
                                });

                            let popped_value = self.context.pop();

                            self.increment_quasiquote_context_if_not_in_quote_context();

                            if let Some(popped) = popped_value {
                                debug_assert!(matches!(popped, ParsingContext::UnquoteTick(_)))
                            }
                            // println!("Exiting Context: {:?}", self.context.pop());
                            current_frame.push(quote_inner?);
                        }
                        TokenType::QuasiQuote => {
                            // println!("Entering context: Quasiquote");

                            self.increment_quasiquote_context_if_not_in_quote_context();

                            self.context
                                .push(ParsingContext::QuasiquoteTick(current_frame.len()));

                            let quote_inner = self
                                .next()
                                .unwrap_or(Err(ParseError::UnexpectedEOF(self.source_name.clone())))
                                .map(|x| self.construct_quasiquote(x, token.span));

                            // self.context.pop();
                            // println!(
                            //     ">>>>>>>>>>>>>>>>>>> Exiting Context: {:?}",
                            //     self.context.pop()
                            // );

                            let popped_value = self.context.pop();

                            self.decrement_quasiquote_context_if_not_in_quote_context();

                            if let Some(popped) = popped_value {
                                // println!("Popped: {:?}", popped);
                                debug_assert!(matches!(popped, ParsingContext::QuasiquoteTick(_)))
                            }

                            current_frame.push(quote_inner?);
                        }
                        TokenType::UnquoteSplice => {
                            // println!("Entering context: UnquoteSplicing");

                            self.decrement_quasiquote_context_if_not_in_quote_context();

                            self.context
                                .push(ParsingContext::UnquoteSplicingTick(current_frame.len()));

                            let quote_inner = self
                                .next()
                                .unwrap_or(Err(ParseError::UnexpectedEOF(self.source_name.clone())))
                                .map(|x| {
                                    if self.quasiquote_depth == 0 && !self.quote_context {
                                        self.construct_raw_unquote_splicing(x, token.span)
                                    } else {
                                        self.construct_unquote_splicing(x, token.span)
                                    }
                                });

                            // self.context.pop();

                            let popped_value = self.context.pop();

                            self.increment_quasiquote_context_if_not_in_quote_context();

                            if let Some(popped) = popped_value {
                                debug_assert!(matches!(
                                    popped,
                                    ParsingContext::UnquoteSplicingTick(_)
                                ))
                            }

                            // println!("Exiting Context: {:?}", self.context.pop());
                            current_frame.push(quote_inner?);
                        }
                        TokenType::OpenParen => {
                            stack.push(current_frame);
                            current_frame = Vec::new();
                        }
                        TokenType::CloseParen => {
                            // This is the match that we'll want to move inside the below stack.pop() match statement
                            // As we close the current context, we check what our current state is -

                            if let Some(mut prev_frame) = stack.pop() {
                                match self.context.last().cloned() {
                                    // TODO: Change this -> This should really be just Some(ParsingContext::Quote)
                                    // If we have _anything_ then we should check if we need to parse it differently. If we're at the last_quote_index,
                                    // then we can pop it off inside there.
                                    Some(ParsingContext::Quote(last_quote_index))
                                    | Some(ParsingContext::Quasiquote(last_quote_index)) => {
                                        // Exit quasiquote context
                                        // if let Some(ParsingContext::Quasiquote(_)) =
                                        //     self.context.last()
                                        // {
                                        //     self.decrement_quasiquote_context_if_not_in_quote_context();
                                        // }

                                        // println!(
                                        //     "Q/QQ: Stack length: {:?}, last_quote_index: {:?}",
                                        //     stack.len(),
                                        //     last_quote_index
                                        // );

                                        // let last_quote_index = *last_quote_index;

                                        if stack.len() <= last_quote_index {
                                            self.context.pop();
                                            // println!("Exiting Context: {:?}", self.context.pop());
                                        }

                                        // println!("Inside here!");
                                        // println!("Frame: {:?}", current_frame);

                                        match current_frame.first() {
                                            Some(ExprKind::Atom(Atom {
                                                syn:
                                                    SyntaxObject {
                                                        ty: TokenType::Quote,
                                                        ..
                                                    },
                                            })) => match self.context.last() {
                                                Some(
                                                    ParsingContext::Quasiquote(_)
                                                    | ParsingContext::QuasiquoteTick(_)
                                                    | ParsingContext::Quote(_)
                                                    | ParsingContext::QuoteTick(_),
                                                ) => prev_frame
                                                    .push(ExprKind::List(List::new(current_frame))),
                                                _ => {
                                                    prev_frame.push(
                                                        ExprKind::try_from(current_frame).map_err(
                                                            |x| {
                                                                x.set_source(
                                                                    self.source_name.clone(),
                                                                )
                                                            },
                                                        )?,
                                                    );
                                                }
                                            },
                                            _ => {
                                                // println!("Converting to list");
                                                // println!("Context here: {:?}", self.context);
                                                prev_frame
                                                    .push(ExprKind::List(List::new(current_frame)))
                                            }
                                        }
                                    }

                                    Some(ParsingContext::QuoteTick(_))
                                    | Some(ParsingContext::QuasiquoteTick(_)) => {
                                        match current_frame.first() {
                                            Some(ExprKind::Atom(Atom {
                                                syn:
                                                    SyntaxObject {
                                                        ty: TokenType::Quote,
                                                        ..
                                                    },
                                            })) => {
                                                // println!("Converting to quote inside quote tick");
                                                prev_frame.push(
                                                    ExprKind::try_from(current_frame).map_err(
                                                        |x| x.set_source(self.source_name.clone()),
                                                    )?,
                                                );
                                            }
                                            _ => {
                                                // if let Some(ParsingContext::QuasiquoteTick(_)) =
                                                //     self.context.last()
                                                // {
                                                //     self.decrement_quasiquote_context_if_not_in_quote_context();
                                                // }

                                                // println!("Converting to list inside quote tick");
                                                prev_frame
                                                    .push(ExprKind::List(List::new(current_frame)))
                                            }
                                        }
                                    }

                                    // If we're in the short hand reader world, just ignore popping off the stack
                                    // but still treat it as a normal expression
                                    Some(ParsingContext::UnquoteTick(_))
                                    | Some(ParsingContext::UnquoteSplicingTick(_)) => {
                                        // self.quasiquote_depth += 1;

                                        // self.increment_quasiquote_context_if_not_in_quote_context();

                                        // println!(
                                        //     "UQ/UQS: Stack length: {:?}, last_quote_index: {:?}",
                                        //     stack.len(),
                                        //     last_quote_index
                                        // );

                                        // if stack.len() <= *last_quote_index {
                                        //     // println!("Exiting Context: {:?}", self.context.pop());
                                        //     self.context.pop();
                                        // }

                                        prev_frame.push(
                                            ExprKind::try_from(current_frame).map_err(|x| {
                                                x.set_source(self.source_name.clone())
                                            })?,
                                        );
                                    }

                                    Some(ParsingContext::Unquote(last_quote_index))
                                    | Some(ParsingContext::UnquoteSplicing(last_quote_index)) => {
                                        // self.quasiquote_depth += 1;

                                        // self.increment_quasiquote_context_if_not_in_quote_context();

                                        // println!(
                                        //     "UQ/UQS: Stack length: {:?}, last_quote_index: {:?}",
                                        //     stack.len(),
                                        //     last_quote_index
                                        // );

                                        if stack.len() <= last_quote_index {
                                            // println!("Exiting Context: {:?}", self.context.pop());
                                            self.context.pop();
                                        }

                                        prev_frame.push(
                                            ExprKind::try_from(current_frame).map_err(|x| {
                                                x.set_source(self.source_name.clone())
                                            })?,
                                        );
                                    }

                                    // Else case, just go ahead and assume it is a normal frame
                                    _ => prev_frame.push(
                                        ExprKind::try_from(current_frame)
                                            .map_err(|x| x.set_source(self.source_name.clone()))?,
                                    ),
                                }

                                // Reinitialize current frame here
                                current_frame = prev_frame;
                            } else {
                                // println!("Else case: {:?}", current_frame);
                                // println!("Context: {:?}", self.context);

                                match self.context.last() {
                                    Some(ParsingContext::QuoteTick(_))
                                    | Some(ParsingContext::QuasiquoteTick(_)) => {
                                        return Ok(ExprKind::List(List::new(current_frame)));
                                    }
                                    _ => {
                                        // dbg!(self.quasiquote_depth);
                                        // println!("=> {}", List::new(current_frame.clone()));
                                        // println!("----------------------------------------");

                                        if self.quasiquote_depth > 0 {
                                            // TODO/HACK - @Matt
                                            // If we're in a define syntax situation, go ahead and just return a normal one
                                            if current_frame
                                                .first()
                                                .map(|x| x.define_syntax_ident())
                                                .unwrap_or_default()
                                            {
                                                return ExprKind::try_from(current_frame).map_err(
                                                    |x| x.set_source(self.source_name.clone()),
                                                );
                                            }

                                            // println!("Should still be quoted here");

                                            return Ok(ExprKind::List(List::new(current_frame)));
                                        }

                                        return ExprKind::try_from(current_frame)
                                            .map_err(|x| x.set_source(self.source_name.clone()));
                                    }
                                }
                            }
                        }

                        _ => {
                            if let TokenType::Quote = &token.ty {
                                // self.quote_stack.push(current_frame.len());
                                self.quote_stack.push(stack.len());
                            }

                            // Mark what context we're inside with the context stack:
                            // This only works when its the first argument - check the function call in open paren?
                            if current_frame.is_empty() {
                                match &token.ty {
                                    TokenType::Quote => {
                                        self.context.push(ParsingContext::Quote(stack.len()))
                                    }
                                    TokenType::Identifier(ident) if *ident == *UNQUOTE => {
                                        self.context.push(ParsingContext::Unquote(stack.len()));
                                        self.decrement_quasiquote_context_if_not_in_quote_context();
                                    }
                                    TokenType::Identifier(ident) if *ident == *QUASIQUOTE => {
                                        self.context.push(ParsingContext::Quasiquote(stack.len()));
                                        self.increment_quasiquote_context_if_not_in_quote_context();
                                    }
                                    TokenType::Identifier(ident) if *ident == *UNQUOTE_SPLICING => {
                                        self.context
                                            .push(ParsingContext::UnquoteSplicing(stack.len()));
                                        self.decrement_quasiquote_context_if_not_in_quote_context();
                                    }
                                    _ => {}
                                }

                                // println!("Context on application: {:?}", self.context);
                            }

                            // println!("{}", token);

                            current_frame.push(ExprKind::Atom(Atom::new(
                                SyntaxObject::from_token_with_source(
                                    &token,
                                    &self.source_name.clone(),
                                ),
                            )))
                        }
                    }
                }

                None => return Err(ParseError::UnexpectedEOF(self.source_name.clone())),
            }
        }
    }
}

fn wrap_in_doc_function(expr: ExprKind, comment: String) -> ExprKind {
    // println!("Found comment : {} for expr {}", comment, expr);

    ExprKind::List(List::new(vec![
        ExprKind::ident("@doc"),
        ExprKind::string_lit(comment),
        expr,
    ]))
}

impl<'a> Parser<'a> {
    fn get_next_and_maybe_wrap_in_doc(&mut self) -> Option<Result<ExprKind>> {
        let mut next;

        loop {
            next = self.tokenizer.next();

            if let Some(res) = next {
                match res.ty {
                    TokenType::Comment => {
                        if self.comment_buffer.is_empty() && !self.collecting_comments {
                            if res.source().trim_start_matches(';').starts_with("@doc") {
                                self.collecting_comments = true;

                                // println!("Found a comment to collect, starting collection...");

                                continue;
                            }
                        }

                        if self.collecting_comments {
                            let doc_line = res.source().trim_start_matches(';');

                            // If we hit another comment, clear it
                            if doc_line.starts_with("@doc") {
                                // println!("Clearing buffer");

                                self.comment_buffer.clear();
                                continue;
                            }

                            // println!("Collecting line: {}", doc_line);

                            self.comment_buffer.push(doc_line.trim_start());
                        }

                        continue;
                    }

                    TokenType::QuoteTick => {
                        // See if this does the job
                        self.shorthand_quote_stack.push(0);

                        let last = self.quote_context;

                        if self.quasiquote_depth == 0 {
                            self.quote_context = true;
                        }

                        // self.quote_context = true;

                        // println!("Entering Context: Quote Tick");
                        self.context.push(ParsingContext::QuoteTick(0));

                        let value = self
                            .next()
                            .unwrap_or(Err(ParseError::UnexpectedEOF(self.source_name.clone())))
                            .map(|x| self.construct_quote_vec(x, res.span));

                        self.shorthand_quote_stack.pop();

                        let popped_value = self.context.pop();

                        if let Some(popped) = popped_value {
                            debug_assert!(matches!(popped, ParsingContext::QuoteTick(_)))
                        }

                        self.quote_context = last;

                        // println!("Exiting context: {:?}", self.context.pop());
                        // println!("Result: {:?}", value);

                        return Some(match value {
                            Ok(v) => ExprKind::try_from(v),
                            Err(e) => Err(e),
                        });
                    }

                    TokenType::Unquote => {
                        // println!("Entering Context: Unquote");
                        self.context.push(ParsingContext::UnquoteTick(0));

                        self.decrement_quasiquote_context_if_not_in_quote_context();

                        let value = self
                            .next()
                            .unwrap_or(Err(ParseError::UnexpectedEOF(self.source_name.clone())))
                            .map(|x| {
                                // dbg!(&self.quasiquote_depth);
                                if self.quasiquote_depth == 0 && !self.quote_context {
                                    self.construct_raw_unquote(x, res.span)
                                } else {
                                    self.construct_unquote(x, res.span)
                                }
                            });

                        let popped_value = self.context.pop();

                        self.increment_quasiquote_context_if_not_in_quote_context();

                        if let Some(popped) = popped_value {
                            debug_assert!(matches!(popped, ParsingContext::UnquoteTick(_)))
                        }
                        // println!("Exiting context: {:?}", self.context.pop());

                        return Some(value);
                    }

                    TokenType::UnquoteSplice => {
                        // println!("Entering Context: Unquotesplicing");
                        self.context.push(ParsingContext::UnquoteSplicingTick(0));

                        self.decrement_quasiquote_context_if_not_in_quote_context();

                        let value = self
                            .next()
                            .unwrap_or(Err(ParseError::UnexpectedEOF(self.source_name.clone())))
                            .map(|x| {
                                if self.quasiquote_depth == 0 && !self.quote_context {
                                    self.construct_raw_unquote_splicing(x, res.span)
                                } else {
                                    self.construct_unquote_splicing(x, res.span)
                                }
                            });

                        let popped_value = self.context.pop();

                        self.increment_quasiquote_context_if_not_in_quote_context();

                        if let Some(popped) = popped_value {
                            debug_assert!(matches!(popped, ParsingContext::UnquoteSplicingTick(_)))
                        }

                        // println!("Exiting context: {:?}", self.context.pop());

                        return Some(value);
                    }

                    TokenType::QuasiQuote => {
                        // println!("Entering Context: Quasiquote - top level");
                        self.context.push(ParsingContext::QuasiquoteTick(0));

                        self.increment_quasiquote_context_if_not_in_quote_context();

                        let value = self
                            .next()
                            .unwrap_or(Err(ParseError::UnexpectedEOF(self.source_name.clone())))
                            .map(|x| self.construct_quasiquote(x, res.span));

                        // println!("{:?}", self.context.pop());

                        // println!("Top level Context: {:?}", self.context);

                        let popped_value = self.context.pop();

                        if let Some(popped) = popped_value {
                            debug_assert!(matches!(popped, ParsingContext::QuasiquoteTick(_)))
                        }

                        self.decrement_quasiquote_context_if_not_in_quote_context();

                        // println!("Exiting context: {:?}", self.context.pop());

                        return Some(value);
                    }

                    TokenType::OpenParen => return Some(self.read_from_tokens()),
                    TokenType::CloseParen => {
                        return Some(Err(ParseError::Unexpected(
                            TokenType::CloseParen,
                            self.source_name.clone(),
                        )))
                    }
                    TokenType::Error => return Some(Err(tokentype_error_to_parse_error(&res))),
                    _ => return Some(Ok(ExprKind::Atom(Atom::new(SyntaxObject::from(&res))))),
                };
            } else {
                // We're done consuming input
                return None;
            }
        }
    }
}

impl<'a> Iterator for Parser<'a> {
    type Item = Result<ExprKind>;

    // TODO -> put the
    fn next(&mut self) -> Option<Self::Item> {
        // self.shorthand_quote_stack = Vec::new();
        // self.quote_stack = Vec::new();

        if self.quote_stack.is_empty()
            && self.shorthand_quote_stack.is_empty()
            && self.context.is_empty()
        {
            // println!("RESETTING QUASIQUOTE DEPTH");
            self.quasiquote_depth = 0;
            self.comment_buffer.clear();
        }

        // println!("Clearing the comment buffer!");

        self.get_next_and_maybe_wrap_in_doc().map(|res| {
            if self.comment_buffer.is_empty() {
                res
            } else {
                // Reset the comment collection until next @doc statement
                self.collecting_comments = false;

                // println!("Setting collecting comments to false, found: {:?}", res);

                res.map(|x| {
                    wrap_in_doc_function(x, self.comment_buffer.join("\n").drain(..).collect())
                })
            }
        })
    }

    // self.tokenizer.next().map(|res| match res.ty {
    //     // Collect the comment until theres something to attach to
    //     TokenType::Comment => {
    //         //     // todo!()

    //         // println!("Found a comment: {}", res.source());
    //         // println!("Buffer now: {:?}", self.comment_buffer);

    //         self.comment_buffer
    //             .push(res.source().trim_start_matches(';').trim_start());

    //         // TODO: Instead of making a recursive call, we should just wrap
    //         // this whole thing in a loop, and accumulate until we return
    //         match self.next() {
    //             Some(v) => {
    //                 println!("Next thing found: {:?}", v);
    //                 v
    //                 // v.map(|x| {
    //                 //     wrap_in_doc_function(x, self.comment_buffer.join("\n").to_string())
    //                 // })
    //             }
    //             None => Err(ParseError::SyntaxError(
    //                 "Doc comment not associated with a top level definition".to_string(),
    //                 res.span(),
    //                 None,
    //             )),
    //         }

    //         // Ok(self.next().unwrap)

    //         //     println!("Found a comment!");
    //     }

    //     // Err(e) => Err(ParseError::TokenError(e)),
    //     TokenType::QuoteTick => {
    //         // See if this does the job
    //         self.shorthand_quote_stack.push(0);

    //         // println!("Entering Context: Quote Tick");
    //         self.context.push(ParsingContext::QuoteTick(0));

    //         let value = self
    //             .next()
    //             .unwrap_or(Err(ParseError::UnexpectedEOF(self.source_name.clone())))
    //             .map(|x| self.construct_quote_vec(x, res.span));

    //         self.shorthand_quote_stack.pop();

    //         let popped_value = self.context.pop();

    //         if let Some(popped) = popped_value {
    //             debug_assert!(matches!(popped, ParsingContext::QuoteTick(_)))
    //         }

    //         // println!("Exiting context: {:?}", self.context.pop());
    //         // println!("Result: {:?}", value);

    //         match value {
    //             Ok(v) => ExprKind::try_from(v),
    //             Err(e) => Err(e),
    //         }
    //     }
    //     TokenType::Unquote => {
    //         // println!("Entering Context: Unquote");
    //         self.context.push(ParsingContext::UnquoteTick(0));

    //         let value = self
    //             .next()
    //             .unwrap_or(Err(ParseError::UnexpectedEOF(self.source_name.clone())))
    //             .map(|x| self.construct_unquote(x, res.span));

    //         let popped_value = self.context.pop();

    //         if let Some(popped) = popped_value {
    //             debug_assert!(matches!(popped, ParsingContext::UnquoteTick(_)))
    //         }
    //         // println!("Exiting context: {:?}", self.context.pop());

    //         value
    //     }
    //     TokenType::UnquoteSplice => {
    //         // println!("Entering Context: Unquotesplicing");
    //         self.context.push(ParsingContext::UnquoteSplicingTick(0));

    //         let value = self
    //             .next()
    //             .unwrap_or(Err(ParseError::UnexpectedEOF(self.source_name.clone())))
    //             .map(|x| self.construct_unquote_splicing(x, res.span));

    //         let popped_value = self.context.pop();

    //         if let Some(popped) = popped_value {
    //             debug_assert!(matches!(popped, ParsingContext::UnquoteSplicingTick(_)))
    //         }

    //         // println!("Exiting context: {:?}", self.context.pop());

    //         value
    //     }
    //     TokenType::QuasiQuote => {
    //         // println!("Entering Context: Quasiquote");
    //         self.context.push(ParsingContext::QuasiquoteTick(0));

    //         let value = self
    //             .next()
    //             .unwrap_or(Err(ParseError::UnexpectedEOF(self.source_name.clone())))
    //             .map(|x| self.construct_quasiquote(x, res.span));

    //         // println!("{:?}", self.context.pop());

    //         // println!("Top level Context: {:?}", self.context);

    //         let popped_value = self.context.pop();

    //         if let Some(popped) = popped_value {
    //             debug_assert!(matches!(popped, ParsingContext::QuasiquoteTick(_)))
    //         }

    //         // println!("Exiting context: {:?}", self.context.pop());

    //         value
    //     }
    //     TokenType::OpenParen => self.read_from_tokens(),
    //     TokenType::CloseParen => Err(ParseError::Unexpected(
    //         TokenType::CloseParen,
    //         self.source_name.clone(),
    //     )),
    //     TokenType::Error => Err(tokentype_error_to_parse_error(&res)),
    //     _ => Ok(ExprKind::Atom(Atom::new(SyntaxObject::from(&res)))),
    // })
}

#[cfg(test)]
mod parser_tests {
    // use super::TokenType::*;
    use super::*;
    use crate::parser::ast::ExprKind;
    use crate::parser::ast::{Begin, Define, If, LambdaFunction, Quote, Return};

    fn atom(ident: &str) -> ExprKind {
        ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(ident.into()))))
    }

    fn int(num: isize) -> ExprKind {
        ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(
            MaybeBigInt::Small(num),
        ))))
    }

    fn character(c: char) -> ExprKind {
        ExprKind::Atom(Atom::new(SyntaxObject::default(
            TokenType::CharacterLiteral(c),
        )))
    }

    #[test]
    fn check_quote_parsing() {
        println!("{:?}", Parser::parse("'(a b 'c)"));
    }

    fn parses(s: &str) {
        let a: Result<Vec<_>> = Parser::new(s, None).collect();
        a.unwrap();
    }

    fn assert_parse(s: &str, result: &[ExprKind]) {
        let a: Result<Vec<ExprKind>> = Parser::new(s, None).collect();
        let a = a.unwrap();
        assert_eq!(a.as_slice(), result);
    }

    fn assert_parse_err(s: &str, err: ParseError) {
        let a: Result<Vec<ExprKind>> = Parser::new(s, None).collect();
        assert_eq!(a, Err(err));
    }

    fn assert_parse_is_err(s: &str) {
        let a: Result<Vec<ExprKind>> = Parser::new(s, None).collect();
        assert!(a.is_err());
    }

    #[test]
    fn check_resulting_parsing() {
        let expr = r#"`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)"#;

        let a: Result<Vec<ExprKind>> = Parser::new(expr, None).collect();
        let a = a.unwrap();

        println!("{}", a[0]);
    }

    #[test]
    fn check_double_unquote_parsing() {
        let expr = r#"(let ([name1 'x] [name2 'y]) `(a `(b ,,name1 ,',name2 d) e))"#;

        let a: Result<Vec<ExprKind>> = Parser::new(expr, None).collect();
        let a = a.unwrap();

        println!("{}", a[0]);
    }

    #[test]
    fn check_parser_with_doc_comments() {
        let expr = r#"
        ;;@doc
        ;; This is a fancy cool comment, that I want to attach to a top level definition!
        ;; This is the second line of the comment, I want this attached as well!
        ;; Macro for creating a new struct, in the form of:
        ;; `(struct <struct-name> (fields ...) options ...)`
        ;; The options can consist of the following:
        ;;
        ;; Single variable options (those which their presence indicates #true)
        ;; - #:mutable
        ;; - #:transparent
        ;;
        ;; Other options must be presented as key value pairs, and will get stored
        ;; in the struct instance. They will also be bound to the variable
        ;; ___<struct-name>-options___ in the same lexical environment where the
        ;; struct was defined. For example:
        ;;
        ;; (Applesauce (a b c) #:mutable #:transparent #:unrecognized-option 1234)
        ;;
        ;; Will result in the value `___Applesauce-options___` like so:
        ;; (hash #:mutable #true #:transparent #true #:unrecognized-option 1234)
        ;;
        ;; By default, structs are immutable, which means setter functions will not
        ;; be generated. Also by default, structs are not transparent, which means
        ;; printing them will result in an opaque struct that does not list the fields
        (define foo 12345)
        "#;

        let parser = Parser::doc_comment_parser(expr, None);

        let result: Result<Vec<_>> = parser.collect();

        println!("{:?}", result.unwrap());
    }

    #[test]
    fn parses_make_struct() {
        parses("(define make-struct (lambda (struct-name fields) (map (lambda (field) (list (quote define) (concat-symbols struct-name field) (quote (lambda (this) (vector-ref this 0))))) fields)))")
    }

    #[test]
    fn parses_quasiquote() {
        parses(r#"(quasiquote ((unquote x) xs ...)) "#);
    }

    #[test]
    fn parse_syntax_rules() {
        parses(
            r#"
            (syntax-rules (unquote unquote-splicing)
              ((quasiquote ((unquote x) xs ...))          (cons x (quasiquote (xs ...))))
              ((quasiquote ((unquote-splicing x)))        (append (list x) '()))
              ((quasiquote ((unquote-splicing x) xs ...)) (append x (quasiquote (xs ...))))
              ((quasiquote (unquote x))                 x)
              ((quasiquote (x))                          '(x))
              ((quasiquote (x xs ...))                   (cons (quasiquote x) (quasiquote (xs ...))))
              ((quasiquote x)                           'x))
            "#,
        );
    }

    #[test]
    fn parse_define_syntax() {
        parses(
            r#"
        (define-syntax quasiquote
            (syntax-rules (unquote unquote-splicing)
              ((quasiquote ((unquote x) xs ...))          (cons x (quasiquote (xs ...))))
              ((quasiquote ((unquote-splicing x)))        (append (list x) '()))
              ((quasiquote ((unquote-splicing x) xs ...)) (append x (quasiquote (xs ...))))
              ((quasiquote (unquote x))                 x)
              ((quasiquote (x))                          '(x))
              ((quasiquote (x xs ...))                   (cons (quasiquote x) (quasiquote (xs ...))))
              ((quasiquote x)                           'x)))
        "#,
        );
    }

    #[test]
    fn parse_quote() {
        // parses("(displayln (match (quote (lambda y z)) '(x y z)))")
        parses("(displayln (match '(lambda y z) '(x y z)))")
    }

    #[test]
    fn parse_unicode() {
        assert_parse("#\\¡", &[character('¡')]);
        assert_parse("#\\\\u{b}", &[character('\u{b}')]);
    }

    #[test]
    fn parse_more_unicode() {
        assert_parse("#\\\\u{a0}", &[character('\u{a0}')]);
    }

    #[test]
    fn parse_strange_characters() {
        assert_parse("#\\^", &[character('^')]);
    }

    #[test]
    fn parse_character_sequence() {
        assert_parse(
            "#\\¡ #\\SPACE #\\g",
            &[character('¡'), character(' '), character('g')],
        )
    }

    #[test]
    fn parse_character_sequence_inside_if() {
        assert_parse(
            "(if #\\¡ #\\SPACE #\\g)",
            &[ExprKind::If(Box::new(If::new(
                character('¡'),
                character(' '),
                character('g'),
                SyntaxObject::default(TokenType::If),
            )))],
        )
    }

    #[test]
    fn parse_close_paren_character() {
        assert_parse("#\\)", &[character(')')]);
        assert_parse("#\\]", &[character(']')])
    }

    #[test]
    fn parse_open_paren_character() {
        assert_parse("#\\(", &[character('(')])
    }

    #[test]
    fn test_error() {
        assert_parse_err("(", ParseError::UnexpectedEOF(None));
        assert_parse_err("(abc", ParseError::UnexpectedEOF(None));
        assert_parse_err("(ab 1 2", ParseError::UnexpectedEOF(None));
        assert_parse_err("((((ab 1 2) (", ParseError::UnexpectedEOF(None));
        assert_parse_err("())", ParseError::Unexpected(TokenType::CloseParen, None));
        assert_parse_err("() ((((", ParseError::UnexpectedEOF(None));
        assert_parse_err("')", ParseError::Unexpected(TokenType::CloseParen, None));
        assert_parse_err("(')", ParseError::Unexpected(TokenType::CloseParen, None));
        assert_parse_err("('", ParseError::UnexpectedEOF(None));
    }

    #[test]
    fn quote_multiple_args_should_err() {
        assert_parse_is_err("(quote a b c)");
    }

    #[test]
    fn test_let_should_err() {
        assert_parse_is_err("(let)");
        assert_parse_is_err("(let (a) 10)");
    }

    #[test]
    fn test_if_should_err() {
        assert_parse_is_err("(if)");
        assert_parse_is_err("(if 1)");
        assert_parse_is_err("(if 1 2)");
        assert_parse_is_err("(if 1 2 3 4)");
    }

    #[test]
    fn test_define_should_err() {
        assert_parse_is_err("(define)");
        assert_parse_is_err("(define blagh)");
        assert_parse_is_err("(define test 1 2)");
        assert_parse_is_err("(define () test");
    }

    #[test]
    fn test_lambda_should_err() {
        assert_parse_is_err("(lambda)");
        assert_parse_is_err("(lambda (x))");
    }

    #[test]
    fn test_empty() {
        assert_parse("", &[]);
        assert_parse("()", &[ExprKind::List(List::new(vec![]))]);
    }

    #[test]
    fn test_empty_quote_inside_if() {
        assert_parse(
            "(if #\\¡ (quote ()) #\\g)",
            &[ExprKind::If(Box::new(If::new(
                character('¡'),
                ExprKind::Quote(
                    Quote::new(
                        List::new(vec![]).into(),
                        SyntaxObject::default(TokenType::Quote),
                    )
                    .into(),
                ),
                character('g'),
                SyntaxObject::default(TokenType::If),
            )))],
        )
    }

    #[test]
    fn test_empty_quote() {
        assert_parse(
            "'()",
            &[ExprKind::Quote(
                Quote::new(
                    List::new(vec![]).into(),
                    SyntaxObject::default(TokenType::Quote),
                )
                .into(),
            )],
        )
    }

    #[test]
    fn test_empty_quote_nested() {
        assert_parse(
            "(list '())",
            &[ExprKind::List(List::new(vec![
                atom("list"),
                ExprKind::Quote(
                    Quote::new(
                        List::new(vec![]).into(),
                        SyntaxObject::default(TokenType::Quote),
                    )
                    .into(),
                ),
            ]))],
        )
    }

    #[test]
    fn test_multi_parse_simple() {
        assert_parse("a b +", &[atom("a"), atom("b"), atom("+")]);
    }

    #[test]
    fn test_multi_parse_complicated() {
        assert_parse(
            "a b (funcall  1 (+ 2 3.5))",
            &[
                atom("a"),
                atom("b"),
                ExprKind::List(List::new(vec![
                    atom("funcall"),
                    int(1),
                    ExprKind::List(List::new(vec![
                        atom("+"),
                        int(2),
                        ExprKind::Atom(Atom::new(SyntaxObject::default(NumberLiteral(3.5)))),
                    ])),
                ])),
            ],
        )
    }
    #[test]
    fn test_parse_simple() {
        assert_parse(
            "(+ 1 2 3) (- 4 3)",
            &[
                ExprKind::List(List::new(vec![atom("+"), int(1), int(2), int(3)])),
                ExprKind::List(List::new(vec![atom("-"), int(4), int(3)])),
            ],
        );
    }

    #[test]
    fn test_parse_nested() {
        assert_parse(
            "(+ 1 (foo (bar 2 3)))",
            &[ExprKind::List(List::new(vec![
                atom("+"),
                int(1),
                ExprKind::List(List::new(vec![
                    atom("foo"),
                    ExprKind::List(List::new(vec![atom("bar"), int(2), int(3)])),
                ])),
            ]))],
        );
        assert_parse(
            "(+ 1 (+ 2 3) (foo (bar 2 3)))",
            &[ExprKind::List(List::new(vec![
                atom("+"),
                int(1),
                ExprKind::List(List::new(vec![atom("+"), int(2), int(3)])),
                ExprKind::List(List::new(vec![
                    atom("foo"),
                    ExprKind::List(List::new(vec![atom("bar"), int(2), int(3)])),
                ])),
            ]))],
        );
    }

    #[test]
    fn test_if() {
        assert_parse(
            "(+ 1 (if 2 3 4) (foo (+ (bar 1 1) 3) 5))",
            &[ExprKind::List(List::new(vec![
                atom("+"),
                int(1),
                ExprKind::If(Box::new(If::new(
                    int(2),
                    int(3),
                    int(4),
                    SyntaxObject::default(If),
                ))),
                ExprKind::List(List::new(vec![
                    atom("foo"),
                    ExprKind::List(List::new(vec![
                        atom("+"),
                        ExprKind::List(List::new(vec![atom("bar"), int(1), int(1)])),
                        int(3),
                    ])),
                    int(5),
                ])),
            ]))],
        );
    }

    #[test]
    fn test_quote() {
        assert_parse(
            "(quote (if 1 2))",
            &[ExprKind::Quote(Box::new(Quote::new(
                ExprKind::List(List::new(vec![
                    ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::If))),
                    int(1),
                    int(2),
                ])),
                SyntaxObject::default(TokenType::Quote),
            )))],
        )
    }

    #[test]
    fn test_quote_shorthand() {
        assert_parse(
            "'(if 1 2)",
            &[ExprKind::Quote(Box::new(Quote::new(
                ExprKind::List(List::new(vec![
                    ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::If))),
                    int(1),
                    int(2),
                ])),
                SyntaxObject::default(TokenType::Quote),
            )))],
        )
    }

    #[test]
    fn test_quote_nested() {
        assert_parse(
            "(quote (if (if 1 2) 3))",
            &[ExprKind::Quote(Box::new(Quote::new(
                ExprKind::List(List::new(vec![
                    ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::If))),
                    ExprKind::List(List::new(vec![
                        ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::If))),
                        int(1),
                        int(2),
                    ])),
                    int(3),
                ])),
                SyntaxObject::default(TokenType::Quote),
            )))],
        )
    }

    #[test]
    fn test_quote_shorthand_nested() {
        assert_parse(
            "'(if (if 1 2) 3)",
            &[ExprKind::Quote(Box::new(Quote::new(
                ExprKind::List(List::new(vec![
                    ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::If))),
                    ExprKind::List(List::new(vec![
                        ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::If))),
                        int(1),
                        int(2),
                    ])),
                    int(3),
                ])),
                SyntaxObject::default(TokenType::Quote),
            )))],
        )
    }

    #[test]
    fn test_quote_shorthand_multiple_exprs() {
        assert_parse(
            "'(if (if 1 2) 3) (+ 1 (if 2 3 4) (foo (+ (bar 1 1) 3) 5))",
            &[
                ExprKind::Quote(Box::new(Quote::new(
                    ExprKind::List(List::new(vec![
                        ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::If))),
                        ExprKind::List(List::new(vec![
                            ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::If))),
                            int(1),
                            int(2),
                        ])),
                        int(3),
                    ])),
                    SyntaxObject::default(TokenType::Quote),
                ))),
                ExprKind::List(List::new(vec![
                    atom("+"),
                    int(1),
                    ExprKind::If(Box::new(If::new(
                        int(2),
                        int(3),
                        int(4),
                        SyntaxObject::default(If),
                    ))),
                    ExprKind::List(List::new(vec![
                        atom("foo"),
                        ExprKind::List(List::new(vec![
                            atom("+"),
                            ExprKind::List(List::new(vec![atom("bar"), int(1), int(1)])),
                            int(3),
                        ])),
                        int(5),
                    ])),
                ])),
            ],
        )
    }

    #[test]
    fn test_quote_inner() {
        assert_parse(
            "'(applesauce 'one)",
            &[ExprKind::Quote(Box::new(Quote::new(
                ExprKind::List(List::new(vec![
                    atom("applesauce"),
                    ExprKind::Quote(Box::new(Quote::new(
                        atom("one"),
                        SyntaxObject::default(TokenType::Quote),
                    ))),
                ])),
                SyntaxObject::default(TokenType::Quote),
            )))],
        )
    }

    #[test]
    fn test_quote_inner_without_shorthand() {
        assert_parse(
            "(quote (applesauce 'one))",
            &[ExprKind::Quote(Box::new(Quote::new(
                ExprKind::List(List::new(vec![
                    atom("applesauce"),
                    ExprKind::Quote(Box::new(Quote::new(
                        atom("one"),
                        SyntaxObject::default(TokenType::Quote),
                    ))),
                ])),
                SyntaxObject::default(TokenType::Quote),
            )))],
        )
    }

    #[test]
    fn test_quasiquote_shorthand() {
        assert_parse(
            "`(+ 1 2)",
            &[ExprKind::List(List::new(vec![
                atom("quasiquote"),
                ExprKind::List(List::new(vec![atom("+"), int(1), int(2)])),
            ]))],
        )
    }

    #[test]
    fn test_quasiquote_normal() {
        assert_parse(
            "(quasiquote (+ 1 2))",
            &[ExprKind::List(List::new(vec![
                atom("quasiquote"),
                ExprKind::List(List::new(vec![atom("+"), int(1), int(2)])),
            ]))],
        )
    }

    #[test]
    fn test_unquote_shorthand() {
        assert_parse(
            ",(+ 1 2)",
            &[ExprKind::List(List::new(vec![
                atom("unquote"),
                ExprKind::List(List::new(vec![atom("+"), int(1), int(2)])),
            ]))],
        )
    }

    #[test]
    fn test_unquote_normal() {
        assert_parse(
            "(unquote (+ 1 2))",
            &[ExprKind::List(List::new(vec![
                atom("unquote"),
                ExprKind::List(List::new(vec![atom("+"), int(1), int(2)])),
            ]))],
        )
    }

    #[test]
    fn test_unquote_splicing_shorthand() {
        assert_parse(
            ",@(+ 1 2)",
            &[ExprKind::List(List::new(vec![
                atom("unquote-splicing"),
                ExprKind::List(List::new(vec![atom("+"), int(1), int(2)])),
            ]))],
        )
    }

    #[test]
    fn test_unquote_splicing_normal() {
        assert_parse(
            "(unquote-splicing (+ 1 2))",
            &[ExprKind::List(List::new(vec![
                atom("unquote-splicing"),
                ExprKind::List(List::new(vec![atom("+"), int(1), int(2)])),
            ]))],
        )
    }

    #[test]
    fn test_define_simple() {
        assert_parse(
            "(define a 10)",
            &[ExprKind::Define(Box::new(Define::new(
                atom("a"),
                int(10),
                SyntaxObject::default(TokenType::Define),
            )))],
        )
    }

    #[test]
    fn test_define_func_simple() {
        assert_parse(
            "(define (foo x) (+ x 10))",
            &[ExprKind::Define(Box::new(Define::new(
                atom("foo"),
                ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
                    vec![atom("x")],
                    ExprKind::List(List::new(vec![atom("+"), atom("x"), int(10)])),
                    SyntaxObject::default(TokenType::Lambda),
                ))),
                SyntaxObject::default(TokenType::Define),
            )))],
        )
    }

    #[test]
    fn test_define_func_multiple_args() {
        assert_parse(
            "(define (foo x y z) (+ x 10))",
            &[ExprKind::Define(Box::new(Define::new(
                atom("foo"),
                ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
                    vec![atom("x"), atom("y"), atom("z")],
                    ExprKind::List(List::new(vec![atom("+"), atom("x"), int(10)])),
                    SyntaxObject::default(TokenType::Lambda),
                ))),
                SyntaxObject::default(TokenType::Define),
            )))],
        )
    }

    #[test]
    fn test_define_func_multiple_args_multiple_body_exprs() {
        assert_parse(
            "(define (foo x y z) (+ x 10) (+ y 20) (+ z 30))",
            &[ExprKind::Define(Box::new(Define::new(
                atom("foo"),
                ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
                    vec![atom("x"), atom("y"), atom("z")],
                    ExprKind::Begin(Begin::new(
                        vec![
                            ExprKind::List(List::new(vec![atom("+"), atom("x"), int(10)])),
                            ExprKind::List(List::new(vec![atom("+"), atom("y"), int(20)])),
                            ExprKind::List(List::new(vec![atom("+"), atom("z"), int(30)])),
                        ],
                        SyntaxObject::default(TokenType::Begin),
                    )),
                    SyntaxObject::default(TokenType::Lambda),
                ))),
                SyntaxObject::default(TokenType::Define),
            )))],
        )
    }

    #[test]
    fn test_recursive_function() {
        assert_parse(
            "(define (test) (define (foo) (bar)) (define (bar) (foo)))",
            &[ExprKind::Define(Box::new(Define::new(
                atom("test"),
                ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
                    vec![],
                    ExprKind::Begin(Begin::new(
                        vec![
                            ExprKind::Define(Box::new(Define::new(
                                atom("foo"),
                                ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
                                    vec![],
                                    ExprKind::List(List::new(vec![atom("bar")])),
                                    SyntaxObject::default(TokenType::Lambda),
                                ))),
                                SyntaxObject::default(TokenType::Define),
                            ))),
                            ExprKind::Define(Box::new(Define::new(
                                atom("bar"),
                                ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
                                    vec![],
                                    ExprKind::List(List::new(vec![atom("foo")])),
                                    SyntaxObject::default(TokenType::Lambda),
                                ))),
                                SyntaxObject::default(TokenType::Define),
                            ))),
                        ],
                        SyntaxObject::default(TokenType::Begin),
                    )),
                    SyntaxObject::default(TokenType::Lambda),
                ))),
                SyntaxObject::default(TokenType::Define),
            )))],
        )
    }

    #[test]
    fn test_return_normal() {
        assert_parse(
            "(return! 10)",
            &[ExprKind::Return(Box::new(Return::new(
                int(10),
                SyntaxObject::default(TokenType::Return),
            )))],
        )
    }

    #[test]
    fn test_begin() {
        assert_parse(
            "(begin 1 2 3)",
            &[ExprKind::Begin(Begin::new(
                vec![int(1), int(2), int(3)],
                SyntaxObject::default(TokenType::Begin),
            ))],
        )
    }

    #[test]
    fn test_lambda_function() {
        assert_parse(
            "(lambda (x) 10)",
            &[ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
                vec![atom("x")],
                int(10),
                SyntaxObject::default(TokenType::Lambda),
            )))],
        )
    }

    #[test]
    fn test_lambda_matches_let() {
        assert_parse(
            "((lambda (a) (+ a 20)) 10)",
            &[ExprKind::List(List::new(vec![
                ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
                    vec![atom("a")],
                    ExprKind::List(List::new(vec![atom("+"), atom("a"), int(20)])),
                    SyntaxObject::default(TokenType::Lambda),
                ))),
                int(10),
            ]))],
        );
    }

    #[test]
    fn test_let() {
        assert_parse(
            "(let ([a 10]) (+ a 20))",
            &[ExprKind::List(List::new(vec![
                ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
                    vec![atom("a")],
                    ExprKind::List(List::new(vec![atom("+"), atom("a"), int(20)])),
                    SyntaxObject::default(TokenType::Let),
                ))),
                int(10),
            ]))],
        )
    }

    #[test]
    fn test_quote_with_inner_nested() {
        assert_parse(
            "'(#f '())",
            &[ExprKind::Quote(
                Quote::new(
                    ExprKind::List(List::new(vec![
                        ExprKind::Atom(Atom::new(SyntaxObject::default(
                            TokenType::BooleanLiteral(false),
                        ))),
                        ExprKind::Quote(
                            Quote::new(
                                List::new(vec![]).into(),
                                SyntaxObject::default(TokenType::Quote),
                            )
                            .into(),
                        ),
                    ])),
                    SyntaxObject::default(TokenType::Quote),
                )
                .into(),
            )],
        )
    }

    #[test]
    fn test_quote_with_inner_nested_sub_expr() {
        assert_parse(
            "(if (null? contents)
                '(#f '())
                (list (car contents) (cdr contents)))",
            &[ExprKind::If(Box::new(If::new(
                ExprKind::List(List::new(vec![atom("null?"), atom("contents")])),
                ExprKind::Quote(
                    Quote::new(
                        ExprKind::List(List::new(vec![
                            ExprKind::Atom(Atom::new(SyntaxObject::default(
                                TokenType::BooleanLiteral(false),
                            ))),
                            ExprKind::Quote(
                                Quote::new(
                                    List::new(vec![]).into(),
                                    SyntaxObject::default(TokenType::Quote),
                                )
                                .into(),
                            ),
                        ])),
                        SyntaxObject::default(TokenType::Quote),
                    )
                    .into(),
                ),
                ExprKind::List(List::new(vec![
                    atom("list"),
                    ExprKind::List(List::new(vec![atom("car"), atom("contents")])),
                    ExprKind::List(List::new(vec![atom("cdr"), atom("contents")])),
                ])),
                SyntaxObject::default(TokenType::If),
            )))],
        );
    }

    #[test]
    fn test_quote_normal_with_inner_nested_sub_expr() {
        assert_parse(
            "(if (null? contents)
                (quote (#f '()))
                (list (car contents) (cdr contents)))",
            &[ExprKind::If(Box::new(If::new(
                ExprKind::List(List::new(vec![atom("null?"), atom("contents")])),
                ExprKind::Quote(
                    Quote::new(
                        ExprKind::List(List::new(vec![
                            ExprKind::Atom(Atom::new(SyntaxObject::default(
                                TokenType::BooleanLiteral(false),
                            ))),
                            ExprKind::Quote(
                                Quote::new(
                                    List::new(vec![]).into(),
                                    SyntaxObject::default(TokenType::Quote),
                                )
                                .into(),
                            ),
                        ])),
                        SyntaxObject::default(TokenType::Quote),
                    )
                    .into(),
                ),
                ExprKind::List(List::new(vec![
                    atom("list"),
                    ExprKind::List(List::new(vec![atom("car"), atom("contents")])),
                    ExprKind::List(List::new(vec![atom("cdr"), atom("contents")])),
                ])),
                SyntaxObject::default(TokenType::If),
            )))],
        );
    }

    #[test]
    fn test_quote_with_inner_sub_expr_even_more_nested() {
        assert_parse(
            "(list 
                (if (null? contents)
                '(#f '())
                (list (car contents) (cdr contents))))",
            &[ExprKind::List(List::new(vec![
                atom("list"),
                ExprKind::If(Box::new(If::new(
                    ExprKind::List(List::new(vec![atom("null?"), atom("contents")])),
                    ExprKind::Quote(
                        Quote::new(
                            ExprKind::List(List::new(vec![
                                ExprKind::Atom(Atom::new(SyntaxObject::default(
                                    TokenType::BooleanLiteral(false),
                                ))),
                                ExprKind::Quote(
                                    Quote::new(
                                        List::new(vec![]).into(),
                                        SyntaxObject::default(TokenType::Quote),
                                    )
                                    .into(),
                                ),
                            ])),
                            SyntaxObject::default(TokenType::Quote),
                        )
                        .into(),
                    ),
                    ExprKind::List(List::new(vec![
                        atom("list"),
                        ExprKind::List(List::new(vec![atom("car"), atom("contents")])),
                        ExprKind::List(List::new(vec![atom("cdr"), atom("contents")])),
                    ])),
                    SyntaxObject::default(TokenType::If),
                ))),
            ]))],
        );
    }

    #[test]
    fn test_define_with_datum_syntax_name() {
        assert_parse(
            "(define (datum->syntax var) (car ret-value))",
            &[ExprKind::Define(Box::new(Define::new(
                ExprKind::List(List::new(vec![atom("datum->syntax"), atom("var")])),
                ExprKind::List(List::new(vec![atom("car"), atom("ret-value")])),
                SyntaxObject::default(TokenType::Define),
            )))],
        )
    }

    #[test]
    fn test_define_with_datum_syntax_function_name() {
        assert_parse(
            "(define ((datum->syntax var) arg) 10)",
            &[ExprKind::Define(Box::new(Define::new(
                ExprKind::List(List::new(vec![atom("datum->syntax"), atom("var")])),
                ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
                    vec![atom("arg")],
                    int(10),
                    SyntaxObject::default(TokenType::Lambda),
                ))),
                SyntaxObject::default(TokenType::Define),
            )))],
        )
    }
}
