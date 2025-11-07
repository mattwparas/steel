use std::{
    borrow::Cow,
    path::PathBuf,
    rc::Rc,
    result,
    sync::atomic::{AtomicU32, AtomicUsize, Ordering},
};

use serde::{Deserialize, Serialize};

use crate::{
    ast::{
        self, parse_begin, parse_define, parse_if, parse_lambda, parse_let, parse_new_let,
        parse_require, parse_set, parse_single_argument, Atom, ExprKind, List, Macro, PatternPair,
        SyntaxRules, Vector, BEGIN, DEFINE, IF, LAMBDA, LAMBDA_FN, LAMBDA_SYMBOL, LET, PLAIN_LET,
        QUASIQUOTE, QUASISYNTAX, QUOTE, RAW_UNQUOTE, RAW_UNQUOTE_SPLICING, RAW_UNSYNTAX,
        RAW_UNSYNTAX_SPLICING, REQUIRE, RETURN, SET, SYNTAX_QUOTE, UNQUOTE, UNQUOTE_SPLICING,
    },
    interner::InternedString,
    lexer::{OwnedTokenStream, ToOwnedString, TokenError, TokenStream},
    span::Span,
    tokens::{Paren, ParenMod, Token, TokenLike, TokenType},
};

static SOURCE_ID_COUNTER: AtomicU32 = AtomicU32::new(0);

#[derive(
    Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Default, Debug, Ord, PartialOrd,
)]
#[repr(C)]
pub struct SourceId(pub u32);

impl SourceId {
    pub const fn none() -> Option<Self> {
        None
    }

    pub fn fresh() -> Self {
        SourceId(SOURCE_ID_COUNTER.fetch_add(1, Ordering::Relaxed))
    }
}

// TODO: Fix the visibility here
pub static SYNTAX_OBJECT_ID: AtomicUsize = AtomicUsize::new(0);

// thread_local! {
//     pub static TL_SYNTAX_OBJECT_ID: Cell<u32> = Cell::new(0);
// }

#[derive(
    Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Default, Debug, Ord, PartialOrd,
)]
pub struct SyntaxObjectId(pub u32);

impl SyntaxObjectId {
    #[inline]
    pub fn fresh() -> Self {
        SyntaxObjectId(SYNTAX_OBJECT_ID.fetch_add(1, Ordering::Relaxed) as _)
        // TODO: Revisit why we're using this here
        // SyntaxObjectId(TL_SYNTAX_OBJECT_ID.with(|x| {
        //     let value = x.get();
        //     x.set(value + 1);
        //     value
        // }))
    }
}

impl From<SyntaxObjectId> for u32 {
    fn from(value: SyntaxObjectId) -> Self {
        value.0
    }
}

impl std::fmt::Display for SyntaxObjectId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
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

/// A syntax object that can hold anything as the syntax
/// In this case, we're using the token type emitted by logos
///
/// This should open the door to interning our strings to make
/// parsing (and optimizations later) faster
#[derive(Serialize, Deserialize)]
pub struct RawSyntaxObject<T> {
    pub ty: T,
    pub span: Span,
    pub syntax_object_id: SyntaxObjectId,
    // TODO: @Matt
    // This is a hack. More or less, we need a way to mark that
    // this particular syntax object is "unresolved" - and thus
    // should mangle its usage. This can also be done by using
    // the syntax object ID separately, but there is enough
    // space on the object itself that keeping it alongside the
    // object seemed to make sense. What we're going to do is mark
    // any unresolved references to variables found in macro expansion.
    // So, for example, consider the following:
    //
    // (define bound-x (vector 10 20 30 40))
    //
    // (define-syntax lexical-capture
    //   (syntax-rules ()
    //     [(_) (list bound-x)]))
    //
    // (let ([bound-x 'inner]) (lexical-capture))
    //
    // The define syntax is expanded without the context of knowing
    // the globals, and as a result isn't aware that bound-x should
    // refer to the local. However, we can expand and see that both
    // `list` and `bound-x` don't refer to anything in the patterns,
    // and so as a result, should be marked as "unresolved". Since
    // they are unresolved, we can conclude that either they're
    // global variables, or a free identifier.
    //
    // Then, when handling shadowed local variables, when we come across
    // a local variable that we'd like to mangle, if its unresolved, we
    // just leave it alone, and otherwise we should treat all locals as
    // variables to be mangled.
    //
    // Then, once we're done with all macro expansion, all variables themselves
    // should be "resolved" since either they refer to an individual variable,
    // or nothing at all.
    pub unresolved: bool,
    pub introduced_via_macro: bool,
}

impl<T: Clone> Clone for RawSyntaxObject<T> {
    fn clone(&self) -> Self {
        Self {
            ty: self.ty.clone(),
            span: self.span,
            syntax_object_id: SyntaxObjectId::fresh(),
            unresolved: self.unresolved,
            introduced_via_macro: self.introduced_via_macro,
        }
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for RawSyntaxObject<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("RawSyntaxObject")
            .field("ty", &self.ty)
            .field("span", &self.span)
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
            syntax_object_id: SyntaxObjectId::fresh(),
            unresolved: false,
            introduced_via_macro: false,
        }
    }

    pub fn default(ty: TokenType<InternedString>) -> Self {
        SyntaxObject {
            ty,
            span: Span::new(0, 0, SourceId::none()),
            // source: None,
            syntax_object_id: SyntaxObjectId::fresh(),
            unresolved: false,
            introduced_via_macro: false,
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
            syntax_object_id: SyntaxObjectId::fresh(),
            unresolved: false,
            introduced_via_macro: false,
        }
    }
}

impl From<&Token<'_, InternedString>> for SyntaxObject {
    fn from(val: &Token<'_, InternedString>) -> SyntaxObject {
        SyntaxObject::new(val.ty.clone(), val.span)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ParseError {
    MismatchedParen(Paren, Span, Option<Rc<PathBuf>>),
    UnexpectedEOF(Span, Option<Rc<PathBuf>>),
    UnexpectedChar(char, Span, Option<Rc<PathBuf>>),
    SyntaxError(String, Span, Option<Rc<PathBuf>>),
    ArityMismatch(String, Span, Option<Rc<PathBuf>>),
}

impl From<TokenLike<'_, TokenError>> for ParseError {
    fn from(value: TokenLike<'_, TokenError>) -> Self {
        match value.ty {
            TokenError::IncompleteString
            | TokenError::IncompleteIdentifier
            | TokenError::IncompleteComment => ParseError::UnexpectedEOF(value.span, None),
            _ => ParseError::SyntaxError(format!("{}", value.ty), value.span, None),
        }
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::MismatchedParen(paren, _, _) => {
                write!(f, "Mismatched parenthesis, expected \"{}\"", paren.close())
            }
            ParseError::UnexpectedEOF(..) => write!(f, "Unexpected EOF"),
            ParseError::UnexpectedChar(l, _, _) => {
                write!(f, "Unexpected character: {:?}", l)
            }
            ParseError::SyntaxError(l, _, _) => write!(f, "Syntax Error: {}", l),
            ParseError::ArityMismatch(l, _, _) => write!(f, "Arity mismatch: {}", l),
        }
    }
}

impl std::error::Error for ParseError {}

impl ParseError {
    pub fn span(&self) -> Span {
        match self {
            ParseError::MismatchedParen(_, s, _) => *s,
            ParseError::UnexpectedEOF(s, _) => *s,
            ParseError::UnexpectedChar(_, s, _) => *s,
            ParseError::SyntaxError(_, s, _) => *s,
            ParseError::ArityMismatch(_, s, _) => *s,
        }
    }

    pub fn set_source(self, source: Option<Rc<PathBuf>>) -> Self {
        use ParseError::*;
        match self {
            ParseError::MismatchedParen(l, s, _) => MismatchedParen(l, s, source),
            ParseError::UnexpectedEOF(s, _) => UnexpectedEOF(s, source),
            ParseError::UnexpectedChar(l, s, _) => UnexpectedChar(l, s, source),
            ParseError::SyntaxError(l, s, _) => SyntaxError(l, s, source),
            ParseError::ArityMismatch(l, s, _) => ArityMismatch(l, s, source),
        }
    }
}

pub struct InternString;

impl ToOwnedString<InternedString> for InternString {
    fn own(&self, s: Cow<str>) -> InternedString {
        (&*s).into()
    }
}

// #[derive(Debug)]
pub struct Parser<'a> {
    tokenizer: OwnedTokenStream<'a>,
    quote_stack: Vec<usize>,
    quasiquote_depth: isize,
    quote_context: bool,
    shorthand_quote_stack: Vec<usize>,
    source_name: Option<Rc<PathBuf>>,
    context: Vec<ParsingContext>,
    comment_buffer: Vec<&'a str>,
    collecting_comments: bool,
    keep_lists: bool,

    queued: Option<ExprKind>,
}

#[derive(Debug, Copy, Clone, PartialEq)]
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
    pub fn parse(expr: &str) -> Result<Vec<ExprKind>> {
        Parser::new(expr, SourceId::none()).collect()
    }

    pub fn parse_without_lowering(expr: &str) -> Result<Vec<ExprKind>> {
        Parser::new(expr, SourceId::none())
            .without_lowering()
            .collect()
    }

    pub fn offset(&self) -> usize {
        self.tokenizer.offset()
    }
}

pub type Result<T> = result::Result<T, ParseError>;

impl<'a> Parser<'a> {
    pub fn new(input: &'a str, source_id: Option<SourceId>) -> Self {
        Parser {
            tokenizer: TokenStream::new(input, false, source_id).into_owned(),
            quote_stack: Vec::new(),
            quasiquote_depth: 0,
            quote_context: false,
            shorthand_quote_stack: Vec::new(),
            source_name: None,
            context: Vec::new(),
            comment_buffer: Vec::new(),
            collecting_comments: false,
            keep_lists: false,
            queued: None,
        }
    }

    pub fn without_lowering(mut self) -> Self {
        self.keep_lists = true;
        self
    }

    pub fn new_flat(input: &'a str, source_id: Option<SourceId>) -> Self {
        Parser {
            tokenizer: TokenStream::new(input, false, source_id).into_owned(),
            quote_stack: Vec::new(),
            quasiquote_depth: 0,
            quote_context: false,
            shorthand_quote_stack: Vec::new(),
            source_name: None,
            context: Vec::new(),
            comment_buffer: Vec::new(),
            collecting_comments: false,
            keep_lists: true,
            queued: None,
        }
    }

    pub fn new_from_source(
        input: &'a str,
        source_name: PathBuf,
        source_id: Option<SourceId>,
    ) -> Self {
        Parser {
            tokenizer: TokenStream::new(input, false, source_id).into_owned(),
            quote_stack: Vec::new(),
            quasiquote_depth: 0,
            quote_context: false,
            shorthand_quote_stack: Vec::new(),
            source_name: Some(Rc::from(source_name)),
            context: Vec::new(),
            comment_buffer: Vec::new(),
            collecting_comments: false,
            keep_lists: false,
            queued: None,
        }
    }

    // Attach comments!
    pub fn doc_comment_parser(input: &'a str, source_id: Option<SourceId>) -> Self {
        Parser {
            tokenizer: TokenStream::new(input, false, source_id).into_owned(),
            quote_stack: Vec::new(),
            quasiquote_depth: 0,
            quote_context: false,
            shorthand_quote_stack: Vec::new(),
            source_name: None,
            context: Vec::new(),
            comment_buffer: Vec::new(),
            collecting_comments: false,
            keep_lists: false,
            queued: None,
        }
    }

    fn construct_quote(&mut self, val: ExprKind, span: Span) -> ExprKind {
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

    // Reader macro for #'
    fn construct_syntax(&mut self, val: ExprKind, span: Span) -> ExprKind {
        let q = {
            let rc_val = TokenType::Identifier(*SYNTAX_QUOTE);
            ExprKind::Atom(Atom::new(SyntaxObject::new(rc_val, span)))
        };

        ExprKind::List(List::new(vec![q, val]))
    }

    // Reader macro for #'
    fn construct_quasiquote_syntax(&mut self, val: ExprKind, span: Span) -> ExprKind {
        let q = {
            let rc_val = TokenType::Identifier(*QUASISYNTAX);
            ExprKind::Atom(Atom::new(SyntaxObject::new(rc_val, span)))
        };

        ExprKind::List(List::new(vec![q, val]))
    }

    fn construct_quasiunquote_syntax(&mut self, val: ExprKind, span: Span) -> ExprKind {
        let q = {
            let rc_val = TokenType::Identifier(*RAW_UNSYNTAX);
            ExprKind::Atom(Atom::new(SyntaxObject::new(rc_val, span)))
        };

        ExprKind::List(List::new(vec![q, val]))
    }

    fn construct_quasiunquote_syntax_splicing(&mut self, val: ExprKind, span: Span) -> ExprKind {
        let q = {
            let rc_val = TokenType::Identifier(*RAW_UNSYNTAX_SPLICING);
            ExprKind::Atom(Atom::new(SyntaxObject::new(rc_val, span)))
        };

        ExprKind::List(List::new(vec![q, val]))
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
            // let rc_val = TokenType::Identifier(*UNQUOTE);
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

    fn maybe_lower(&self, exprs: Vec<ExprKind>) -> Result<ExprKind> {
        if self.keep_lists {
            Ok(ExprKind::List(List::new(exprs)))
        } else {
            ExprKind::try_from(exprs)
        }
    }

    fn maybe_lower_frame(&self, frame: Frame, close: Span) -> Result<ExprKind> {
        frame.build_expr(close, |exprs| self.maybe_lower(exprs))
    }

    fn read_from_tokens(
        &mut self,
        (open, paren, paren_mod): (Span, Paren, Option<ParenMod>),
    ) -> Result<ExprKind> {
        let mut last = open;

        // Can we reuse this?
        // TODO: I think we have to shove this into the parser state.
        // Alongside the current frame and anything else.
        //
        // That way, we can just keep it around if we hit an EOF and
        // we're in incremental mode, where we can keep feeding data
        // in and it can continue to produce parsed values.
        let mut stack: Vec<Frame> = Vec::new();

        // self.stack.clear();

        let mut current_frame = Frame {
            open,
            paren,
            paren_mod,
            exprs: Vec::new(),
            dot: None,
            comment: 0,
        };

        self.quote_stack = Vec::new();

        // println!("READING FROM TOKENS");
        // self.quasiquote_depth = 0;

        while let Some(token) = self.next_token() {
            let token = token?;
            last = token.span;

            match token.ty {
                TokenType::Dot => {
                    if current_frame.dot.is_some() {
                        return Err(ParseError::SyntaxError(
                            "improper lists can only have a single dot".into(),
                            token.span,
                            None,
                        ));
                    } else if current_frame.exprs.is_empty() {
                        return Err(ParseError::SyntaxError(
                            "improper lists must have a car element before the dot".into(),
                            token.span,
                            None,
                        ));
                    } else if current_frame.paren_mod.is_some() {
                        let paren_mod = current_frame.paren_mod.unwrap();
                        let object = match paren_mod {
                            ParenMod::Vector => "vector",
                            ParenMod::Bytes => "bytevector",
                        };

                        return Err(ParseError::SyntaxError(
                            format!("{object} literals cannot contain dots"),
                            token.span,
                            None,
                        ));
                    } else if current_frame.comment > 0 {
                        return Err(ParseError::SyntaxError(
                            "commented-out datum cannot start with a dot".into(),
                            token.span,
                            None,
                        ));
                    } else {
                        current_frame.dot = Some((current_frame.exprs.len() as _, token.span));
                    }
                }
                TokenType::Comment => {
                    // println!("Found a comment!");
                    // Internal comments, we're gonna skip for now
                }
                TokenType::DatumComment => {
                    current_frame.comment += 1;
                }

                TokenType::QuoteSyntax => {
                    let quote_inner = self
                        .next()
                        .unwrap_or(Err(ParseError::UnexpectedEOF(
                            token.span,
                            self.source_name.clone(),
                        )))
                        .map(|x| self.construct_syntax(x, token.span))?;

                    current_frame.push(quote_inner)?
                }

                TokenType::QuasiQuoteSyntax => {
                    let quote_inner = self
                        .next()
                        .unwrap_or(Err(ParseError::UnexpectedEOF(
                            token.span,
                            self.source_name.clone(),
                        )))
                        .map(|x| self.construct_quasiquote_syntax(x, token.span))?;

                    current_frame.push(quote_inner)?
                }

                TokenType::UnquoteSyntax => {
                    let quote_inner = self
                        .next()
                        .unwrap_or(Err(ParseError::UnexpectedEOF(
                            token.span,
                            self.source_name.clone(),
                        )))
                        .map(|x| self.construct_quasiunquote_syntax(x, token.span))?;

                    current_frame.push(quote_inner)?
                }

                TokenType::UnquoteSpliceSyntax => {
                    let quote_inner = self
                        .next()
                        .unwrap_or(Err(ParseError::UnexpectedEOF(
                            token.span,
                            self.source_name.clone(),
                        )))
                        .map(|x| self.construct_quasiunquote_syntax_splicing(x, token.span))?;

                    current_frame.push(quote_inner)?
                }

                TokenType::QuoteTick => {
                    // quote_count += 1;
                    // self.quote_stack.push(current_frame.exprs.len());
                    self.shorthand_quote_stack.push(stack.len());

                    let last_context = self.quote_context;

                    if self.quasiquote_depth == 0 {
                        self.quote_context = true;
                    }

                    // println!("Entering context: Quote Tick in read from tokens");

                    self.context.push(ParsingContext::QuoteTick(stack.len()));

                    let quote_inner = self
                        .next()
                        .unwrap_or(Err(ParseError::UnexpectedEOF(
                            token.span,
                            self.source_name.clone(),
                        )))
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
                        // dbg!(&popped);
                        debug_assert!(matches!(popped, ParsingContext::QuoteTick(_)))
                    }

                    current_frame.push(quote_inner?)?;
                }
                TokenType::Unquote => {
                    // println!("Entering context: Unquote");

                    // This could underflow and panic - if its negative then we have a problem. Maybe just use an isize and let it underflow?
                    self.decrement_quasiquote_context_if_not_in_quote_context();

                    self.context.push(ParsingContext::UnquoteTick(stack.len()));

                    let quote_inner = self
                        .next()
                        .unwrap_or(Err(ParseError::UnexpectedEOF(
                            token.span,
                            self.source_name.clone(),
                        )))
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
                    current_frame.push(quote_inner?)?;
                }
                TokenType::QuasiQuote => {
                    // println!("Entering context: Quasiquote");

                    self.increment_quasiquote_context_if_not_in_quote_context();

                    self.context
                        .push(ParsingContext::QuasiquoteTick(stack.len()));

                    let quote_inner = self
                        .next()
                        .unwrap_or(Err(ParseError::UnexpectedEOF(
                            token.span,
                            self.source_name.clone(),
                        )))
                        .map(|x| self.construct_quasiquote(x, token.span));

                    // self.context.pop();
                    // println!(
                    //     ">>>>>>>>>>>>>>>>>>> Exiting Context: {:?}",
                    //     self.context.pop()
                    // );

                    let popped_value = self.context.pop();

                    self.decrement_quasiquote_context_if_not_in_quote_context();

                    if let Some(popped) = popped_value {
                        debug_assert!(matches!(popped, ParsingContext::QuasiquoteTick(_)))
                    }

                    current_frame.push(quote_inner?)?;
                }
                TokenType::UnquoteSplice => {
                    // println!("Entering context: UnquoteSplicing");

                    self.decrement_quasiquote_context_if_not_in_quote_context();

                    self.context
                        .push(ParsingContext::UnquoteSplicingTick(stack.len()));

                    let quote_inner = self
                        .next()
                        .unwrap_or(Err(ParseError::UnexpectedEOF(
                            token.span,
                            self.source_name.clone(),
                        )))
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
                        debug_assert!(matches!(popped, ParsingContext::UnquoteSplicingTick(_)))
                    }

                    // println!("Exiting Context: {:?}", self.context.pop());
                    current_frame.push(quote_inner?)?;
                }
                TokenType::OpenParen(paren, paren_mod) => {
                    stack.push(current_frame);

                    current_frame = Frame {
                        open: token.span,
                        paren_mod,
                        exprs: Vec::with_capacity(4),
                        dot: None,
                        paren,
                        comment: 0,
                    };
                }
                TokenType::CloseParen(paren) => {
                    let close = token.span;
                    // This is the match that we'll want to move inside the below stack.pop() match statement
                    // As we close the current context, we check what our current state is -

                    if paren != current_frame.paren {
                        return Err(ParseError::MismatchedParen(
                            current_frame.paren,
                            token.span,
                            self.source_name.clone(),
                        ));
                    }

                    if let Some(mut prev_frame) = stack.pop() {
                        match prev_frame
                            .exprs
                            .first_mut()
                            .and_then(|x| x.atom_identifier_mut())
                        {
                            Some(ident) if *ident == *UNQUOTE => {
                                // self.increment_quasiquote_context_if_not_in_quote_context();
                                if self.quasiquote_depth == 0 && !self.quote_context {
                                    *ident = *RAW_UNQUOTE;
                                }
                                self.increment_quasiquote_context_if_not_in_quote_context();

                                // println!("Exiting unquote");
                            }
                            Some(ident) if *ident == *QUASIQUOTE => {
                                self.decrement_quasiquote_context_if_not_in_quote_context();

                                // println!("Exiting quasiquote");
                            }
                            Some(ident) if *ident == *UNQUOTE_SPLICING => {
                                // self.increment_quasiquote_context_if_not_in_quote_context();

                                if self.quasiquote_depth == 0 && !self.quote_context {
                                    *ident = *RAW_UNQUOTE_SPLICING;
                                }
                                self.increment_quasiquote_context_if_not_in_quote_context();

                                // println!("Exiting unquote");
                            }
                            _ => {}
                        }

                        match self.context.last().copied() {
                            // TODO: Change this -> This should really be just Some(ParsingContext::Quote)
                            // If we have _anything_ then we should check if we need to parse it differently. If we're at the last_quote_index,
                            // then we can pop it off inside there.
                            Some(ParsingContext::Quote(last_quote_index))
                            | Some(ParsingContext::Quasiquote(last_quote_index)) => {
                                if stack.len() <= last_quote_index {
                                    self.context.pop();
                                }

                                match current_frame.exprs.first() {
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
                                        ) => prev_frame.push(current_frame.into_expr(close)?)?,
                                        _ => {
                                            prev_frame.push(
                                                self.maybe_lower_frame(current_frame, close)?,
                                            )?;
                                        }
                                    },
                                    _ => {
                                        // println!("Converting to list");
                                        // println!("Context here: {:?}", self.context);
                                        prev_frame.push(current_frame.into_expr(close)?)?
                                    }
                                }
                            }

                            Some(ParsingContext::QuoteTick(_))
                            | Some(ParsingContext::QuasiquoteTick(_)) => {
                                match current_frame.exprs.first() {
                                    Some(ExprKind::Atom(Atom {
                                        syn:
                                            SyntaxObject {
                                                ty: TokenType::Quote,
                                                ..
                                            },
                                    })) => {
                                        // println!("Converting to quote inside quote tick");
                                        prev_frame
                                            .push(self.maybe_lower_frame(current_frame, close)?)?;
                                    }
                                    _ => {
                                        // if let Some(ParsingContext::QuasiquoteTick(_)) =
                                        //     self.context.last()
                                        // {
                                        //     self.decrement_quasiquote_context_if_not_in_quote_context();
                                        // }

                                        // println!("Converting to list inside quote tick");
                                        prev_frame.push(current_frame.into_expr(close)?)?
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

                                prev_frame.push(self.maybe_lower_frame(current_frame, close)?)?;
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
                                    // println!("{} - {}", stack.len(), last_quote_index);
                                    // println!("Exiting Context: {:?}", self.context.pop());
                                    self.context.pop();
                                }

                                prev_frame.push(self.maybe_lower_frame(current_frame, close)?)?;
                            }

                            // Else case, just go ahead and assume it is a normal frame
                            _ => prev_frame.push(self.maybe_lower_frame(current_frame, close)?)?,
                        }

                        // Reinitialize current frame here
                        current_frame = prev_frame;
                    } else {
                        // println!("Else case: {:?}", current_frame.exprs);
                        // println!("Context: {:?}", self.context);

                        // dbg!(&self.quote_stack);
                        // dbg!(&self.context);
                        // dbg!(&self.shorthand_quote_stack);
                        match self.context.last() {
                            Some(ParsingContext::QuoteTick(_))
                            | Some(ParsingContext::QuasiquoteTick(_)) => {
                                // | Some(ParsingContext::Quote(d)) && d > 0 => {

                                return current_frame.into_expr(close);
                            }
                            Some(ParsingContext::Quote(x)) if *x > 0 => {
                                self.context.pop();

                                return current_frame.into_expr(close);
                            }
                            Some(ParsingContext::Quote(0)) => {
                                self.context.pop();

                                return self.maybe_lower_frame(current_frame, close);
                            }
                            _ => {
                                // dbg!(self.quasiquote_depth);
                                // println!("=> {}", List::new(current_frame.exprs.clone()));
                                // println!("----------------------------------------");

                                if self.quasiquote_depth > 0 {
                                    // TODO/HACK - @Matt
                                    // If we're in a define syntax situation, go ahead and just return a normal one
                                    if current_frame
                                        .exprs
                                        .first()
                                        .map(|x| x.define_syntax_ident())
                                        .unwrap_or_default()
                                    {
                                        return self.maybe_lower_frame(current_frame, close);
                                    }

                                    // println!("Should still be quoted here");

                                    return current_frame.into_expr(close);
                                }

                                return self.maybe_lower_frame(current_frame, close);
                            }
                        }
                    }
                }

                _ => {
                    if let TokenType::Quote = &token.ty {
                        // self.quote_stack.push(current_frame.exprs.len());
                        self.quote_stack.push(stack.len());
                    }

                    // dbg!(&self.context);

                    // Mark what context we're inside with the context stack:
                    // This only works when its the first argument - check the function call in open paren?
                    if current_frame.exprs.is_empty() {
                        match &token.ty {
                            TokenType::Quote => {
                                if self.context == [ParsingContext::QuoteTick(0)] {
                                    self.context.push(ParsingContext::Quote(1))
                                } else {
                                    self.context.push(ParsingContext::Quote(stack.len()))
                                }

                                // self.context.push(ParsingContext::Quote(stack.len()))
                            }
                            TokenType::Identifier(ident) if *ident == *UNQUOTE => {
                                // println!("Entering unquote");

                                self.context.push(ParsingContext::Unquote(stack.len()));
                                self.decrement_quasiquote_context_if_not_in_quote_context();
                            }
                            TokenType::Identifier(ident) if *ident == *QUASIQUOTE => {
                                // println!("Entering quasiquote");

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

                    let atom = Atom::new(SyntaxObject::from_token_with_source(
                        &token,
                        &self.source_name.clone(),
                    ));

                    current_frame.push(ExprKind::Atom(atom))?
                }
            }
        }

        Err(ParseError::UnexpectedEOF(last, self.source_name.clone()))
    }

    fn next_token(&mut self) -> Option<Result<Token<'a, InternedString>>> {
        let res = self.tokenizer.next()?;

        Some(res.map_err(|err| ParseError::from(err).set_source(self.source_name.clone())))
    }
}

enum DocResult {
    Single(ExprKind),
    Double(ExprKind, ExprKind),
}

// TODO: Hack in the doc comment extraction for macros too since
// those can be special cased
fn wrap_in_doc_function(expr: ExprKind, comment: String) -> DocResult {
    if let ExprKind::List(l) = &expr {
        if let Some(ExprKind::Atom(Atom {
            syn:
                RawSyntaxObject {
                    ty: TokenType::DefineSyntax,
                    ..
                },
        })) = l.first()
        {
            // Just emit a single @doc line for the define-syntax,
            // and we'll handle the macro itself separately.
            if let Some(ident) = l.second_ident() {
                let doc = ExprKind::List(List::new(vec![
                    ExprKind::ident("@doc"),
                    ExprKind::string_lit(comment),
                    ExprKind::ident("#%macro"),
                    ExprKind::ident(ident.resolve()),
                ]));

                return DocResult::Double(doc, expr);
            }
        }
    }

    DocResult::Single(ExprKind::List(List::new(vec![
        ExprKind::ident("@doc"),
        ExprKind::string_lit(comment),
        expr,
    ])))
}

impl<'a> Parser<'a> {
    fn get_next_and_maybe_wrap_in_doc(&mut self) -> Option<Result<ExprKind>> {
        let mut datum_comments = vec![];

        macro_rules! maybe_return {
            ($value:expr) => {{
                let value = $value;

                if value.is_ok() {
                    if datum_comments.len() > 0 {
                        let _ = datum_comments.pop();

                        continue;
                    }
                }

                return Some(value);
            }};
        }

        while let Some(res) = self.next_token() {
            let res = match res {
                Ok(res) => res,
                Err(err) => {
                    return Some(Err(err));
                }
            };

            match res.ty {
                TokenType::Comment => {
                    if self.comment_buffer.is_empty()
                        && !self.collecting_comments
                        && res.source().trim_start_matches(';').starts_with("@doc")
                    {
                        self.collecting_comments = true;

                        continue;
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

                        let line = doc_line
                            .strip_prefix(" ")
                            .unwrap_or(doc_line)
                            .trim_end_matches(['\n', '\r']);
                        self.comment_buffer.push(line);
                    }

                    continue;
                }

                TokenType::DatumComment => {
                    datum_comments.push(res.span);
                }

                // Just turn this into `syntax`
                TokenType::QuoteSyntax => {
                    let value = self
                        .next()
                        .unwrap_or(Err(ParseError::UnexpectedEOF(
                            res.span,
                            self.source_name.clone(),
                        )))
                        .map(|x| self.construct_syntax(x, res.span));

                    maybe_return![value];
                }

                TokenType::QuasiQuoteSyntax => {
                    let value = self
                        .next()
                        .unwrap_or(Err(ParseError::UnexpectedEOF(
                            res.span,
                            self.source_name.clone(),
                        )))
                        .map(|x| self.construct_quasiquote_syntax(x, res.span));

                    maybe_return![value];
                }

                TokenType::UnquoteSyntax => {
                    let quote_inner = self
                        .next()
                        .unwrap_or(Err(ParseError::UnexpectedEOF(
                            res.span,
                            self.source_name.clone(),
                        )))
                        .map(|x| self.construct_quasiunquote_syntax(x, res.span));

                    maybe_return!(quote_inner);
                }

                TokenType::UnquoteSpliceSyntax => {
                    let quote_inner = self
                        .next()
                        .unwrap_or(Err(ParseError::UnexpectedEOF(
                            res.span,
                            self.source_name.clone(),
                        )))
                        .map(|x| self.construct_quasiunquote_syntax_splicing(x, res.span));

                    maybe_return!(quote_inner);
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
                        .unwrap_or(Err(ParseError::UnexpectedEOF(
                            res.span,
                            self.source_name.clone(),
                        )))
                        .map(|x| self.construct_quote_vec(x, res.span));

                    self.shorthand_quote_stack.pop();

                    let popped_value = self.context.pop();

                    if let Some(popped) = popped_value {
                        // dbg!(&popped);
                        debug_assert!(matches!(popped, ParsingContext::QuoteTick(_)))
                    }

                    self.quote_context = last;

                    // println!("Exiting context: {:?}", self.context.pop());
                    // println!("Result: {:?}", value);

                    // println!("{}", List::new(value.clone().unwrap()));

                    maybe_return!(match value {
                        Ok(v) => {
                            // Ok(ExprKind::List(List::new(v)))

                            self.maybe_lower(v)
                        }
                        Err(e) => Err(e),
                    });
                }

                TokenType::Unquote => {
                    // println!("Entering Context: Unquote");
                    self.context.push(ParsingContext::UnquoteTick(0));

                    self.decrement_quasiquote_context_if_not_in_quote_context();

                    let value = self
                        .next()
                        .unwrap_or(Err(ParseError::UnexpectedEOF(
                            res.span,
                            self.source_name.clone(),
                        )))
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

                    maybe_return!(value);
                }

                TokenType::UnquoteSplice => {
                    // println!("Entering Context: Unquotesplicing");
                    self.context.push(ParsingContext::UnquoteSplicingTick(0));

                    self.decrement_quasiquote_context_if_not_in_quote_context();

                    let value = self
                        .next()
                        .unwrap_or(Err(ParseError::UnexpectedEOF(
                            res.span,
                            self.source_name.clone(),
                        )))
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

                    maybe_return!(value);
                }
                // Make this also handle quasisyntax
                TokenType::QuasiQuote => {
                    self.context.push(ParsingContext::QuasiquoteTick(0));

                    self.increment_quasiquote_context_if_not_in_quote_context();

                    let value = self
                        .next()
                        .unwrap_or(Err(ParseError::UnexpectedEOF(
                            res.span,
                            self.source_name.clone(),
                        )))
                        .map(|x| self.construct_quasiquote(x, res.span));

                    let popped_value = self.context.pop();

                    if let Some(popped) = popped_value {
                        debug_assert!(matches!(popped, ParsingContext::QuasiquoteTick(_)))
                    }

                    self.decrement_quasiquote_context_if_not_in_quote_context();

                    maybe_return![value];
                }

                TokenType::OpenParen(paren, paren_mod) => {
                    let value = self
                        .read_from_tokens((res.span, paren, paren_mod))
                        .map_err(|err| err.set_source(self.source_name.clone()));

                    maybe_return![value];
                }
                TokenType::CloseParen(paren) => {
                    maybe_return!(Err(ParseError::UnexpectedChar(
                        paren.close(),
                        res.span,
                        self.source_name.clone(),
                    )))
                }
                _ => {
                    maybe_return![Ok(ExprKind::Atom(Atom::new(SyntaxObject::from(&res))))];
                }
            };
        }

        if !datum_comments.is_empty() {
            return Some(Err(ParseError::SyntaxError(
                "unfinished commented-out expression".into(),
                datum_comments.pop().unwrap(),
                None,
            )));
        }

        // We're done consuming input
        None
    }
}

impl<'a> Iterator for Parser<'a> {
    type Item = Result<ExprKind>;

    // TODO -> put the
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next) = self.queued.take() {
            return Some(Ok(next));
        }

        if self.quote_stack.is_empty()
            && self.shorthand_quote_stack.is_empty()
            && self.context.is_empty()
        {
            self.quasiquote_depth = 0;
            self.comment_buffer.clear();
        }

        self.get_next_and_maybe_wrap_in_doc().map(|res| {
            if self.comment_buffer.is_empty() || !self.context.is_empty() {
                res
            } else {
                // Reset the comment collection until next @doc statement
                self.collecting_comments = false;
                res.map(|x| {
                    let result = wrap_in_doc_function(
                        x,
                        self.comment_buffer.drain(..).collect::<Vec<_>>().join("\n"),
                    );

                    match result {
                        DocResult::Single(expr_kind) => expr_kind,
                        DocResult::Double(expr_kind, expr_kind1) => {
                            self.queued = Some(expr_kind1);
                            expr_kind
                        }
                    }
                })
            }
        })
    }
}

// Lower the syntax rules down from the list representation
pub fn lower_syntax_rules(expr: ExprKind) -> Result<SyntaxRules> {
    let mut value_iter = expr.into_list().into_iter();
    let syn = value_iter
        .next()
        .unwrap()
        .into_atom_syntax_object()
        .unwrap();

    let syntax_vec = if let Some(ExprKind::List(l)) = value_iter.next() {
        l.args
    } else {
        return Err(ParseError::SyntaxError(
            "syntax-rules expects a list of new syntax forms used in the macro".to_string(),
            syn.span,
            None,
        ));
    };

    let mut pairs = Vec::new();
    let rest: Vec<_> = value_iter.collect();

    for pair in rest {
        if let ExprKind::List(l) = pair {
            if l.args.len() != 2 {
                return Err(ParseError::SyntaxError(
                    "syntax-rules requires only one pattern to one body".to_string(),
                    syn.span,
                    None,
                ));
            }

            let mut pair_iter = l.args.into_iter();
            let pair_object =
                PatternPair::new(pair_iter.next().unwrap(), pair_iter.next().unwrap())?;
            pairs.push(pair_object);
        } else {
            return Err(ParseError::SyntaxError(
                "syntax-rules requires pattern to expressions to be in a list".to_string(),
                syn.span,
                None,
            ));
        }
    }

    Ok(SyntaxRules::new(syntax_vec, pairs, syn))
}

// Lower define-syntax down from the list representation
pub fn lower_macro_and_require_definitions(expr: ExprKind) -> Result<ExprKind> {
    let as_list = expr.list();

    // If this qualifies as
    if as_list.map(List::is_define_syntax).unwrap_or_default()
        && as_list
            .unwrap()
            .get(2)
            .and_then(ExprKind::list)
            .map(List::is_syntax_rules)
            .unwrap_or_default()
    {
        let mut value_iter = expr.into_list().into_iter();

        let define_syntax = value_iter.next().unwrap();

        let name = value_iter.next().unwrap();
        let syntax = lower_syntax_rules(value_iter.next().unwrap())?;

        return Ok(ExprKind::Macro(Box::new(Macro::new(
            name,
            Box::new(syntax),
            define_syntax.into_atom_syntax_object().unwrap(),
        ))));
    }

    if as_list.map(List::is_require).unwrap_or_default() {
        let mut raw = expr.into_list().args;

        let syn = raw.remove(0).into_atom_syntax_object().unwrap();

        if raw.is_empty() {
            return Err(ParseError::ArityMismatch(
                "require expects at least one identifier or string".to_string(),
                syn.span,
                None,
            ));
        }

        return Ok(ExprKind::Require(Box::new(ast::Require::new(raw, syn))));
    }

    let mut expr = expr;

    // TODO: Here, we should lower syntax-case itself
    // to a defmacro, so that things seem to work out correctly.

    // HACK:
    // If we get here, we can convert the define-syntax back into an identifier
    // so that other macro expansion can occur on it.
    if let Some(first) = expr
        .list_mut()
        .and_then(|x| x.args.first_mut().and_then(|x| x.atom_syntax_object_mut()))
    {
        if first.ty == TokenType::DefineSyntax {
            first.ty = TokenType::Identifier("define-syntax".into());
        }
    }

    Ok(expr)
}

struct ASTLowerPass {
    quote_depth: usize,
}

impl ASTLowerPass {
    fn lower(&mut self, expr: &mut ExprKind) -> Result<()> {
        match expr {
            ExprKind::List(ref mut value) => {
                if value.is_quote() {
                    // println!("Found quote: {:?}", value);
                    self.quote_depth += 1;
                }

                // Visit the children first, on the way back up, assign into the
                // correct AST node
                // value.args = value
                //     .args
                //     .into_iter()
                //     .map(|x| self.lower(x))
                //     .collect::<Result<_>>()?;

                for expr in value.args.iter_mut() {
                    self.lower(expr)?;
                }

                if value.is_quote() {
                    self.quote_depth -= 1;
                }

                if let Some(f) = value.args.first_mut().and_then(|x| {
                    if let ExprKind::Atom(a) = x {
                        if a.syn.ty == TokenType::DefineSyntax {
                            a.syn.ty = TokenType::Identifier("define-syntax".into());
                        }

                        Some(x.clone())
                    } else {
                        None
                    }
                }) {
                    match f {
                        ExprKind::Atom(a) if self.quote_depth == 0 && value.is_quote() => {
                            match &a.syn.ty {
                                TokenType::Quote => {
                                    *expr = parse_single_argument(
                                        std::mem::take(&mut value.args).into_iter(),
                                        a.syn.clone(),
                                        "quote",
                                        |expr, syn| ast::Quote::new(expr, syn).into(),
                                    )?;

                                    Ok(())
                                }
                                _ => unreachable!(),
                            }
                        }
                        ExprKind::Atom(a) if self.quote_depth == 0 => {
                            let value = std::mem::replace(value, List::new(vec![]));

                            *expr = match &a.syn.ty {
                                TokenType::If => {
                                    parse_if(value.args_proper(TokenType::If)?.into_iter(), a.syn)
                                }
                                TokenType::Identifier(expr) if *expr == *IF => {
                                    parse_if(value.args_proper(TokenType::If)?.into_iter(), a.syn)
                                }

                                TokenType::Define => parse_define(
                                    value.args_proper(TokenType::Define)?.into_iter(),
                                    a.syn,
                                ),
                                TokenType::Identifier(expr) if *expr == *DEFINE => parse_define(
                                    value.args_proper(TokenType::Define)?.into_iter(),
                                    a.syn,
                                ),

                                TokenType::Let => parse_let(
                                    value.args_proper(TokenType::Let)?.into_iter(),
                                    a.syn.clone(),
                                ),
                                TokenType::Identifier(expr) if *expr == *LET => {
                                    parse_let(value.args_proper(TokenType::Let)?.into_iter(), a.syn)
                                }

                                // TODO: Deprecate
                                TokenType::TestLet => parse_new_let(
                                    value.args_proper(TokenType::TestLet)?.into_iter(),
                                    a.syn,
                                ),
                                TokenType::Identifier(expr) if *expr == *PLAIN_LET => {
                                    parse_new_let(
                                        value.args_proper(TokenType::TestLet)?.into_iter(),
                                        a.syn,
                                    )
                                }

                                TokenType::Quote => parse_single_argument(
                                    value.args_proper(TokenType::Quote)?.into_iter(),
                                    a.syn,
                                    "quote",
                                    |expr, syn| ast::Quote::new(expr, syn).into(),
                                ),
                                TokenType::Identifier(expr) if *expr == *QUOTE => {
                                    parse_single_argument(
                                        value.args_proper(TokenType::Quote)?.into_iter(),
                                        a.syn,
                                        "quote",
                                        |expr, syn| ast::Quote::new(expr, syn).into(),
                                    )
                                }

                                TokenType::Return => parse_single_argument(
                                    value.args_proper(TokenType::Return)?.into_iter(),
                                    a.syn,
                                    "return!",
                                    |expr, syn| ast::Return::new(expr, syn).into(),
                                ),
                                TokenType::Identifier(expr) if *expr == *RETURN => {
                                    parse_single_argument(
                                        value.args_proper(TokenType::Return)?.into_iter(),
                                        a.syn,
                                        "return!",
                                        |expr, syn| ast::Return::new(expr, syn).into(),
                                    )
                                }

                                TokenType::Require => {
                                    parse_require(&a, value.args_proper(TokenType::Require)?)
                                }
                                TokenType::Identifier(expr) if *expr == *REQUIRE => {
                                    parse_require(&a, value.args_proper(TokenType::Require)?)
                                }

                                TokenType::Set => parse_set(&a, value.args_proper(TokenType::Set)?),
                                TokenType::Identifier(expr) if *expr == *SET => {
                                    parse_set(&a, value.args_proper(TokenType::Set)?)
                                }

                                TokenType::Begin => {
                                    parse_begin(a, value.args_proper(TokenType::Begin)?)
                                }
                                TokenType::Identifier(expr) if *expr == *BEGIN => {
                                    parse_begin(a, value.args_proper(TokenType::Begin)?)
                                }

                                TokenType::Lambda => {
                                    parse_lambda(a, value.args_proper(TokenType::Lambda)?)
                                }
                                TokenType::Identifier(expr)
                                    if *expr == *LAMBDA
                                        || *expr == *LAMBDA_FN
                                        || *expr == *LAMBDA_SYMBOL =>
                                {
                                    parse_lambda(a, value.args_proper(TokenType::Lambda)?)
                                }

                                _ => Ok(ExprKind::List(value)),
                            }?;

                            Ok(())
                        }
                        _ => Ok(()),
                    }
                } else {
                    Ok(())
                }
            }
            ExprKind::Atom(_) => Ok(()),
            ExprKind::If(iff) => {
                self.lower(&mut iff.test_expr)?;
                self.lower(&mut iff.then_expr)?;
                self.lower(&mut iff.else_expr)?;
                Ok(())
            }
            ExprKind::Let(l) => {
                for (left, right) in l.bindings.iter_mut() {
                    self.lower(left)?;
                    self.lower(right)?;
                }

                self.lower(&mut l.body_expr)?;

                Ok(())
            }
            ExprKind::Define(d) => {
                self.lower(&mut d.name)?;
                self.lower(&mut d.body)?;

                Ok(())
            }

            ExprKind::LambdaFunction(f) => {
                for arg in f.args.iter_mut() {
                    self.lower(arg)?;
                }

                self.lower(&mut f.body)?;

                Ok(())
            }
            ExprKind::Begin(b) => {
                for expr in b.exprs.iter_mut() {
                    self.lower(expr)?;
                }

                Ok(())
            }
            // Ok(ExprKind::Begin(ast::Begin::new(
            //     b.exprs
            //         .into_iter()
            //         .map(|x| self.lower(x))
            //         .collect::<Result<_>>()?,
            //     b.location,
            // ))),
            ExprKind::Return(r) => {
                self.lower(&mut r.expr)?;
                Ok(())
            }
            // Ok(ExprKind::Return(Box::new(ast::Return::new(
            //     self.lower(r.expr)?,
            //     r.location,
            // )))),
            ExprKind::Quote(_) => Ok(()),
            ExprKind::Macro(_) => Ok(()),
            ExprKind::SyntaxRules(_) => Ok(()),
            ExprKind::Set(s) => {
                self.lower(&mut s.variable)?;
                self.lower(&mut s.expr)?;

                Ok(())
            }
            // Ok(ExprKind::Set(Box::new(ast::Set::new(
            //     self.lower(s.variable)?,
            //     self.lower(s.expr)?,
            //     s.location,
            // )))),
            ExprKind::Require(_) => Ok(()),
            ExprKind::Vector(v) => {
                for arg in &mut v.args {
                    self.lower(arg)?;
                }

                Ok(())
            }
        }
    }
}

// TODO: Lower the rest of the AST post expansion, such that
pub fn lower_entire_ast(expr: &mut ExprKind) -> Result<()> {
    ASTLowerPass { quote_depth: 0 }.lower(expr)
}

#[derive(Debug)]
struct Frame {
    open: Span,
    paren: Paren,
    paren_mod: Option<ParenMod>,
    exprs: Vec<ExprKind>,
    dot: Option<(u32, Span)>,
    comment: u8,
}

impl Frame {
    fn into_expr(self, close: Span) -> Result<ExprKind> {
        self.build_expr(close, |exprs| Ok(List::new(exprs).into()))
    }

    fn build_expr(
        self,
        close: Span,
        builder: impl FnOnce(Vec<ExprKind>) -> Result<ExprKind>,
    ) -> Result<ExprKind> {
        if self.comment > 0 {
            return Err(ParseError::SyntaxError(
                "invalid datum comment".into(),
                self.open,
                None,
            ));
        }

        if let Some(paren_mod) = self.paren_mod {
            let bytes = matches!(paren_mod, ParenMod::Bytes);

            return Ok(Vector {
                args: self.exprs,
                bytes,
                span: Span::merge(self.open, close),
            }
            .into());
        };

        let improper = self.improper()?;

        let mut expr = builder(self.exprs)?;

        match &mut expr {
            ExprKind::List(list) => {
                list.location = Span::merge(self.open, close);

                if improper {
                    list.make_improper();
                }
            }
            _ if improper => todo!(),
            _ => {}
        };

        Ok(expr)
    }

    fn push(&mut self, expr: ExprKind) -> Result<()> {
        if let Some(idx) = self.dot.as_ref().map(|x| x.0) {
            debug_assert!(!self.exprs.is_empty());

            if idx as usize != self.exprs.len() {
                debug_assert_eq!(idx + 1, self.exprs.len() as _);

                return Err(ParseError::SyntaxError(
                    "improper list must have a single cdr".to_owned(),
                    expr.span(),
                    None,
                ));
            }
        }

        let valid_for_bytes = match (self.paren_mod, &expr) {
            (Some(ParenMod::Bytes), ExprKind::Atom(atom)) if atom.byte().is_some() => true,
            (Some(ParenMod::Bytes), _) => false,
            _ => true,
        };

        if !valid_for_bytes {
            return Err(ParseError::SyntaxError(
                "bytevector literals can only contain integer literals in the 0-255 range".into(),
                expr.span(),
                None,
            ));
        }

        if self.comment > 0 {
            self.comment -= 1;

            return Ok(());
        }

        self.exprs.push(expr);

        Ok(())
    }

    fn improper(&self) -> Result<bool> {
        match self.dot {
            Some((idx, _)) if idx + 1 == self.exprs.len() as _ => Ok(true),
            Some((idx, span)) => {
                debug_assert_eq!(idx, self.exprs.len() as _);

                Err(ParseError::SyntaxError(
                    "improper list must have a single cdr".into(),
                    span,
                    None,
                ))
            }
            None => Ok(false),
        }
    }
}

#[cfg(test)]
mod parser_tests {
    // use super::TokenType::*;
    use super::*;
    use crate::parser::ast::{Begin, Define, If, LambdaFunction, Quote, Return};
    use crate::tokens::{Paren, RealLiteral};
    use crate::visitors::Eraser;
    use crate::{parser::ast::ExprKind, tokens::IntLiteral};

    fn atom(ident: &str) -> ExprKind {
        ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Identifier(
            ident.into(),
        ))))
    }

    fn int(num: isize) -> ExprKind {
        ExprKind::Atom(Atom::new(SyntaxObject::default(
            IntLiteral::Small(num).into(),
        )))
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
        let a: Result<Vec<_>> = Parser::new(s, SourceId::none()).collect();
        a.unwrap();
    }

    fn parse_err(s: &str) -> ParseError {
        let a: Result<Vec<_>> = Parser::new(s, SourceId::none()).collect();
        a.unwrap_err()
    }

    fn assert_parse(s: &str, result: &[ExprKind]) {
        let a: Result<Vec<ExprKind>> = Parser::new(s, SourceId::none()).collect();
        let mut a = a.unwrap();

        let mut eraser = Eraser;

        eraser.visit_many(&mut a);

        assert_eq!(a.as_slice(), result);
    }

    fn assert_syntax_err(s: &str, expected: &str) {
        let a: Result<Vec<ExprKind>> = Parser::new(s, SourceId::none()).collect();
        let Err(ParseError::SyntaxError(err, _, _)) = a else {
            panic!("expected syntax error, got {a:?}");
        };

        assert_eq!(err, expected);
    }

    fn assert_parse_is_err(s: &str) {
        let a: Result<Vec<ExprKind>> = Parser::new(s, SourceId::none()).collect();
        assert!(a.is_err());
    }

    #[test]
    fn check_resulting_parsing() {
        let expr = r#"`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)"#;

        let a: Result<Vec<ExprKind>> = Parser::new(expr, SourceId::none()).collect();
        let a = a.unwrap();

        println!("{}", a[0]);
    }

    #[test]
    fn check_double_unquote_parsing() {
        let expr = r#"(let ([name1 'x] [name2 'y]) `(a `(b ,,name1 ,',name2 d) e))"#;

        let a: Result<Vec<ExprKind>> = Parser::new(expr, SourceId::none()).collect();
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

        let parser = Parser::doc_comment_parser(expr, SourceId::none());

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
        assert_parse("#\\u{b}", &[character('\u{b}')]);
    }

    #[test]
    fn parse_more_unicode() {
        assert_parse("#\\u{a0}", &[character('\u{a0}')]);
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

    macro_rules! assert_matches {
        ($expr:expr, $pat:pat) => {{
            let value = $expr;
            match value {
                $pat => {}
                _ => panic!("{value:?} does not match"),
            }
        }};
    }

    #[test]
    fn test_error() {
        assert_matches![parse_err("("), ParseError::UnexpectedEOF(..)];
        assert_matches![parse_err("(abc"), ParseError::UnexpectedEOF(..)];
        assert_matches![parse_err("(ab 1 2"), ParseError::UnexpectedEOF(..)];
        assert_matches![parse_err("((((ab 1 2) ("), ParseError::UnexpectedEOF(..)];
        assert_matches![parse_err("())"), ParseError::UnexpectedChar(')', ..)];
        assert_matches![parse_err("() (((("), ParseError::UnexpectedEOF(..)];
        assert_matches![parse_err("')"), ParseError::UnexpectedChar(')', ..)];
        assert_matches![parse_err("(')"), ParseError::UnexpectedChar(')', ..)];
        assert_matches![parse_err("('"), ParseError::UnexpectedEOF(..)];
        assert_matches![parse_err(r#""abc"#), ParseError::UnexpectedEOF(..)];
        assert_matches![
            parse_err("(]"),
            ParseError::MismatchedParen(Paren::Round, _, None)
        ];
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
        // assert_parse_is_err("(if 1 2)");
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
                        ExprKind::Atom(Atom::new(SyntaxObject::default(
                            RealLiteral::Float(3.5).into(),
                        ))),
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
                    SyntaxObject::default(TokenType::If),
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
                        SyntaxObject::default(TokenType::If),
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
                    ExprKind::Begin(Box::new(Begin::new(
                        vec![
                            ExprKind::List(List::new(vec![atom("+"), atom("x"), int(10)])),
                            ExprKind::List(List::new(vec![atom("+"), atom("y"), int(20)])),
                            ExprKind::List(List::new(vec![atom("+"), atom("z"), int(30)])),
                        ],
                        SyntaxObject::default(TokenType::Begin),
                    ))),
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
                    ExprKind::Begin(Box::new(Begin::new(
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
                    ))),
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
            &[ExprKind::Begin(Box::new(Begin::new(
                vec![int(1), int(2), int(3)],
                SyntaxObject::default(TokenType::Begin),
            )))],
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
    fn test_lambda_function_with_rest() {
        assert_parse(
            "(lambda (x . y) 10)",
            &[ExprKind::LambdaFunction(Box::new(
                LambdaFunction::new_maybe_rest(
                    vec![atom("x"), atom("y")],
                    int(10),
                    SyntaxObject::default(TokenType::Lambda),
                    true,
                ),
            ))],
        )
    }

    #[test]
    fn test_lambda_function_with_rest_only() {
        assert_parse(
            "(lambda x 10)",
            &[ExprKind::LambdaFunction(Box::new(
                LambdaFunction::new_maybe_rest(
                    vec![atom("x")],
                    int(10),
                    SyntaxObject::default(TokenType::Lambda),
                    true,
                ),
            ))],
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
                    SyntaxObject::default(TokenType::Lambda),
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

    #[test]
    fn test_parse_without_lowering_ast() {
        let a: Result<Vec<ExprKind>> =
            Parser::new_flat("(define (quote a) 10) (require foo bar)", SourceId::none())
                .map(|x| x.and_then(lower_macro_and_require_definitions))
                .collect();

        let a = a.unwrap();

        println!("{:#?}", a);
    }

    #[test]
    fn test_delayed_lowering() {
        let a: Result<Vec<ExprKind>> = Parser::new_flat(
            r#"
            ;; (define foo (quote a)) (require foo bar)

               ;;  (define (foo) (if 10 20 30))

               ;; (define (foo) (quote (define 10 20)))

               ;; (define foo '())

            (define-syntax
   with-handler
   (syntax-rules
      ()
      ((with-handler handler expr)
         (reset
            (call-with-exception-handler
               (λ (err)
                 (begin (handler err) (shift k (k void))))
               (λ ()
                 expr))))
      ((with-handler handler expr ...)
         (reset
            (call-with-exception-handler
               (λ (err)
                 (begin (handler err) (shift k (k void))))
               (λ ()
                 (begin expr ...)))))))



                "#,
            SourceId::none(),
        )
        .map(|x| x.and_then(lower_macro_and_require_definitions))
        .map(|x| {
            x.and_then(|mut expr| {
                lower_entire_ast(&mut expr)?;
                Ok(expr)
            })
        })
        .collect();

        let a = a.unwrap();

        println!("{:#?}", a);
    }

    #[test]
    fn test_improper_list() {
        let mut pair = List::new(vec![atom("x"), atom("y")]);
        pair.make_improper();

        assert_parse("(x . y)", &[ExprKind::List(pair)]);

        assert_parse(
            "(x . (y . ()))",
            &[ExprKind::List(List::new(vec![atom("x"), atom("y")]))],
        )
    }

    #[test]
    fn test_improper_list_failures() {
        assert_syntax_err(
            "(. a)",
            "improper lists must have a car element before the dot",
        );
        assert_syntax_err("(a .)", "improper list must have a single cdr");
        assert_syntax_err("(a . b . )", "improper lists can only have a single dot");
        assert_syntax_err("(a . b . c)", "improper lists can only have a single dot");
        assert_syntax_err("(a . b c)", "improper list must have a single cdr");
        assert_syntax_err("(a . b (c))", "improper list must have a single cdr");
    }

    #[test]
    fn test_vectors() {
        assert_parse(
            "#(a b)",
            &[ExprKind::Vector(Vector {
                args: vec![atom("a"), atom("b")],
                bytes: false,
                span: Span::default(),
            })],
        );

        assert_parse(
            "#u8(1 3)",
            &[ExprKind::Vector(Vector {
                args: vec![int(1), int(3)],
                bytes: true,
                span: Span::default(),
            })],
        );
    }

    #[test]
    fn test_malformed_vectors() {
        assert_syntax_err(
            "#u8(#\\a)",
            "bytevector literals can only contain integer literals in the 0-255 range",
        );

        assert_syntax_err(
            "#u8(())",
            "bytevector literals can only contain integer literals in the 0-255 range",
        );

        assert_syntax_err("#u8(1 . 2)", "bytevector literals cannot contain dots");

        assert_syntax_err("#(1 . 2)", "vector literals cannot contain dots");
    }

    #[test]
    fn test_datum_comments() {
        assert_parse("#; 1 2", &[int(2)]);

        assert_parse("#; #; #false (2) 3", &[int(3)]);

        assert_parse("#; ,@foo 4", &[int(4)]);

        assert_parse(
            "(1 #; #(a b c) 2)",
            &[ExprKind::List(List::new(vec![int(1), int(2)]))],
        )
    }

    #[test]
    fn test_invalid_datum_comments() {
        assert_syntax_err("( 1 #; )", "invalid datum comment");

        assert_syntax_err(
            "( 1 2 #; . b)",
            "commented-out datum cannot start with a dot",
        );
    }
}

use std::cell::RefCell;
use std::default::Default;
use std::ops::{Deref, DerefMut};

pub trait Recyclable {
    fn put(self);
    fn get() -> Self;
    fn get_with_capacity(capacity: usize) -> Self;
}

pub struct Recycle<T: Recyclable + Default> {
    t: T,
}

impl<T: Recyclable + Default> Default for Recycle<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Recyclable + Default> Recycle<T> {
    pub fn new() -> Self {
        Recycle { t: T::get() }
    }

    pub fn new_with_capacity(capacity: usize) -> Self {
        Recycle {
            t: T::get_with_capacity(capacity),
        }
    }
}

impl<T: Recyclable + Default> Drop for Recycle<T> {
    fn drop(&mut self) {
        T::put(std::mem::take(&mut self.t))
    }
}

impl<T: Recyclable + Default> Deref for Recycle<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.t
    }
}

impl<T: Recyclable + Default + 'static> DerefMut for Recycle<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.t
    }
}

impl<T: Recyclable + Clone + Default> Clone for Recycle<T> {
    fn clone(&self) -> Self {
        Recycle { t: self.t.clone() }
    }
}

impl<T: Recyclable + std::fmt::Debug + Default> std::fmt::Debug for Recycle<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Recycle").field("t", &self.t).finish()
    }
}

impl<T: Recyclable + std::hash::Hash + Default> std::hash::Hash for Recycle<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.t.hash(state);
    }
}

macro_rules! impl_recyclable {
    ($tl:ident, $t:ty) => {
        impl_recyclable!($tl, $t, Default::default(), Self::with_capacity);
    };
    ($tl:ident, $t:ty, $constructor:expr, $constructor_capacity:expr) => {
        thread_local! {
            static $tl: RefCell<Vec<$t>> = RefCell::new(Vec::new())
        }

        impl Recyclable for $t {
            fn put(mut self) {
                let _ = $tl.try_with(|p| {
                    let p = p.try_borrow_mut();

                    if let Ok(mut p) = p {
                        // This _should_ be cleared, but it seems it is not!
                        // debug_assert!(self.is_empty());
                        // self.clear();

                        // for frame in &self {
                        //     for expr in &frame.exprs {
                        //         println!("dropping: {}", expr);
                        //     }
                        // }

                        // dbg!(&self);
                        self.clear();

                        p.push(self);
                    }
                });
            }

            fn get() -> Self {
                $tl.with(|p| {
                    let mut p = p.borrow_mut();
                    p.pop()
                })
                .unwrap_or($constructor)
            }

            fn get_with_capacity(capacity: usize) -> Self {
                $tl.with(|p| {
                    let mut p = p.borrow_mut();
                    p.pop()
                })
                .unwrap_or(($constructor_capacity)(capacity))
            }
        }
    };
}

impl_recyclable!(TL_V_STEELVAL, Vec<Frame>);
