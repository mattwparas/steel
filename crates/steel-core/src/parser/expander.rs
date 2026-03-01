use crate::compiler::passes::VisitorMutRefUnit;
use crate::compiler::program::{BEGIN, DEFINE, ELLIPSES_SYMBOL, IF, LAMBDA};
use crate::parser::ast::{Atom, ExprKind, List, Macro, PatternPair, Vector};
use crate::parser::expand_visitor::GlobalMap;
use crate::parser::parser::SyntaxObject;
use crate::parser::rename_idents::RenameIdentifiersVisitor;
use crate::parser::replace_idents::replace_identifiers;
use crate::parser::tokens::TokenType;

use crate::parser::span::Span;

use crate::rvals::{IntoSteelVal, Result};
use core::cell::RefCell;
use quickscope::ScopeSet;
use std::{
    collections::HashMap,
    fs::File,
    io::{Read, Write},
    iter::FromIterator,
    path::{Path, PathBuf},
};

use rustc_hash::{FxBuildHasher, FxHashMap, FxHashSet};
use serde::{Deserialize, Serialize};
use smallvec::SmallVec;
#[cfg(test)]
use steel_parser::tokens::IntLiteral;
use steel_parser::tokens::InternedNumber;

use super::macro_template::MacroTemplate;
use super::{ast::Quote, interner::InternedString, parser::Parser};

use thin_vec::thin_vec;

// Given path, update the extension
fn update_extension(mut path: PathBuf, extension: &str) -> PathBuf {
    path.set_extension(extension);
    path
}

// Prepend the given path with the working directory
pub fn path_from_working_dir<P: AsRef<Path>>(path: P) -> std::io::Result<PathBuf> {
    let mut working_dir = std::env::current_dir()?;
    working_dir.push(path);
    Ok(working_dir)
}

/// Manages macros for a single namespace
#[derive(Default, Serialize, Deserialize, Debug)]
pub struct LocalMacroManager {
    macros: FxHashMap<InternedString, SteelMacro>,
}

impl LocalMacroManager {
    /// Look to see if it exists on disk, otherwise parse from the associated file
    pub fn initialize_from_path(path: PathBuf, force_update: bool) -> Result<Self> {
        let raw_path = update_extension(path.clone(), "rkt");
        let compiled_path = update_extension(path, "macro");

        // println!("Raw path: {:?}", raw_path);
        // println!("Compiled path: {:?}", compiled_path);

        if compiled_path.exists() && !force_update {
            Self::from_file(compiled_path)
        } else if raw_path.exists() {
            let parsed = Self::from_expression_file(raw_path)?;
            parsed.write_to_file(compiled_path)?;

            Ok(parsed)
        } else {
            stop!(Generic => "path to macro file does not exist")
        }
    }

    fn write_to_file(&self, path: PathBuf) -> Result<()> {
        let mut file = File::create(path)?;

        match bincode::serialize(self) {
            Ok(bytes) => {
                file.write_all(&bytes)?;
                Ok(())
            }
            Err(e) => stop!(Generic => format!("Unable to write macro to file: {e:?}")),
        }
    }

    /// Expand the expressions according to the macros in the current scope
    pub fn expand(&self, exprs: &mut [ExprKind]) -> Result<()> {
        for expr in exprs.iter_mut() {
            crate::parser::expand_visitor::expand(
                expr,
                &self.macros,
                super::expand_visitor::GlobalMap::Set(&Default::default()),
            )?;
        }

        Ok(())
    }

    /// Read in a file of expressions containing macros, parse then, and create the data struture
    fn from_expression_file(path: PathBuf) -> Result<Self> {
        let mut file = File::open(path)?;
        let mut raw_exprs = String::new();
        file.read_to_string(&mut raw_exprs)?;
        let parsed_exprs = Parser::parse(&raw_exprs)?;
        Self::from_exprs(parsed_exprs)
    }

    /// After serializing the macro manager to a file and read from that file
    fn from_file(path: PathBuf) -> Result<Self> {
        let mut file = File::open(path)?;
        let mut buffer = Vec::new();
        let _ = file.read_to_end(&mut buffer)?;
        match bincode::deserialize(&buffer) {
            Ok(inner) => Ok(inner),
            Err(e) => {
                stop!(Generic => format!("Unable to deserialize local macro manager from file: {e:?}"))
            }
        }
    }

    /// Initialize the macro manager from a vec of macro expressions
    /// Note: this will fail if passed other expressions
    pub fn from_exprs(exprs: impl IntoIterator<Item = ExprKind>) -> Result<Self> {
        exprs
            .into_iter()
            .map(|x| {
                if let ExprKind::Macro(m) = x {
                    SteelMacro::parse_from_ast_macro(m)
                } else {
                    stop!(Generic => "Unexpected non-macro definition")
                }
            })
            .collect()
    }
}

impl FromIterator<SteelMacro> for LocalMacroManager {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = SteelMacro>,
    {
        Self {
            macros: iter.into_iter().map(|x| (x.name.clone(), x)).collect(),
        }
    }
}

// Global macro manager, manages macros across modules
pub struct GlobalMacroManager {
    _scopes: HashMap<PathBuf, HashMap<String, SteelMacro>>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct SteelMacro {
    name: InternedString,
    special_forms: Vec<InternedString>,
    pub(crate) cases: Vec<MacroCase>,
    mangled: bool,
    pub(crate) location: Span,
    pub(crate) special_mangled: bool,
}

impl SteelMacro {
    #[cfg(test)]
    pub fn new(
        name: InternedString,
        special_forms: Vec<InternedString>,
        cases: Vec<MacroCase>,
        location: Span,
    ) -> Self {
        SteelMacro {
            name,
            special_forms,
            cases,
            mangled: false,
            location,
            special_mangled: false,
        }
    }

    pub fn name(&self) -> &InternedString {
        &self.name
    }

    pub fn span(&self) -> Span {
        self.location
    }

    pub fn exprs_mut(&mut self) -> impl Iterator<Item = &mut ExprKind> {
        self.cases.iter_mut().map(|x| &mut x.body)
    }

    pub fn exprs(&self) -> impl Iterator<Item = &ExprKind> {
        self.cases.iter().map(|x| &x.body)
    }

    pub fn mark_mangled(&mut self) {
        self.mangled = true;
    }

    pub fn is_mangled(&self) -> bool {
        self.mangled
    }

    pub fn parse_from_ast_macro(mut ast_macro: Box<Macro>) -> Result<Self> {
        // HACK: Parse the token as a specific kind of identifier
        if let Some(ident) = ast_macro.name.atom_syntax_object_mut() {
            if ident.ty == TokenType::DefineSyntax {
                ident.ty = TokenType::Identifier("define-syntax".into())
            }
        }

        let name = *ast_macro
            .name
            .atom_identifier_or_else(throw!(BadSyntax => "macros only currently support
                    identifiers as the name"; ast_macro.location.span))?;

        let special_forms = ast_macro
            .syntax_rules
            .syntax
            .into_iter()
            .map(|x| {
                x.atom_identifier_or_else(
                    throw!(BadSyntax => format!("macros only support identifiers for special syntax, found: {}", x); x.span()),
                )
                .cloned()
            })
            .collect::<Result<Vec<_>>>()?;

        let cases = ast_macro
            .syntax_rules
            .patterns
            .into_iter()
            .map(|x| MacroCase::parse_from_pattern_pair(x, &name, &special_forms))
            .collect::<Result<Vec<_>>>()?;

        Ok(SteelMacro {
            name,
            special_forms,
            cases,
            mangled: false,
            location: ast_macro.location.span,
            special_mangled: false,
        })
    }

    // TODO the case matching should be a little bit more informed than this
    // I think it should also not be greedy, and should report if there are ambiguous matchings
    pub(crate) fn match_case(
        &self,
        expr: &List,
        in_scope: &ScopeSet<InternedString, FxBuildHasher>,
        globals: GlobalMap,
    ) -> Result<&MacroCase> {
        for case in &self.cases {
            // println!("Matching: {:?}", case.args);
            if case.recursive_match(expr, in_scope, globals) {
                return Ok(case);
            }
        }

        if let Some(ExprKind::Atom(a)) = expr.first() {
            // for case in &self.cases {
            //     println!("{:?}", case.args);
            //     println!("{}", expr);
            // }

            stop!(BadSyntax => format!("macro expansion unable to match case: {expr}"); a.syn.span);
        } else {
            unreachable!()
        }
    }

    pub(crate) fn match_case_index(&self, expr: &List) -> Result<(&MacroCase, usize)> {
        // TODO: Ellipses need to not be mangled?
        for (index, case) in self.cases.iter().enumerate() {
            if case.recursive_match(
                expr,
                &ScopeSet::default(),
                GlobalMap::Set(&Default::default()),
            ) {
                return Ok((case, index));
            }
        }

        if let Some(ExprKind::Atom(a)) = expr.first() {
            stop!(BadSyntax => format!("macro expansion unable to match case for index: {expr}"); a.syn.span);
        } else {
            unreachable!()
        }
    }

    pub fn expand(
        &self,
        expr: List,
        span: Span,
        in_scope: &ScopeSet<InternedString, FxBuildHasher>,
        globals: GlobalMap,
    ) -> Result<ExprKind> {
        let case_to_expand = self.match_case(&expr, in_scope, globals)?;
        let expanded_expr = case_to_expand.expand(expr, span, in_scope, globals)?;

        Ok(expanded_expr)
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct PatternList {
    args: Vec<MacroPattern>,
    improper: bool,
}

impl PatternList {
    pub fn new(args: Vec<MacroPattern>) -> Self {
        Self {
            args,
            improper: false,
        }
    }

    pub fn with_improper(mut self, improper: bool) -> Self {
        self.improper = improper;
        self
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct PatternListRef<'a> {
    args: &'a [MacroPattern],
    improper: bool,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct MacroCase {
    // TODO: Label this as an improper list
    // args: Vec<MacroPattern>,
    args: PatternList,

    pub(crate) body: ExprKind,
}

struct PatternContext<'a> {
    name: InternedString,
    special_forms: &'a [InternedString],
    bindings: FxHashMap<InternedString, u8>,
    depth: u8,
}

impl MacroCase {
    #[cfg(test)]
    pub fn new(args: PatternList, body: ExprKind) -> Self {
        MacroCase { args, body }
    }

    pub fn all_bindings(&self) -> Vec<String> {
        fn walk_bindings(pattern: &MacroPattern, idents: &mut Vec<String>) {
            match pattern {
                MacroPattern::Rest(r) => walk_bindings(r, idents),
                MacroPattern::Single(s) | MacroPattern::Quote(s)
                    if *s != InternedString::from_static("_") =>
                {
                    idents.push(s.resolve().to_owned());
                }
                MacroPattern::Nested(n, _) => {
                    for p in &n.args {
                        walk_bindings(p, idents)
                    }
                }
                MacroPattern::Many(pat) => walk_bindings(pat, idents),
                _ => {}
            }
        }

        let mut idents = Vec::new();

        for pattern in &self.args.args {
            walk_bindings(pattern, &mut idents)
        }

        idents
    }

    fn parse_from_pattern_pair(
        pattern_pair: PatternPair,
        name: &InternedString,
        special_forms: &[InternedString],
    ) -> Result<Self> {
        let PatternPair { pattern, mut body } = pattern_pair;

        let mut ctx = PatternContext {
            name: *name,
            special_forms,
            bindings: Default::default(),
            depth: 0,
        };

        let ((mut args, macro_keyword), improper) = if let ExprKind::List(l) = pattern {
            let improper = l.improper;

            (MacroPattern::parse_from_list(l, &mut ctx, true)?, improper)
        } else {
            stop!(Generic => "unable to parse macro");
        };

        if let Some(macro_keyword) = macro_keyword {
            ctx.bindings.remove(&macro_keyword);
        };

        let args_str: SmallVec<[_; 8]> = ctx.bindings.keys().copied().collect();

        // TODO: @Matt - if the
        let template_parser = MacroTemplate::new(ctx.bindings);
        template_parser.verify(&body)?;

        RenameIdentifiersVisitor::new(&args_str, special_forms).rename_identifiers(&mut body);

        args.iter_mut().for_each(|x| x.mangle(special_forms));

        // println!("Found args: {:?}", args);
        // println!("Renamed body: {:?}", &body);

        Ok(MacroCase {
            args: PatternList::new(args).with_improper(improper),
            body,
        })
    }

    fn recursive_match(
        &self,
        list: &List,
        in_scope: &ScopeSet<InternedString, FxBuildHasher>,
        globals: GlobalMap,
    ) -> bool {
        match_list_pattern(
            &self.args.args[1..],
            &list.args[1..],
            list.improper,
            in_scope,
            globals,
        )
    }

    pub(crate) fn gather_bindings(
        &self,
        expr: List,
    ) -> Result<(
        FxHashMap<InternedString, ExprKind>,
        FxHashMap<InternedString, BindingKind>,
    )> {
        let mut bindings = Default::default();
        let mut binding_kind = Default::default();

        collect_bindings(
            &self.args.args[1..],
            &expr[1..],
            &mut bindings,
            &mut binding_kind,
            expr.improper,
        )?;

        Ok((bindings, binding_kind))
    }

    fn expand(
        &self,
        expr: List,
        span: Span,
        in_scope: &ScopeSet<InternedString, FxBuildHasher>,
        globals: GlobalMap,
    ) -> Result<ExprKind> {
        thread_local! {
            static BINDINGS: RefCell<FxHashMap<InternedString, ExprKind>> = RefCell::new(FxHashMap::default());
            static BINDINGS_KIND: RefCell<FxHashMap<InternedString, BindingKind>> = RefCell::new(FxHashMap::default());
            static FALLBACK_BINDINGS: RefCell<FxHashMap<InternedString, ExprKind>> = RefCell::new(FxHashMap::default());
        }

        BINDINGS.with(|bindings| {
            BINDINGS_KIND.with(|binding_kind| {
                FALLBACK_BINDINGS.with(|fallback_bindings| {
                    let mut bindings = bindings.borrow_mut();
                    let mut binding_kind = binding_kind.borrow_mut();
                    let mut fallback_bindings = fallback_bindings.borrow_mut();

                    bindings.clear();
                    binding_kind.clear();
                    fallback_bindings.clear();

                    collect_bindings(
                        &self.args.args[1..],
                        &expr[1..],
                        &mut bindings,
                        &mut binding_kind,
                        expr.improper,
                    )?;

                    // Mark each identifier in the bindings
                    // as potentially being introduced by a macro:

                    for (_, v) in bindings.iter_mut() {
                        struct IntroducedByMacro;
                        impl VisitorMutRefUnit for IntroducedByMacro {
                            fn visit_atom(&mut self, a: &mut Atom) {
                                a.syn.introduced_via_macro = true;
                            }
                        }

                        IntroducedByMacro.visit(v);
                    }

                    let mut body = self.body.clone();

                    // println!("Expanding: {}", body);
                    // println!("Bindings");
                    // for p in bindings.iter() {
                    //     println!("{} -> {}", p.0, p.1);
                    // }

                    replace_identifiers(
                        &mut body,
                        &mut bindings,
                        &mut binding_kind,
                        &mut fallback_bindings,
                        in_scope,
                        globals,
                        span,
                    )?;

                    Ok(body)
                })
            })
        })
    }
}

#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub enum MacroPattern {
    Rest(Box<MacroPattern>),
    Single(InternedString),
    // TODO: We need to encode
    // the global offset, or at least something
    // related to the scope identifier for this
    // value such that we can do more macro things.
    //
    // This is so that we can see if something has
    // already been bound.
    Syntax(InternedString, bool),
    Many(Box<MacroPattern>),
    Nested(PatternList, bool),
    CharacterLiteral(char),
    BytesLiteral(Vec<u8>),
    NumberLiteral(InternedNumber),
    StringLiteral(InternedString),
    BooleanLiteral(bool),
    QuotedExpr(Box<Quote>),
    Quote(InternedString),
    Keyword(InternedString),
}

impl core::fmt::Debug for MacroPattern {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            MacroPattern::Single(s) => f.debug_tuple("Single").field(&s.resolve()).finish(),
            MacroPattern::Syntax(s, _) => f.debug_tuple("Syntax").field(&s.resolve()).finish(),
            MacroPattern::Many(m) => f.debug_tuple("Many").field(&m).finish(),
            MacroPattern::Nested(n, v) => f.debug_tuple("Nested").field(n).field(v).finish(),
            MacroPattern::CharacterLiteral(c) => {
                f.debug_tuple("CharacterLiteral").field(c).finish()
            }
            MacroPattern::NumberLiteral(n) => f.debug_tuple("NumberLiteral").field(n).finish(),
            MacroPattern::StringLiteral(s) => f.debug_tuple("StringLiteral").field(s).finish(),
            MacroPattern::BooleanLiteral(b) => f.debug_tuple("BooleanLiteral").field(b).finish(),
            MacroPattern::QuotedExpr(s) => f.debug_tuple("QuotedExpr").field(s).finish(),
            MacroPattern::Quote(i) => f.debug_tuple("Quote").field(&i.resolve()).finish(),
            MacroPattern::Keyword(k) => f.debug_tuple("Keyword").field(k).finish(),
            MacroPattern::Rest(r) => f.debug_tuple("Rest").field(r).finish(),
            MacroPattern::BytesLiteral(v) => f.debug_tuple("BytesLiteral").field(v).finish(),
        }
    }
}

impl MacroPattern {
    pub fn is_many(&self) -> bool {
        matches!(self, MacroPattern::Many(..))
    }

    fn mangle(&mut self, special_forms: &[InternedString]) {
        use MacroPattern::*;
        match self {
            Single(s) => {
                let special = special_forms.contains(s) || *s == InternedString::from_static("_");

                if !special {
                    *self = Single(("##".to_string() + s.resolve()).into())
                }
            }
            Nested(v, _) => {
                v.args.iter_mut().for_each(|x| x.mangle(special_forms));
            }
            Many(pat) => pat.mangle(special_forms),
            Rest(v) => {
                v.mangle(special_forms);
            }
            _ => {}
        }
    }

    fn parse_from_list(
        list: List,
        ctx: &mut PatternContext,
        top_level: bool,
    ) -> Result<(Vec<MacroPattern>, Option<InternedString>)> {
        let mut pattern_vec: Vec<MacroPattern> = Vec::with_capacity(4);
        let len = list.args.len();
        let mut exprs_iter = list.args.into_iter().enumerate().peekable();
        let mut ellipsis = false;

        let wildcard = InternedString::from_static("_");

        macro_rules! add_binding {
            ($key:expr, $span:expr, $many:expr) => {
                if $key != wildcard && ctx.bindings.insert($key, ctx.depth + if $many { 1 } else { 0 }).is_some() {
                    let var = $key.resolve();
                    stop!(BadSyntax => "repeated pattern variable {}", var ; $span);
                }
            };
        }

        macro_rules! check_ellipsis {
            ($last:expr, $span:expr) => {
                if ellipsis {
                    stop!(BadSyntax => "pattern with more than one ellipsis"; $span);
                } else if list.improper && $last {
                    stop!(BadSyntax => "ellipsis cannot appear as list tail"; $span);
                }

                ellipsis = true;
            }
        }

        macro_rules! parse_nested {
            ($list:expr, $index:expr, $vec:expr) => {{
                let ellipsis_span = exprs_iter
                    .peek()
                    .and_then(|(_, expr)| expr.atom_syntax_object())
                    .filter(|syn| syn.ty == TokenType::Ellipses)
                    .map(|syn| syn.span);

                if ellipsis_span.is_some() {
                    ctx.depth += 1;
                }

                let improper = $list.improper;
                let (patterns, _) = Self::parse_from_list($list, ctx, false)?;

                if let Some(span) = ellipsis_span {
                    let _ = exprs_iter.next();

                    ctx.depth -= 1;

                    check_ellipsis!($index + 2 == len, span);
                }

                let nested =
                    MacroPattern::Nested(PatternList::new(patterns).with_improper(improper), $vec);

                if ellipsis_span.is_some() {
                    MacroPattern::Many(nested.into())
                } else {
                    nested
                }
            }};
        }

        let mut macro_keyword = None;

        while let Some((i, expr)) = exprs_iter.next() {
            match expr {
                ExprKind::Atom(Atom {
                    syn: SyntaxObject { ty: s, span, .. },
                }) => match s {
                    TokenType::Identifier(t) => {
                        if t == ctx.name || ctx.special_forms.contains(&t) {
                            pattern_vec.push(MacroPattern::Syntax(t, false))
                        } else {
                            let peek = exprs_iter.peek().map(|(_, expr)| expr);

                            let ellipsis_span = match peek {
                                Some(ExprKind::Atom(Atom {
                                    syn:
                                        SyntaxObject {
                                            ty: TokenType::Ellipses,
                                            span,
                                            ..
                                        },
                                })) => Some(*span),

                                // HACK: Somewhere, the ellipses are getting converted back into the raw
                                // form when doing syntax-case expansion. This is duplicated code of the
                                // above.
                                Some(ExprKind::Atom(Atom {
                                    syn:
                                        SyntaxObject {
                                            ty: TokenType::Identifier(ty),
                                            span,
                                            ..
                                        },
                                })) if *ty == *ELLIPSES_SYMBOL => Some(*span),

                                _ => None,
                            };

                            if let Some(span) = ellipsis_span {
                                check_ellipsis!(i + 2 == len, span);

                                exprs_iter.next();

                                if i == 0 && top_level {
                                    stop!(BadSyntax => "macro name cannot be followed by ellipsis"; span);
                                }

                                add_binding!(t, span, true);
                                pattern_vec.push(MacroPattern::Many(MacroPattern::Single(t).into()))
                            } else {
                                if i == 0 && top_level {
                                    macro_keyword = Some(t);
                                }

                                add_binding!(t, span, false);
                                pattern_vec.push(MacroPattern::Single(t));
                            }
                        }
                    }
                    // TODO: Add all of the tokens here
                    TokenType::Begin => {
                        pattern_vec.push(MacroPattern::Syntax(*BEGIN, false));
                    }
                    TokenType::Define => {
                        pattern_vec.push(MacroPattern::Syntax(*DEFINE, false));
                    }
                    TokenType::Lambda => {
                        pattern_vec.push(MacroPattern::Syntax(*LAMBDA, false));
                    }
                    TokenType::If => {
                        pattern_vec.push(MacroPattern::Syntax(*IF, false));
                    }
                    TokenType::BooleanLiteral(b) => {
                        pattern_vec.push(MacroPattern::BooleanLiteral(b));
                    }
                    TokenType::Number(n) => {
                        pattern_vec.push(MacroPattern::NumberLiteral(n));
                    }
                    TokenType::CharacterLiteral(c) => {
                        pattern_vec.push(MacroPattern::CharacterLiteral(c));
                    }
                    TokenType::StringLiteral(s) => {
                        pattern_vec.push(MacroPattern::StringLiteral(s));
                    }
                    TokenType::Keyword(k) => {
                        pattern_vec.push(MacroPattern::Keyword(k));
                    }
                    // TODO: Crunch the quoted values here, and pull them in so that this
                    // holds a body of possible quoted values
                    TokenType::Quote => {
                        // pattern_vec.push(MacroPattern::Quote);
                        // match peek_token_iter.peek() {
                        //     Some(ExprKind::Atom(Atom {
                        //         syn:
                        //             SyntaxObject {
                        //                 ty: TokenType::Ellipses,
                        //                 ..
                        //             },
                        //     })) => {
                        //         peek_token_iter.next();
                        //         pattern_vec.push(MacroPattern::Many(t.clone()));
                        //     }
                        //     _ => {
                        //         pattern_vec.push(MacroPattern::Single(t.clone()));
                        //     }
                        // }

                        let next = exprs_iter.next().map(|(_, expr)| expr);

                        match next {
                            Some(ExprKind::Atom(Atom {
                                syn:
                                    SyntaxObject {
                                        ty: TokenType::Identifier(s),
                                        ..
                                    },
                            })) => {
                                pattern_vec.push(MacroPattern::Quote(s));
                            }
                            _ => {
                                stop!(TypeMismatch => "syntax-rules with quote don't
                                    yet support arbitrary constants")
                                // pattern_vec.push(MacroPattern::Single(t.clone()));
                            }
                        }
                    }
                    TokenType::Ellipses => {
                        check_ellipsis!(i + 1 == len, span);

                        let last = pattern_vec.pop();

                        match last {
                            Some(MacroPattern::Nested(..)) => unreachable!(),

                            Some(
                                pat @ (MacroPattern::BytesLiteral(..)
                                | MacroPattern::CharacterLiteral(..)
                                | MacroPattern::StringLiteral(..)
                                | MacroPattern::NumberLiteral(..)
                                | MacroPattern::BooleanLiteral(..)),
                            ) => {
                                pattern_vec.push(MacroPattern::Many(pat.into()));
                            }

                            Some(other) => {
                                stop!(BadSyntax => format!("cannot bind pattern to ellipsis: {:?}", other); span)
                            }
                            None => {
                                stop!(BadSyntax => "cannot bind pattern to ellipsis"; span)
                            }
                        }
                    }
                    _ => {
                        stop!(BadSyntax => "syntax-rules requires identifiers in the pattern"; span);
                    }
                },
                ExprKind::List(l) => {
                    let nested = parse_nested![l, i, false];

                    pattern_vec.push(nested)
                }
                ExprKind::Quote(q) => pattern_vec.push(MacroPattern::QuotedExpr(q)),
                ExprKind::Vector(Vector {
                    args, bytes: false, ..
                }) => {
                    let list = List::new(args);

                    let nested = parse_nested![list, i, true];

                    pattern_vec.push(nested)
                }
                ExprKind::Vector(v @ Vector { bytes: true, .. }) => {
                    let bytes = v.as_bytes().collect();

                    pattern_vec.push(MacroPattern::BytesLiteral(bytes));
                }
                _ => {
                    // TODO: Add pattern matching on other kinds of forms here - probably just a special IR
                    // for the pattern to match against here
                    stop!(BadSyntax => format!("illegal special form found in macro pattern: {}", expr) ; expr.span());
                }
            }
        }

        if list.improper {
            let rest = pattern_vec.pop().map(|pat| MacroPattern::Rest(pat.into()));
            pattern_vec.extend(rest);
        }

        Ok((pattern_vec, macro_keyword))
    }

    fn variables(&self) -> FxHashSet<InternedString> {
        let mut vars = Default::default();

        fn accumulate_vars(pattern: &MacroPattern, acc: &mut FxHashSet<InternedString>) {
            match pattern {
                MacroPattern::Many(pat) | MacroPattern::Rest(pat) => {
                    accumulate_vars(pat, acc);
                }
                MacroPattern::Single(ident) => {
                    acc.insert(*ident);
                }
                MacroPattern::Nested(pats, _) => {
                    for pat in &pats.args {
                        accumulate_vars(pat, acc);
                    }
                }
                _ => {}
            }
        }

        accumulate_vars(self, &mut vars);

        vars
    }
}

fn match_list_pattern(
    patterns: &[MacroPattern],
    list: &[ExprKind],
    improper: bool,
    in_scope: &ScopeSet<InternedString, FxBuildHasher>,
    globals: GlobalMap,
) -> bool {
    let (proper_patterns, rest_pattern) = match patterns.split_last() {
        Some((MacroPattern::Rest(pat), rest)) => (rest, Some(&**pat)),
        _ => (patterns, None),
    };

    let proper_list = if improper {
        &list[..(list.len() - 1)]
    } else {
        list
    };

    let has_ellipsis = patterns.iter().any(|pat| pat.is_many());
    let multi_arity = has_ellipsis || rest_pattern.is_some();

    let len_matches = if multi_arity {
        proper_list.len() + 1 >= proper_patterns.len()
    } else {
        proper_list.len() == proper_patterns.len()
    };

    if !len_matches {
        return false;
    }

    let unmatched_tail = if has_ellipsis {
        &list[proper_list.len()..]
    } else {
        list.get(proper_patterns.len()..).unwrap_or(&[])
    };

    let tail_to_match = match rest_pattern {
        None if unmatched_tail.is_empty() => None,
        None => return false,
        Some(pat) => Some((pat, unmatched_tail)),
    };

    let mut exprs_iter = proper_list.iter().enumerate();

    let expected_many_captures = proper_list.len() + 1 - proper_patterns.len();

    for pat in proper_patterns {
        match &pat {
            MacroPattern::Many(subpat) if has_ellipsis => {
                for _ in 0..expected_many_captures {
                    let Some((_, item)) = exprs_iter.next() else {
                        return false;
                    };

                    if !match_single_pattern(subpat, item, in_scope, globals) {
                        return false;
                    }
                }
                continue;
            }
            _ => {}
        }

        let Some((_, expr)) = exprs_iter.next() else {
            return false;
        };

        if !match_single_pattern(pat, expr, in_scope, globals) {
            return false;
        }
    }

    if let Some((pat, exprs)) = tail_to_match {
        let tail_matches = if improper && exprs.len() == 1 {
            match_single_pattern(pat, &exprs[0], in_scope, globals)
        } else {
            match_rest_pattern(pat, exprs, improper, in_scope, globals)
        };

        if !tail_matches {
            return false;
        }
    }

    true
}

fn match_rest_pattern(
    pattern: &MacroPattern,
    exprs: &[ExprKind],
    improper: bool,
    in_scope: &ScopeSet<InternedString, FxBuildHasher>,
    globals: GlobalMap,
) -> bool {
    match pattern {
        MacroPattern::Single(_) => true,
        MacroPattern::Nested(patterns, false) => {
            match_list_pattern(&patterns.args, exprs, improper, in_scope, globals)
        }
        _ => false,
    }
}

fn match_single_pattern(
    pattern: &MacroPattern,
    expr: &ExprKind,
    in_scope: &ScopeSet<InternedString, FxBuildHasher>,
    globals: GlobalMap,
) -> bool {
    match pattern {
        MacroPattern::Many(_) => unreachable!(),
        MacroPattern::Rest(_) => unreachable!(),
        MacroPattern::Single(_) => true,
        MacroPattern::Syntax(v, _) => match expr {
            ExprKind::Atom(Atom {
                syn:
                    SyntaxObject {
                        ty: TokenType::Identifier(s),
                        ..
                    },
            }) if s == v && !in_scope.contains(s) && !globals.contains(s) => true,
            ExprKind::Atom(Atom {
                syn:
                    SyntaxObject {
                        ty: TokenType::Define,
                        ..
                    },
            }) if *v == *DEFINE => true,
            ExprKind::Atom(Atom {
                syn:
                    SyntaxObject {
                        ty: TokenType::Lambda,
                        ..
                    },
            }) if *v == *LAMBDA => true,
            ExprKind::Atom(Atom {
                syn:
                    SyntaxObject {
                        ty: TokenType::Begin,
                        ..
                    },
            }) if *v == *BEGIN => true,
            ExprKind::Atom(Atom {
                syn: SyntaxObject {
                    ty: TokenType::If, ..
                },
            }) if *v == *IF => true,
            ExprKind::Atom(Atom {
                syn:
                    SyntaxObject {
                        ty: TokenType::Ellipses,
                        ..
                    },
            }) => true,
            _ => false,
        },
        MacroPattern::Keyword(k) => match expr {
            ExprKind::Atom(Atom {
                syn:
                    SyntaxObject {
                        ty: TokenType::Keyword(s),
                        ..
                    },
            }) if s == k => true,
            _ => false,
        },
        MacroPattern::BooleanLiteral(b) => match expr {
            ExprKind::Atom(Atom {
                syn:
                    SyntaxObject {
                        ty: TokenType::BooleanLiteral(s),
                        ..
                    },
            }) if s == b => true,
            _ => false,
        },
        MacroPattern::NumberLiteral(n) => match expr {
            ExprKind::Atom(Atom {
                syn:
                    SyntaxObject {
                        ty: TokenType::Number(t),
                        ..
                    },
            }) => {
                let Ok(n) = n.resolve().into_steelval() else {
                    return false;
                };

                let Ok(t) = t.resolve().into_steelval() else {
                    return false;
                };

                n == t
            }
            _ => false,
        },
        MacroPattern::CharacterLiteral(c) => match expr {
            ExprKind::Atom(Atom {
                syn:
                    SyntaxObject {
                        ty: TokenType::CharacterLiteral(s),
                        ..
                    },
            }) if s == c => true,
            _ => false,
        },
        MacroPattern::StringLiteral(s) => match expr {
            ExprKind::Atom(Atom {
                syn:
                    SyntaxObject {
                        ty: TokenType::StringLiteral(b),
                        ..
                    },
            }) if s == b => true,
            _ => false,
        },
        MacroPattern::BytesLiteral(v) => match expr {
            ExprKind::Vector(Vector {
                args, bytes: true, ..
            }) if args.len() == v.len() => {
                let byte_atoms = args.iter().map(|expr| {
                    if let ExprKind::Atom(atom) = expr {
                        atom.byte()
                    } else {
                        None
                    }
                });

                byte_atoms
                    .zip(v.iter().copied())
                    .all(|(atom, byte)| atom == Some(byte))
            }
            _ => false,
        },
        MacroPattern::QuotedExpr(q) => {
            // println!("MATCHING QUOTED EXPR: {}", q);
            match expr {
                ExprKind::Quote(boxed_q) if q == boxed_q => true,
                _ => false,
            }
        }
        MacroPattern::Quote(_q) => {
            // println!("MATCHING QUOTE {} with val: {}", q, val);
            match expr {
                ExprKind::Quote(_) => true,

                // ExprKind::Atom(Atom {
                //     syn:
                //         SyntaxObject {
                //             ty: TokenType::Quote | TokenType::QuoteTick,
                //             ..
                //         },
                // }) => {
                //     println!("MATCHING ON QUOTE");
                //     true;
                // }

                // ExprKind::Atom(Atom {
                //     syn:
                //         SyntaxObject {
                //             ty: TokenType::Identifier(s),
                //             ..
                //         },
                // }) if s.resolve() == "QUOTE" => {
                //     println!("MATCHING ON QUOTE");
                //     true;
                // }
                // ExprKind::Quote()
                _ => false,
            }
        }
        MacroPattern::Nested(patterns, is_vec) => {
            match expr {
                // Make the recursive call on the next layer
                ExprKind::List(l) => {
                    !is_vec && match_list_pattern(&patterns.args, l, l.improper, in_scope, globals)
                }
                ExprKind::Vector(v) => {
                    *is_vec
                        && !v.bytes
                        && match_list_pattern(&patterns.args, &v.args, false, in_scope, globals)
                }
                // TODO: Come back here
                ExprKind::Quote(_) => {
                    !is_vec && matches!(patterns.args.as_slice(), &[MacroPattern::Quote(_)])
                }
                _ => {
                    if let Some(pat) = non_list_match(&patterns.args) {
                        match_single_pattern(pat, expr, in_scope, globals)
                    } else {
                        false
                    }
                }
            }
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BindingKind {
    Many,
    Single,
}

fn collect_bindings(
    patterns: &[MacroPattern],
    list: &[ExprKind],
    bindings: &mut FxHashMap<InternedString, ExprKind>,
    binding_kind: &mut FxHashMap<InternedString, BindingKind>,
    improper: bool,
) -> Result<()> {
    let mut expr_iter = list.iter().enumerate();

    let expected_many_captures = list.len() + 1 - patterns.len();

    for pattern in patterns {
        match pattern {
            // bind the expression to a single variable
            MacroPattern::Single(s) => {
                if let Some((_, e)) = expr_iter.next() {
                    bindings.insert(*s, e.clone());
                } else {
                    stop!(ArityMismatch => "macro invocation expected a value, found none");
                }
            }
            // actually check if the syntax matches
            MacroPattern::Syntax(s, _) => {
                let error_func = throw!(BadSyntax => format!("macro expand expected keyword {s} - within {:?}", list));

                let (_, e) = expr_iter.next().ok_or_else(error_func)?;

                if let ExprKind::Atom(Atom {
                    syn:
                        SyntaxObject {
                            ty: TokenType::Identifier(syn),
                            ..
                        },
                }) = e
                {
                    if s != syn {
                        stop!(BadSyntax => "macro expansion expected keyword, found: {}", syn)
                    }
                }
            }
            MacroPattern::Many(pat) if expected_many_captures == 0 => {
                for ident in pat.variables() {
                    bindings.insert(ident, List::new(thin_vec![]).into());
                    binding_kind.insert(ident, BindingKind::Many);
                }
            }
            MacroPattern::Many(pat) => {
                let mut nested_bindings = FxHashMap::default();
                let mut list_bindings = FxHashMap::default();

                for _ in 0..expected_many_captures {
                    if let Some((_, expr)) = expr_iter.next() {
                        collect_bindings(
                            core::slice::from_ref(pat),
                            core::slice::from_ref(expr),
                            &mut nested_bindings,
                            binding_kind,
                            false,
                        )?;
                    }

                    for (ident, captured) in nested_bindings.drain() {
                        list_bindings
                            .entry(ident)
                            .or_insert(thin_vec![])
                            .push(captured);
                    }
                }

                for (ident, captured_list) in list_bindings {
                    bindings.insert(ident, List::new(captured_list).into());
                    binding_kind.insert(ident, BindingKind::Many);
                }
            }
            MacroPattern::Nested(children, is_vec) => {
                let (_, child) = expr_iter
                    .next()
                    .ok_or_else(throw!(ArityMismatch => "Macro expected a pattern"))?;

                match child {
                    ExprKind::List(l) => {
                        if !is_vec {
                            collect_bindings(
                                &children.args,
                                l,
                                bindings,
                                binding_kind,
                                l.improper,
                            )?;
                        }
                    }
                    ExprKind::Vector(v) => {
                        if *is_vec {
                            collect_bindings(
                                &children.args,
                                &v.args,
                                bindings,
                                binding_kind,
                                false,
                            )?;
                        }
                    }
                    ExprKind::Quote(q) => {
                        if let &[MacroPattern::Quote(x)] = children.args.as_slice() {
                            bindings.insert(x, q.expr.clone());
                        } else {
                            stop!(BadSyntax => "macro expected a list of values, not including keywords, found: {}", child)
                        }
                    }
                    _ => {
                        if let Some(pat) = non_list_match(&children.args) {
                            collect_bindings(
                                core::slice::from_ref(pat),
                                core::slice::from_ref(child),
                                bindings,
                                binding_kind,
                                false,
                            )?;
                        } else {
                            stop!(BadSyntax => "macro expected a list of values, not including keywords, found: {}", child)
                        }
                    }
                }
            }
            MacroPattern::Quote(s) => {
                if let Some((_, e)) = expr_iter.next() {
                    if let ExprKind::Quote(inner) = e {
                        bindings.insert(*s, inner.expr.clone());
                    } else {
                        stop!(Generic => "something went wrong with macro expansion")
                    }
                } else {
                    stop!(ArityMismatch => "macro invocation expected a value, found none");
                }
            }

            MacroPattern::Rest(pat) => {
                let list = match expr_iter.next() {
                    Some((i, expr)) if improper && i + 1 == list.len() => [expr.clone()],
                    Some((i, _)) => {
                        let list = List::new_maybe_improper(list[i..].into(), improper);

                        [ExprKind::List(list)]
                    }
                    None => [ExprKind::List(List::new(thin_vec![]))],
                };

                collect_bindings(&[(**pat).clone()], &list, bindings, binding_kind, false)?;
            }

            // Matching on literals
            _ => {
                expr_iter.next();
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod match_list_pattern_tests {
    use steel_parser::tokens::{NumberLiteral, RealLiteral};

    use super::*;

    fn atom(t: TokenType<InternedString>) -> ExprKind {
        ExprKind::Atom(Atom::new(SyntaxObject::default(t)))
    }

    fn atom_identifier(s: &str) -> ExprKind {
        atom(TokenType::Identifier(s.into()))
    }

    fn atom_int(n: isize) -> ExprKind {
        atom(IntLiteral::Small(n).into())
    }

    #[test]
    fn test_match_basic() {
        let pattern_args = vec![
            MacroPattern::Syntax("and".into(), false),
            MacroPattern::Single("a".into()),
            MacroPattern::Single("b".into()),
        ];
        let list_expr = List::new(thin_vec![
            atom_identifier("and"),
            ExprKind::List(List::new(thin_vec![
                atom_identifier("+"),
                atom_identifier("x"),
                atom_identifier("y"),
            ])),
            ExprKind::List(List::new(thin_vec![
                atom_identifier("+"),
                atom_identifier("x"),
                atom_identifier("y"),
            ])),
        ]);

        assert!(match_list_pattern(
            &pattern_args,
            &list_expr,
            false,
            &ScopeSet::default(),
            GlobalMap::Set(&Default::default())
        ));
    }

    #[test]
    fn test_match_many() {
        let pattern_args = vec![
            MacroPattern::Syntax("and".into(), false),
            MacroPattern::Single("a".into()),
            MacroPattern::Many(MacroPattern::Single("b".into()).into()),
        ];
        let list_expr = List::new(thin_vec![
            atom_identifier("and"),
            ExprKind::List(List::new(thin_vec![
                atom_identifier("+"),
                atom_identifier("x"),
                atom_identifier("y"),
            ])),
            ExprKind::List(List::new(thin_vec![
                atom_identifier("+"),
                atom_identifier("x"),
                atom_identifier("y"),
            ])),
        ]);
        assert!(match_list_pattern(
            &pattern_args,
            &list_expr,
            false,
            &ScopeSet::default(),
            GlobalMap::Set(&Default::default())
        ));
    }

    #[test]
    fn test_match_many_multiple() {
        let pattern_args = vec![
            MacroPattern::Syntax("and".into(), false),
            MacroPattern::Single("a".into()),
            MacroPattern::Many(MacroPattern::Single("b".into()).into()),
        ];
        let list_expr = List::new(thin_vec![
            atom_identifier("and"),
            ExprKind::List(List::new(thin_vec![
                atom_identifier("+"),
                atom_identifier("x"),
                atom_identifier("y"),
            ])),
            atom_identifier("+"),
            atom_identifier("x"),
            atom_identifier("y"),
        ]);
        assert!(match_list_pattern(
            &pattern_args,
            &list_expr,
            false,
            &ScopeSet::default(),
            GlobalMap::Set(&Default::default())
        ));
    }

    #[test]
    fn test_nested() {
        let pattern_args = vec![
            MacroPattern::Syntax("->>".into(), false),
            MacroPattern::Single("a".into()),
            MacroPattern::Nested(
                PatternList::new(vec![
                    MacroPattern::Single("b".into()),
                    MacroPattern::Many(MacroPattern::Single("c".into()).into()),
                ]),
                false,
            ),
        ];

        let list_expr = List::new(thin_vec![
            atom_identifier("->>"),
            atom_int(1),
            ExprKind::List(List::new(thin_vec![
                atom_identifier("apple"),
                atom_identifier("sauce"),
                atom_identifier("is-good"),
            ])),
        ]);
        assert!(match_list_pattern(
            &pattern_args,
            &list_expr,
            false,
            &ScopeSet::default(),
            GlobalMap::Set(&Default::default())
        ));
    }

    #[test]
    fn test_no_match_simple() {
        let pattern_args = vec![
            MacroPattern::Syntax("->>".into(), false),
            MacroPattern::Single("a".into()),
            MacroPattern::Single("bad".into()),
        ];

        let list_expr = List::new(thin_vec![
            atom_identifier("->>"),
            atom_int(1),
            atom_int(2),
            atom_int(3),
        ]);

        assert!(!match_list_pattern(
            &pattern_args,
            &list_expr,
            false,
            &ScopeSet::default(),
            GlobalMap::Set(&Default::default())
        ));
    }

    #[test]
    fn test_nested_no_match() {
        let pattern_args = vec![
            MacroPattern::Syntax("->>".into(), false),
            MacroPattern::Single("a".into()),
            MacroPattern::Single("bad".into()),
            MacroPattern::Nested(
                PatternList::new(vec![
                    MacroPattern::Single("b".into()),
                    MacroPattern::Many(MacroPattern::Single("c".into()).into()),
                ]),
                false,
            ),
        ];

        let list_expr = List::new(thin_vec![
            atom_identifier("->>"),
            atom_int(1),
            ExprKind::List(List::new(thin_vec![
                atom_identifier("apple"),
                atom_identifier("sauce"),
                atom_identifier("is-good"),
            ])),
        ]);

        assert!(!match_list_pattern(
            &pattern_args,
            &list_expr,
            false,
            &ScopeSet::default(),
            GlobalMap::Set(&Default::default())
        ));
    }

    #[test]
    fn test_number_literals() {
        let pattern_args = vec![
            MacroPattern::Syntax("->>".into(), false),
            MacroPattern::NumberLiteral(
                NumberLiteral::Real(RealLiteral::Rational(
                    IntLiteral::Small(3),
                    IntLiteral::Small(4),
                ))
                .into(),
            ),
        ];

        let list_expr = List::new(thin_vec![
            atom_identifier("->>"),
            atom(TokenType::Number(
                NumberLiteral::Real(RealLiteral::Rational(
                    IntLiteral::Small(18),
                    IntLiteral::Small(24),
                ))
                .into(),
            )),
        ]);

        assert!(match_list_pattern(
            &pattern_args,
            &list_expr,
            false,
            &ScopeSet::default(),
            GlobalMap::Set(&Default::default())
        ));
    }
}

#[cfg(test)]
mod collect_bindings_tests {

    use super::*;

    fn atom_identifier(s: &str) -> ExprKind {
        ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Identifier(
            s.into(),
        ))))
    }

    fn atom_int(n: isize) -> ExprKind {
        ExprKind::Atom(Atom::new(SyntaxObject::default(
            IntLiteral::Small(n).into(),
        )))
    }

    #[test]
    fn test_collect_basic() {
        let mut bindings = FxHashMap::default();
        let mut binding_kind = FxHashMap::default();
        let pattern_args = vec![
            MacroPattern::Syntax("and".into(), false),
            MacroPattern::Single("a".into()),
            MacroPattern::Single("b".into()),
        ];
        let list_expr = List::new(thin_vec![
            atom_identifier("and"),
            ExprKind::List(List::new(thin_vec![
                atom_identifier("+"),
                atom_identifier("x"),
                atom_identifier("y"),
            ])),
            ExprKind::List(List::new(thin_vec![
                atom_identifier("+"),
                atom_identifier("x"),
                atom_identifier("y"),
            ])),
        ]);

        collect_bindings(
            &pattern_args,
            &list_expr,
            &mut bindings,
            &mut binding_kind,
            false,
        )
        .unwrap();

        let mut post_bindings = FxHashMap::default();
        post_bindings.insert(
            "a".into(),
            ExprKind::List(List::new(thin_vec![
                atom_identifier("+"),
                atom_identifier("x"),
                atom_identifier("y"),
            ])),
        );

        post_bindings.insert(
            "b".into(),
            ExprKind::List(List::new(thin_vec![
                atom_identifier("+"),
                atom_identifier("x"),
                atom_identifier("y"),
            ])),
        );

        assert_eq!(bindings, post_bindings);
    }

    #[test]
    fn test_collect_many() {
        let mut bindings = FxHashMap::default();
        let mut binding_kind = FxHashMap::default();
        let pattern_args = vec![
            MacroPattern::Syntax("and".into(), false),
            MacroPattern::Single("a".into()),
            MacroPattern::Many(MacroPattern::Single("b".into()).into()),
        ];
        let list_expr = List::new(thin_vec![
            atom_identifier("and"),
            ExprKind::List(List::new(thin_vec![
                atom_identifier("+"),
                atom_identifier("x"),
                atom_identifier("y"),
            ])),
            ExprKind::List(List::new(thin_vec![
                atom_identifier("+"),
                atom_identifier("x"),
                atom_identifier("y"),
            ])),
        ]);

        collect_bindings(
            &pattern_args,
            &list_expr,
            &mut bindings,
            &mut binding_kind,
            false,
        )
        .unwrap();

        let mut post_bindings = FxHashMap::default();
        post_bindings.insert(
            "a".into(),
            ExprKind::List(List::new(thin_vec![
                atom_identifier("+"),
                atom_identifier("x"),
                atom_identifier("y"),
            ])),
        );
        post_bindings.insert(
            "b".into(),
            ExprKind::List(List::new(thin_vec![ExprKind::List(List::new(thin_vec![
                atom_identifier("+"),
                atom_identifier("x"),
                atom_identifier("y"),
            ]))])),
        );

        assert_eq!(bindings, post_bindings);
    }

    #[test]
    fn test_collect_many_multiple_singles() {
        let mut bindings = FxHashMap::default();
        let mut binding_kind = FxHashMap::default();
        let pattern_args = vec![
            MacroPattern::Syntax("and".into(), false),
            MacroPattern::Single("a".into()),
            MacroPattern::Many(MacroPattern::Single("b".into()).into()),
        ];
        let list_expr = List::new(thin_vec![
            atom_identifier("and"),
            ExprKind::List(List::new(thin_vec![
                atom_identifier("+"),
                atom_identifier("x"),
                atom_identifier("y"),
            ])),
            atom_identifier("+"),
            atom_identifier("x"),
            atom_identifier("y"),
        ]);

        collect_bindings(
            &pattern_args,
            &list_expr,
            &mut bindings,
            &mut binding_kind,
            false,
        )
        .unwrap();
        let mut post_bindings = FxHashMap::default();

        post_bindings.insert(
            "a".into(),
            ExprKind::List(List::new(thin_vec![
                atom_identifier("+"),
                atom_identifier("x"),
                atom_identifier("y"),
            ])),
        );

        post_bindings.insert(
            "b".into(),
            ExprKind::List(List::new(thin_vec![
                atom_identifier("+"),
                atom_identifier("x"),
                atom_identifier("y"),
            ])),
        );

        assert_eq!(bindings, post_bindings);
    }

    #[test]
    fn test_nested() {
        let mut bindings = FxHashMap::default();
        let mut binding_kind = FxHashMap::default();
        // (->> a (b c ...))
        let pattern_args = vec![
            MacroPattern::Syntax("->>".into(), false),
            MacroPattern::Single("a".into()),
            MacroPattern::Nested(
                PatternList::new(vec![
                    MacroPattern::Single("b".into()),
                    MacroPattern::Many(MacroPattern::Single("c".into()).into()),
                ]),
                false,
            ),
        ];

        let list_expr = List::new(thin_vec![
            atom_identifier("->>"),
            atom_int(1),
            ExprKind::List(List::new(thin_vec![
                atom_identifier("apple"),
                atom_identifier("sauce"),
                atom_identifier("is-good"),
            ])),
        ]);

        let mut post_bindings = FxHashMap::default();

        collect_bindings(
            &pattern_args,
            &list_expr,
            &mut bindings,
            &mut binding_kind,
            false,
        )
        .unwrap();
        post_bindings.insert("a".into(), atom_int(1));
        post_bindings.insert("b".into(), atom_identifier("apple"));
        post_bindings.insert(
            "c".into(),
            ExprKind::List(List::new(thin_vec![
                atom_identifier("sauce"),
                atom_identifier("is-good"),
            ])),
        );

        assert_eq!(bindings, post_bindings);
    }
}

fn non_list_match(patterns: &[MacroPattern]) -> Option<&MacroPattern> {
    let rest = patterns.last().and_then(|pat| match pat {
        MacroPattern::Rest(pat) => Some(&**pat),
        _ => None,
    });

    rest.filter(|_| patterns.len() == 2 && patterns[0].is_many())
}

#[cfg(test)]
mod macro_case_expand_test {
    use steel_parser::parser::SourceId;

    use super::MacroCase;

    use super::*;

    // macro_rules! map {
    //     ($ ( $key:expr => $value:expr ), *,) => {{
    //         let mut hm: HashMap<String, ExprKind> = HashMap::new();
    //         $ (hm.insert($key.to_string(), $value); ) *
    //         hm
    //     }};
    // }

    fn atom_identifier(s: &str) -> ExprKind {
        ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Identifier(
            s.into(),
        ))))
    }

    fn atom_int(n: isize) -> ExprKind {
        ExprKind::Atom(Atom::new(SyntaxObject::default(
            IntLiteral::Small(n).into(),
        )))
    }

    #[test]
    fn test_basic_expansion() {
        let case = MacroCase {
            args: PatternList::new(vec![
                MacroPattern::Syntax("test".into(), false),
                MacroPattern::Single("a".into()),
                MacroPattern::Single("b".into()),
                MacroPattern::Single("c".into()),
            ]),
            body: List::new(thin_vec![
                atom_identifier("fun-call"),
                atom_identifier("inserted-variable"),
                atom_identifier("a"),
                atom_identifier("b"),
                atom_identifier("c"),
            ])
            .into(),
        };

        let input = List::new(thin_vec![
            atom_identifier("test"),
            atom_int(1),
            atom_identifier("apple"),
            atom_int(2),
        ]);

        let expected: ExprKind = List::new(thin_vec![
            atom_identifier("fun-call"),
            atom_identifier("inserted-variable"),
            atom_int(1),
            atom_identifier("apple"),
            atom_int(2),
        ])
        .into();

        let output = case
            .expand(
                input,
                Span::new(0, 0, SourceId::none()),
                &ScopeSet::default(),
                GlobalMap::Map(&Default::default()),
            )
            .unwrap();

        assert_eq!(output, expected);
    }
}
