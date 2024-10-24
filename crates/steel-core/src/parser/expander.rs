use crate::compiler::program::{BEGIN, DEFINE, IF};
use crate::parser::ast::{Atom, ExprKind, List, Macro, PatternPair, Vector};
use crate::parser::parser::SyntaxObject;
use crate::parser::rename_idents::RenameIdentifiersVisitor;
use crate::parser::replace_idents::replace_identifiers;
use crate::parser::tokens::TokenType;

use crate::parser::span::Span;

use crate::rvals::Result;
use std::cell::RefCell;
use std::{
    collections::HashMap,
    fs::File,
    io::{Read, Write},
    iter::FromIterator,
    path::{Path, PathBuf},
};

use fxhash::{FxHashMap, FxHashSet};
use serde::{Deserialize, Serialize};
use steel_parser::tokens::{IntLiteral, NumberLiteral, RealLiteral};

use super::{ast::Quote, interner::InternedString, parser::Parser};

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
    pub fn expand(&self, exprs: &mut Vec<ExprKind>) -> Result<()> {
        for expr in exprs.iter_mut() {
            crate::parser::expand_visitor::expand(expr, &self.macros)?
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
    cases: Vec<MacroCase>,
    mangled: bool,
    pub(crate) location: Span,
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
        }
    }

    pub fn name(&self) -> &InternedString {
        &self.name
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

    pub fn parse_from_ast_macro(ast_macro: Box<Macro>) -> Result<Self> {
        let name = *ast_macro
            .name
            .atom_identifier_or_else(throw!(BadSyntax => "macros only currently support
                    identifiers as the name"; ast_macro.location.span))?;

        let sp = ast_macro.location.span;

        let special_forms = ast_macro
            .syntax_rules
            .syntax
            .into_iter()
            .map(|x| {
                x.atom_identifier_or_else(
                    throw!(BadSyntax => format!("macros only support identifiers for special syntax, found: {}", x); sp),
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
        })
    }

    // TODO the case matching should be a little bit more informed than this
    // I think it should also not be greedy, and should report if there are ambiguous matchings
    fn match_case(&self, expr: &List) -> Result<&MacroCase> {
        for case in &self.cases {
            if case.recursive_match(expr) {
                return Ok(case);
            }
        }

        if let Some(ExprKind::Atom(a)) = expr.first() {
            stop!(BadSyntax => format!("macro expansion unable to match case: {expr}"); a.syn.span);
        } else {
            unreachable!()
        }
    }

    pub fn expand(&self, expr: List, span: Span) -> Result<ExprKind> {
        // if log::log_enabled!(log::Level::Debug) {
        //     log::debug!("Expanding macro with tokens: {}", expr);
        // }

        // log::debug!("Expanding with span: {:?}", span);

        let case_to_expand = self.match_case(&expr)?;
        let expanded_expr = case_to_expand.expand(expr, span)?;

        // if log::log_enabled!(log::Level::Debug) {
        //     debug!("Macro Expansion: {}", expanded_expr);
        // }

        Ok(expanded_expr)
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct MacroCase {
    args: Vec<MacroPattern>,
    body: ExprKind,
}

impl MacroCase {
    #[cfg(test)]
    pub fn new(args: Vec<MacroPattern>, body: ExprKind) -> Self {
        MacroCase { args, body }
    }

    fn parse_from_pattern_pair(
        pattern_pair: PatternPair,
        name: &InternedString,
        special_forms: &[InternedString],
    ) -> Result<Self> {
        let PatternPair { pattern, mut body } = pattern_pair;

        let mut bindings = FxHashSet::default();

        let (mut args, macro_keyword) = if let ExprKind::List(l) = pattern {
            MacroPattern::parse_from_list(l, name, special_forms, &mut bindings, true)?
        } else {
            stop!(Generic => "unable to parse macro");
        };

        if let Some(macro_keyword) = macro_keyword {
            bindings.remove(&macro_keyword);
        };

        let args_str: Vec<_> = bindings.iter().copied().collect();

        // dbg!(args_str.iter().map(|x| x.resolve()).collect::<Vec<_>>());

        // let syntaxes: Vec<&str> = args
        //     .iter()
        //     .map(|x| x.deconstruct_syntax())
        //     .flatten()
        //     .collect();

        // TODO: @Matt - if the
        RenameIdentifiersVisitor::new(&args_str, special_forms).rename_identifiers(&mut body);

        args.iter_mut().for_each(|x| x.mangle(special_forms));

        // println!("Found args: {:?}", args);
        // println!("Renamed body: {:?}", &body);

        Ok(MacroCase { args, body })
    }

    fn recursive_match(&self, list: &List) -> bool {
        match_list_pattern(&self.args[1..], &list.args[1..], list.improper)
    }

    fn expand(&self, expr: List, span: Span) -> Result<ExprKind> {
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
                        &self.args[1..],
                        &expr[1..],
                        &mut bindings,
                        &mut binding_kind,
                        expr.improper,
                    )?;

                    let mut body = self.body.clone();

                    replace_identifiers(
                        &mut body,
                        &mut bindings,
                        &mut binding_kind,
                        &mut fallback_bindings,
                        span,
                    )?;

                    Ok(body)
                })
            })
        })

        // TODO: Consider using a thread local allocation, and just
        // clear the hashmap after each use?
        // let mut bindings = FxHashMap::default();
        // let mut binding_kind = FxHashMap::default();
        // let mut fallback_bindings = FxHashMap::default();
        // collect_bindings(
        //     &self.args[1..],
        //     &expr[1..],
        //     &mut bindings,
        //     &mut binding_kind,
        // )?;
        // replace_identifiers(
        //     self.body.clone(),
        //     &mut bindings,
        //     &mut binding_kind,
        //     &mut fallback_bindings,
        //     span,
        // )
    }
}

#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub enum MacroPattern {
    Rest(Box<MacroPattern>),
    Single(InternedString),
    Syntax(InternedString),
    Many(InternedString),
    Nested(Vec<MacroPattern>, bool),
    ManyNested(Vec<MacroPattern>, bool),
    CharacterLiteral(char),
    BytesLiteral(Vec<u8>),
    IntLiteral(isize),
    StringLiteral(String),
    FloatLiteral(f64),
    BooleanLiteral(bool),
    QuotedExpr(Box<Quote>),
    Quote(InternedString),
    Keyword(InternedString),
}

// pub enum QuotedLiteral {

// }

impl std::fmt::Debug for MacroPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MacroPattern::Single(s) => f.debug_tuple("Single").field(&s.resolve()).finish(),
            MacroPattern::Syntax(s) => f.debug_tuple("Syntax").field(&s.resolve()).finish(),
            MacroPattern::Many(m) => f.debug_tuple("Many").field(&m.resolve()).finish(),
            MacroPattern::Nested(n, v) => f.debug_tuple("Nested").field(n).field(v).finish(),
            MacroPattern::CharacterLiteral(c) => {
                f.debug_tuple("CharacterLiteral").field(c).finish()
            }
            MacroPattern::IntLiteral(i) => f.debug_tuple("IntLiteral").field(i).finish(),
            MacroPattern::StringLiteral(s) => f.debug_tuple("StringLiteral").field(s).finish(),
            MacroPattern::FloatLiteral(fl) => f.debug_tuple("FloatLiteral").field(fl).finish(),
            MacroPattern::BooleanLiteral(b) => f.debug_tuple("BooleanLiteral").field(b).finish(),
            MacroPattern::QuotedExpr(s) => f.debug_tuple("QuotedExpr").field(s).finish(),
            MacroPattern::Quote(i) => f.debug_tuple("Quote").field(&i.resolve()).finish(),
            MacroPattern::ManyNested(n, v) => {
                f.debug_tuple("ManyNested").field(n).field(v).finish()
            }
            MacroPattern::Keyword(k) => f.debug_tuple("Keyword").field(k).finish(),
            MacroPattern::Rest(r) => f.debug_tuple("Rest").field(r).finish(),
            MacroPattern::BytesLiteral(v) => f.debug_tuple("BytesLiteral").field(v).finish(),
        }
    }
}

impl MacroPattern {
    pub fn is_many(&self) -> bool {
        matches!(self, MacroPattern::Many(_) | MacroPattern::ManyNested(..))
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
            // Syntax(_) => {
            // if special_forms.contains(s) {
            // Syntax(*s)
            // } else {
            // Syntax("##".to_string() + s)
            // }
            // }
            Many(s) => {
                if special_forms.contains(s) {
                    // Many(*s)

                    // *self = Many(*s)
                } else {
                    *self = Many(("##".to_string() + s.resolve()).into())
                }
            }
            // Silly, needs revisiting
            ManyNested(m, _) => {
                m.iter_mut().for_each(|x| x.mangle(special_forms));
            }
            Nested(v, _) => {
                v.iter_mut().for_each(|x| x.mangle(special_forms));
            }
            Rest(v) => {
                v.mangle(special_forms);
            }
            _ => {}
        }
    }

    fn parse_from_list(
        list: List,
        macro_name: &InternedString,
        special_forms: &[InternedString],
        bindings: &mut FxHashSet<InternedString>,
        top_level: bool,
    ) -> Result<(Vec<MacroPattern>, Option<InternedString>)> {
        let mut pattern_vec: Vec<MacroPattern> = Vec::new();
        let len = list.args.len();
        let mut exprs_iter = list.args.into_iter().enumerate().peekable();
        let mut ellipsis = false;

        let wildcard = InternedString::from_static("_");

        macro_rules! add_binding {
            ($key:expr, $span:expr) => {
                if $key != wildcard && !bindings.insert($key) {
                    let var = $key.resolve();
                    stop!(BadSyntax => "repeated pattern variable {}", var ; $span);
                }
            };
        }

        macro_rules! check_ellipsis {
            ($improper:expr, $span:expr) => {
                if ellipsis {
                    stop!(BadSyntax => "pattern with more than one ellipsis"; $span);
                } else if $improper {
                    stop!(BadSyntax => "ellipsis cannot appear as list tail"; $span);
                }

                ellipsis = true;
            }
        }

        let mut macro_keyword = None;

        while let Some((i, expr)) = exprs_iter.next() {
            match expr {
                ExprKind::Atom(Atom {
                    syn: SyntaxObject { ty: s, span, .. },
                }) => match s {
                    TokenType::Identifier(t) => {
                        if t == *macro_name || special_forms.contains(&t) {
                            pattern_vec.push(MacroPattern::Syntax(t.clone()))
                        } else {
                            let peek = exprs_iter.peek().map(|(_, expr)| expr);

                            match peek {
                                Some(ExprKind::Atom(Atom {
                                    syn:
                                        SyntaxObject {
                                            ty: TokenType::Ellipses,
                                            span,
                                            ..
                                        },
                                })) => {
                                    let span = *span;

                                    let last_proper = list.improper && i + 2 == len;
                                    check_ellipsis!(last_proper, span);

                                    exprs_iter.next();

                                    if i == 0 && top_level {
                                        stop!(BadSyntax => "macro name cannot be followed by ellipsis"; span);
                                    }

                                    add_binding!(t, span);
                                    pattern_vec.push(MacroPattern::Many(t));
                                }
                                _ => {
                                    if i == 0 && top_level {
                                        macro_keyword = Some(t);
                                    }

                                    add_binding!(t, span);
                                    pattern_vec.push(MacroPattern::Single(t));
                                }
                            }
                        }
                    }
                    // TODO: Add all of the tokens here
                    TokenType::Begin => {
                        pattern_vec.push(MacroPattern::Syntax(*BEGIN));
                    }
                    TokenType::Define => {
                        pattern_vec.push(MacroPattern::Syntax(*DEFINE));
                    }
                    TokenType::If => {
                        pattern_vec.push(MacroPattern::Syntax(*IF));
                    }
                    TokenType::BooleanLiteral(b) => {
                        pattern_vec.push(MacroPattern::BooleanLiteral(b));
                    }
                    TokenType::Number(n) => match *n {
                        NumberLiteral::Real(re) => match re {
                            RealLiteral::Int(IntLiteral::Small(i)) => {
                                pattern_vec.push(MacroPattern::IntLiteral(i))
                            }
                            RealLiteral::Int(IntLiteral::Big(_)) => {
                                stop!(BadSyntax => format!("big integers not supported: {}", re));
                            }
                            RealLiteral::Float(f) => {
                                pattern_vec.push(MacroPattern::FloatLiteral(f))
                            }
                            RealLiteral::Rational(_, _) => {
                                stop!(BadSyntax => format!("rationals numbers are not supported: {}", re))
                            }
                        },
                        c @ NumberLiteral::Complex(_, _) => {
                            stop!(BadSyntax => format!("complex numbers not supported: {}", c))
                        }
                    },
                    TokenType::CharacterLiteral(c) => {
                        pattern_vec.push(MacroPattern::CharacterLiteral(c));
                    }
                    TokenType::StringLiteral(s) => {
                        pattern_vec.push(MacroPattern::StringLiteral(*s));
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
                                    yet support arbitrary constants yet")
                                // pattern_vec.push(MacroPattern::Single(t.clone()));
                            }
                        }
                    }
                    TokenType::Ellipses => {
                        if let Some(MacroPattern::Nested(inner, vec)) = pattern_vec.pop() {
                            let improper = list.improper && i + 1 == len;
                            check_ellipsis!(improper, span);

                            pattern_vec.push(MacroPattern::ManyNested(inner, vec));
                        } else {
                            stop!(BadSyntax => "cannot bind pattern to ellipsis"; span)
                        }
                    }
                    _ => {
                        stop!(BadSyntax => "syntax-rules requires identifiers in the pattern"; span);
                    }
                },
                ExprKind::List(l) => {
                    let (patterns, _) =
                        Self::parse_from_list(l, macro_name, special_forms, bindings, false)?;

                    pattern_vec.push(MacroPattern::Nested(patterns, false))
                }
                ExprKind::Quote(q) => pattern_vec.push(MacroPattern::QuotedExpr(q)),
                ExprKind::Vector(Vector {
                    args, bytes: false, ..
                }) => {
                    let list = List::new(args);

                    let (patterns, _) =
                        Self::parse_from_list(list, macro_name, special_forms, bindings, false)?;

                    pattern_vec.push(MacroPattern::Nested(patterns, true))
                }
                ExprKind::Vector(v @ Vector { bytes: true, .. }) => {
                    let bytes = v.as_bytes().collect();

                    pattern_vec.push(MacroPattern::BytesLiteral(bytes));
                }
                _ => {
                    // TODO: Add pattern matching on other kinds of forms here - probably just a special IR
                    // for the pattern to match against here
                    stop!(BadSyntax => format!("illegal special form found in macro pattern: {}", expr) ; opt expr.span());
                }
            }
        }

        if list.improper {
            let rest = pattern_vec.pop().map(|pat| MacroPattern::Rest(pat.into()));
            pattern_vec.extend(rest.into_iter());
        }

        Ok((pattern_vec, macro_keyword))
    }
}

fn match_list_pattern(patterns: &[MacroPattern], list: &[ExprKind], improper: bool) -> bool {
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
        &list[proper_patterns.len()..]
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
            MacroPattern::Many(_) if has_ellipsis => {
                for _ in 0..expected_many_captures {
                    let item = exprs_iter.next();
                    debug_assert!(item.is_some());
                }
                continue;
            }
            MacroPattern::ManyNested(subpatterns, is_vec) if has_ellipsis => {
                for _ in 0..expected_many_captures {
                    let Some((_, next)) = exprs_iter.next() else {
                        return false;
                    };

                    // FIXME: this is not quite correct. It does not handle
                    // the "improper-list-becomes-non-list" e.g.
                    // (a ... . b) matching "foobar"
                    let (args, improper) = match (next, is_vec) {
                        (ExprKind::List(l), false) => (&l.args, l.improper),
                        (
                            ExprKind::Vector(Vector {
                                bytes: false, args, ..
                            }),
                            true,
                        ) => (args, false),
                        _ => return false,
                    };

                    if !match_list_pattern(subpatterns, &args, improper) {
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

        if !match_single_pattern(pat, expr) {
            return false;
        }
    }

    if let Some((pat, exprs)) = tail_to_match {
        let tail_matches = if improper && exprs.len() == 1 {
            match_single_pattern(pat, &exprs[0])
        } else {
            match_rest_pattern(pat, exprs, improper)
        };

        if !tail_matches {
            return false;
        }
    }

    true
}

fn match_rest_pattern(pattern: &MacroPattern, exprs: &[ExprKind], improper: bool) -> bool {
    match pattern {
        MacroPattern::Single(_) => true,
        MacroPattern::Nested(patterns, false) => match_list_pattern(patterns, exprs, improper),
        _ => false,
    }
}

fn match_single_pattern(pattern: &MacroPattern, expr: &ExprKind) -> bool {
    match pattern {
        MacroPattern::Many(_) => unreachable!(),
        MacroPattern::ManyNested(..) => unreachable!(),
        MacroPattern::Rest(_) => unreachable!(),
        MacroPattern::Single(_) => true,
        // TODO: @Matt - if the atom is bound locally, then do not match on this
        MacroPattern::Syntax(v) => match expr {
            ExprKind::Atom(Atom {
                syn:
                    SyntaxObject {
                        ty: TokenType::Identifier(s),
                        ..
                    },
            }) if s == v => true,
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
            _ => return false,
        },
        MacroPattern::Keyword(k) => match expr {
            ExprKind::Atom(Atom {
                syn:
                    SyntaxObject {
                        ty: TokenType::Keyword(s),
                        ..
                    },
            }) if s == k => true,
            _ => return false,
        },
        MacroPattern::BooleanLiteral(b) => match expr {
            ExprKind::Atom(Atom {
                syn:
                    SyntaxObject {
                        ty: TokenType::BooleanLiteral(s),
                        ..
                    },
            }) if s == b => true,
            _ => return false,
        },
        // MacroPattern::IntLiteral(i) => match expr {
        //     ExprKind::Atom(Atom {
        //         syn:
        //             SyntaxObject {
        //                 ty:
        //                     TokenType::Number(NumberLiteral::Real(RealLiteral::Int(IntLiteral::Small(
        //                         s,
        //                     )))),
        //                 ..
        //             },
        //     }) if s == i => true,
        //     _ => return false,
        // },
        MacroPattern::IntLiteral(i) => match expr {
            ExprKind::Atom(Atom {
                syn:
                    SyntaxObject {
                        ty: TokenType::Number(n),
                        ..
                    },
            }) => match n.as_ref() {
                NumberLiteral::Real(RealLiteral::Int(IntLiteral::Small(s))) if s == i => true,
                _ => return false,
            },
            _ => return false,
        },

        // MacroPattern::FloatLiteral(f) => match expr {
        //     ExprKind::Atom(Atom {
        //         syn:
        //             SyntaxObject {
        //                 ty: TokenType::Number(NumberLiteral::Real(RealLiteral::Float(s))),
        //                 ..
        //             },
        //     }) if s == f => true,
        //     _ => return false,
        // },
        MacroPattern::FloatLiteral(f) => match expr {
            ExprKind::Atom(Atom {
                syn:
                    SyntaxObject {
                        ty: TokenType::Number(n),
                        ..
                    },
            }) => match n.as_ref() {
                NumberLiteral::Real(RealLiteral::Float(s)) if s == f => true,
                _ => return false,
            },
            _ => return false,
        },

        MacroPattern::CharacterLiteral(c) => match expr {
            ExprKind::Atom(Atom {
                syn:
                    SyntaxObject {
                        ty: TokenType::CharacterLiteral(s),
                        ..
                    },
            }) if s == c => true,
            _ => return false,
        },
        MacroPattern::StringLiteral(s) => match expr {
            ExprKind::Atom(Atom {
                syn:
                    SyntaxObject {
                        ty: TokenType::StringLiteral(b),
                        ..
                    },
            }) if s.as_str() == b.as_str() => true,
            _ => return false,
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
                _ => {
                    return false;
                }
            }
        }
        // TODO: Come back here and do constants
        MacroPattern::Quote(_q) => {
            // println!("MATCHING QUOTE {} with val: {}", q, val);
            match expr {
                ExprKind::Quote(_) => return true,

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
                _ => return false,
            }
        }

        // Now that we have ManyNested - need to figure out
        // the recursive step better here
        MacroPattern::Nested(patterns, is_vec) => {
            match expr {
                // Make the recursive call on the next layer
                ExprKind::List(l) => !is_vec && match_list_pattern(patterns, l, l.improper),
                ExprKind::Vector(v) => {
                    *is_vec && !v.bytes && match_list_pattern(patterns, &v.args, false)
                }
                // TODO: Come back here
                ExprKind::Quote(_) => {
                    !is_vec && matches!(patterns.as_slice(), &[MacroPattern::Quote(_)])
                }
                _ => {
                    if let Some(pat) = non_list_match(patterns) {
                        match_single_pattern(pat, expr)
                    } else {
                        false
                    }
                }
            }
        }
    }
}

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
            MacroPattern::Syntax(s) => {
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
            // bind the ellipses to the rest of the statement
            MacroPattern::Many(ident) => {
                let mut captured = vec![];

                for _ in 0..expected_many_captures {
                    captured.extend(expr_iter.next().map(|(_, exp)| exp).cloned().into_iter());
                }

                bindings.insert(*ident, List::new(captured).into());
                binding_kind.insert(*ident, BindingKind::Many);
            }
            MacroPattern::Nested(children, is_vec) => {
                let (_, child) = expr_iter
                    .next()
                    .ok_or_else(throw!(ArityMismatch => "Macro expected a pattern"))?;

                match child {
                    ExprKind::List(l) => {
                        if !is_vec {
                            collect_bindings(children, l, bindings, binding_kind, l.improper)?;
                        }
                    }
                    ExprKind::Vector(v) => {
                        if *is_vec {
                            collect_bindings(children, &v.args, bindings, binding_kind, false)?;
                        }
                    }
                    ExprKind::Quote(q) => {
                        if let &[MacroPattern::Quote(x)] = children.as_slice() {
                            bindings.insert(x, q.expr.clone());
                        } else {
                            stop!(BadSyntax => "macro expected a list of values, not including keywords, found: {}", child)
                        }
                    }
                    _ => {
                        if let Some(pat) = non_list_match(&children) {
                            collect_bindings(
                                &[pat.clone()],
                                &[child.clone()],
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
            MacroPattern::ManyNested(children, is_vec) => {
                let exprs_to_destruct = {
                    let mut exprs = smallvec::SmallVec::<[_; 8]>::new();

                    for _ in 0..expected_many_captures {
                        exprs.extend(expr_iter.next().map(|(_, exp)| exp).cloned().into_iter());
                    }

                    exprs
                };

                for (i, child) in children.iter().enumerate() {
                    // for i in 0..children.len() {
                    let mut values_to_bind = Vec::new();

                    let is_ellipses_pattern = matches!(child, MacroPattern::Many(_));

                    for expr in &exprs_to_destruct {
                        match expr {
                            ExprKind::List(l) if !is_vec => {
                                // Not what we want to do here!

                                if is_ellipses_pattern {
                                    // Bind the "rest" of the values into this
                                    values_to_bind.push(List::new(l[i..].to_vec()).into());
                                } else {
                                    values_to_bind.push(l[i].clone());
                                }
                            }
                            ExprKind::Vector(v) if *is_vec => {
                                values_to_bind.push(v.args[i].clone());
                            }
                            _ => unreachable!(),
                        }
                    }

                    match child {
                        MacroPattern::Single(ident) => {
                            let list_of_values = List::new(values_to_bind).into();

                            // Turn this into a many!
                            bindings.insert(*ident, list_of_values);
                            binding_kind.insert(*ident, BindingKind::Many);
                        }
                        // MacroPattern::Syntax(_) => todo!(),
                        MacroPattern::Many(ident) => {
                            let list_of_values = List::new(values_to_bind).into();

                            // println!("-- Binding: {} => {}", ident, list_of_values);

                            bindings.insert(*ident, list_of_values);
                            binding_kind.insert(*ident, BindingKind::Many);
                        }
                        // These I'll need to handle
                        MacroPattern::Nested(nested_children, is_vec) => {
                            let mut final_bindings: HashMap<_, Vec<_>> = HashMap::new();

                            for expr in values_to_bind {
                                let (list, improper) = match expr {
                                    ExprKind::List(l) if !is_vec => (l.args, l.improper),
                                    ExprKind::Vector(vec) if !is_vec => (vec.args, false),
                                    _ => stop!(BadSyntax => "Unreachable"),
                                };

                                let mut new_bindings = FxHashMap::default();

                                collect_bindings(
                                    nested_children,
                                    &list,
                                    &mut new_bindings,
                                    binding_kind,
                                    improper,
                                )?;

                                for (key, value) in new_bindings {
                                    // println!("Marking as a many binding => {}", key);
                                    binding_kind.insert(key, BindingKind::Many);

                                    if let Some(x) = final_bindings.get_mut(&key) {
                                        x.push(value);
                                    } else {
                                        final_bindings.insert(key, vec![value]);
                                    }
                                }
                            }

                            for (key, value) in final_bindings {
                                bindings.insert(key, List::new(value).into());
                            }
                        }
                        MacroPattern::ManyNested(..) => {
                            stop!(BadSyntax => "Internal compiler error - unexpected ellipses expansion found within ellipses expansion")
                        }
                        _ => {
                            continue;
                        }
                    }
                }

                return Ok(());
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
                        let list = List::new_maybe_improper(list[i..].to_vec(), improper);

                        [ExprKind::List(list)]
                    }
                    None => [ExprKind::List(List::new(vec![]))],
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
    fn test_match_basic() {
        let pattern_args = vec![
            MacroPattern::Syntax("and".into()),
            MacroPattern::Single("a".into()),
            MacroPattern::Single("b".into()),
        ];
        let list_expr = List::new(vec![
            atom_identifier("and"),
            ExprKind::List(List::new(vec![
                atom_identifier("+"),
                atom_identifier("x"),
                atom_identifier("y"),
            ])),
            ExprKind::List(List::new(vec![
                atom_identifier("+"),
                atom_identifier("x"),
                atom_identifier("y"),
            ])),
        ]);

        assert!(match_list_pattern(&pattern_args, &list_expr, false));
    }

    #[test]
    fn test_match_many() {
        let pattern_args = vec![
            MacroPattern::Syntax("and".into()),
            MacroPattern::Single("a".into()),
            MacroPattern::Many("b".into()),
        ];
        let list_expr = List::new(vec![
            atom_identifier("and"),
            ExprKind::List(List::new(vec![
                atom_identifier("+"),
                atom_identifier("x"),
                atom_identifier("y"),
            ])),
            ExprKind::List(List::new(vec![
                atom_identifier("+"),
                atom_identifier("x"),
                atom_identifier("y"),
            ])),
        ]);
        assert!(match_list_pattern(&pattern_args, &list_expr, false));
    }

    #[test]
    fn test_match_many_multiple() {
        let pattern_args = vec![
            MacroPattern::Syntax("and".into()),
            MacroPattern::Single("a".into()),
            MacroPattern::Many("b".into()),
        ];
        let list_expr = List::new(vec![
            atom_identifier("and"),
            ExprKind::List(List::new(vec![
                atom_identifier("+"),
                atom_identifier("x"),
                atom_identifier("y"),
            ])),
            atom_identifier("+"),
            atom_identifier("x"),
            atom_identifier("y"),
        ]);
        assert!(match_list_pattern(&pattern_args, &list_expr, false));
    }

    #[test]
    fn test_nested() {
        let pattern_args = vec![
            MacroPattern::Syntax("->>".into()),
            MacroPattern::Single("a".into()),
            MacroPattern::Nested(
                vec![
                    MacroPattern::Single("b".into()),
                    MacroPattern::Many("c".into()),
                ],
                false,
            ),
        ];

        let list_expr = List::new(vec![
            atom_identifier("->>"),
            atom_int(1),
            ExprKind::List(List::new(vec![
                atom_identifier("apple"),
                atom_identifier("sauce"),
                atom_identifier("is-good"),
            ])),
        ]);
        assert!(match_list_pattern(&pattern_args, &list_expr, false));
    }

    #[test]
    fn test_no_match_simple() {
        let pattern_args = vec![
            MacroPattern::Syntax("->>".into()),
            MacroPattern::Single("a".into()),
            MacroPattern::Single("bad".into()),
        ];

        let list_expr = List::new(vec![
            atom_identifier("->>"),
            atom_int(1),
            atom_int(2),
            atom_int(3),
        ]);

        assert!(!match_list_pattern(&pattern_args, &list_expr, false));
    }

    #[test]
    fn test_nested_no_match() {
        let pattern_args = vec![
            MacroPattern::Syntax("->>".into()),
            MacroPattern::Single("a".into()),
            MacroPattern::Single("bad".into()),
            MacroPattern::Nested(
                vec![
                    MacroPattern::Single("b".into()),
                    MacroPattern::Many("c".into()),
                ],
                false,
            ),
        ];

        let list_expr = List::new(vec![
            atom_identifier("->>"),
            atom_int(1),
            ExprKind::List(List::new(vec![
                atom_identifier("apple"),
                atom_identifier("sauce"),
                atom_identifier("is-good"),
            ])),
        ]);

        assert!(!match_list_pattern(&pattern_args, &list_expr, false));
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
            MacroPattern::Syntax("and".into()),
            MacroPattern::Single("a".into()),
            MacroPattern::Single("b".into()),
        ];
        let list_expr = List::new(vec![
            atom_identifier("and"),
            ExprKind::List(List::new(vec![
                atom_identifier("+"),
                atom_identifier("x"),
                atom_identifier("y"),
            ])),
            ExprKind::List(List::new(vec![
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
            ExprKind::List(List::new(vec![
                atom_identifier("+"),
                atom_identifier("x"),
                atom_identifier("y"),
            ])),
        );

        post_bindings.insert(
            "b".into(),
            ExprKind::List(List::new(vec![
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
            MacroPattern::Syntax("and".into()),
            MacroPattern::Single("a".into()),
            MacroPattern::Many("b".into()),
        ];
        let list_expr = List::new(vec![
            atom_identifier("and"),
            ExprKind::List(List::new(vec![
                atom_identifier("+"),
                atom_identifier("x"),
                atom_identifier("y"),
            ])),
            ExprKind::List(List::new(vec![
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
            ExprKind::List(List::new(vec![
                atom_identifier("+"),
                atom_identifier("x"),
                atom_identifier("y"),
            ])),
        );
        post_bindings.insert(
            "b".into(),
            ExprKind::List(List::new(vec![ExprKind::List(List::new(vec![
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
            MacroPattern::Syntax("and".into()),
            MacroPattern::Single("a".into()),
            MacroPattern::Many("b".into()),
        ];
        let list_expr = List::new(vec![
            atom_identifier("and"),
            ExprKind::List(List::new(vec![
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
            ExprKind::List(List::new(vec![
                atom_identifier("+"),
                atom_identifier("x"),
                atom_identifier("y"),
            ])),
        );

        post_bindings.insert(
            "b".into(),
            ExprKind::List(List::new(vec![
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
            MacroPattern::Syntax("->>".into()),
            MacroPattern::Single("a".into()),
            MacroPattern::Nested(
                vec![
                    MacroPattern::Single("b".into()),
                    MacroPattern::Many("c".into()),
                ],
                false,
            ),
        ];

        let list_expr = List::new(vec![
            atom_identifier("->>"),
            atom_int(1),
            ExprKind::List(List::new(vec![
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
            ExprKind::List(List::new(vec![
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
            args: vec![
                MacroPattern::Syntax("test".into()),
                MacroPattern::Single("a".into()),
                MacroPattern::Single("b".into()),
                MacroPattern::Single("c".into()),
            ],
            body: List::new(vec![
                atom_identifier("fun-call"),
                atom_identifier("inserted-variable"),
                atom_identifier("a"),
                atom_identifier("b"),
                atom_identifier("c"),
            ])
            .into(),
        };

        let input = List::new(vec![
            atom_identifier("test"),
            atom_int(1),
            atom_identifier("apple"),
            atom_int(2),
        ]);

        let expected: ExprKind = List::new(vec![
            atom_identifier("fun-call"),
            atom_identifier("inserted-variable"),
            atom_int(1),
            atom_identifier("apple"),
            atom_int(2),
        ])
        .into();

        let output = case
            .expand(input, Span::new(0, 0, SourceId::none()))
            .unwrap();

        assert_eq!(output, expected);
    }
}
