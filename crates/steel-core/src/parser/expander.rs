use crate::parser::ast::{Atom, ExprKind, List, Macro, PatternPair};
use crate::parser::parser::SyntaxObject;
use crate::parser::rename_idents::RenameIdentifiersVisitor;
use crate::parser::replace_idents::replace_identifiers;
use crate::parser::tokens::TokenType;

use crate::parser::span::Span;

use crate::rvals::Result;
use std::{
    collections::HashMap,
    fs::File,
    io::{Read, Write},
    iter::FromIterator,
    path::{Path, PathBuf},
};

use log::{debug, error};
use serde::{Deserialize, Serialize};
use steel_parser::tokens::MaybeBigInt;

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
    macros: HashMap<InternedString, SteelMacro>,
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
    pub fn expand(&self, exprs: impl IntoIterator<Item = ExprKind>) -> Result<Vec<ExprKind>> {
        exprs
            .into_iter()
            .map(|expr| crate::parser::expand_visitor::expand(expr, &self.macros))
            .collect()
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
}

impl SteelMacro {
    #[cfg(test)]
    pub fn new(
        name: InternedString,
        special_forms: Vec<InternedString>,
        cases: Vec<MacroCase>,
    ) -> Self {
        SteelMacro {
            name,
            special_forms,
            cases,
            mangled: false,
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
        })
    }

    // TODO the case matching should be a little bit more informed than this
    // I think it should also not be greedy, and should report if there are ambiguous matchings
    fn match_case(&self, expr: &List) -> Result<&MacroCase> {
        for case in &self.cases {
            if (case.has_ellipses() && expr.len() >= (case.arity() - 1))
                || case.arity() == expr.len()
            {
                if case.recursive_match(expr) {
                    return Ok(case);
                }
            }
        }
        error!("Macro expansion unable to match case with: {}", expr);

        if let Some(ExprKind::Atom(a)) = expr.first() {
            stop!(BadSyntax => format!("macro expansion unable to match case: {expr}"); a.syn.span);
        } else {
            unreachable!()
        }
    }

    pub fn expand(&self, expr: List, span: Span) -> Result<ExprKind> {
        // if log::log_enabled!(log::Level::Debug) {
        //     debug!("Expanding macro with tokens: {}", expr);
        // }

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

        let args = if let ExprKind::List(l) = pattern {
            MacroPattern::parse_from_list(l, name, special_forms)?
        } else {
            stop!(Generic => "unable to parse macro");
        };

        let args_str: Vec<&InternedString> = args
            .iter()
            .flat_map(|x| x.deconstruct_without_syntax())
            .collect();

        // let syntaxes: Vec<&str> = args
        //     .iter()
        //     .map(|x| x.deconstruct_syntax())
        //     .flatten()
        //     .collect();

        // println!("Args pre mangle: {:?}", &args_str);

        RenameIdentifiersVisitor::new(&args_str, special_forms).rename_identifiers(&mut body);

        let args = args.into_iter().map(|x| x.mangle(special_forms)).collect();

        // println!("Found args: {:?}", args);
        // println!("Renamed body: {:?}", &body);

        Ok(MacroCase { args, body })
    }

    fn has_ellipses(&self) -> bool {
        self.args
            .iter()
            .any(|x| matches!(x, MacroPattern::Many(_) | MacroPattern::ManyNested(_)))
    }

    fn arity(&self) -> usize {
        self.args
            .iter()
            .map(|x| if let MacroPattern::Many(_) = x { 1 } else { 1 })
            .sum()
    }

    fn recursive_match(&self, list: &List) -> bool {
        match_vec_pattern(&self.args, list)
    }

    fn expand(&self, expr: List, span: Span) -> Result<ExprKind> {
        // TODO: Consider using a thread local allocation, and just
        // clear the hashmap after each use?
        let mut bindings = HashMap::new();
        let mut binding_kind = HashMap::new();
        let mut fallback_bindings = HashMap::new();
        collect_bindings(&self.args, &expr, &mut bindings, &mut binding_kind)?;
        replace_identifiers(
            self.body.clone(),
            &mut bindings,
            &mut binding_kind,
            &mut fallback_bindings,
            span,
        )
    }
}

#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub enum MacroPattern {
    Single(InternedString),
    Syntax(InternedString),
    Many(InternedString),
    Nested(Vec<MacroPattern>),
    ManyNested(Vec<MacroPattern>),
    CharacterLiteral(char),
    IntLiteral(isize),
    StringLiteral(String),
    FloatLiteral(f64),
    BooleanLiteral(bool),
    QuotedExpr(Box<Quote>),
    Quote(InternedString),
}

// pub enum QuotedLiteral {

// }

impl std::fmt::Debug for MacroPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MacroPattern::Single(s) => f.debug_tuple("Single").field(&s.resolve()).finish(),
            MacroPattern::Syntax(s) => f.debug_tuple("Syntax").field(&s.resolve()).finish(),
            MacroPattern::Many(m) => f.debug_tuple("Many").field(&m.resolve()).finish(),
            MacroPattern::Nested(n) => f.debug_tuple("Nested").field(n).finish(),
            MacroPattern::CharacterLiteral(c) => {
                f.debug_tuple("CharacterLiteral").field(c).finish()
            }
            MacroPattern::IntLiteral(i) => f.debug_tuple("IntLiteral").field(i).finish(),
            MacroPattern::StringLiteral(s) => f.debug_tuple("StringLiteral").field(s).finish(),
            MacroPattern::FloatLiteral(fl) => f.debug_tuple("FloatLiteral").field(fl).finish(),
            MacroPattern::BooleanLiteral(b) => f.debug_tuple("BooleanLiteral").field(b).finish(),
            MacroPattern::QuotedExpr(s) => f.debug_tuple("QuotedExpr").field(s).finish(),
            MacroPattern::Quote(i) => f.debug_tuple("Quote").field(&i.resolve()).finish(),
            MacroPattern::ManyNested(n) => f.debug_tuple("ManyNested").field(n).finish(),
        }
    }
}

impl MacroPattern {
    fn mangle(&self, special_forms: &[InternedString]) -> Self {
        use MacroPattern::*;
        match self {
            Single(s) => {
                if special_forms.contains(s) {
                    Single(*s)
                } else {
                    Single(("##".to_string() + s.resolve()).into())
                }
            }
            Syntax(s) => {
                // if special_forms.contains(s) {
                Syntax(*s)
                // } else {
                // Syntax("##".to_string() + s)
                // }
            }
            Many(s) => {
                if special_forms.contains(s) {
                    Many(*s)
                } else {
                    Many(("##".to_string() + s.resolve()).into())
                }
            }
            // Silly, needs revisiting
            ManyNested(m) => ManyNested(m.iter().map(|x| x.mangle(special_forms)).collect()),
            Nested(v) => Nested(v.iter().map(|x| x.mangle(special_forms)).collect()),
            _ => self.clone(),
        }
    }

    fn parse_from_list(
        list: List,
        macro_name: &InternedString,
        special_forms: &[InternedString],
    ) -> Result<Vec<Self>> {
        let mut pattern_vec: Vec<MacroPattern> = Vec::new();
        let mut peek_token_iter = list.args.into_iter().peekable();

        while let Some(token) = peek_token_iter.next() {
            match token {
                ExprKind::Atom(Atom {
                    syn: SyntaxObject { ty: s, span, .. },
                }) => match s {
                    TokenType::Identifier(t) => {
                        if t == *macro_name || special_forms.contains(&t) {
                            pattern_vec.push(MacroPattern::Syntax(t.clone()))
                        } else {
                            match peek_token_iter.peek() {
                                Some(ExprKind::Atom(Atom {
                                    syn:
                                        SyntaxObject {
                                            ty: TokenType::Ellipses,
                                            ..
                                        },
                                })) => {
                                    peek_token_iter.next();
                                    pattern_vec.push(MacroPattern::Many(t.clone()));
                                }
                                _ => {
                                    pattern_vec.push(MacroPattern::Single(t.clone()));
                                }
                            }
                        }
                    }
                    TokenType::BooleanLiteral(b) => {
                        pattern_vec.push(MacroPattern::BooleanLiteral(b));
                    }
                    TokenType::IntegerLiteral(MaybeBigInt::Small(i)) => {
                        pattern_vec.push(MacroPattern::IntLiteral(i));
                    }
                    TokenType::CharacterLiteral(c) => {
                        pattern_vec.push(MacroPattern::CharacterLiteral(c));
                    }
                    TokenType::NumberLiteral(n) => {
                        pattern_vec.push(MacroPattern::FloatLiteral(n));
                    }
                    TokenType::StringLiteral(s) => {
                        pattern_vec.push(MacroPattern::StringLiteral(s));
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

                        let next = peek_token_iter.next();

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
                        if let Some(MacroPattern::Nested(inner)) = pattern_vec.pop() {
                            pattern_vec.push(MacroPattern::ManyNested(inner));
                        } else {
                            stop!(BadSyntax => "cannot bind pattern to ellipses"; span)
                        }
                    }
                    _ => {
                        stop!(BadSyntax => "syntax-rules requires identifiers in the pattern"; span);
                    }
                },
                ExprKind::List(l) => pattern_vec.push(MacroPattern::Nested(Self::parse_from_list(
                    l,
                    macro_name,
                    special_forms,
                )?)),
                ExprKind::Quote(q) => pattern_vec.push(MacroPattern::QuotedExpr(q)),
                _ => {
                    // TODO: Add pattern matching on other kinds of forms here - probably just a special IR
                    // for the pattern to match against here
                    stop!(BadSyntax => "illegal special form found in macro pattern");
                }
            }
        }
        Ok(pattern_vec)
    }
}

impl MacroPattern {
    // TODO make this not so trash
    pub fn deconstruct(&self) -> Vec<&InternedString> {
        match self {
            Self::Syntax(s) => vec![s],
            Self::Single(s) => vec![s],
            Self::Many(s) => vec![s],
            Self::ManyNested(v) => v.iter().flat_map(|x| x.deconstruct()).collect(),
            Self::Nested(v) => v.iter().flat_map(|x| x.deconstruct()).collect(),
            _ => vec![],
        }
    }

    pub fn deconstruct_without_syntax(&self) -> Vec<&InternedString> {
        match self {
            Self::Single(s) => vec![s],
            Self::Many(s) => vec![s],
            Self::Nested(v) => v.iter().flat_map(|x| x.deconstruct()).collect(),
            Self::ManyNested(v) => v.iter().flat_map(|x| x.deconstruct()).collect(),
            _ => vec![],
        }
    }

    pub fn is_many(&self) -> bool {
        matches!(self, MacroPattern::Many(_) | MacroPattern::ManyNested(_))
    }
}

pub fn match_vec_pattern(args: &[MacroPattern], list: &List) -> bool {
    let mut token_iter = list.iter();

    for pat in args {
        if let Some(val) = token_iter.next() {
            // dbg!(&pat);
            // dbg!(&val);

            match pat {
                MacroPattern::Single(_) | MacroPattern::Many(_) => continue,
                // TODO: @Matt - if the atom is bound locally, then do not match on this
                MacroPattern::Syntax(v) => match val {
                    ExprKind::Atom(Atom {
                        syn:
                            SyntaxObject {
                                ty: TokenType::Identifier(s),
                                ..
                            },
                    }) if s == v => continue,
                    ExprKind::Atom(Atom {
                        syn:
                            SyntaxObject {
                                ty: TokenType::Ellipses,
                                ..
                            },
                    }) => continue,
                    _ => return false,
                },
                MacroPattern::BooleanLiteral(b) => match val {
                    ExprKind::Atom(Atom {
                        syn:
                            SyntaxObject {
                                ty: TokenType::BooleanLiteral(s),
                                ..
                            },
                    }) if s == b => continue,
                    _ => return false,
                },
                MacroPattern::IntLiteral(i) => match val {
                    ExprKind::Atom(Atom {
                        syn:
                            SyntaxObject {
                                ty: TokenType::IntegerLiteral(MaybeBigInt::Small(s)),
                                ..
                            },
                    }) if s == i => continue,
                    _ => return false,
                },
                MacroPattern::FloatLiteral(f) => match val {
                    ExprKind::Atom(Atom {
                        syn:
                            SyntaxObject {
                                ty: TokenType::NumberLiteral(s),
                                ..
                            },
                    }) if s == f => continue,
                    _ => return false,
                },
                MacroPattern::CharacterLiteral(c) => match val {
                    ExprKind::Atom(Atom {
                        syn:
                            SyntaxObject {
                                ty: TokenType::CharacterLiteral(s),
                                ..
                            },
                    }) if s == c => continue,
                    _ => return false,
                },
                MacroPattern::StringLiteral(s) => match val {
                    ExprKind::Atom(Atom {
                        syn:
                            SyntaxObject {
                                ty: TokenType::StringLiteral(b),
                                ..
                            },
                    }) if s == b => continue,
                    _ => return false,
                },
                MacroPattern::QuotedExpr(q) => {
                    // println!("MATCHING QUOTED EXPR: {}", q);
                    match val {
                        ExprKind::Quote(boxed_q) if q == boxed_q => continue,
                        _ => {
                            return false;
                        }
                    }
                }
                // TODO: Come back here and do constants
                MacroPattern::Quote(_q) => {
                    // println!("MATCHING QUOTE {} with val: {}", q, val);
                    match val {
                        ExprKind::Quote(_) => return true,

                        // ExprKind::Atom(Atom {
                        //     syn:
                        //         SyntaxObject {
                        //             ty: TokenType::Quote | TokenType::QuoteTick,
                        //             ..
                        //         },
                        // }) => {
                        //     println!("MATCHING ON QUOTE");
                        //     continue;
                        // }

                        // ExprKind::Atom(Atom {
                        //     syn:
                        //         SyntaxObject {
                        //             ty: TokenType::Identifier(s),
                        //             ..
                        //         },
                        // }) if s.resolve() == "QUOTE" => {
                        //     println!("MATCHING ON QUOTE");
                        //     continue;
                        // }
                        // ExprKind::Quote()
                        _ => return false,
                    }
                }
                // Now that we have ManyNested - need to figure out
                // the recursive step better here
                MacroPattern::Nested(vec) => {
                    if let ExprKind::List(l) = val {
                        // TODO more elegant let* case
                        if vec.is_empty() && !l.is_empty() {
                            return false;
                        }

                        // This solves the destructuring test case
                        if vec.len() < l.len() && !vec.iter().any(|x| x.is_many()) {
                            debug!("Matching failed - ellipses doesn't match up");
                            return false;
                        }

                        // Make the recursive call on the next layer
                        if match_vec_pattern(vec, l) {
                            continue;
                        } else {
                            debug!("Matching failed due to child not matching");
                            return false;
                        }
                    } else if let ExprKind::Quote(_) = val {
                        // TODO: Come back here
                        return matches!(vec.as_slice(), &[MacroPattern::Quote(_)]);
                        // return true;
                    } else {
                        debug!("Matching failed - atom does not match list");
                        return false;
                    }
                }
                MacroPattern::ManyNested(vec) => {
                    if let ExprKind::List(l) = val {
                        if !match_vec_pattern(vec, l) {
                            return false;
                        }
                    } else {
                        return false;
                    }

                    for maybe_next in token_iter {
                        if let ExprKind::List(l) = maybe_next {
                            if match_vec_pattern(vec, l) {
                                continue;
                            }
                        } else {
                            return false;
                        }
                    }

                    return true;
                }
            }
        } else {
            // In the case where we have no more patterns, we should be able to contninue matching
            if pat.is_many() {
                continue;
            } else {
                debug!(
                    "Matching failed due to insufficient tokens - next pat: {:?}",
                    pat
                );
                return false;
            }
        }
    }

    if token_iter.next().is_some() && !matches!(args.last(), Some(MacroPattern::Many(_))) {
        debug!("Matching failed due to leftover tokens");
        return false;
    }

    true
}

pub enum BindingKind {
    Many,
    Single,
}

pub fn collect_bindings(
    args: &[MacroPattern],
    list: &List,
    bindings: &mut HashMap<InternedString, ExprKind>,
    binding_kind: &mut HashMap<InternedString, BindingKind>,
) -> Result<()> {
    let mut token_iter = list.iter();

    for arg in args {
        match arg {
            // bind the expression to a single variable
            MacroPattern::Single(s) => {
                if let Some(e) = token_iter.next() {
                    bindings.insert(*s, e.clone());
                } else {
                    stop!(ArityMismatch => "macro invocation expected a value, found none");
                }
            }
            // actually check if the syntax matches
            MacroPattern::Syntax(s) => {
                let error_func = throw!(BadSyntax => format!("macro expand expected keyword {s} - within {}", list));

                let e = token_iter.next().ok_or_else(error_func)?;

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
                let rest: Vec<_> = token_iter.cloned().collect();
                bindings.insert(*ident, List::new(rest).into());
                binding_kind.insert(*ident, BindingKind::Many);
                break;
            }
            MacroPattern::Nested(children) => {
                let child = token_iter
                    .next()
                    .ok_or_else(throw!(ArityMismatch => "Macro expected a pattern"))?;

                if let ExprKind::List(l) = child {
                    collect_bindings(children, l, bindings, binding_kind)?;
                } else if let ExprKind::Quote(q) = child {
                    if let &[MacroPattern::Quote(x)] = children.as_slice() {
                        bindings.insert(x, q.expr.clone());
                    } else {
                        stop!(BadSyntax => "macro expected a list of values, 
                            not including keywords, found: {}", child)
                    }
                } else {
                    stop!(BadSyntax => "macro expected a list of values, 
                        not including keywords, found: {}", child)
                }
            }
            MacroPattern::ManyNested(children) => {
                // dbg!(children);

                let exprs_to_destruct = token_iter.collect::<Vec<_>>();

                for i in 0..children.len() {
                    let mut values_to_bind = Vec::new();

                    let is_ellipses_pattern = matches!(&children[i], MacroPattern::Many(_));

                    for expr in &exprs_to_destruct {
                        if let ExprKind::List(l) = expr {
                            // Not what we want to do here!

                            if is_ellipses_pattern {
                                // Bind the "rest" of the values into this
                                values_to_bind.push(List::new(l[i..].to_vec()).into());
                            } else {
                                values_to_bind.push(l[i].clone());
                            }
                        } else {
                            unreachable!()
                        }
                    }

                    match &children[i] {
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
                        MacroPattern::Nested(nested_children) => {
                            let mut final_bindings: HashMap<_, Vec<_>> = HashMap::new();

                            for expr in values_to_bind {
                                let list_expr =
                                    expr.list_or_else(throw!(BadSyntax => "Unreachable!"))?;
                                let mut new_bindings = HashMap::new();
                                collect_bindings(
                                    nested_children,
                                    list_expr,
                                    &mut new_bindings,
                                    binding_kind,
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
                        MacroPattern::ManyNested(_) => {
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
                if let Some(e) = token_iter.next() {
                    if let ExprKind::Quote(inner) = e {
                        bindings.insert(*s, inner.expr.clone());
                    } else {
                        stop!(Generic => "something went wrong with macro expansion")
                    }
                } else {
                    stop!(ArityMismatch => "macro invocation expected a value, found none");
                }
            }

            // Matching on literals
            _ => {
                token_iter.next();
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod match_vec_pattern_tests {
    use super::*;

    fn atom_identifier(s: &str) -> ExprKind {
        ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Identifier(
            s.into(),
        ))))
    }

    fn atom_int(n: isize) -> ExprKind {
        ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::IntegerLiteral(
            MaybeBigInt::Small(n),
        ))))
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

        assert!(match_vec_pattern(&pattern_args, &list_expr));
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
        assert!(match_vec_pattern(&pattern_args, &list_expr));
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
        assert!(match_vec_pattern(&pattern_args, &list_expr));
    }

    #[test]
    fn test_nested() {
        let pattern_args = vec![
            MacroPattern::Syntax("->>".into()),
            MacroPattern::Single("a".into()),
            MacroPattern::Nested(vec![
                MacroPattern::Single("b".into()),
                MacroPattern::Many("c".into()),
            ]),
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
        assert!(match_vec_pattern(&pattern_args, &list_expr));
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

        assert!(!match_vec_pattern(&pattern_args, &list_expr));
    }

    #[test]
    fn test_nested_no_match() {
        let pattern_args = vec![
            MacroPattern::Syntax("->>".into()),
            MacroPattern::Single("a".into()),
            MacroPattern::Single("bad".into()),
            MacroPattern::Nested(vec![
                MacroPattern::Single("b".into()),
                MacroPattern::Many("c".into()),
            ]),
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

        assert!(!match_vec_pattern(&pattern_args, &list_expr));
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
        ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::IntegerLiteral(
            MaybeBigInt::Small(n),
        ))))
    }

    #[test]
    fn test_collect_basic() {
        let mut bindings = HashMap::new();
        let mut binding_kind = HashMap::new();
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

        collect_bindings(&pattern_args, &list_expr, &mut bindings, &mut binding_kind).unwrap();

        let mut post_bindings = HashMap::new();
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
        let mut bindings = HashMap::new();
        let mut binding_kind = HashMap::new();
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

        collect_bindings(&pattern_args, &list_expr, &mut bindings, &mut binding_kind).unwrap();

        let mut post_bindings = HashMap::new();
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
        let mut bindings = HashMap::new();
        let mut binding_kind = HashMap::new();
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

        collect_bindings(&pattern_args, &list_expr, &mut bindings, &mut binding_kind).unwrap();
        let mut post_bindings = HashMap::new();

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
        let mut bindings = HashMap::new();
        let mut binding_kind = HashMap::new();
        // (->> a (b c ...))
        let pattern_args = vec![
            MacroPattern::Syntax("->>".into()),
            MacroPattern::Single("a".into()),
            MacroPattern::Nested(vec![
                MacroPattern::Single("b".into()),
                MacroPattern::Many("c".into()),
            ]),
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

        let mut post_bindings = HashMap::new();

        collect_bindings(&pattern_args, &list_expr, &mut bindings, &mut binding_kind).unwrap();
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

#[cfg(test)]
mod macro_case_expand_test {
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
        ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::IntegerLiteral(
            MaybeBigInt::Small(n),
        ))))
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

        let output = case.expand(input, Span::new(0, 0, None)).unwrap();

        assert_eq!(output, expected);
    }
}
