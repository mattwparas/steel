use crate::parser::ast::{Atom, ExprKind, List, Macro, PatternPair};
use crate::parser::parser::SyntaxObject;
use crate::parser::rename_idents::RenameIdentifiersVisitor;
use crate::parser::replace_idents::replace_identifiers;
use crate::parser::tokens::TokenType;
// use crate::parser::visitors::VisitorMutRef;

use crate::parser::span::Span;

use crate::rerrs::SteelErr;
use crate::rvals::Result;
use std::collections::HashMap;

use log::{debug, error, info};

// use std::convert::TryFrom;

#[derive(Clone, Debug, PartialEq)]
pub struct SteelMacro {
    name: String,
    special_forms: Vec<String>,
    cases: Vec<MacroCase>,
}

impl SteelMacro {
    pub fn new(name: String, special_forms: Vec<String>, cases: Vec<MacroCase>) -> Self {
        SteelMacro {
            name,
            special_forms,
            cases,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn parse_from_ast_macro(ast_macro: Macro) -> Result<Self> {
        let name = ast_macro
            .name
            .atom_identifier_or_else(
                throw!(BadSyntax => "macros only currently support identifiers as the name"),
            )?
            .to_string();

        let special_forms = ast_macro
            .syntax_rules
            .syntax
            .into_iter()
            .map(|x| {
                x.atom_identifier_or_else(
                    throw!(BadSyntax => "macros only support identifiers for special syntax"),
                )
                .map(|x| x.to_string())
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
        })
    }

    fn match_case(&self, expr: &List) -> Result<&MacroCase> {
        // todo!();

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

        stop!(BadSyntax => "macro expansion unable to match case");
    }

    pub fn expand(&self, expr: List, span: Span) -> Result<ExprKind> {
        debug!("Expanding macro with tokens: {}", expr);

        let case_to_expand = self.match_case(&expr)?;
        let expanded_expr = case_to_expand.expand(expr, span)?;

        info!("Macro Expansion: {}", expanded_expr);

        Ok(expanded_expr)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct MacroCase {
    args: Vec<MacroPattern>,
    body: ExprKind,
}

// impl TryFrom<PatternPair> for MacroCase {
//     type Error = SteelErr;

//     fn try_from(value: PatternPair) -> std::result::Result<Self, Self::Error> {
//         unimplemented!();
//     }
// }

impl MacroCase {
    pub fn new(args: Vec<MacroPattern>, body: ExprKind) -> Self {
        MacroCase { args, body }
    }

    fn parse_from_pattern_pair(
        pattern_pair: PatternPair,
        name: &str,
        special_forms: &[String],
    ) -> Result<Self> {
        let PatternPair { pattern, mut body } = pattern_pair;

        let args = if let ExprKind::List(l) = pattern {
            MacroPattern::parse_from_list(l, name, special_forms)?
        } else {
            stop!(Generic => "unable to parse macro");
        };

        let args_str: Vec<&str> = args.iter().map(|x| x.deconstruct()).flatten().collect();

        RenameIdentifiersVisitor::new(&args_str).rename_identifiers(&mut body);

        Ok(MacroCase { args, body })
    }

    fn has_ellipses(&self) -> bool {
        self.args.iter().any(|x| matches!(x, MacroPattern::Many(_)))
    }

    fn arity(&self) -> usize {
        self.args
            .iter()
            .map(|x| if let MacroPattern::Many(_) = x { 2 } else { 1 })
            .sum()
    }

    fn recursive_match(&self, list: &List) -> bool {
        match_vec_pattern(&self.args, list)
    }

    fn expand(&self, expr: List, span: Span) -> Result<ExprKind> {
        let mut bindings = HashMap::new();
        collect_bindings(&self.args, &expr, &mut bindings)?;
        replace_identifiers(self.body.clone(), &bindings, span)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum MacroPattern {
    Single(String),
    Syntax(String),
    Many(String),
    Nested(Vec<MacroPattern>),
}

impl MacroPattern {
    fn parse_from_list(
        list: List,
        macro_name: &str,
        special_forms: &[String],
    ) -> Result<Vec<Self>> {
        let mut pattern_vec: Vec<MacroPattern> = Vec::new();
        let mut peek_token_iter = list.args.into_iter().peekable();

        while let Some(token) = peek_token_iter.next() {
            match token {
                ExprKind::Atom(Atom {
                    syn: SyntaxObject { ty: s, .. },
                }) => {
                    if let TokenType::Identifier(t) = s {
                        if t == macro_name || special_forms.contains(&t) {
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
                    } else {
                        stop!(BadSyntax => "syntax-rules requires identifiers in the pattern");
                    }
                }
                ExprKind::List(l) => pattern_vec.push(MacroPattern::Nested(Self::parse_from_list(
                    l,
                    macro_name,
                    special_forms,
                )?)),
                _ => {
                    stop!(BadSyntax => "illegal special form found in macro pattern");
                }
            }
        }
        Ok(pattern_vec)
    }
}

impl MacroPattern {
    // TODO make this not so trash
    pub fn deconstruct(&self) -> Vec<&str> {
        match self {
            Self::Syntax(s) => vec![&s],
            Self::Single(s) => vec![&s],
            Self::Many(s) => vec![&s],
            Self::Nested(v) => v.iter().map(|x| x.deconstruct()).flatten().collect(),
        }
    }

    pub fn is_many(&self) -> bool {
        if let MacroPattern::Many(_) = self {
            true
        } else {
            false
        }
    }
}

pub fn match_vec_pattern(args: &[MacroPattern], list: &List) -> bool {
    let mut token_iter = list.iter();

    for pat in args {
        if let Some(val) = token_iter.next() {
            match pat {
                MacroPattern::Single(_) | MacroPattern::Many(_) => continue,
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
                MacroPattern::Nested(vec) => {
                    if let ExprKind::List(l) = val {
                        // TODO more elegant let* case
                        if vec.is_empty() && !l.is_empty() {
                            return false;
                        }

                        // This solves the destructuring test case
                        if vec.len() < l.len() && vec.iter().find(|x| x.is_many()).is_none() {
                            debug!("Matching failed - ellipses doesn't match up");
                            return false;
                        }

                        // Make the recursive call on the next layer
                        if match_vec_pattern(&vec, l) {
                            continue;
                        } else {
                            debug!("Matching failed due to child not matching");
                            return false;
                        }
                    } else {
                        debug!("Matching failed - atom does not match list");
                        return false;
                    }
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

pub fn collect_bindings(
    args: &[MacroPattern],
    list: &List,
    bindings: &mut HashMap<String, ExprKind>,
) -> Result<()> {
    let mut token_iter = list.iter();

    for arg in args {
        match arg {
            // bind the expression to a single variable
            MacroPattern::Single(s) => {
                if let Some(e) = token_iter.next() {
                    bindings.insert(s.to_string(), e.clone());
                } else {
                    stop!(ArityMismatch => "macro invocation expected a value, found none");
                }
            }
            // actually check if the syntax matches
            MacroPattern::Syntax(s) => {
                let e = token_iter.next().ok_or_else(
                    throw!(BadSyntax => format!("macro expand expected keyword {}", s)),
                )?;

                if let ExprKind::Atom(Atom {
                    syn:
                        SyntaxObject {
                            ty: TokenType::Identifier(syn),
                            ..
                        },
                }) = e
                {
                    if s != syn {
                        stop!(BadSyntax => "macro expansion expected keyword")
                    }
                }
            }
            // bind the ellipses to the rest of the statement
            MacroPattern::Many(ident) => {
                let rest: Vec<_> = token_iter.cloned().collect();
                bindings.insert(ident.to_string(), List::new(rest).into());
                break;
            }
            MacroPattern::Nested(children) => {
                let child = token_iter
                    .next()
                    .ok_or_else(throw!(ArityMismatch => "Macro expected a pattern"))?;

                if let ExprKind::List(l) = child {
                    collect_bindings(&children, l, bindings)?;
                } else {
                    stop!(BadSyntax => "macro expected a list of values, not including keywords")
                }
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
            s.to_string(),
        ))))
    }

    fn atom_int(n: isize) -> ExprKind {
        ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::IntegerLiteral(
            n,
        ))))
    }

    #[test]
    fn test_match_basic() {
        let pattern_args = vec![
            MacroPattern::Syntax("and".to_string()),
            MacroPattern::Single("a".to_string()),
            MacroPattern::Single("b".to_string()),
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
            MacroPattern::Syntax("and".to_string()),
            MacroPattern::Single("a".to_string()),
            MacroPattern::Many("b".to_string()),
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
            MacroPattern::Syntax("and".to_string()),
            MacroPattern::Single("a".to_string()),
            MacroPattern::Many("b".to_string()),
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
            MacroPattern::Syntax("->>".to_string()),
            MacroPattern::Single("a".to_string()),
            MacroPattern::Nested(vec![
                MacroPattern::Single("b".to_string()),
                MacroPattern::Many("c".to_string()),
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
            MacroPattern::Syntax("->>".to_string()),
            MacroPattern::Single("a".to_string()),
            MacroPattern::Single("bad".to_string()),
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
            MacroPattern::Syntax("->>".to_string()),
            MacroPattern::Single("a".to_string()),
            MacroPattern::Single("bad".to_string()),
            MacroPattern::Nested(vec![
                MacroPattern::Single("b".to_string()),
                MacroPattern::Many("c".to_string()),
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
            s.to_string(),
        ))))
    }

    fn atom_int(n: isize) -> ExprKind {
        ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::IntegerLiteral(
            n,
        ))))
    }

    #[test]
    fn test_collect_basic() {
        let mut bindings = HashMap::new();
        let pattern_args = vec![
            MacroPattern::Syntax("and".to_string()),
            MacroPattern::Single("a".to_string()),
            MacroPattern::Single("b".to_string()),
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

        collect_bindings(&pattern_args, &list_expr, &mut bindings).unwrap();

        let mut post_bindings = HashMap::new();
        post_bindings.insert(
            "a".to_string(),
            ExprKind::List(List::new(vec![
                atom_identifier("+"),
                atom_identifier("x"),
                atom_identifier("y"),
            ])),
        );

        post_bindings.insert(
            "b".to_string(),
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
        let pattern_args = vec![
            MacroPattern::Syntax("and".to_string()),
            MacroPattern::Single("a".to_string()),
            MacroPattern::Many("b".to_string()),
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

        collect_bindings(&pattern_args, &list_expr, &mut bindings).unwrap();

        let mut post_bindings = HashMap::new();
        post_bindings.insert(
            "a".to_string(),
            ExprKind::List(List::new(vec![
                atom_identifier("+"),
                atom_identifier("x"),
                atom_identifier("y"),
            ])),
        );
        post_bindings.insert(
            "b".to_string(),
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
        let pattern_args = vec![
            MacroPattern::Syntax("and".to_string()),
            MacroPattern::Single("a".to_string()),
            MacroPattern::Many("b".to_string()),
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

        collect_bindings(&pattern_args, &list_expr, &mut bindings).unwrap();
        let mut post_bindings = HashMap::new();

        post_bindings.insert(
            "a".to_string(),
            ExprKind::List(List::new(vec![
                atom_identifier("+"),
                atom_identifier("x"),
                atom_identifier("y"),
            ])),
        );

        post_bindings.insert(
            "b".to_string(),
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
        // (->> a (b c ...))
        let pattern_args = vec![
            MacroPattern::Syntax("->>".to_string()),
            MacroPattern::Single("a".to_string()),
            MacroPattern::Nested(vec![
                MacroPattern::Single("b".to_string()),
                MacroPattern::Many("c".to_string()),
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

        collect_bindings(&pattern_args, &list_expr, &mut bindings).unwrap();
        post_bindings.insert("a".to_string(), atom_int(1));
        post_bindings.insert("b".to_string(), atom_identifier("apple"));
        post_bindings.insert(
            "c".to_string(),
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
            s.to_string(),
        ))))
    }

    fn atom_int(n: isize) -> ExprKind {
        ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::IntegerLiteral(
            n,
        ))))
    }

    #[test]
    fn test_basic_expansion() {
        let case = MacroCase {
            args: vec![
                MacroPattern::Syntax("test".to_string()),
                MacroPattern::Single("a".to_string()),
                MacroPattern::Single("b".to_string()),
                MacroPattern::Single("c".to_string()),
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

        let output = case.expand(input, Span::new(0, 0)).unwrap();

        assert_eq!(output, expected);
    }
}
