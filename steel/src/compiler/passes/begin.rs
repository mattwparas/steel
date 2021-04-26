use crate::parser::ast::{Atom, Begin, Define, ExprKind, LambdaFunction};
use crate::parser::tokens::TokenType;
use std::collections::HashSet;

use super::{Folder, VisitorMutUnit};

struct FlattenBegin {}
impl FlattenBegin {
    fn flatten(expr: ExprKind) -> ExprKind {
        FlattenBegin {}.visit(expr)
    }
}

impl Folder for FlattenBegin {
    #[inline]
    fn visit_begin(&mut self, begin: Begin) -> ExprKind {
        let span = begin.location;

        // Flatten begins
        let flattened_exprs = begin
            .exprs
            .into_iter()
            .map(|x| {
                if let ExprKind::Begin(b) = x {
                    b.exprs
                        .into_iter()
                        .map(|x| self.visit(x))
                        .collect::<Vec<_>>()
                } else {
                    vec![x]
                }
            })
            .flatten()
            .collect::<Vec<_>>();

        ExprKind::Begin(Begin::new(flattened_exprs, span))
    }
}

struct MergeDefines {
    referenced_identifiers: HashSet<String>,
}

impl MergeDefines {
    fn new() -> Self {
        MergeDefines {
            referenced_identifiers: HashSet::new(),
        }
    }

    fn insert(&mut self, value: &str) {
        self.referenced_identifiers.insert(value.to_string());
    }

    fn get(&mut self, value: &str) -> Option<&str> {
        self.referenced_identifiers.get(value).map(|x| x.as_str())
    }
}

impl VisitorMutUnit for MergeDefines {
    fn visit_atom(&mut self, a: &Atom) {
        if let TokenType::Identifier(ident) = &a.syn.ty {
            self.referenced_identifiers.insert(ident.clone());
        }
    }
}

struct DefinedVars<'a> {
    defined_identifiers: &'a [&'a str],
}

impl<'a> DefinedVars<'a> {
    fn new(defined_identifiers: &'a [&'a str]) -> Self {
        DefinedVars {
            defined_identifiers,
        }
    }
}

// Want to take the highest precedence form. For each of these, we can fold
// the expressions into themselves. If theres a mutual reference, turn everything into a letrec form.
// If there are no functions with no mutual references,
enum IdentifierReferenceType {
    // A function references itself - forced to be a letrec
    FuncSelfReference,
    // A function references another variable defined in the scope - combine with letrec
    FuncMutualReference,
    // A variable references no other variable in the scope - normal let
    FlatNoReference,
    // A variable references a variable defined prior in the scope - coalesce with prev define if possible
    FlatPriorReference,
}

// Snag the names from the defines for the current (flattened) begin statement
fn collect_defines_from_current_scope<'a>(begin_exprs: &'a [ExprKind]) -> Vec<&'a str> {
    let mut names = Vec::new();
    for expr in begin_exprs {
        match expr {
            ExprKind::Define(d) => {
                let name = d
                    .name
                    .atom_identifier_or_else(|| {})
                    .expect("Define without a legal name");

                names.push(name);
            }
            _ => {}
        }
    }
    names
}

// enum ExprTypeState {
//     FlatDefineToLet(ExprKind),
//     FlatDefineToLetStar(ExprKind),
//     LetRecSelfRef(ExprKind),
//     LetRecMutualRef(ExprKind),
// }

// See if we need to keep track of any of the local variables
fn merge_defines(exprs: Vec<ExprKind>) -> Begin {
    let mut let_rec_exprs: Vec<ExprKind> = Vec::new();

    let defines = collect_defines_from_current_scope(&exprs);

    for expr in &exprs {
        match expr {
            ExprKind::Define(d) => {
                let name = d
                    .name
                    .atom_identifier_or_else(|| {})
                    .expect("Define without a legal name");

                match &d.body {
                    ExprKind::LambdaFunction(l) => {
                        let mut merge_defines = MergeDefines::new();
                        merge_defines.visit(&l.body);
                    }
                    _ => {
                        unimplemented!()
                    }
                }
            }
            _ => {}
        }
    }

    unimplemented!()
}

#[cfg(test)]
mod flatten_begin_test {

    use super::*;
    use crate::parser::ast::ExprKind;
    use crate::parser::ast::{Atom, Begin, List};

    use crate::parser::parser::SyntaxObject;
    use crate::parser::tokens::TokenType;
    use crate::parser::tokens::TokenType::*;

    #[test]
    fn basic_flatten_one_level() {
        let expr = ExprKind::Begin(Begin::new(
            vec![
                ExprKind::Begin(Begin::new(
                    vec![ExprKind::List(List::new(vec![
                        ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                            "+".to_string(),
                        )))),
                        ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                            "x".to_string(),
                        )))),
                        ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(10)))),
                    ]))],
                    SyntaxObject::default(TokenType::Begin),
                )),
                ExprKind::List(List::new(vec![
                    ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                        "+".to_string(),
                    )))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                        "y".to_string(),
                    )))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(20)))),
                ])),
                ExprKind::List(List::new(vec![
                    ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                        "+".to_string(),
                    )))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                        "z".to_string(),
                    )))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(30)))),
                ])),
            ],
            SyntaxObject::default(TokenType::Begin),
        ));

        let expected = ExprKind::Begin(Begin::new(
            vec![
                ExprKind::List(List::new(vec![
                    ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                        "+".to_string(),
                    )))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                        "x".to_string(),
                    )))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(10)))),
                ])),
                ExprKind::List(List::new(vec![
                    ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                        "+".to_string(),
                    )))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                        "y".to_string(),
                    )))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(20)))),
                ])),
                ExprKind::List(List::new(vec![
                    ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                        "+".to_string(),
                    )))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                        "z".to_string(),
                    )))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(30)))),
                ])),
            ],
            SyntaxObject::default(TokenType::Begin),
        ));

        assert_eq!(FlattenBegin::flatten(expr), expected);
    }

    #[test]
    fn basic_flatten_multiple_levels() {
        let expr = ExprKind::Begin(Begin::new(
            vec![
                ExprKind::Begin(Begin::new(
                    vec![ExprKind::List(List::new(vec![
                        ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                            "+".to_string(),
                        )))),
                        ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                            "x".to_string(),
                        )))),
                        ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(10)))),
                    ]))],
                    SyntaxObject::default(TokenType::Begin),
                )),
                ExprKind::Begin(Begin::new(
                    vec![ExprKind::List(List::new(vec![
                        ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                            "+".to_string(),
                        )))),
                        ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                            "y".to_string(),
                        )))),
                        ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(20)))),
                    ]))],
                    SyntaxObject::default(TokenType::Begin),
                )),
                ExprKind::Begin(Begin::new(
                    vec![ExprKind::List(List::new(vec![
                        ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                            "+".to_string(),
                        )))),
                        ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                            "z".to_string(),
                        )))),
                        ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(30)))),
                    ]))],
                    SyntaxObject::default(TokenType::Begin),
                )),
            ],
            SyntaxObject::default(TokenType::Begin),
        ));

        let expected = ExprKind::Begin(Begin::new(
            vec![
                ExprKind::List(List::new(vec![
                    ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                        "+".to_string(),
                    )))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                        "x".to_string(),
                    )))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(10)))),
                ])),
                ExprKind::List(List::new(vec![
                    ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                        "+".to_string(),
                    )))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                        "y".to_string(),
                    )))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(20)))),
                ])),
                ExprKind::List(List::new(vec![
                    ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                        "+".to_string(),
                    )))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                        "z".to_string(),
                    )))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(30)))),
                ])),
            ],
            SyntaxObject::default(TokenType::Begin),
        ));

        assert_eq!(FlattenBegin::flatten(expr), expected);
    }
}
