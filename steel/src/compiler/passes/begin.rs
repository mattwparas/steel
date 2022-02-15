use crate::parser::{ast::Let, tokens::TokenType};
use crate::parser::{
    ast::{Atom, Begin, ExprKind, LambdaFunction, List, Set},
    parser::SyntaxObject,
};
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

        if begin.exprs.len() == 1 {
            return self.visit(begin.exprs.into_iter().next().unwrap());
        }

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
                    vec![self.visit(x)]
                }
            })
            .flatten()
            .collect::<Vec<_>>();

        if flattened_exprs.len() == 1 {
            flattened_exprs.into_iter().next().unwrap()
        } else {
            ExprKind::Begin(Begin::new(flattened_exprs, span))
        }
    }
}

pub fn flatten_begins_and_expand_defines(exprs: Vec<ExprKind>) -> Vec<ExprKind> {
    // println!("###################################################");
    exprs
        .into_iter()
        .map(FlattenBegin::flatten)
        .map(ConvertDefinesToLets::convert_defines)
        .collect()
}

struct DefinedVars<'a> {
    defined_identifiers: HashSet<&'a str>,
    output: bool,
}

impl<'a> DefinedVars<'a> {
    fn new() -> Self {
        DefinedVars {
            defined_identifiers: HashSet::new(),
            output: false,
        }
    }

    fn insert(&mut self, name: &'a str) {
        self.defined_identifiers.insert(name);
    }

    fn check_output(&mut self) -> bool {
        let output = self.output;
        self.output = false;
        output
    }
}

impl<'a> VisitorMutUnit for DefinedVars<'a> {
    fn visit_atom(&mut self, a: &Atom) {
        if let TokenType::Identifier(ident) = &a.syn.ty {
            self.output = self.defined_identifiers.contains(ident.as_str()) || self.output;
        }
    }
}

struct ConvertDefinesToLets {
    depth: usize,
}

impl ConvertDefinesToLets {
    fn new() -> Self {
        Self { depth: 0 }
    }

    fn convert_defines(expr: ExprKind) -> ExprKind {
        ConvertDefinesToLets::new().visit(expr)
    }
}

impl Folder for ConvertDefinesToLets {
    #[inline]
    fn visit_lambda_function(&mut self, mut lambda_function: Box<LambdaFunction>) -> ExprKind {
        self.depth += 1;
        lambda_function.body = self.visit(lambda_function.body);
        self.depth -= 1;
        ExprKind::LambdaFunction(lambda_function)
    }

    // TODO
    #[inline]
    fn visit_begin(&mut self, mut begin: Begin) -> ExprKind {
        if self.depth > 0 {
            match convert_exprs_to_let(begin) {
                ExprKind::Begin(mut b) => {
                    b.exprs = b.exprs.into_iter().map(|e| self.visit(e)).collect();
                    ExprKind::Begin(b)
                }
                ExprKind::List(mut l) => {
                    l.args = l.args.into_iter().map(|x| self.visit(x)).collect();
                    ExprKind::List(l)
                }
                ExprKind::Let(mut l) => {
                    let mut visited_bindings = Vec::new();

                    for (binding, expr) in l.bindings {
                        visited_bindings.push((self.visit(binding), self.visit(expr)));
                    }

                    l.bindings = visited_bindings;
                    l.body_expr = self.visit(l.body_expr);

                    ExprKind::Let(l)
                }
                _ => panic!("Something went wrong in define conversion"),
            }
        } else {
            // println!("Ignoring begin");
            begin.exprs = begin.exprs.into_iter().map(|e| self.visit(e)).collect();
            ExprKind::Begin(begin)
        }
    }
}

#[derive(PartialEq)]
enum ExpressionType<'a> {
    DefineConst(&'a str),
    DefineFlat(&'a str),
    DefineFlatStar(&'a str),
    DefineFunction(&'a str),
    Expression,
}

impl<'a> ExpressionType<'a> {
    fn is_expression(&self) -> bool {
        if let ExpressionType::Expression = self {
            true
        } else {
            false
        }
    }

    fn eval_atom(t: &SyntaxObject) -> bool {
        match &t.ty {
            TokenType::BooleanLiteral(_)
            | TokenType::NumberLiteral(_)
            | TokenType::StringLiteral(_)
            | TokenType::CharacterLiteral(_)
            | TokenType::IntegerLiteral(_) => true,
            _ => false,
        }
    }

    fn is_constant(expr: &'a ExprKind) -> bool {
        match expr {
            ExprKind::Atom(Atom { syn, .. }) => Self::eval_atom(syn),
            _ => false,
        }
    }

    fn generate_expression_types(exprs: &'a [ExprKind]) -> Vec<ExpressionType<'a>> {
        let mut expression_types = Vec::with_capacity(exprs.len());
        let mut defined_idents = DefinedVars::new();

        for expr in exprs {
            match expr {
                ExprKind::Define(d) => {
                    let name = d
                        .name
                        .atom_identifier_or_else(|| {})
                        .expect("Define without a legal name");

                    defined_idents.insert(name);

                    match &d.body {
                        ExprKind::LambdaFunction(_) => {
                            expression_types.push(ExpressionType::DefineFunction(name));
                        }
                        _ => {
                            defined_idents.visit(&d.body);
                            if defined_idents.check_output() {
                                expression_types.push(ExpressionType::DefineFlatStar(name));
                            } else {
                                if Self::is_constant(&d.body) {
                                    expression_types.push(ExpressionType::DefineConst(name));
                                } else {
                                    expression_types.push(ExpressionType::DefineFlat(name));
                                }
                            }
                        }
                    }
                }
                _ => expression_types.push(ExpressionType::Expression),
            }
        }

        expression_types
    }
}

fn atom(name: String) -> ExprKind {
    ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Identifier(
        name,
    ))))
}

fn set(var: ExprKind, expr: ExprKind) -> ExprKind {
    ExprKind::Set(Box::new(Set::new(
        var,
        expr,
        SyntaxObject::default(TokenType::Set),
    )))
}

fn apply_ident(func: ExprKind) -> ExprKind {
    ExprKind::List(List::new(vec![func]))
}

fn convert_exprs_to_let(begin: Begin) -> ExprKind {
    // let defines = collect_defines_from_current_scope(&exprs);

    let expression_types = ExpressionType::generate_expression_types(&begin.exprs);

    // Go ahead and quit if there are
    if expression_types.iter().all(|x| x.is_expression()) {
        return ExprKind::Begin(begin);
    }

    let mut exprs = begin.exprs.clone();

    // let mut last_expression = expression_types.len();

    let idx = expression_types
        .iter()
        .rev()
        .position(|x| !x.is_expression())
        .expect("Convert exprs to let in define conversion found no trailing expressions in begin");

    let idx = expression_types.len() - 1 - idx;

    let mut body = exprs.split_off(idx + 1);

    // These are going to be the
    let args = exprs
        .iter()
        .map(|x| {
            if let ExprKind::Define(d) = x {
                d.body.clone()
            } else {
                x.clone()
            }
        })
        .collect::<Vec<_>>();

    // This corresponds to the (let ((apple ..) (banana ..) (cucumber ..)))
    //                               ^^^^^^     ^^^^^^^      ^^^^^^^^
    let mut top_level_arguments: Vec<ExprKind> = Vec::new();

    // This corresponds to the set expressions
    // (set! apple #####apple0)
    // (set! banana #####banana1)
    // (set! cucumber #####cucumber1)
    let mut set_expressions: Vec<ExprKind> = Vec::new();

    // corresponds to #####apple0, #####banana1, #####cucumber1, etc
    let mut bound_names: Vec<ExprKind> = Vec::new();

    // TODO - check that the last expression does not contain any usages of the constant?
    // if expression_types[0..idx + 1]
    //     .iter()
    //     .all(|x| matches!(x, ExpressionType::DefineConst(_)))
    // {
    //     return ExprKind::Begin(Begin::new(body, begin.location));
    // }

    // Top level application with dummy arguments that will immediately get overwritten
    let mut top_level_dummy_args = vec![
        // ExprKind::Atom(Atom::new(SyntaxObject::default(
        //     TokenType::IntegerLiteral(123)
        // )));
        // top_level_arguments.len()
    ];

    let mut new_args = Vec::new();

    for ((i, expression), arg) in expression_types[0..idx + 1]
        .into_iter()
        .enumerate()
        .zip(args)
    {
        match expression {
            ExpressionType::DefineFunction(name) => {
                if let ExprKind::Define(d) = &exprs[i] {
                    top_level_arguments.push(d.name.clone());
                    top_level_dummy_args.push(ExprKind::Atom(Atom::new(SyntaxObject::default(
                        TokenType::IntegerLiteral(123),
                    ))));
                    let name_prime = atom("#####".to_string() + name + i.to_string().as_str());
                    let set_expr = set(d.name.clone(), name_prime.clone());
                    bound_names.push(name_prime);
                    set_expressions.push(set_expr);
                    new_args.push(arg);
                } else {
                    panic!("expected define, found: {}", &exprs[i]);
                };

                // let name = Atom::new(SyntaxObject::new)
            }
            ExpressionType::DefineFlat(name) => {
                if let ExprKind::Define(d) = &exprs[i] {
                    top_level_arguments.push(d.name.clone());
                    top_level_dummy_args.push(ExprKind::Atom(Atom::new(SyntaxObject::default(
                        TokenType::IntegerLiteral(123),
                    ))));
                    let name_prime = atom("#####".to_string() + name + i.to_string().as_str());
                    let set_expr = set(d.name.clone(), name_prime.clone());
                    bound_names.push(name_prime);
                    set_expressions.push(set_expr);
                    new_args.push(arg);
                } else {
                    panic!("expected define, found: {}", &exprs[i]);
                };
            }
            ExpressionType::DefineConst(_name) => {
                if let ExprKind::Define(d) = &exprs[i] {
                    top_level_dummy_args.push(arg);
                    top_level_arguments.push(d.name.clone());
                    // top_level_arguments.push(d.name.clone());
                    // let name_prime = atom("#####".to_string() + name + i.to_string().as_str());
                    // let set_expr = set(d.name.clone(), name_prime.clone());
                    // bound_names.push(name_prime);
                    // set_expressions.push(set_expr);
                } else {
                    panic!("expected define, found: {}", &exprs[i]);
                };
            }
            ExpressionType::DefineFlatStar(name) => {
                if let ExprKind::Define(d) = &exprs[i] {
                    top_level_arguments.push(d.name.clone());
                    top_level_dummy_args.push(ExprKind::Atom(Atom::new(SyntaxObject::default(
                        TokenType::IntegerLiteral(123),
                    ))));
                    let name_prime = atom("#####".to_string() + name + i.to_string().as_str());

                    // Make this a (set! x (x'))
                    // Applying the function
                    let set_expr = set(d.name.clone(), apply_ident(name_prime.clone()));

                    // Set this to be an empty function (lambda () <expr>)
                    new_args.push(
                        LambdaFunction::new(
                            Vec::new(),
                            arg.clone(),
                            SyntaxObject::default(TokenType::Lambda),
                        )
                        .into(),
                    );

                    bound_names.push(name_prime);
                    set_expressions.push(set_expr);
                } else {
                    panic!("expected define, found: {}", &exprs[i]);
                };
            }
            ExpressionType::Expression => {
                let expr = atom("#####define-conversion".to_string() + i.to_string().as_str());
                top_level_dummy_args.push(ExprKind::Atom(Atom::new(SyntaxObject::default(
                    TokenType::IntegerLiteral(123),
                ))));

                // This also gets bound in the inner function for now
                bound_names.push(expr.clone());

                top_level_arguments.push(expr);
                new_args.push(arg);
            }
        }
    }

    // // Append the body instructions to the set!
    set_expressions.append(&mut body);

    let inner_lambda = LambdaFunction::new(
        bound_names,
        ExprKind::Begin(Begin::new(
            set_expressions,
            SyntaxObject::default(TokenType::Begin),
        )),
        SyntaxObject::default(TokenType::Lambda),
    );

    new_args.insert(0, ExprKind::LambdaFunction(Box::new(inner_lambda)));

    let inner_application = ExprKind::List(List::new(new_args));

    let outer_lambda = LambdaFunction::new(
        top_level_arguments,
        inner_application,
        SyntaxObject::default(TokenType::Lambda),
    );

    top_level_dummy_args.insert(0, ExprKind::LambdaFunction(Box::new(outer_lambda)));

    ExprKind::List(List::new(top_level_dummy_args))

    // TODO: This is the real transformation that needs to take place once lets are fixed
    // follow-up - let rec is going to be completely broken

    // let pairs = bound_names
    //     .into_iter()
    //     .zip(new_args.into_iter())
    //     .collect::<Vec<_>>();

    // let inner_let = Let::new(
    //     pairs,
    //     ExprKind::Begin(Begin::new(
    //         set_expressions,
    //         SyntaxObject::default(TokenType::Begin),
    //     )),
    //     SyntaxObject::default(TokenType::Let),
    // );

    // let outer_pairs = top_level_arguments
    //     .into_iter()
    //     .zip(top_level_dummy_args.into_iter())
    //     .collect::<Vec<_>>();

    // let outer_let = Let::new(
    //     outer_pairs,
    //     ExprKind::Let(Box::new(inner_let)),
    //     SyntaxObject::default(TokenType::Let),
    // );

    // let output = ExprKind::Let(Box::new(outer_let));

    // println!("-----------------");
    // println!("{}", output.to_pretty(60));

    // output
}

#[cfg(test)]
mod flatten_begin_test {

    use super::*;
    use crate::parser::ast::ExprKind;
    use crate::parser::ast::{Atom, Begin, List};

    use crate::parser::parser::SyntaxObject;
    use crate::parser::tokens::TokenType;
    use crate::parser::tokens::TokenType::*;

    use crate::parser::parser::Parser;

    #[test]
    fn defines_translates_to_simple_let() {
        let expr = r#"
        (lambda () 
            (begin 
                (define x 10)
                (define y 20)
                (define z 30)
                (+ x y z)))"#;

        let expected = r#"
        (lambda () ((lambda (x y z) ((lambda () (begin (+ x y z))))) 10 20 30))
        "#;

        let parsed = Parser::parse(expr).unwrap();
        let expected_parsed = Parser::parse(expected).unwrap();

        let result = flatten_begins_and_expand_defines(parsed);

        assert_eq!(result, expected_parsed);
    }

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
