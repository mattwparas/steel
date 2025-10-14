use smallvec::SmallVec;
use steel_parser::{
    ast::{Define, If, Let, Macro, Quote, Require, Return, SyntaxRules, Vector},
    tokens::IntLiteral,
};

use crate::parser::{
    ast::{Atom, Begin, ExprKind, LambdaFunction, List, Set},
    parser::SyntaxObject,
    visitors::VisitorMutRef,
};
use crate::parser::{interner::InternedString, tokens::TokenType};

#[cfg(feature = "profiling")]
use std::time::Instant;

use super::{Folder, VisitorMutRefUnit, VisitorMutUnit};

pub(crate) struct CheckDefinesAreInLegalPositions {
    depth: usize,
}
impl VisitorMutRef for CheckDefinesAreInLegalPositions {
    type Output = crate::rvals::Result<()>;

    #[inline]
    fn visit_lambda_function(&mut self, lambda_function: &mut LambdaFunction) -> Self::Output {
        for var in &mut lambda_function.args {
            self.visit(var)?;
        }
        self.depth += 1;
        self.visit(&mut lambda_function.body)?;
        self.depth -= 1;
        Ok(())
    }

    #[inline]
    fn visit_if(&mut self, f: &mut If) -> Self::Output {
        self.depth += 1;
        self.visit(&mut f.test_expr)?;
        self.visit(&mut f.then_expr)?;
        self.visit(&mut f.else_expr)?;
        self.depth -= 1;

        Ok(())
    }

    #[inline]
    fn visit_let(&mut self, l: &mut Let) -> Self::Output {
        self.depth += 1;

        for x in l.bindings.iter_mut() {
            self.visit(&mut x.1)?;
        }

        self.visit(&mut l.body_expr)?;
        self.depth -= 1;

        Ok(())
    }

    #[inline]
    fn visit_define(&mut self, define: &mut Define) -> Self::Output {
        if self.depth != 0 {
            crate::stop!(BadSyntax => "Define cannot exist except at the top level, unless within another lexical context or begin expression"; define.location.span);
        }

        self.visit(&mut define.name)?;
        self.visit(&mut define.body)?;

        Ok(())
    }

    fn visit_begin(&mut self, begin: &mut Begin) -> Self::Output {
        for expr in &mut begin.exprs {
            self.visit(expr)?;
        }

        Ok(())
    }

    fn visit(&mut self, expr: &mut ExprKind) -> Self::Output {
        match expr {
            ExprKind::If(f) => self.visit_if(f),
            ExprKind::Define(d) => self.visit_define(d),
            ExprKind::LambdaFunction(l) => self.visit_lambda_function(l),
            ExprKind::Begin(b) => self.visit_begin(b),
            ExprKind::Return(r) => self.visit_return(r),
            ExprKind::Quote(q) => self.visit_quote(q),
            ExprKind::Macro(m) => self.visit_macro(m),
            ExprKind::Atom(a) => self.visit_atom(a),
            ExprKind::List(l) => self.visit_list(l),
            ExprKind::SyntaxRules(s) => self.visit_syntax_rules(s),
            ExprKind::Set(s) => self.visit_set(s),
            ExprKind::Require(r) => self.visit_require(r),
            ExprKind::Let(l) => self.visit_let(l),
            ExprKind::Vector(v) => self.visit_vector(v),
        }
    }

    #[inline]
    fn visit_return(&mut self, r: &mut Return) -> Self::Output {
        self.visit(&mut r.expr)?;
        Ok(())
    }

    #[inline]
    fn visit_quote(&mut self, _quote: &mut Quote) -> Self::Output {
        // self.visit(&mut quote.expr)?;
        Ok(())
    }

    #[inline]
    fn visit_macro(&mut self, _m: &mut Macro) -> Self::Output {
        Ok(())
    }

    #[inline]
    fn visit_atom(&mut self, _a: &mut Atom) -> Self::Output {
        Ok(())
    }

    #[inline]
    fn visit_list(&mut self, l: &mut List) -> Self::Output {
        for expr in &mut l.args {
            self.visit(expr)?;
        }
        Ok(())
    }

    #[inline]
    fn visit_syntax_rules(&mut self, _l: &mut SyntaxRules) -> Self::Output {
        Ok(())
    }

    #[inline]
    fn visit_set(&mut self, s: &mut Set) -> Self::Output {
        self.visit(&mut s.variable)?;
        self.visit(&mut s.expr)?;
        Ok(())
    }

    #[inline]
    fn visit_require(&mut self, _s: &mut Require) -> Self::Output {
        Ok(())
    }

    #[inline]
    fn visit_vector(&mut self, v: &mut Vector) -> Self::Output {
        if v.bytes {
            return Ok(());
        }

        for arg in &mut v.args {
            self.visit(arg)?;
        }

        Ok(())
    }
}

pub(crate) struct FlattenBegin {}
impl FlattenBegin {
    pub(crate) fn flatten(expr: &mut ExprKind) {
        FlattenBegin {}.visit(expr)
    }
}

impl VisitorMutRefUnit for FlattenBegin {
    fn visit(&mut self, expr: &mut ExprKind) {
        match expr {
            ExprKind::If(f) => self.visit_if(f),
            ExprKind::Define(d) => self.visit_define(d),
            ExprKind::LambdaFunction(l) => self.visit_lambda_function(l),
            ExprKind::Begin(begin) => {
                for expr in begin.exprs.iter_mut() {
                    self.visit(expr);
                }

                if begin.exprs.len() == 1 {
                    *expr = core::mem::take(&mut begin.exprs)
                        .into_iter()
                        .next()
                        .unwrap();

                    return;
                }

                let begin_exprs = core::mem::take(&mut begin.exprs);

                let mut flattened_exprs = Vec::with_capacity(begin_exprs.len());

                for expr in begin_exprs {
                    if let ExprKind::Begin(mut b) = expr {
                        flattened_exprs.append(&mut b.exprs)
                    } else {
                        flattened_exprs.push(expr);
                    }
                }

                begin.exprs = flattened_exprs;

                if begin.exprs.len() == 1 {
                    *expr = core::mem::take(&mut begin.exprs)
                        .into_iter()
                        .next()
                        .unwrap();
                }
            }
            ExprKind::Return(r) => self.visit_return(r),
            ExprKind::Quote(q) => self.visit_quote(q),
            ExprKind::Macro(m) => self.visit_macro(m),
            ExprKind::Atom(a) => self.visit_atom(a),
            ExprKind::List(l) => self.visit_list(l),
            ExprKind::SyntaxRules(s) => self.visit_syntax_rules(s),
            ExprKind::Set(s) => self.visit_set(s),
            ExprKind::Require(r) => self.visit_require(r),
            ExprKind::Let(l) => self.visit_let(l),
            ExprKind::Vector(v) => self.visit_vector(v),
        }
    }
}

pub fn flatten_begins_and_expand_defines(
    exprs: Vec<ExprKind>,
) -> crate::rvals::Result<Vec<ExprKind>> {
    #[cfg(feature = "profiling")]
    let flatten_begins_and_expand_defines_time = Instant::now();

    let res = exprs
        .into_iter()
        .map(|mut x| {
            FlattenBegin::flatten(&mut x);
            x
        })
        .map(ConvertDefinesToLets::convert_defines)
        .map(|mut x| {
            let mut checker = CheckDefinesAreInLegalPositions { depth: 0 };
            checker.visit(&mut x)?;
            Ok(x)
        })
        .collect();

    #[cfg(feature = "profiling")]
    log::debug!(
        target: "pipeline_time",
        "Flatten begins and expand defines time: {:?}",
        flatten_begins_and_expand_defines_time.elapsed()
    );
    res
}

struct DefinedVars {
    defined_identifiers: smallvec::SmallVec<[InternedString; 32]>,
    output: bool,
}

impl DefinedVars {
    fn new() -> Self {
        DefinedVars {
            defined_identifiers: smallvec::SmallVec::default(),
            output: false,
        }
    }

    fn insert(&mut self, name: InternedString) {
        self.defined_identifiers.push(name);
    }

    fn check_output(&mut self) -> bool {
        let output = self.output;
        self.output = false;
        output
    }
}

impl VisitorMutUnit for DefinedVars {
    fn visit_atom(&mut self, a: &Atom) {
        if let TokenType::Identifier(ident) = &a.syn.ty {
            self.output = self.output || self.defined_identifiers.contains(ident);
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

// TODO: Replace this with mutation!
impl Folder for ConvertDefinesToLets {
    #[inline]
    fn visit_lambda_function(&mut self, mut lambda_function: Box<LambdaFunction>) -> ExprKind {
        self.depth += 1;
        lambda_function.body = self.visit(lambda_function.body);
        self.depth -= 1;
        ExprKind::LambdaFunction(lambda_function)
    }

    #[inline]
    fn visit_let(&mut self, mut l: Box<steel_parser::ast::Let>) -> ExprKind {
        let mut visited_bindings = Vec::new();

        self.depth += 1;

        for (binding, expr) in l.bindings {
            visited_bindings.push((self.visit(binding), self.visit(expr)));
        }

        l.bindings = visited_bindings;
        l.body_expr = self.visit(l.body_expr);

        self.depth -= 1;

        ExprKind::Let(l)
    }

    // TODO
    #[inline]
    fn visit_begin(&mut self, mut begin: Box<Begin>) -> ExprKind {
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
                other => panic!("Something went wrong in define conversion, found: {other:?}"),
            }
        } else {
            // println!("Ignoring begin");
            begin.exprs = begin.exprs.into_iter().map(|e| self.visit(e)).collect();
            ExprKind::Begin(begin)
        }
    }
}

#[derive(PartialEq, Debug)]
enum ExpressionType {
    DefineConst(InternedString),
    DefineFlat(InternedString),
    DefineFlatStar(InternedString),
    DefineFunction(InternedString),
    Expression,
}

impl ExpressionType {
    fn is_expression(&self) -> bool {
        matches!(self, ExpressionType::Expression)
    }

    fn is_atom(t: &SyntaxObject) -> bool {
        matches!(
            t.ty,
            TokenType::BooleanLiteral(_)
                | TokenType::Number(_)
                | TokenType::StringLiteral(_)
                | TokenType::CharacterLiteral(_)
        )
    }

    fn is_constant(expr: &ExprKind) -> bool {
        match expr {
            ExprKind::Atom(Atom { syn, .. }) => Self::is_atom(syn),
            _ => false,
        }
    }

    fn generate_expression_types(exprs: &[ExprKind]) -> Vec<ExpressionType> {
        let mut expression_types = Vec::with_capacity(exprs.len());
        let mut defined_idents = DefinedVars::new();

        for expr in exprs {
            match expr {
                ExprKind::Define(d) => {
                    let name = d
                        .name
                        .atom_identifier_or_else(|| {})
                        .expect("Define without a legal name");

                    defined_idents.insert(*name);

                    match &d.body {
                        ExprKind::LambdaFunction(_) => {
                            expression_types.push(ExpressionType::DefineFunction(*name));
                        }
                        _ => {
                            defined_idents.visit(&d.body);
                            if defined_idents.check_output() {
                                expression_types.push(ExpressionType::DefineFlatStar(*name));
                            } else if Self::is_constant(&d.body) {
                                expression_types.push(ExpressionType::DefineConst(*name));
                            } else {
                                expression_types.push(ExpressionType::DefineFlat(*name));
                            }
                        }
                    }
                }
                _ => expression_types.push(ExpressionType::Expression),
            }
        }

        // println!("Expression types: {:?}", expression_types);

        expression_types
    }
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

fn convert_exprs_to_let(begin: Box<Begin>) -> ExprKind {
    // let defines = collect_defines_from_current_scope(&exprs);

    let expression_types = ExpressionType::generate_expression_types(&begin.exprs);

    // Go ahead and quit if there are only expressions here
    if expression_types.iter().all(|x| x.is_expression()) {
        return ExprKind::Begin(begin);
    }

    if !expression_types
        .iter()
        .any(|x| matches!(x, ExpressionType::DefineFunction(_)))
    {
        // let starting_iter = ExprKind::atom("void".to_string())

        // TODO: last expression needs to be something, otherwise this doesn't work
        // if let Some(last) = expression_types.last() {
        //     if !last.is_expression() {}
        // }

        // println!("IN HERE");
        // println!("Expression_types")

        return begin
            .exprs
            .into_iter()
            .rev()
            .reduce(|accum, expr| {
                // println!("Accum: {:?}, Expr: {:?}", accum, expr);

                match expr {
                    ExprKind::Define(d) => {
                        // let constructed_function =

                        let constructed_function = ExprKind::LambdaFunction(
                            LambdaFunction::new(
                                vec![d.name],
                                accum,
                                SyntaxObject::default(TokenType::Lambda),
                            )
                            .into(),
                        );

                        ExprKind::List(List::new(vec![constructed_function, d.body]))
                    }
                    other => ExprKind::Begin(Box::new(Begin::new(
                        vec![other, accum],
                        SyntaxObject::default(TokenType::Begin),
                    ))),
                }
            })
            .expect("Empty expression");

        // return todo!();
    }

    let mut exprs = begin.exprs.clone();

    // let mut last_expression = expression_types.len();

    let idx = expression_types
        .iter()
        .rev()
        .position(|x| !x.is_expression())
        .expect("Convert exprs to let in define conversion found no trailing expressions in begin");

    // println!("Last expression index: {:?}", idx);

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
        .collect::<SmallVec<[_; 8]>>();

    // This corresponds to the (let ((apple ..) (banana ..) (cucumber ..)))
    //                               ^^^^^^     ^^^^^^^      ^^^^^^^^
    let mut top_level_arguments: Vec<ExprKind> = Vec::with_capacity(idx + 1);

    // This corresponds to the set expressions
    // (set! apple #####apple0)
    // (set! banana #####banana1)
    // (set! cucumber #####cucumber1)
    let mut set_expressions: Vec<ExprKind> = Vec::with_capacity(idx + 1);

    // corresponds to #####apple0, #####banana1, #####cucumber1, etc
    let mut bound_names: Vec<ExprKind> = Vec::with_capacity(idx + 1);

    // TODO - check that the last expression does not contain any usages of the constant?
    // if expression_types[0..idx + 1]
    //     .iter()
    //     .all(|x| matches!(x, ExpressionType::DefineConst(_)))
    // {
    //     return ExprKind::Begin(Begin::new(body, begin.location));
    // }

    // Top level application with dummy arguments that will immediately get overwritten
    let mut top_level_dummy_args = Vec::with_capacity(idx + 1);

    let mut new_args = Vec::with_capacity(idx + 1);

    // println!("{:#?}", expression_types);

    // TODO:
    // If there are functions at all, go through this weird path
    // Otherwise, we should do the following transformation:
    //
    // If there are no functions at all, just do a series of lets, similar to let*

    // TODO: Move this up so we don't have to use raw_exprs anymore

    for ((i, expression), arg) in expression_types[0..idx + 1].iter().enumerate().zip(args) {
        match expression {
            ExpressionType::DefineFunction(name) => {
                if let ExprKind::Define(d) = &exprs[i] {
                    top_level_arguments.push(d.name.clone());
                    top_level_dummy_args.push(ExprKind::Atom(Atom::new(SyntaxObject::default(
                        IntLiteral::Small(0).into(),
                    ))));
                    let name_prime = ExprKind::atom(compact_str::format_compact!(
                        "_____{}{}",
                        name.resolve(),
                        i
                    ));
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
                        IntLiteral::Small(0).into(),
                    ))));
                    let name_prime = ExprKind::atom(compact_str::format_compact!(
                        "_____{}{}",
                        name.resolve(),
                        i
                    ));
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
                        IntLiteral::Small(0).into(),
                    ))));
                    let name_prime = ExprKind::atom(compact_str::format_compact!(
                        "_____{}{}",
                        name.resolve(),
                        i
                    ));

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
            // TODO: Move this down, don't put it with the lets, put it down in order with the set expressions
            // That way we're not at risk of accidentally goofing up the ordering of the expressions.
            // If will _only_ go in the right order of assignment
            ExpressionType::Expression => {
                // TODO: This is definitly not right
                // let expr = atom("#####define-conversion".to_string() + i.to_string().as_str());
                // top_level_dummy_args.push(ExprKind::Atom(Atom::new(SyntaxObject::default(
                //     TokenType::IntegerLiteral(123),
                // ))));

                // // This also gets bound in the inner function for now
                // bound_names.push(expr.clone());

                // top_level_arguments.push(expr);
                // new_args.push(arg);

                set_expressions.push(arg)
            }
        }
    }

    // // Append the body instructions to the set!
    set_expressions.append(&mut body);

    let inner_lambda = LambdaFunction::new(
        bound_names,
        ExprKind::Begin(Box::new(Begin::new(
            set_expressions,
            SyntaxObject::default(TokenType::Begin),
        ))),
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

    // #[test]
    // fn defines_translates_to_simple_let() {
    //     let expr = r#"
    //     (lambda ()
    //         (begin
    //             (define x 10)
    //             (define y 20)
    //             (define z 30)
    //             (+ x y z)))"#;

    //     let expected = r#"
    //     (lambda () ((lambda (x y z) ((lambda () (begin (+ x y z))))) 10 20 30))
    //     "#;

    //     let parsed = Parser::parse(expr).unwrap();
    //     let expected_parsed = Parser::parse(expected).unwrap();

    //     let result = flatten_begins_and_expand_defines(parsed);

    //     assert_eq!(result, expected_parsed);
    // }

    fn atom(ident: &str) -> ExprKind {
        ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(ident.into()))))
    }

    fn int(num: isize) -> ExprKind {
        ExprKind::Atom(Atom::new(SyntaxObject::default(
            IntLiteral::Small(num).into(),
        )))
    }

    #[test]
    fn basic_flatten_one_level() {
        let mut expr = ExprKind::Begin(Box::new(Begin::new(
            vec![
                ExprKind::Begin(Box::new(Begin::new(
                    vec![ExprKind::List(List::new(vec![
                        atom("+"),
                        atom("x"),
                        int(10),
                    ]))],
                    SyntaxObject::default(TokenType::Begin),
                ))),
                ExprKind::List(List::new(vec![atom("+"), atom("y"), int(20)])),
                ExprKind::List(List::new(vec![atom("+"), atom("z"), int(30)])),
            ],
            SyntaxObject::default(TokenType::Begin),
        )));

        let expected = ExprKind::Begin(Box::new(Begin::new(
            vec![
                ExprKind::List(List::new(vec![atom("+"), atom("x"), int(10)])),
                ExprKind::List(List::new(vec![atom("+"), atom("y"), int(20)])),
                ExprKind::List(List::new(vec![atom("+"), atom("z"), int(30)])),
            ],
            SyntaxObject::default(TokenType::Begin),
        )));

        FlattenBegin::flatten(&mut expr);

        assert_eq!(expr, expected);
    }

    #[test]
    fn basic_flatten_multiple_levels() {
        let mut expr = ExprKind::Begin(Box::new(Begin::new(
            vec![
                ExprKind::Begin(Box::new(Begin::new(
                    vec![ExprKind::List(List::new(vec![
                        atom("+"),
                        atom("x"),
                        int(10),
                    ]))],
                    SyntaxObject::default(TokenType::Begin),
                ))),
                ExprKind::Begin(Box::new(Begin::new(
                    vec![ExprKind::List(List::new(vec![
                        atom("+"),
                        atom("y"),
                        int(20),
                    ]))],
                    SyntaxObject::default(TokenType::Begin),
                ))),
                ExprKind::Begin(Box::new(Begin::new(
                    vec![ExprKind::List(List::new(vec![
                        atom("+"),
                        atom("z"),
                        int(30),
                    ]))],
                    SyntaxObject::default(TokenType::Begin),
                ))),
            ],
            SyntaxObject::default(TokenType::Begin),
        )));

        let expected = ExprKind::Begin(Box::new(Begin::new(
            vec![
                ExprKind::List(List::new(vec![atom("+"), atom("x"), int(10)])),
                ExprKind::List(List::new(vec![atom("+"), atom("y"), int(20)])),
                ExprKind::List(List::new(vec![atom("+"), atom("z"), int(30)])),
            ],
            SyntaxObject::default(TokenType::Begin),
        )));

        FlattenBegin::flatten(&mut expr);

        assert_eq!(expr, expected);
    }
}
