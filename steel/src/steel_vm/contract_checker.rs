use std::collections::HashMap;

use crate::{
    compiler::passes::{VisitorMutUnit, VisitorMutUnitRef},
    parser::{span::Span, visitors::VisitorMut},
};
use crate::{parser::ast::ExprKind, rvals::Result};

/*
(define applesauce
    (bind/c
        (make-function/c
            (make/c int? 'int?)
            (make/c int? 'int?)
            (make/c int? 'int?)
            (make/c int? 'int?))
        (lambda (x y z) (begin (+ x y z)))
    applesauce))

(define dummy
    (bind/c
        (make-function/c
            (make/c string? string?)
            (make/c integer? integer?)
            (make/c
                (make-function/c
                    (make/c string? string?)
                    (make/c string? string?))
            (make-function/c
                (make/c string? string?)
                (make/c string? string?))))
    (lambda (foo bar)
        (begin (list (int->string bar) foo))) dummy))


*/

// Corresponds to a concrete type, referenced by a contract
// For instance, this should be coerced from the contract type given the inference
// integer? -> Int
// string? -> String
// UnknownStruct? -> Other("UnknownStruct?")

#[derive(Debug, PartialEq)]
pub enum BaseTypeKind<'a> {
    Int,
    String,
    Other(&'a str),
}

impl<'a> BaseTypeKind<'a> {
    fn from_str(input: &'a str) -> BaseTypeKind<'a> {
        match input {
            "int?" | "integer?" => BaseTypeKind::Int,
            "string?" => BaseTypeKind::String,
            _ => BaseTypeKind::Other(input),
        }
    }
}

// Concrete, inferred type
#[derive(Debug)]
enum Type {
    Int,
    Bool,
    List(Box<Type>),
    Func(Vec<Type>, Box<Type>),
}

pub enum BuiltInFunctionContract {
    // Things that have a fixed arity
    FixedArity(Vec<BaseTypeKind<'static>>, BaseTypeKind<'static>),
    // Things that match any number of arity in the precondition but all match
    // For instance, addition accepts all numbers in the precondition
    AnyArity(BaseTypeKind<'static>, BaseTypeKind<'static>),
}

// Generate the bindings for the built in functions
pub fn built_in_contract_map() -> HashMap<&'static str, StaticContract<'static>> {
    use StaticContract::*;
    let mut map = HashMap::new();

    map.insert(
        "+",
        AnyArityFunction(
            Box::new(Atom(BaseTypeKind::Int)),
            Box::new(Atom(BaseTypeKind::Int)),
        ),
    );
    map.insert(
        "-",
        AnyArityFunction(
            Box::new(Atom(BaseTypeKind::Int)),
            Box::new(Atom(BaseTypeKind::Int)),
        ),
    );

    map
}

#[derive(Debug, PartialEq)]
pub enum StaticContract<'a> {
    Atom(BaseTypeKind<'a>),
    ListOf(Box<StaticContract<'a>>),
    Function(Vec<StaticContract<'a>>, Box<StaticContract<'a>>),
    // For now just unify the internal representation of built ins vs not
    AnyArityFunction(Box<StaticContract<'static>>, Box<StaticContract<'static>>),
}

impl<'a> StaticContract<'a> {
    fn from_exprkind(expr: &'a ExprKind) -> Result<StaticContract<'a>> {
        match expr {
            ExprKind::Atom(a) => {
                let name = BaseTypeKind::from_str(a.ident().unwrap()); // TODO -> clean up this error handling

                Ok(StaticContract::Atom(name))
            }
            ExprKind::List(l) => match l.first_ident() {
                Some("listof") => {
                    let body = l.get(1).unwrap();

                    Ok(StaticContract::ListOf(Box::new(Self::from_exprkind(body)?)))
                }
                Some("make/c") => {
                    // Just recur on the actual contract here
                    Self::from_exprkind(l.get(1).unwrap())
                }
                Some("make-function/c") => {
                    // Pre conditions of the function contract
                    let pre_conditions = &l.args[1..l.args.len() - 1];
                    // Post condition of the function contract
                    let post_condition = &l.args[l.args.len() - 1];

                    let pre_condition_contracts = pre_conditions
                        .iter()
                        .map(Self::from_exprkind)
                        .collect::<Result<Vec<_>>>()?;

                    let post_condition_contract = Self::from_exprkind(post_condition)?;

                    Ok(StaticContract::Function(
                        pre_condition_contracts,
                        Box::new(post_condition_contract),
                    ))
                }
                _ => {
                    println!("{}", expr);
                    stop!(Generic => "Unexpected contract combinator")
                }
            },
            _ => {
                stop!(BadSyntax => "contracts can either be atoms or lists")
            }
        }
    }
}

pub type TypeId = usize;

// This is a contract bound to the body expression
#[derive(Debug)]
pub struct BoundContract<'a> {
    body: &'a ExprKind,
    contract: StaticContract<'a>,
}

impl<'a> BoundContract<'a> {
    // TODO make this resiliant to syntax errors
    pub fn new(contract: StaticContract<'a>, body: &'a ExprKind) -> Self {
        // if body.lambda_function().unwrap().args

        match &contract {
            StaticContract::Function(pre_conditions, _) => {
                assert_eq!(
                    body.lambda_function().unwrap().args.len(),
                    pre_conditions.len(),
                    "The arguments and contracts must be of matching lengths"
                );
            }
            _ => {}
        }

        BoundContract { body, contract }
    }
}

// Is this expression referring to a contract (is this a bind/c instance)
fn is_contract(expr: &ExprKind) -> bool {
    fn is_contract_option(expr: &ExprKind) -> Option<bool> {
        expr.list()?.first_ident().map(|x| x == "bind/c")
    }

    if let Some(inner) = is_contract_option(expr) {
        inner
    } else {
        false
    }
}

// Is this bind/c instance referring to a make-function/c instance
fn function_contract<'a>(expr: &'a ExprKind) -> Option<Result<StaticContract<'a>>> {
    let body = expr.list()?;

    if body.first_ident()? == "bind/c" {
        let make_function = body.get(1)?;

        if make_function.list()?.first_ident()? == "make-function/c" {
            // This just deconstructs the (bind/c into a individual struct for access)
            // return Some(BindContract::new(make_function, body.get(2)?));

            return Some(StaticContract::from_exprkind(make_function));
        }
    }

    None
}

#[derive(Default, Debug)]
pub struct GlobalContractCollector<'a> {
    contracts: HashMap<String, StaticContract<'a>>,
    built_ins: HashMap<&'static str, StaticContract<'static>>,
}

impl<'a> GlobalContractCollector<'a> {
    pub fn collect_contracts(exprs: impl IntoIterator<Item = &'a ExprKind>) -> Self {
        let mut collector = Self::default();

        // TODO: Attach the built-ins here
        collector.built_ins = built_in_contract_map();

        exprs.into_iter().for_each(|x| collector.visit(x));
        collector
    }

    pub fn get(&self, ident: &str) -> Option<&StaticContract<'a>> {
        self.contracts
            .get(ident)
            .or_else(|| self.built_ins.get(ident))
    }
}

impl<'a> VisitorMutUnitRef<'a> for GlobalContractCollector<'a> {
    fn visit_define(&mut self, define: &'a crate::parser::ast::Define) {
        if let Some(contract) = function_contract(&define.body) {
            // If we try to collect contracts on something thats not expanded, we'll have problems anyway
            self.contracts.insert(
                define.name.atom_identifier().unwrap().to_string(),
                contract.unwrap(),
            );
        }
    }
}

#[derive(Debug)]
pub struct ContractChecker<'a> {
    global_contract_info: GlobalContractCollector<'a>,
}

impl<'a> ContractChecker<'a> {
    pub fn new(global_contract_info: GlobalContractCollector<'a>) -> Self {
        ContractChecker {
            global_contract_info,
        }
    }

    pub fn check(&mut self, exprs: impl IntoIterator<Item = &'a ExprKind>) -> Result<()> {
        for expr in exprs {
            self.visit(expr)?;
        }
        Ok(())
    }
}

impl<'a> VisitorMut for ContractChecker<'a> {
    type Output = Result<()>;

    fn visit_if(&mut self, f: &crate::parser::ast::If) -> Self::Output {
        self.visit(&f.test_expr)?;
        self.visit(&f.then_expr)?;
        self.visit(&f.else_expr)
    }

    fn visit_define(&mut self, define: &crate::parser::ast::Define) -> Self::Output {
        self.visit(&define.name)?;
        self.visit(&define.body)
    }

    fn visit_lambda_function(
        &mut self,
        lambda_function: &crate::parser::ast::LambdaFunction,
    ) -> Self::Output {
        self.visit(&lambda_function.body)
    }

    fn visit_begin(&mut self, begin: &crate::parser::ast::Begin) -> Self::Output {
        for expr in &begin.exprs {
            self.visit(&expr)?;
        }
        Ok(())
    }

    fn visit_return(&mut self, r: &crate::parser::ast::Return) -> Self::Output {
        self.visit(&r.expr)
    }

    fn visit_quote(&mut self, _quote: &crate::parser::ast::Quote) -> Self::Output {
        Ok(())
    }

    fn visit_struct(&mut self, _s: &crate::parser::ast::Struct) -> Self::Output {
        panic!("Unexpected struct")
    }

    fn visit_macro(&mut self, _m: &crate::parser::ast::Macro) -> Self::Output {
        panic!("Unexpected macro")
    }

    fn visit_atom(&mut self, a: &crate::parser::ast::Atom) -> Self::Output {
        Ok(())
    }

    fn visit_list(&mut self, l: &crate::parser::ast::List) -> Self::Output {
        // Visit the children in order
        for expr in &l.args[1..] {
            self.visit(expr)?;
        }

        // Get the function that is being called
        let function = l.args.get(0).unwrap();

        if let ExprKind::Atom(a) = function {
            let ident = a.ident().unwrap();
            log::debug!("Calling function: {}", ident);

            let contract_info = self.global_contract_info.get(ident);

            log::debug!("{:#?}", contract_info);
        }

        Ok(())
    }

    fn visit_syntax_rules(&mut self, l: &crate::parser::ast::SyntaxRules) -> Self::Output {
        panic!("Unexpected syntax rules")
    }

    fn visit_set(&mut self, s: &crate::parser::ast::Set) -> Self::Output {
        self.visit(&s.expr)
    }

    fn visit_require(&mut self, s: &crate::parser::ast::Require) -> Self::Output {
        panic!("Unexpected require")
    }

    fn visit_callcc(&mut self, cc: &crate::parser::ast::CallCC) -> Self::Output {
        self.visit(&cc.expr)
    }

    fn visit_let(&mut self, l: &crate::parser::ast::Let) -> Self::Output {
        for binding in &l.bindings {
            self.visit(&binding.1)?;
        }

        self.visit(&l.body_expr)
    }
}
