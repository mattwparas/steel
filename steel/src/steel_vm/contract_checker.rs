use std::collections::{BTreeSet, HashMap, HashSet};

use quickscope::ScopeMap;

use crate::{
    compiler::passes::{VisitorMutUnit, VisitorMutUnitRef},
    parser::{span::Span, tokens::TokenType, visitors::VisitorMut},
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

#[derive(Debug, PartialEq, PartialOrd)]
pub enum BaseTypeKind<'a> {
    Any,
    Int,
    String,
    Float,
    Boolean,
    Character,
    Symbol,
    Void,
    Other(&'a str),
}

impl<'a> BaseTypeKind<'a> {
    // TODO -> don't have this just refer directly to unknown
    fn to_type_info(&self) -> TypeInfo {
        match self {
            BaseTypeKind::Any => TypeInfo::Any,
            BaseTypeKind::Int => TypeInfo::Int,
            BaseTypeKind::String => TypeInfo::String,
            BaseTypeKind::Float => TypeInfo::Float,
            BaseTypeKind::Boolean => TypeInfo::Boolean,
            BaseTypeKind::Character => TypeInfo::Char,
            BaseTypeKind::Void => TypeInfo::Void,
            BaseTypeKind::Symbol => TypeInfo::Symbol,
            BaseTypeKind::Other(_) => TypeInfo::Unknown,
        }
    }

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
pub fn built_in_contract_map() -> HashMap<&'static str, TypeInfo> {
    use TypeInfo::*;
    let mut map = HashMap::new();

    map.insert(
        "+",
        AnyArityFunction(
            Box::new(UnionOf(
                vec![TypeInfo::Int, TypeInfo::Float].into_iter().collect(),
            )),
            Box::new(UnionOf(
                vec![TypeInfo::Int, TypeInfo::Float].into_iter().collect(),
            )),
        ),
    );
    map.insert(
        "-",
        AnyArityFunction(Box::new(TypeInfo::Int), Box::new(TypeInfo::Int)),
    );
    map.insert(
        "int->string",
        FixedArityFunction(vec![TypeInfo::Int], Box::new(TypeInfo::String)),
    );

    map.insert(
        "list",
        DependentFunction(|args: Vec<TypeInfo>| {
            TypeInfo::UnionOf(
                args.into_iter()
                    .chain(std::iter::once(TypeInfo::Any))
                    .collect(),
            )
        }),
    );

    map
}

#[derive(Debug, PartialEq, PartialOrd)]
pub enum StaticContract<'a> {
    Atom(BaseTypeKind<'a>),
    ListOf(Box<StaticContract<'a>>),
    UnionOf(Vec<StaticContract<'a>>),
    IntersectionOf(Vec<StaticContract<'a>>),
    Function(Vec<StaticContract<'a>>, Box<StaticContract<'a>>),
    // For now just unify the internal representation of built ins vs not
    AnyArityFunction(Box<StaticContract<'static>>, Box<StaticContract<'static>>),
}

impl<'a> StaticContract<'a> {
    fn to_type_info(&self) -> TypeInfo {
        match self {
            StaticContract::Atom(a) => a.to_type_info(),
            StaticContract::ListOf(l) => TypeInfo::ListOf(Box::new(l.to_type_info())),
            StaticContract::Function(pre, post) => TypeInfo::FixedArityFunction(
                pre.into_iter().map(|x| x.to_type_info()).collect(),
                Box::new(post.to_type_info()),
            ),
            StaticContract::AnyArityFunction(pre, post) => TypeInfo::AnyArityFunction(
                Box::new(pre.to_type_info()),
                Box::new(post.to_type_info()),
            ),
            StaticContract::UnionOf(u) => {
                TypeInfo::UnionOf(u.into_iter().map(|x| x.to_type_info()).collect())
            }
            StaticContract::IntersectionOf(u) => {
                TypeInfo::IntersectionOf(u.into_iter().map(|x| x.to_type_info()).collect())
            }
        }
    }

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
    built_ins: HashMap<&'static str, TypeInfo>,
}

impl<'a> GlobalContractCollector<'a> {
    pub fn collect_contracts(exprs: impl IntoIterator<Item = &'a ExprKind>) -> Self {
        let mut collector = Self::default();

        // TODO: Attach the built-ins here
        collector.built_ins = built_in_contract_map();

        exprs.into_iter().for_each(|x| collector.visit(x));
        collector
    }

    pub fn get(&self, ident: &str) -> Option<TypeInfo> {
        self.contracts
            .get(ident)
            .map(|x| x.to_type_info())
            .or_else(|| self.built_ins.get(ident).cloned())
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

pub struct ContractChecker<'a> {
    global_contract_info: GlobalContractCollector<'a>,
    scope_map: ScopeMap<String, TypeInfo>,
    inferred_globals: HashMap<String, TypeInfo>,
}

impl<'a> std::fmt::Debug for ContractChecker<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ContractChecker")
            .field("global_contract_info", &self.global_contract_info)
            .field(
                "scope_map",
                &self.scope_map.iter_top().collect::<HashMap<_, _>>(),
            )
            .finish()
    }
}

impl<'a> ContractChecker<'a> {
    pub fn new(global_contract_info: GlobalContractCollector<'a>) -> Self {
        ContractChecker {
            global_contract_info,
            scope_map: ScopeMap::default(),
            inferred_globals: HashMap::default(),
        }
    }

    pub fn check(&mut self, exprs: impl IntoIterator<Item = &'a ExprKind>) -> Result<()> {
        for expr in exprs {
            let type_checked = self.visit(expr)?;
            println!("{:#?}", type_checked);
        }
        Ok(())
    }

    // TODO -> have a way to make sure that inferred globals can be refined in some capacity if
    // more information can be applied to them
    pub fn get_ident(&self, ident: &str) -> Option<TypeInfo> {
        self.scope_map
            .get(ident)
            .cloned()
            .or_else(|| self.global_contract_info.get(ident))
            .or_else(|| self.inferred_globals.get(ident).cloned())
    }

    pub fn local_ident_exists(&self, ident: &str) -> bool {
        self.scope_map.get(ident).is_some()
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub enum TypeInfo {
    // We don't have enough information to say what this type is
    // Either, the function has come externally or it was unable to be inferred for some reason
    Unknown,
    Any,
    Void,
    Int,
    Float,
    Boolean,
    Char,
    String,
    Symbol,
    ListOf(Box<TypeInfo>),
    // Basic function usage, this has some fixed number of arguments with contracts attached
    FixedArityFunction(Vec<TypeInfo>, Box<TypeInfo>),
    // This is something like addition, which has a contract with any arity in the preconditions
    AnyArityFunction(Box<TypeInfo>, Box<TypeInfo>),

    // Function where the input type directly creates the output type in some fashion
    DependentFunction(fn(Vec<TypeInfo>) -> TypeInfo),

    // If a function can return multiple things, or the return value can satisfy multiple contracts (or/c)
    UnionOf(BTreeSet<TypeInfo>),
    // If the return value of a function must satisfy multiple contracts (and/c)
    IntersectionOf(BTreeSet<TypeInfo>),
}

impl std::fmt::Debug for TypeInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unknown => write!(f, "Unknown"),
            Self::Any => write!(f, "Any"),
            Self::Void => write!(f, "Void"),
            Self::Int => write!(f, "Int"),
            Self::Float => write!(f, "Float"),
            Self::Boolean => write!(f, "Boolean"),
            Self::Char => write!(f, "Char"),
            Self::String => write!(f, "String"),
            Self::Symbol => write!(f, "Symbol"),
            Self::ListOf(arg0) => f.debug_tuple("ListOf").field(arg0).finish(),
            Self::FixedArityFunction(arg0, arg1) => f
                .debug_tuple("FixedArityFunction")
                .field(arg0)
                .field(arg1)
                .finish(),
            Self::AnyArityFunction(arg0, arg1) => f
                .debug_tuple("AnyArityFunction")
                .field(arg0)
                .field(arg1)
                .finish(),
            Self::DependentFunction(_) => f
                .debug_tuple("DependentFunction")
                .field(&"<#function>".to_string())
                .finish(),
            Self::UnionOf(arg0) => f.debug_tuple("UnionOf").field(arg0).finish(),
            Self::IntersectionOf(arg0) => f.debug_tuple("IntersectionOf").field(arg0).finish(),
        }
    }
}

impl TypeInfo {
    // TODO -> this is almost assuredly wrong
    pub fn is_compatible_with(&self, other: &TypeInfo) -> bool {
        match (self, other) {
            (TypeInfo::Any, _) | (_, TypeInfo::Any) => true,
            // TODO -> there might be a better way of doing this
            (TypeInfo::UnionOf(l), TypeInfo::UnionOf(r)) => {
                l.is_subset(r) || l.contains(&TypeInfo::Any) || r.contains(&TypeInfo::Any)

                // l == r
            }
            (value, TypeInfo::UnionOf(set)) => set.contains(value) || set.contains(&TypeInfo::Any),
            (TypeInfo::UnionOf(set), value) => set.contains(value) || set.contains(&TypeInfo::Any),
            (TypeInfo::Unknown, _) | (_, TypeInfo::Unknown) => true,
            (TypeInfo::ListOf(l), TypeInfo::ListOf(r)) => l.is_compatible_with(r),
            (left, right) => left == right,
        }
    }
}

#[test]
fn test_list_composition() {
    let left = TypeInfo::ListOf(Box::new(TypeInfo::UnionOf(
        vec![TypeInfo::Any].into_iter().collect(),
    )));

    let right = TypeInfo::ListOf(Box::new(TypeInfo::UnionOf(
        vec![TypeInfo::Any, TypeInfo::String].into_iter().collect(),
    )));

    println!("{}", left.is_compatible_with(&right));

    let left = TypeInfo::ListOf(Box::new(TypeInfo::String));

    let right = TypeInfo::ListOf(Box::new(TypeInfo::UnionOf(
        vec![TypeInfo::Any, TypeInfo::String].into_iter().collect(),
    )));

    println!("{}", left.is_compatible_with(&right));
}

impl<'a> VisitorMut for ContractChecker<'a> {
    type Output = Result<TypeInfo>;

    fn visit_if(&mut self, f: &crate::parser::ast::If) -> Self::Output {
        self.visit(&f.test_expr)?;

        let then_expr = self.visit(&f.then_expr)?;
        let else_expr = self.visit(&f.else_expr)?;

        if then_expr == else_expr {
            Ok(else_expr)
        } else {
            Ok(TypeInfo::UnionOf([then_expr, else_expr].into()))
        }
    }

    fn visit_define(&mut self, define: &crate::parser::ast::Define) -> Self::Output {
        // self.visit(&define.name)?;

        self.visit(&define.body)
    }

    fn visit_lambda_function(
        &mut self,
        lambda_function: &crate::parser::ast::LambdaFunction,
    ) -> Self::Output {
        self.scope_map.push_layer();

        for arg in lambda_function
            .args
            .iter()
            .map(|x| x.atom_identifier().unwrap())
        {
            self.scope_map.define(arg.to_string(), TypeInfo::Unknown);
        }

        let return_value = self.visit(&lambda_function.body)?;

        // Ideally, by the time we get back to this position in the type checking, we've propagated information upwards given what we know about
        // the usage of the arguments. If these conflict with the type given to the function, this can be
        let args = lambda_function
            .args
            .iter()
            .map(|x| self.get_ident(x.atom_identifier().unwrap()).unwrap())
            .collect::<Vec<_>>();

        // We've exited the scope, we're done
        self.scope_map.pop_layer();

        Ok(TypeInfo::FixedArityFunction(args, Box::new(return_value)))
    }

    fn visit_begin(&mut self, begin: &crate::parser::ast::Begin) -> Self::Output {
        let mut last = TypeInfo::Void;
        for expr in &begin.exprs {
            last = self.visit(&expr)?;
        }
        Ok(last)
    }

    // TODO -> return should mark the return value
    fn visit_return(&mut self, r: &crate::parser::ast::Return) -> Self::Output {
        self.visit(&r.expr)
    }

    // TODO -> eval quote to get value
    fn visit_quote(&mut self, quote: &crate::parser::ast::Quote) -> Self::Output {
        match &quote.expr {
            ExprKind::Atom(a) => match a.syn.ty {
                TokenType::StringLiteral(_) => Ok(TypeInfo::String),
                TokenType::IntegerLiteral(_) => Ok(TypeInfo::Int),
                TokenType::NumberLiteral(_) => Ok(TypeInfo::Float),
                TokenType::CharacterLiteral(_) => Ok(TypeInfo::Char),
                TokenType::BooleanLiteral(_) => Ok(TypeInfo::Boolean),
                TokenType::Identifier(_) => Ok(TypeInfo::Symbol),
                _ => todo!("Not sure what to do here"),
            },
            // If we get back a generic quoted list, it can be any valid program
            // which is effectively the union of all constants
            // TODO -> this can also accept the list of any constants, which then
            // makes the type definition recursive...
            ExprKind::List(_) => Ok(TypeInfo::ListOf(Box::new(TypeInfo::UnionOf({
                let mut hs = BTreeSet::new();
                hs.insert(TypeInfo::String);
                hs.insert(TypeInfo::Int);
                hs.insert(TypeInfo::Float);
                hs.insert(TypeInfo::Char);
                hs.insert(TypeInfo::Boolean);
                hs.insert(TypeInfo::Symbol);
                // This _should_ be a recursive case on the list of self
                // but I don't currently have a way to resolve recursive type references
                hs.insert(TypeInfo::ListOf(Box::new(TypeInfo::Any)));
                hs
            })))),
            _ => stop!(TypeMismatch => "this shouldn't work"),
        }
    }

    fn visit_struct(&mut self, _s: &crate::parser::ast::Struct) -> Self::Output {
        panic!("Unexpected struct")
    }

    fn visit_macro(&mut self, _m: &crate::parser::ast::Macro) -> Self::Output {
        panic!("Unexpected macro")
    }

    // TODO - resolve identifiers with a scope map
    fn visit_atom(&mut self, a: &crate::parser::ast::Atom) -> Self::Output {
        match &a.syn.ty {
            TokenType::Identifier(a) => Ok(self.get_ident(a.as_str()).unwrap_or(TypeInfo::Unknown)),
            TokenType::StringLiteral(_) => Ok(TypeInfo::String),
            TokenType::IntegerLiteral(_) => Ok(TypeInfo::Int),
            TokenType::NumberLiteral(_) => Ok(TypeInfo::Float),
            TokenType::CharacterLiteral(_) => Ok(TypeInfo::Char),
            TokenType::BooleanLiteral(_) => Ok(TypeInfo::Boolean),
            _ => stop!(TypeMismatch => "Unexpected token type"),
        }
    }

    fn visit_list(&mut self, l: &crate::parser::ast::List) -> Self::Output {
        let function_type = self.visit(&l.args[0])?;
        let expected_arity = l.args.len() - 1;

        println!("Type checking this expression: {}", l);
        println!("Function type: {:?}", function_type);

        match &function_type {
            TypeInfo::Unknown => {
                // If we're in this situation we don't know what this function is
                // we can attempt to infer what the corresponding type is based on the types of the argument
                let argument_types = l.args[1..]
                    .iter()
                    .map(|x| self.visit(x))
                    .collect::<Result<Vec<_>>>()?;

                // In the case where we are calling a function with an unknown return type, propagate an unknown
                // to try to continue inference
                let return_type = TypeInfo::Unknown;

                // Inferred function type at this node
                // TODO -> this should be registered into the inferred globals if necessary
                let inferred_type =
                    TypeInfo::FixedArityFunction(argument_types, Box::new(return_type.clone()));

                if let Some(ident) = &l.args[0].atom_identifier() {
                    if let Some(local) = self.scope_map.get_mut(*ident) {
                        log::debug!(
                            "Inferring local var: {} with type {:?} - old type: {:?}",
                            ident,
                            inferred_type,
                            local
                        );

                        *local = inferred_type;
                    } else {
                        log::debug!(
                            "Inferring global var: {} with type {:?}",
                            ident,
                            inferred_type
                        );

                        // TODO ->

                        // self.inferred_globals
                        //     .insert(ident.to_string(), inferred_type);
                    }
                }

                return Ok(return_type);
            }

            TypeInfo::DependentFunction(func) => {
                let argument_types = l.args[1..]
                    .iter()
                    .map(|x| self.visit(x))
                    .collect::<Result<Vec<_>>>()?;

                let return_type = func(argument_types);

                return Ok(return_type);
            }

            TypeInfo::AnyArityFunction(pre, post) => {
                if l.args.len() == 1 {
                    return Ok(*(post.clone()));
                }

                // Visit the children in order
                for expr in &l.args[1..] {
                    let mut found = self.visit(expr)?;

                    // If we've found an unknown, we don't necessarily have enough information
                    // to say what this type is. We attempt to resolve it by the inferring that the type
                    // is the type expected by the function call. Later, this will be rejected if there is a
                    // a mismatch.
                    if matches!(found, TypeInfo::Unknown) {
                        if let Some(ident) = expr.atom_identifier() {
                            if let Some(local) = self.scope_map.get_mut(ident) {
                                *local = *(pre.clone());
                            } else {
                                log::warn!("Unable to resolve reference to variable: {}", ident);
                            }
                            found = *(pre.clone());
                        }
                    }

                    if !pre.is_compatible_with(&found) {
                        stop!(ContractViolation => format!("type mismatch: expected {:?}, found {:?}", pre, found))
                    }
                }

                return Ok(*(post.clone()));
            }
            TypeInfo::FixedArityFunction(pre, post) => {
                let found_arity = pre.len();
                if expected_arity != found_arity {
                    stop!(ArityMismatch => format!("Function application mismatched with expected - function expected {} arguments but found {}", found_arity, expected_arity))
                }

                if l.args.len() == 1 {
                    return Ok(*(post.clone()));
                }

                let visited = l.args[1..]
                    .iter()
                    .map(|x| self.visit(x))
                    .collect::<Result<Vec<_>>>()?;

                for ((expected, mut found), unvisited) in
                    pre.into_iter().zip(visited).zip(l.args[1..].iter())
                {
                    println!("Found argument type: {:?}", found);

                    // If we've found an unknown, we don't necessarily have enough information
                    // to say what this type is. We attempt to resolve it by the inferring that the type
                    // is the type expected by the function call. Later, this will be rejected if there is a
                    // a mismatch.
                    if matches!(found, TypeInfo::Unknown) {
                        if let Some(ident) = unvisited.atom_identifier() {
                            if let Some(local) = self.scope_map.get_mut(ident) {
                                *local = expected.clone();
                            } else {
                                log::warn!("Unable to resolve reference to variable: {}", ident);
                            }
                            found = expected.clone();
                        }
                    }

                    if !expected.is_compatible_with(&found) {
                        stop!(ContractViolation => format!("type mismatch: expected {:?}, found {:?}", expected, found))
                    }
                }

                return Ok(*(post.clone()));
            }
            _ => {
                stop!(TypeMismatch => format!("Function application not a procedure, expected a function, found: {:?}", function_type))
            }
        }
    }

    fn visit_syntax_rules(&mut self, _l: &crate::parser::ast::SyntaxRules) -> Self::Output {
        panic!("Unexpected syntax rules")
    }

    fn visit_set(&mut self, s: &crate::parser::ast::Set) -> Self::Output {
        self.visit(&s.expr)
    }

    fn visit_require(&mut self, _s: &crate::parser::ast::Require) -> Self::Output {
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
