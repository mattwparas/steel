use std::collections::{BTreeSet, HashMap};

use steel::{
    compiler::passes::{analysis::SemanticAnalysis, VisitorMutUnitRef},
    define_primitive_symbols, define_symbols,
    parser::{
        ast::{Define, ExprKind},
        interner::InternedString,
        parser::SyntaxObjectId,
    },
    steel_vm::engine::Engine,
    stop,
};
use tower_lsp::lsp_types::Url;

struct DiagnosticContext<'a> {
    engine: &'a mut Engine,
    analysis: &'a SemanticAnalysis<'a>,
    uri: &'a Url,
}

// Produces LSP compatible diagnostic
trait Diagnostic {
    fn diagnose(
        &mut self,
        context: &mut DiagnosticContext,
    ) -> Vec<tower_lsp::lsp_types::Diagnostic>;
}

// Attempt to bind the contracted values, if possible
define_symbols! {
    BIND_C => "bind/c",
    PRIM_BIND_C => "mangler#%private/steel/contract__%#__bind/c",
    MAKE_FUNCTION_C => "make-function/c",
    PRIM_MAKE_FUNCTION_C => "mangler#%private/steel/contract__%#__make-function/c",
    MAKE_C => "make/c",
    PRIM_MAKE_C => "mangler#%private/steel/contract__%#__make/c",
    LIST_OF => "listof",
    PRIM_LIST_OF => "mangler#%private/steel/contract__%#__listof",
}

define_primitive_symbols! {
    (PRIM_INTP, INTP) => "int?",
    (PRIM_INTEGERP, INTEGERP) => "integer?",
    (PRIM_STRINGP, STRINGP) => "string?",
}

fn is_bind_c(ident: &InternedString) -> bool {
    *ident == *BIND_C || *ident == *PRIM_BIND_C
}

fn is_make_function_c(ident: &InternedString) -> bool {
    *ident == *MAKE_FUNCTION_C || *ident == *PRIM_MAKE_FUNCTION_C
}

fn is_make_c(ident: &InternedString) -> bool {
    *ident == *MAKE_C || *ident == *PRIM_MAKE_C
}

fn is_list_of(ident: &InternedString) -> bool {
    *ident == *LIST_OF || *ident == *PRIM_LIST_OF
}

// Is this expression referring to a contract (is this a bind/c instance)
fn is_contract(expr: &ExprKind) -> bool {
    fn is_contract_option(expr: &ExprKind) -> Option<bool> {
        expr.list()?.first_ident().map(is_bind_c)
    }

    if let Some(inner) = is_contract_option(expr) {
        inner
    } else {
        false
    }
}

#[derive(Debug, PartialEq, PartialOrd)]
pub enum BaseTypeKind {
    Any,
    Int,
    String,
    Float,
    Boolean,
    Character,
    Symbol,
    Void,
    Other(InternedString),
}

impl BaseTypeKind {
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

    fn from_str(input: InternedString) -> BaseTypeKind {
        match input {
            input
                if input == *INTP
                    || input == *INTEGERP
                    || input == *PRIM_INTP
                    || input == *PRIM_INTEGERP =>
            {
                BaseTypeKind::Int
            }

            input if input == *PRIM_STRINGP || input == *STRINGP => BaseTypeKind::String,
            _ => BaseTypeKind::Other(input),
        }
    }
}

#[derive(Debug, PartialEq, PartialOrd)]
pub enum StaticContract {
    Atom(BaseTypeKind),
    ListOf(Box<StaticContract>),
    UnionOf(Vec<StaticContract>),
    IntersectionOf(Vec<StaticContract>),
    Function(Vec<StaticContract>, Box<StaticContract>),
    // For now just unify the internal representation of built ins vs not
    AnyArityFunction(Box<StaticContract>, Box<StaticContract>),
}

// Is this bind/c instance referring to a make-function/c instance
fn function_contract(expr: &ExprKind) -> Option<steel::rvals::Result<StaticContract>> {
    let body = expr.list()?;

    if is_bind_c(body.first_ident()?) {
        let make_function = body.get(1)?;

        if is_make_function_c(make_function.list()?.first_ident()?) {
            // This just deconstructs the (bind/c into a individual struct for access)
            // return Some(BindContract::new(make_function, body.get(2)?));

            return Some(StaticContract::from_exprkind(make_function));
        }
    }

    None
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

impl StaticContract {
    fn to_type_info(&self) -> TypeInfo {
        match self {
            StaticContract::Atom(a) => a.to_type_info(),
            StaticContract::ListOf(l) => TypeInfo::ListOf(Box::new(l.to_type_info())),
            StaticContract::Function(pre, post) => TypeInfo::FixedArityFunction(
                pre.iter().map(|x| x.to_type_info()).collect(),
                Box::new(post.to_type_info()),
            ),
            StaticContract::AnyArityFunction(pre, post) => TypeInfo::AnyArityFunction(
                Box::new(pre.to_type_info()),
                Box::new(post.to_type_info()),
            ),
            StaticContract::UnionOf(u) => {
                TypeInfo::UnionOf(u.iter().map(|x| x.to_type_info()).collect())
            }
            StaticContract::IntersectionOf(u) => {
                TypeInfo::IntersectionOf(u.iter().map(|x| x.to_type_info()).collect())
            }
        }
    }

    fn from_exprkind(expr: &ExprKind) -> steel::rvals::Result<StaticContract> {
        match expr {
            ExprKind::Atom(a) => {
                let name = BaseTypeKind::from_str(*a.ident().unwrap()); // TODO -> clean up this error handling

                Ok(StaticContract::Atom(name))
            }
            ExprKind::List(l) => match l.first_ident() {
                Some(x) if is_list_of(x) => {
                    let body = l.get(1).unwrap();

                    Ok(StaticContract::ListOf(Box::new(Self::from_exprkind(body)?)))
                }
                Some(x) if is_make_c(x) => {
                    // Just recur on the actual contract here
                    Self::from_exprkind(l.get(1).unwrap())
                }
                Some(x) if is_make_function_c(x) => {
                    // Pre conditions of the function contract
                    let pre_conditions = &l.args[1..l.args.len() - 1];
                    // Post condition of the function contract
                    let post_condition = &l.args[l.args.len() - 1];

                    let pre_condition_contracts = pre_conditions
                        .iter()
                        .map(Self::from_exprkind)
                        .collect::<steel::rvals::Result<Vec<_>>>()?;

                    let post_condition_contract = Self::from_exprkind(post_condition)?;

                    Ok(StaticContract::Function(
                        pre_condition_contracts,
                        Box::new(post_condition_contract),
                    ))
                }
                _ => {
                    println!("{expr}");
                    stop!(Generic => "Unexpected contract combinator")
                }
            },
            _ => {
                stop!(BadSyntax => "contracts can either be atoms or lists")
            }
        }
    }
}

#[derive(Default, Debug)]
pub struct GlobalContractCollector {
    contracts: HashMap<InternedString, StaticContract>,
    syntax_id_to_string: HashMap<SyntaxObjectId, InternedString>,
}

impl<'a> VisitorMutUnitRef<'a> for GlobalContractCollector {
    fn visit_define(&mut self, define: &'a Define) {
        if let Some(contract) = function_contract(&define.body) {
            let atom_identifier = define.name.atom_syntax_object().unwrap();

            let name = define.name.atom_identifier().unwrap();
            let syntax_object_id = atom_identifier.syntax_object_id;

            // If we try to collect contracts on something thats not expanded, we'll have problems anyway
            self.contracts.insert(*name, contract.unwrap());

            self.syntax_id_to_string.insert(syntax_object_id, *name);
        }
    }

    fn visit_set(&mut self, s: &'a steel::parser::ast::Set) {
        if let Some(contract) = function_contract(&s.expr) {
            let atom_identifier = s.variable.atom_syntax_object().unwrap();
            let name = s.variable.atom_identifier().unwrap();
            let syntax_object_id = atom_identifier.syntax_object_id;

            // If we try to collect contracts on something thats not expanded, we'll have problems anyway
            self.contracts.insert(*name, contract.unwrap());

            self.syntax_id_to_string.insert(syntax_object_id, *name);
        }
    }
}

#[test]
fn resolve_contracts() {
    let expression = r#"
(define/contract (my-fun-contracted-function x y)
  (->/c int? int? int?)
  (+ x y))        
    "#;

    let mut engine = Engine::new();

    let exprs = engine
        .emit_expanded_ast_without_optimizations(expression, None)
        .unwrap();

    let mut collector = GlobalContractCollector {
        contracts: HashMap::default(),
        syntax_id_to_string: HashMap::default(),
    };

    for expr in &exprs {
        collector.visit(expr);
    }

    println!("{:#?}", collector.contracts);

    // Now analyze the resulting AST, and build the map
}
