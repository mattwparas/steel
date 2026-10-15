use std::path::PathBuf;

use compact_str::CompactString;
use rustc_hash::{FxHashMap, FxHashSet};
use steel_parser::ast::{Define, If, LambdaFunction, Let, List, Set};
use steel_parser::tokens::TokenType;
use thin_vec::ThinVec;

use crate::compiler::modules::{CompiledModule, MODULE_PREFIX};
use crate::parser::ast::ExprKind;
use crate::parser::interner::InternedString;
use crate::parser::parser::SyntaxObject;

use super::VisitorMutUnit;

/// The module that defines `bind/c` and the contract combinators.
const CONTRACT_MODULE: &str = "#%private/steel/contract";

/// Builtin predicates that test what kind of value their argument is. They take
/// one argument, never raise and have no effects, so checking one twice is the
/// same as checking it once.
const TYPE_PREDICATES: &[&str] = &[
    "boolean?",
    "bool?",
    "bytes?",
    "bytevector?",
    "char?",
    "complex?",
    "empty?",
    "exact-integer?",
    "fixnum?",
    "flat-vector?",
    "float?",
    "function?",
    "hash?",
    "immutable-vector?",
    "improper-pair?",
    "int?",
    "integer?",
    "list?",
    "mutable-vector?",
    "null?",
    "number?",
    "pair?",
    "procedure?",
    "rational?",
    "real?",
    "set?",
    "string?",
    "symbol?",
    "vector?",
    "void?",
];

/// How one argument or the result of a contracted function is checked.
enum Check {
    /// `any/c`, which every value satisfies.
    Nothing,
    /// A call to a builtin type predicate.
    Predicate(InternedString),
}

/// A function provided with `(contract/out name (->/c ...))` is bound to the
/// result of `bind/c`, a closure the inliner can't see into. When every
/// argument and the result are checked by a builtin type predicate or
/// `any/c`, this binds it instead to a function that calls the predicates
/// itself, and defined in the module so a call from another module can be
/// inlined. A failed check goes through `bind/c`'s function, which reports it
/// as before. Returns whether anything changed.
pub fn specialize_flat_contracts(
    exprs: &mut [ExprKind],
    modules: &crate::HashMap<PathBuf, CompiledModule>,
) -> bool {
    let Some(contracts) = modules
        .iter()
        .find(|(name, _)| name.to_str() == Some(CONTRACT_MODULE))
        .map(|(_, module)| module.prefix())
    else {
        return false;
    };

    let mut aliases = FxHashMap::default();
    let mut assigned = FxHashSet::default();
    let module_name: InternedString = (CompactString::new(MODULE_PREFIX) + &contracts).into();
    for expr in exprs.iter() {
        collect_aliases(expr, module_name, &mut aliases);
        AssignedNames(&mut assigned).visit(expr);
    }

    let exports = ContractExports {
        prefix: contracts,
        aliases,
    };

    let mut changed = false;
    for expr in exprs.iter_mut() {
        if let ExprKind::Begin(b) = expr {
            changed |= specialize_module(&mut b.exprs, &exports, &assigned);
        }
    }
    changed
}

/// The names the contract module's functions are bound to.
struct ContractExports {
    prefix: CompactString,
    // A module's own name for something the contract module provides
    aliases: FxHashMap<InternedString, InternedString>,
}

impl ContractExports {
    /// Whether `expr` names the contract module's `export`.
    fn is(&self, expr: &ExprKind, export: &str) -> bool {
        let Some(name) = expr.atom_identifier() else {
            return false;
        };
        name.resolve()
            .strip_prefix(self.prefix.as_str())
            .is_some_and(|rest| rest == export)
            || self
                .aliases
                .get(name)
                .is_some_and(|x| x.resolve() == export)
    }

    fn name(&self, export: &str) -> ExprKind {
        ExprKind::atom(CompactString::new(&self.prefix) + export)
    }
}

/// `(define x (%proto-hash-get% <the contract module> 'name))`, at the top level
/// of a module.
fn collect_aliases(
    expr: &ExprKind,
    module: InternedString,
    aliases: &mut FxHashMap<InternedString, InternedString>,
) {
    match expr {
        ExprKind::Begin(b) => {
            for expr in &b.exprs {
                collect_aliases(expr, module, aliases);
            }
        }
        ExprKind::Define(d) => {
            let (Some(name), Some(body)) = (d.name.atom_identifier(), d.body.list()) else {
                return;
            };
            if let [head, from, ExprKind::Quote(q)] = body.args.as_slice() {
                if head
                    .atom_identifier()
                    .is_some_and(|x| x.resolve() == "%proto-hash-get%")
                    && from.atom_identifier() == Some(&module)
                {
                    if let Some(export) = q.expr.atom_identifier() {
                        aliases.insert(*name, *export);
                    }
                }
            }
        }
        _ => {}
    }
}

/// Every variable something assigns with `set!`.
struct AssignedNames<'a>(&'a mut FxHashSet<InternedString>);

impl VisitorMutUnit for AssignedNames<'_> {
    fn visit_set(&mut self, s: &Set) {
        if let Some(name) = s.variable.atom_identifier() {
            self.0.insert(*name);
        }
        self.visit(&s.expr);
    }
}

fn specialize_module(
    exprs: &mut Vec<ExprKind>,
    exports: &ContractExports,
    assigned: &FxHashSet<InternedString>,
) -> bool {
    let Some(position) = exprs.iter().position(|expr| {
        matches!(expr, ExprKind::Define(d)
            if d.name.atom_identifier().is_some_and(|x| x.resolve().starts_with(MODULE_PREFIX)))
    }) else {
        return false;
    };

    let mut definitions = Vec::new();
    if let ExprKind::Define(d) = &mut exprs[position] {
        let ExprKind::List(hash) = &mut d.body else {
            return false;
        };
        if !hash
            .first_ident()
            .is_some_and(|x| x.resolve() == "%proto-hash%")
        {
            return false;
        }
        for value in hash.args.iter_mut().skip(2).step_by(2) {
            if let Some(specialized) = specialize(value, exports, assigned) {
                definitions.extend(specialized);
            }
        }
    }

    let changed = !definitions.is_empty();
    exprs.splice(position..position, definitions);
    changed
}

/// For `(bind/c (make-function/c (make/c p 'p) ...) f 'f)`, the definitions of
/// a function that checks the contract itself, replacing the expression with
/// its name.
fn specialize(
    value: &mut ExprKind,
    exports: &ContractExports,
    assigned: &FxHashSet<InternedString>,
) -> Option<Vec<ExprKind>> {
    let ExprKind::List(bind) = value else {
        return None;
    };
    let [head, contract, function, ExprKind::Quote(_)] = bind.args.as_slice() else {
        return None;
    };
    if !exports.is(head, "bind/c") {
        return None;
    }
    let function_name = *function.atom_identifier()?;
    if assigned.contains(&function_name) {
        return None;
    }

    let ExprKind::List(contract) = contract else {
        return None;
    };
    let (make_function, conditions) = contract.args.split_first()?;
    if !exports.is(make_function, "make-function/c") || conditions.is_empty() {
        return None;
    }
    let checks = conditions
        .iter()
        .map(|condition| flat_check(condition, exports))
        .collect::<Option<Vec<_>>>()?;
    let (result, arguments) = checks.split_last()?;

    let slow: InternedString = format!("{}##contract-slow", function_name.resolve()).into();
    let checked: InternedString = format!("{}##contracted", function_name.resolve()).into();
    let parameters: Vec<InternedString> = (0..arguments.len())
        .map(|i| format!("##contract-arg{i}").into())
        .collect();

    // The function's result, checked against the contract's
    let call = list(
        core::iter::once(function.clone())
            .chain(parameters.iter().map(|x| ExprKind::atom(*x)))
            .collect(),
    );
    let output: InternedString = "##contract-output".into();
    let mut body = match result {
        Check::Nothing => call,
        Check::Predicate(predicate) => ExprKind::Let(Box::new(Let::new(
            vec![(ExprKind::atom(output), call)],
            if_expr(
                list(vec![ExprKind::atom(*predicate), ExprKind::atom(output)]),
                ExprKind::atom(output),
                list(vec![
                    exports.name("#%contract-result-violation"),
                    ExprKind::atom(slow),
                    ExprKind::atom(output),
                ]),
            ),
            SyntaxObject::default(TokenType::Let),
        ))),
    };

    // A failed argument check calls the function `bind/c` made, which reports it
    let failed = list(
        core::iter::once(ExprKind::atom(slow))
            .chain(parameters.iter().map(|x| ExprKind::atom(*x)))
            .collect(),
    );
    for (check, parameter) in arguments.iter().zip(&parameters).rev() {
        if let Check::Predicate(predicate) = check {
            body = if_expr(
                list(vec![ExprKind::atom(*predicate), ExprKind::atom(*parameter)]),
                body,
                failed.clone(),
            );
        }
    }

    let lambda = ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
        parameters.iter().map(|x| ExprKind::atom(*x)).collect(),
        body,
        SyntaxObject::default(TokenType::Lambda),
    )));

    let original = core::mem::replace(value, ExprKind::atom(checked));
    Some(vec![
        define(slow, original),
        define(checked, lambda),
        // Values the contract module looks up a function's contract on
        list(vec![
            exports.name("#%contract-attach"),
            ExprKind::atom(checked),
            ExprKind::atom(slow),
        ]),
    ])
}

/// `(make/c p 'p)` where `p` is a builtin type predicate, or `any/c`.
fn flat_check(condition: &ExprKind, exports: &ContractExports) -> Option<Check> {
    let ExprKind::List(l) = condition else {
        return None;
    };
    let [make, predicate, ExprKind::Quote(_)] = l.args.as_slice() else {
        return None;
    };
    if !exports.is(make, "make/c") {
        return None;
    }
    if exports.is(predicate, "any/c") {
        return Some(Check::Nothing);
    }
    let name = predicate.atom_identifier()?;
    let builtin = name.resolve().strip_prefix("#%prim.")?;
    TYPE_PREDICATES
        .contains(&builtin)
        .then_some(Check::Predicate(*name))
}

fn list(args: Vec<ExprKind>) -> ExprKind {
    ExprKind::List(List::new(ThinVec::from(args)))
}

fn if_expr(test: ExprKind, then: ExprKind, otherwise: ExprKind) -> ExprKind {
    ExprKind::If(Box::new(If::new(
        test,
        then,
        otherwise,
        SyntaxObject::default(TokenType::If),
    )))
}

fn define(name: InternedString, body: ExprKind) -> ExprKind {
    ExprKind::Define(Box::new(Define::new(
        ExprKind::atom(name),
        body,
        SyntaxObject::default(TokenType::Define),
    )))
}
