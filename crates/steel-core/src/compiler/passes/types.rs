//! Type inference over the expanded AST.
//!
//! Computes, for every binding (`define`, `let`, lambda parameter), every
//! identifier reference, every call and every lambda's return, the set of kinds
//! of value it can hold ([`TypeSet`]). Used by the language server for inlay
//! hints, and meant to feed the jit.
//!
//! What it knows comes from:
//! - literals and quoted data;
//! - native functions' declared return types, and their `args` declarations:
//!   a call that returns normally proves its arguments had the declared types,
//!   so an immutable variable passed to one is narrowed from then on;
//! - type predicates in `if` tests (`string?`, `pair?`, `not`, `and`, ...),
//!   narrowing the tested variable in each branch;
//! - user functions: return types from their bodies, and - for functions that
//!   never escape (a local helper or loop only ever called directly) -
//!   parameter types from their call sites. Both are solved as a fixpoint, so
//!   recursive loops get types.
//!
//! It is sound in the sense that every value an expression actually produces is
//! in its set (so an empty set means "cannot return"), under two assumptions:
//! a call to a native function behaves as its annotations say, and a global
//! binding is not redefined from outside the program being analyzed. Things it
//! cannot see are `any`: mutated variables, parameters of functions that
//! escape (including every global function - it can be called from anywhere),
//! results of calls to unknown functions.

use crate::compiler::passes::analysis::Analysis;
use crate::parser::ast::{Atom, ExprKind, LambdaFunction};
use crate::parser::parser::SyntaxObjectId;
use crate::parser::tokens::TokenType;
use steel_parser::tokens::{IntLiteral, NumberLiteral, RealLiteral};
use crate::rvals::SteelVal;
use crate::steel_vm::builtin::{get_function_metadata, BuiltInFunctionType, FunctionSignatureMetadata};
use crate::steel_vm::engine::ModuleContainer;
use crate::value_type::TypeSet;
use rustc_hash::{FxHashMap, FxHashSet};

/// The signatures of the native functions a program can refer to, by name.
#[derive(Default)]
pub struct PrimitiveSignatures {
    by_name: FxHashMap<String, FunctionSignatureMetadata>,
}

impl PrimitiveSignatures {
    /// Every function value bound in `modules`, under every name it is bound to.
    pub fn from_modules(modules: &ModuleContainer) -> Self {
        let mut by_name = FxHashMap::default();
        for module in modules.inner().values() {
            for name in module.names() {
                let Some(value) = module.try_get_ref(&name) else {
                    continue;
                };
                if let Some(metadata) = signature_of(&value) {
                    by_name.entry(name).or_insert(metadata);
                }
            }
        }
        Self { by_name }
    }

    /// `name` may carry the `#%prim.` prefix the expander uses for prelude
    /// functions.
    pub fn get(&self, name: &str) -> Option<&FunctionSignatureMetadata> {
        self.by_name.get(name.strip_prefix("#%prim.").unwrap_or(name))
    }
}

fn signature_of(value: &SteelVal) -> Option<FunctionSignatureMetadata> {
    get_function_metadata(match value {
        SteelVal::FuncV(f) => BuiltInFunctionType::Reference(*f),
        SteelVal::MutFunc(f) => BuiltInFunctionType::Mutable(*f),
        SteelVal::BuiltIn(f) => BuiltInFunctionType::Context(*f),
        _ => return None,
    })
}

/// What the type predicates tell each branch of an `if`: the then-branch can
/// intersect the tested value's type with `holds`, and the else-branch can
/// remove `excludes` (only the kinds for which the predicate is always true).
fn predicate(name: &str) -> Option<(TypeSet, TypeSet)> {
    use TypeSet as T;
    let vectors = T::VECTOR.union(T::MUTABLE_VECTOR);
    Some(match name.strip_prefix("#%prim.").unwrap_or(name) {
        "string?" => (T::STRING, T::STRING),
        "symbol?" => (T::SYMBOL, T::SYMBOL),
        "char?" => (T::CHAR, T::CHAR),
        "boolean?" | "bool?" => (T::BOOL, T::BOOL),
        "void?" => (T::VOID, T::VOID),
        "hash?" => (T::HASHMAP, T::HASHMAP),
        "set?" => (T::HASHSET, T::HASHSET),
        "bytes?" | "bytevector?" => (T::BYTEVECTOR, T::BYTEVECTOR),
        "mutable-vector?" => (T::MUTABLE_VECTOR, T::MUTABLE_VECTOR),
        "immutable-vector?" => (T::VECTOR, T::VECTOR),
        // Flat vectors are OTHER.
        "vector?" => (vectors.union(T::OTHER), vectors),
        "list?" => (T::LIST, T::LIST),
        // True for a non-empty list or a pair; '() is a list too.
        "pair?" => (T::LIST.union(T::PAIR), T::PAIR),
        // Also true for empty vectors.
        "null?" => (T::LIST.union(vectors), T::BOTTOM),
        "empty?" => (T::LIST, T::BOTTOM),
        "number?" | "complex?" => (T::NUMBER, T::NUMBER),
        "real?" => (T::NUMBER, T::INT.union(T::FLOAT)),
        // Integral floats count; a fixnum always does.
        "int?" | "integer?" | "rational?" => (T::NUMBER, T::INT),
        "exact-integer?" => (T::INT.union(T::OTHER_NUMBER), T::INT),
        "float?" => (T::FLOAT, T::FLOAT),
        "function?" => (T::PROCEDURE, T::PROCEDURE),
        // A struct with a procedure property is OTHER.
        "procedure?" => (T::PROCEDURE.union(T::OTHER), T::PROCEDURE),
        _ => return None,
    })
}

/// The result of [`infer_types`].
#[derive(Default, Clone, PartialEq, Debug)]
pub struct TypeInfo {
    /// Binding sites (`define` / `let` names, lambda parameters), by the
    /// syntax object id of the name.
    pub bindings: FxHashMap<SyntaxObjectId, TypeSet>,
    /// Identifier references, by their syntax object id: what the variable
    /// holds at that point (after narrowing).
    pub references: FxHashMap<SyntaxObjectId, TypeSet>,
    /// Calls, by the list's syntax object id: what the call returns.
    pub calls: FxHashMap<u32, TypeSet>,
    /// Lambdas, by their syntax object id: what a call of it returns.
    pub returns: FxHashMap<u32, TypeSet>,
    /// How many passes over the program the fixpoint took.
    pub iterations: usize,
}

/// Infers types for `exprs`, which `analysis` was computed for.
pub fn infer_types(
    exprs: &[ExprKind],
    analysis: &Analysis,
    primitives: &PrimitiveSignatures,
) -> TypeInfo {
    let facts = Facts::collect(exprs, analysis, primitives);

    let mut previous = TypeInfo::default();
    let mut previous_params: FxHashMap<u32, Vec<TypeSet>> = FxHashMap::default();

    // Every step can only grow a set, and a set can only grow 17 times, so
    // this terminates; the bound is a guard against a bug making it cycle.
    for iteration in 1..=64 {
        let mut inference = Inference {
            analysis,
            primitives,
            facts: &facts,
            previous: &previous,
            previous_params: &previous_params,
            current: TypeInfo::default(),
            params: FxHashMap::default(),
        };
        let mut env = Env::default();
        for expr in exprs {
            inference.infer(expr, &mut env);
        }
        let Inference {
            current, params, ..
        } = inference;
        if current == previous && params == previous_params {
            return TypeInfo {
                iterations: iteration,
                ..current
            };
        }
        previous = current;
        previous_params = params;
    }

    // Did not settle: claim nothing.
    let mut result = previous;
    for ty in result
        .bindings
        .values_mut()
        .chain(result.references.values_mut())
        .chain(result.calls.values_mut())
        .chain(result.returns.values_mut())
    {
        *ty = TypeSet::TOP;
    }
    result
}

/// What a variable is known to hold, when it is never mutated.
#[derive(Clone, Copy, Debug, PartialEq)]
enum Known {
    /// A lambda, by id.
    Lambda(u32),
    /// A native function, by name.
    Primitive(InternedName),
    /// A box whose only `set-box!` stores this lambda: how a named `let` or
    /// `letrec` expands (`(#%box _)`, `(#%set-box! box (λ ...))`, and calls as
    /// `((#%unbox box) args ...)`).
    BoxedLambda(u32),
}

type InternedName = crate::parser::interner::InternedString;

/// Where a variable reference appears.
#[derive(Clone, Copy, PartialEq)]
enum Position {
    /// The operator of a call.
    Operator,
    /// `(#%unbox x)` as the operator of a call.
    UnboxOperator,
    /// The box of a `(#%set-box! x ...)`.
    SetBoxTarget,
    /// The value of a `(#%set-box! box x)`, stored into that box.
    BoxStore(SyntaxObjectId),
    /// Anywhere else: the value escapes.
    Value,
}

/// Program-wide facts, gathered once before the fixpoint.
#[derive(Default)]
struct Facts {
    /// Bindings that are the target of a `set!`.
    mutated: FxHashSet<SyntaxObjectId>,
    /// Every binding site in the program.
    defined: FxHashSet<SyntaxObjectId>,
    /// Top-level `define`s: callable from outside, so their parameters are
    /// never inferred from call sites.
    globals: FxHashSet<SyntaxObjectId>,
    known: FxHashMap<SyntaxObjectId, Known>,
    /// Lambdas by id: (fixed parameter count, has a rest parameter).
    arity: FxHashMap<u32, (usize, bool)>,
    /// Lambdas whose every use is a direct call we can see, so their
    /// parameter types are the union of the argument types at those calls.
    closed: FxHashSet<u32>,
}

impl Facts {
    fn collect(exprs: &[ExprKind], analysis: &Analysis, primitives: &PrimitiveSignatures) -> Self {
        let mut collector = Collector {
            analysis,
            primitives,
            facts: Facts::default(),
            boxes: FxHashSet::default(),
            box_stores: FxHashMap::default(),
            aliases: FxHashMap::default(),
            uses: FxHashMap::default(),
            bound_lambdas: FxHashMap::default(),
            applied_lambdas: FxHashSet::default(),
        };
        for expr in exprs {
            collector.top_level(expr);
        }
        collector.finish()
    }
}

struct Collector<'a> {
    analysis: &'a Analysis,
    primitives: &'a PrimitiveSignatures,
    facts: Facts,
    /// Bindings initialized with `(#%box _)`.
    boxes: FxHashSet<SyntaxObjectId>,
    /// `(#%set-box! b v)`: the lambda stored (None when it is not a lambda).
    box_stores: FxHashMap<SyntaxObjectId, Vec<Option<u32>>>,
    /// Bindings initialized with another variable.
    aliases: FxHashMap<SyntaxObjectId, SyntaxObjectId>,
    uses: FxHashMap<SyntaxObjectId, Vec<Position>>,
    /// Lambdas bound directly to a variable (`let`, `define`) or stored in a box.
    bound_lambdas: FxHashMap<u32, SyntaxObjectId>,
    /// Lambdas that are the operator of a call.
    applied_lambdas: FxHashSet<u32>,
}

impl<'a> Collector<'a> {
    fn top_level(&mut self, expr: &ExprKind) {
        match expr {
            ExprKind::Define(define) => {
                if let Some(id) = binding_site(&define.name) {
                    self.facts.globals.insert(id);
                }
                self.expr(expr, Position::Value);
            }
            ExprKind::Begin(begin) => {
                for expr in &begin.exprs {
                    self.top_level(expr);
                }
            }
            _ => self.expr(expr, Position::Value),
        }
    }

    fn reference(&self, atom: &Atom) -> Option<SyntaxObjectId> {
        reference(self.analysis, atom)
    }

    fn bind(&mut self, name: &ExprKind, init: Option<&ExprKind>) {
        let Some(id) = binding_site(name) else { return };
        self.facts.defined.insert(id);
        match init {
            Some(ExprKind::LambdaFunction(lambda)) => {
                self.facts.known.insert(id, Known::Lambda(lambda.syntax_object_id));
                self.bound_lambdas.insert(lambda.syntax_object_id, id);
            }
            Some(ExprKind::Atom(atom)) => match self.reference(atom) {
                Some(target) => {
                    self.aliases.insert(id, target);
                }
                None => {
                    if let Some(name) = atom.ident() {
                        if self.primitives.get(name.resolve()).is_some() {
                            self.facts.known.insert(id, Known::Primitive(*name));
                        }
                    }
                }
            },
            Some(ExprKind::List(list)) if operator_name(list.args.first()) == Some("#%box") => {
                self.boxes.insert(id);
            }
            _ => {}
        }
    }

    fn expr(&mut self, expr: &ExprKind, position: Position) {
        match expr {
            ExprKind::Atom(atom) => {
                if let Some(id) = self.reference(atom) {
                    self.uses.entry(id).or_default().push(position);
                }
            }
            ExprKind::Define(define) => {
                self.bind(&define.name, Some(&define.body));
                self.expr(&define.body, Position::Value);
            }
            ExprKind::Let(let_) => {
                for (name, init) in &let_.bindings {
                    self.bind(name, Some(init));
                    self.expr(init, Position::Value);
                }
                self.expr(&let_.body_expr, Position::Value);
            }
            ExprKind::LambdaFunction(lambda) => {
                self.facts.arity.insert(
                    lambda.syntax_object_id,
                    (lambda.args.len() - usize::from(lambda.rest), lambda.rest),
                );
                for arg in &lambda.args {
                    self.bind(arg, None);
                }
                self.expr(&lambda.body, Position::Value);
            }
            ExprKind::Set(set) => {
                if let ExprKind::Atom(atom) = &set.variable {
                    if let Some(id) = self.reference(atom) {
                        self.facts.mutated.insert(id);
                    }
                }
                self.expr(&set.expr, Position::Value);
            }
            ExprKind::If(if_) => {
                self.expr(&if_.test_expr, Position::Value);
                self.expr(&if_.then_expr, Position::Value);
                self.expr(&if_.else_expr, Position::Value);
            }
            ExprKind::Begin(begin) => {
                for expr in &begin.exprs {
                    self.expr(expr, Position::Value);
                }
            }
            ExprKind::Return(return_) => self.expr(&return_.expr, Position::Value),
            ExprKind::List(list) => {
                let Some((operator, args)) = list.args.split_first() else {
                    return;
                };
                match operator_name(Some(operator)) {
                    Some("#%set-box!") if args.len() == 2 => {
                        let target = match &args[0] {
                            ExprKind::Atom(atom) => self.reference(atom),
                            _ => None,
                        };
                        if let Some(id) = target {
                            // The lambda itself, or a variable bound to one
                            // (how `letrec` expands).
                            let stored = match &args[1] {
                                ExprKind::LambdaFunction(lambda) => {
                                    self.bound_lambdas.insert(lambda.syntax_object_id, id);
                                    Some(lambda.syntax_object_id)
                                }
                                ExprKind::Atom(atom) => self
                                    .reference(atom)
                                    .and_then(|v| self.facts.known.get(&v).copied())
                                    .and_then(|known| match known {
                                        Known::Lambda(lambda) => Some(lambda),
                                        _ => None,
                                    }),
                                _ => None,
                            };
                            self.box_stores.entry(id).or_default().push(stored);
                        }
                        self.expr(&args[0], Position::SetBoxTarget);
                        let position = target.map_or(Position::Value, Position::BoxStore);
                        self.expr(&args[1], position);
                        return;
                    }
                    _ => {}
                }
                match operator {
                    ExprKind::List(inner)
                        if operator_name(inner.args.first()) == Some("#%unbox")
                            && inner.args.len() == 2 =>
                    {
                        self.expr(&inner.args[0], Position::Value);
                        self.expr(&inner.args[1], Position::UnboxOperator);
                    }
                    ExprKind::LambdaFunction(lambda) => {
                        self.applied_lambdas.insert(lambda.syntax_object_id);
                        self.expr(operator, Position::Operator);
                    }
                    _ => self.expr(operator, Position::Operator),
                }
                for arg in args {
                    self.expr(arg, Position::Value);
                }
            }
            ExprKind::Quote(_)
            | ExprKind::Vector(_)
            | ExprKind::Macro(_)
            | ExprKind::SyntaxRules(_)
            | ExprKind::Require(_) => {}
        }
    }

    fn finish(mut self) -> Facts {
        // Boxed lambdas: a box whose one and only store is a lambda.
        for (id, stores) in &self.box_stores {
            if let ([Some(lambda)], true) = (stores.as_slice(), self.boxes.contains(id)) {
                if !self.facts.mutated.contains(id) {
                    self.facts.known.insert(*id, Known::BoxedLambda(*lambda));
                }
            }
        }

        // Aliases of known values (chains resolve over a few rounds).
        for _ in 0..8 {
            let mut changed = false;
            for (alias, target) in &self.aliases {
                if self.facts.known.contains_key(alias) || self.facts.mutated.contains(target) {
                    continue;
                }
                if let Some(known) = self.facts.known.get(target).copied() {
                    self.facts.known.insert(*alias, known);
                    changed = true;
                }
            }
            if !changed {
                break;
            }
        }

        // A mutated variable holds whatever was last assigned.
        for id in &self.facts.mutated {
            self.facts.known.remove(id);
        }

        // Closed lambdas: every way to reach one is a direct call we can see.
        // A box holding a lambda is closed when it is only ever unboxed to be
        // called (plus its one store).
        let box_closed = |facts: &Facts, uses: &FxHashMap<SyntaxObjectId, Vec<Position>>, b: &SyntaxObjectId| {
            matches!(facts.known.get(b), Some(Known::BoxedLambda(_)))
                && !facts.globals.contains(b)
                && uses.get(b).map_or(true, |uses| {
                    uses.iter()
                        .all(|p| matches!(p, Position::UnboxOperator | Position::SetBoxTarget))
                })
        };
        for (&lambda, binding) in &self.bound_lambdas {
            if self.facts.globals.contains(binding) {
                continue;
            }
            let aliased = self.aliases.values().any(|target| target == binding);
            if aliased {
                continue;
            }
            let uses = self.uses.get(binding).map(Vec::as_slice).unwrap_or(&[]);
            let closed = match self.facts.known.get(binding) {
                // Bound to a variable: called directly, or stored into a closed
                // box that holds it.
                Some(Known::Lambda(l)) if *l == lambda => uses.iter().all(|position| match position {
                    Position::Operator => true,
                    Position::BoxStore(b) => {
                        matches!(self.facts.known.get(b), Some(Known::BoxedLambda(l)) if *l == lambda)
                            && box_closed(&self.facts, &self.uses, b)
                    }
                    _ => false,
                }),
                // Stored straight into a box.
                Some(Known::BoxedLambda(l)) if *l == lambda => box_closed(&self.facts, &self.uses, binding),
                _ => false,
            };
            if closed {
                self.facts.closed.insert(lambda);
            }
        }
        // An immediately applied lambda is only ever called there.
        for lambda in &self.applied_lambdas {
            if !self.bound_lambdas.contains_key(lambda) {
                self.facts.closed.insert(*lambda);
            }
        }

        self.facts
    }
}

/// The syntax object id a binding site (`define` / `let` name, parameter)
/// declares.
fn binding_site(name: &ExprKind) -> Option<SyntaxObjectId> {
    match name {
        ExprKind::Atom(atom) if atom.ident().is_some() => Some(atom.syn.syntax_object_id),
        _ => None,
    }
}

/// The binding an identifier reference refers to, if it is bound in the program.
fn reference(analysis: &Analysis, atom: &Atom) -> Option<SyntaxObjectId> {
    atom.ident()?;
    analysis
        .info
        .get(&atom.syn.syntax_object_id)
        .and_then(|info| info.refers_to)
}

fn operator_name(expr: Option<&ExprKind>) -> Option<&str> {
    let name = expr?.atom_identifier()?.resolve();
    Some(name.strip_prefix("#%prim.").unwrap_or(name))
}

/// Narrowed types of variables at a point in the program. Only ever holds
/// immutable bindings: narrowing a variable that can be reassigned would not
/// survive the assignment.
type Env = FxHashMap<SyntaxObjectId, TypeSet>;

/// What a call's operator is.
enum Callee<'p> {
    Primitive(&'p FunctionSignatureMetadata),
    Lambda(u32),
    Unknown,
}

struct Inference<'a> {
    analysis: &'a Analysis,
    primitives: &'a PrimitiveSignatures,
    facts: &'a Facts,
    /// The last iteration's results: what calls to functions analyzed later
    /// (or recursively) return.
    previous: &'a TypeInfo,
    previous_params: &'a FxHashMap<u32, Vec<TypeSet>>,
    current: TypeInfo,
    /// Parameter types of closed lambdas, from this iteration's call sites.
    params: FxHashMap<u32, Vec<TypeSet>>,
}

impl<'a> Inference<'a> {
    /// What binding `id` holds, before any narrowing.
    fn declared(&self, id: SyntaxObjectId) -> TypeSet {
        if self.facts.mutated.contains(&id) || !self.facts.defined.contains(&id) {
            return TypeSet::TOP;
        }
        self.current
            .bindings
            .get(&id)
            .or_else(|| self.previous.bindings.get(&id))
            .copied()
            // Not analyzed yet in any iteration: optimistic, the fixpoint grows it.
            .unwrap_or(TypeSet::BOTTOM)
    }

    fn lookup(&self, id: SyntaxObjectId, env: &Env) -> TypeSet {
        match env.get(&id) {
            Some(ty) => *ty,
            None => self.declared(id),
        }
    }

    /// Records `ty` for binding `id`. A name can be bound more than once
    /// (a top-level redefinition): it can hold either.
    fn bind(&mut self, name: &ExprKind, ty: TypeSet) {
        if let Some(id) = binding_site(name) {
            let ty = if self.facts.mutated.contains(&id) { TypeSet::TOP } else { ty };
            let entry = self.current.bindings.entry(id).or_insert(TypeSet::BOTTOM);
            *entry = entry.union(ty);
        }
    }

    /// Narrows an immutable variable in `env`.
    fn narrow(&self, expr: &ExprKind, env: &mut Env, to: impl FnOnce(TypeSet) -> TypeSet) {
        let ExprKind::Atom(atom) = expr else { return };
        let Some(id) = reference(self.analysis, atom) else { return };
        if self.facts.mutated.contains(&id) || !self.facts.defined.contains(&id) {
            return;
        }
        let narrowed = to(self.lookup(id, env));
        env.insert(id, narrowed);
    }

    fn callee(&self, operator: &ExprKind) -> Callee<'a> {
        match operator {
            ExprKind::LambdaFunction(lambda) => Callee::Lambda(lambda.syntax_object_id),
            ExprKind::Atom(atom) => {
                let Some(name) = atom.ident() else { return Callee::Unknown };
                match reference(self.analysis, atom) {
                    Some(id) => match self.facts.known.get(&id) {
                        Some(Known::Lambda(lambda)) => Callee::Lambda(*lambda),
                        Some(Known::Primitive(name)) => self
                            .primitives
                            .get(name.resolve())
                            .map_or(Callee::Unknown, Callee::Primitive),
                        // Bound in the program to something we cannot see
                        // through - even if it shares a builtin's name (a
                        // module can provide its own `car`).
                        _ => Callee::Unknown,
                    },
                    None => self.free_primitive(name.resolve()).map_or(Callee::Unknown, Callee::Primitive),
                }
            }
            ExprKind::List(list)
                if operator_name(list.args.first()) == Some("#%unbox") && list.args.len() == 2 =>
            {
                let ExprKind::Atom(atom) = &list.args[1] else { return Callee::Unknown };
                match reference(self.analysis, atom).and_then(|id| self.facts.known.get(&id)) {
                    Some(Known::BoxedLambda(lambda)) => Callee::Lambda(*lambda),
                    _ => Callee::Unknown,
                }
            }
            _ => Callee::Unknown,
        }
    }

    fn infer(&mut self, expr: &ExprKind, env: &mut Env) -> TypeSet {
        match expr {
            ExprKind::Atom(atom) => self.atom(atom, env),
            ExprKind::Quote(quote) => datum_type(&quote.expr),
            ExprKind::Vector(vector) => {
                if vector.bytes {
                    TypeSet::BYTEVECTOR
                } else {
                    TypeSet::VECTOR
                }
            }
            ExprKind::If(if_) => {
                self.infer(&if_.test_expr, env);
                let mut then_env = env.clone();
                let mut else_env = env.clone();
                self.test(&if_.test_expr, &mut then_env, &mut else_env);
                let then_ty = self.infer(&if_.then_expr, &mut then_env);
                let else_ty = self.infer(&if_.else_expr, &mut else_env);
                *env = self.join(then_env, else_env);
                then_ty.union(else_ty)
            }
            ExprKind::Let(let_) => {
                for (name, init) in &let_.bindings {
                    let ty = self.infer(init, env);
                    self.bind(name, ty);
                }
                self.infer(&let_.body_expr, env)
            }
            ExprKind::Define(define) => {
                let ty = self.infer(&define.body, env);
                self.bind(&define.name, ty);
                TypeSet::VOID
            }
            ExprKind::LambdaFunction(lambda) => {
                self.lambda(lambda, env);
                TypeSet::PROCEDURE
            }
            ExprKind::Begin(begin) => {
                let mut ty = TypeSet::VOID;
                for expr in &begin.exprs {
                    ty = self.infer(expr, env);
                }
                ty
            }
            ExprKind::Set(set) => {
                self.infer(&set.expr, env);
                // `set!` evaluates to the variable's previous value - and an
                // assigned variable can hold anything.
                TypeSet::TOP
            }
            ExprKind::Return(return_) => {
                self.infer(&return_.expr, env);
                TypeSet::TOP
            }
            ExprKind::List(list) => {
                let ty = self.call(list, env);
                self.current.calls.insert(list.syntax_object_id, ty);
                ty
            }
            ExprKind::Macro(_) | ExprKind::SyntaxRules(_) | ExprKind::Require(_) => TypeSet::VOID,
        }
    }

    fn atom(&mut self, atom: &Atom, env: &Env) -> TypeSet {
        let ty = match &atom.syn.ty {
            TokenType::Identifier(name) => match reference(self.analysis, atom) {
                Some(id) if self.facts.defined.contains(&id) => self.lookup(id, env),
                // Bound to something outside what we analyzed.
                Some(_) => TypeSet::TOP,
                None if self.free_primitive(name.resolve()).is_some() => TypeSet::PROCEDURE,
                None => TypeSet::TOP,
            },
            TokenType::Number(number) => number_type(&number.resolve()),
            TokenType::StringLiteral(_) => TypeSet::STRING,
            TokenType::CharacterLiteral(_) => TypeSet::CHAR,
            TokenType::BooleanLiteral(_) => TypeSet::BOOL,
            _ => TypeSet::TOP,
        };
        if atom.ident().is_some() {
            self.current.references.insert(atom.syn.syntax_object_id, ty);
        }
        ty
    }

    fn lambda(&mut self, lambda: &LambdaFunction, env: &Env) {
        let id = lambda.syntax_object_id;
        let closed = self.facts.closed.contains(&id);
        let fixed = lambda.args.len() - usize::from(lambda.rest);
        for (i, arg) in lambda.args.iter().enumerate() {
            let ty = if lambda.rest && i == fixed {
                TypeSet::LIST
            } else if closed {
                self.previous_params
                    .get(&id)
                    .and_then(|params| params.get(i))
                    .copied()
                    .unwrap_or(TypeSet::BOTTOM)
            } else {
                TypeSet::TOP
            };
            self.bind(arg, ty);
        }
        // Captured variables keep the narrowing they have here: only immutable
        // ones are ever narrowed, and they hold the same value in the body.
        let mut body_env = env.clone();
        let ty = self.infer(&lambda.body, &mut body_env);
        self.current.returns.insert(id, ty);
    }

    fn call(&mut self, list: &crate::parser::ast::List, env: &mut Env) -> TypeSet {
        let Some((operator, args)) = list.args.split_first() else {
            // `()`
            return TypeSet::LIST;
        };

        let callee = self.callee(operator);
        self.infer(operator, env);
        let arg_types: Vec<TypeSet> = args.iter().map(|arg| self.infer(arg, env)).collect();

        match callee {
            Callee::Primitive(signature) => {
                // A call that returned proves its arguments had the declared types.
                if let Some(declared) = &signature.arg_types {
                    let count_ok = args.len() >= declared.fixed.len()
                        && (declared.rest.is_some() || args.len() == declared.fixed.len());
                    if count_ok {
                        for (i, arg) in args.iter().enumerate() {
                            let Some(ty) = declared.get(i) else { continue };
                            let ty = TypeSet::from_value_type(ty);
                            if !ty.is_top() {
                                self.narrow(arg, env, |current| current.intersect(ty));
                            }
                        }
                    }
                }
                TypeSet::from_value_type(signature.return_type)
            }
            Callee::Lambda(lambda) => {
                if self.facts.closed.contains(&lambda) {
                    if let Some(&(fixed, rest)) = self.facts.arity.get(&lambda) {
                        if arg_types.len() == fixed || (rest && arg_types.len() >= fixed) {
                            let params = self
                                .params
                                .entry(lambda)
                                .or_insert_with(|| vec![TypeSet::BOTTOM; fixed]);
                            for (param, ty) in params.iter_mut().zip(&arg_types) {
                                *param = param.union(*ty);
                            }
                        }
                    }
                }
                self.current
                    .returns
                    .get(&lambda)
                    .or_else(|| self.previous.returns.get(&lambda))
                    .copied()
                    .unwrap_or(TypeSet::BOTTOM)
            }
            Callee::Unknown => TypeSet::TOP,
        }
    }

    /// Narrows `then_env` / `else_env` for the branches of an `if` testing `test`.
    fn test(&self, test: &ExprKind, then_env: &mut Env, else_env: &mut Env) {
        match test {
            // Only #f is false.
            ExprKind::Atom(_) => self.narrow(test, else_env, |ty| ty.intersect(TypeSet::BOOL)),
            ExprKind::List(list) if list.args.len() == 2 => {
                let name = operator_name(list.args.first());
                if name == Some("not") && self.is_primitive(&list.args[0]) {
                    self.test(&list.args[1], else_env, then_env);
                    return;
                }
                if let Some((holds, excludes)) = name.and_then(predicate) {
                    if self.is_primitive(&list.args[0]) {
                        self.narrow(&list.args[1], then_env, |ty| ty.intersect(holds));
                        self.narrow(&list.args[1], else_env, |ty| ty.without(excludes));
                    }
                }
            }
            // `(and a b)` expands to `(if a b #f)`: when it is true, both were.
            ExprKind::If(if_) if is_false_literal(&if_.else_expr) => {
                let mut ignored = then_env.clone();
                self.test(&if_.test_expr, then_env, &mut ignored);
                let mut ignored = then_env.clone();
                self.test(&if_.then_expr, then_env, &mut ignored);
            }
            _ => {}
        }
    }

    /// The native function a free identifier (bound nowhere in the program)
    /// denotes. `#%prim.<name>` is always the builtin; a bare name is the
    /// builtin only because nothing in the program binds it - the analysis's
    /// own `builtin` flag goes by name alone, and is set for a module's `car`
    /// too.
    fn free_primitive(&self, name: &str) -> Option<&'a FunctionSignatureMetadata> {
        self.primitives.get(name)
    }

    /// Whether `operator` refers to a native function (not a user binding
    /// that happens to share its name).
    fn is_primitive(&self, operator: &ExprKind) -> bool {
        matches!(self.callee(operator), Callee::Primitive(_))
    }

    /// The environment after an `if`: a variable is either branch's type.
    fn join(&self, left: Env, right: Env) -> Env {
        let ids: FxHashSet<SyntaxObjectId> = left.keys().chain(right.keys()).copied().collect();
        ids.into_iter()
            .map(|id| {
                let l = left.get(&id).copied().unwrap_or_else(|| self.declared(id));
                let r = right.get(&id).copied().unwrap_or_else(|| self.declared(id));
                (id, l.union(r))
            })
            .collect()
    }
}

fn is_false_literal(expr: &ExprKind) -> bool {
    matches!(expr, ExprKind::Atom(atom) if matches!(atom.syn.ty, TokenType::BooleanLiteral(false)))
}

fn number_type(number: &NumberLiteral) -> TypeSet {
    match number {
        NumberLiteral::Real(RealLiteral::Int(IntLiteral::Small(_))) => TypeSet::INT,
        NumberLiteral::Real(RealLiteral::Int(IntLiteral::Big(_))) => TypeSet::OTHER_NUMBER,
        // Normalized: 4/2 is the fixnum 2.
        NumberLiteral::Real(RealLiteral::Rational(..)) => TypeSet::INT.union(TypeSet::OTHER_NUMBER),
        NumberLiteral::Real(RealLiteral::Float(_)) => TypeSet::FLOAT,
        NumberLiteral::Complex(..) | NumberLiteral::Polar(..) => TypeSet::NUMBER,
    }
}

/// The type of a quoted datum.
fn datum_type(expr: &ExprKind) -> TypeSet {
    match expr {
        ExprKind::Atom(atom) => match &atom.syn.ty {
            TokenType::Identifier(_) => TypeSet::SYMBOL,
            TokenType::Number(number) => number_type(&number.resolve()),
            TokenType::StringLiteral(_) => TypeSet::STRING,
            TokenType::CharacterLiteral(_) => TypeSet::CHAR,
            TokenType::BooleanLiteral(_) => TypeSet::BOOL,
            _ => TypeSet::TOP,
        },
        ExprKind::List(list) if list.improper => TypeSet::PAIR,
        ExprKind::List(_) => TypeSet::LIST,
        ExprKind::Vector(vector) if vector.bytes => TypeSet::BYTEVECTOR,
        ExprKind::Vector(_) => TypeSet::VECTOR,
        _ => TypeSet::TOP,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use thin_vec::thin_vec;
    use crate::compiler::passes::analysis::SemanticAnalysis;
    use crate::parser::ast::ExprKind;
    use crate::steel_vm::engine::Engine;
    use std::collections::BTreeMap;

    /// Infers types for `src`, and describes them by the source text of each
    /// name: `name` for a binding (numbered `name#2`... when a name is bound
    /// more than once), `name()` for what the function bound to it returns,
    /// and `name@line:col` for a narrowed reference.
    fn describe(src: &str) -> BTreeMap<String, String> {
        describe_at(src, None)
    }

    /// [`describe`] for a program at `path` (so its `require`s resolve).
    fn describe_at(src: &str, path: Option<std::path::PathBuf>) -> BTreeMap<String, String> {
        let mut engine = Engine::new();
        let mut exprs = engine
            .emit_expanded_ast_without_optimizations(src, path)
            .unwrap();
        let analysis = SemanticAnalysis::new(&mut exprs);
        let primitives = PrimitiveSignatures::from_modules(engine.builtin_modules());
        let info = infer_types(&analysis.exprs, &analysis.analysis, &primitives);

        let text = |atom: &Atom| -> Option<String> {
            let span = atom.syn.span;
            src.get(span.start as usize..span.end as usize).map(str::to_string)
        };
        let mut out = BTreeMap::new();
        let insert = |out: &mut BTreeMap<String, String>, key: String, value: String| {
            let mut key_n = key.clone();
            let mut n = 1;
            while out.contains_key(&key_n) {
                n += 1;
                key_n = format!("{key}#{n}");
            }
            out.insert(key_n, value);
        };

        fn walk(expr: &ExprKind, f: &mut dyn FnMut(&ExprKind)) {
            f(expr);
            match expr {
                ExprKind::If(x) => {
                    walk(&x.test_expr, f);
                    walk(&x.then_expr, f);
                    walk(&x.else_expr, f);
                }
                ExprKind::Let(x) => {
                    for (name, init) in &x.bindings {
                        walk(name, f);
                        walk(init, f);
                    }
                    walk(&x.body_expr, f);
                }
                ExprKind::Define(x) => {
                    walk(&x.name, f);
                    walk(&x.body, f);
                }
                ExprKind::LambdaFunction(x) => {
                    for arg in &x.args {
                        walk(arg, f);
                    }
                    walk(&x.body, f);
                }
                ExprKind::Begin(x) => x.exprs.iter().for_each(|e| walk(e, f)),
                ExprKind::Set(x) => {
                    walk(&x.variable, f);
                    walk(&x.expr, f);
                }
                ExprKind::List(x) => x.args.iter().for_each(|e| walk(e, f)),
                ExprKind::Return(x) => walk(&x.expr, f),
                _ => {}
            }
        }

        for expr in analysis.exprs.iter() {
            walk(expr, &mut |expr| {
                // Functions: `name()` is the return type of the lambda bound to `name`.
                let named_lambda = match expr {
                    ExprKind::Define(d) => Some((&d.name, &d.body)),
                    ExprKind::Let(l) if l.bindings.len() == 1 => {
                        Some((&l.bindings[0].0, &l.bindings[0].1))
                    }
                    _ => None,
                };
                if let Some((ExprKind::Atom(name), ExprKind::LambdaFunction(lambda))) = named_lambda {
                    if let (Some(name), Some(ty)) = (text(name), info.returns.get(&lambda.syntax_object_id)) {
                        insert(&mut out, format!("{name}()"), ty.to_string());
                    }
                }
                let ExprKind::Atom(atom) = expr else { return };
                let Some(name) = text(atom) else { return };
                let id = atom.syn.syntax_object_id;
                if let Some(ty) = info.bindings.get(&id) {
                    insert(&mut out, name, ty.to_string());
                } else if let Some(ty) = info.references.get(&id) {
                    let (line, col) = line_col(src, atom.syn.span.start as usize);
                    insert(&mut out, format!("{name}@{line}:{col}"), ty.to_string());
                }
            });
        }
        out
    }

    fn collect_binding_names(
        expr: &ExprKind,
        out: &mut FxHashMap<SyntaxObjectId, (String, crate::parser::span::Span)>,
    ) {
        fn add(
            name: &ExprKind,
            out: &mut FxHashMap<SyntaxObjectId, (String, crate::parser::span::Span)>,
        ) {
            if let ExprKind::Atom(atom) = name {
                if let Some(ident) = atom.ident() {
                    out.insert(atom.syn.syntax_object_id, (ident.resolve().to_string(), atom.syn.span));
                }
            }
        }
        match expr {
            ExprKind::Define(d) => {
                add(&d.name, out);
                collect_binding_names(&d.body, out);
            }
            ExprKind::Let(l) => {
                for (name, init) in &l.bindings {
                    add(name, out);
                    collect_binding_names(init, out);
                }
                collect_binding_names(&l.body_expr, out);
            }
            ExprKind::LambdaFunction(l) => {
                for arg in &l.args {
                    add(arg, out);
                }
                collect_binding_names(&l.body, out);
            }
            ExprKind::If(i) => {
                collect_binding_names(&i.test_expr, out);
                collect_binding_names(&i.then_expr, out);
                collect_binding_names(&i.else_expr, out);
            }
            ExprKind::Begin(b) => b.exprs.iter().for_each(|e| collect_binding_names(e, out)),
            ExprKind::List(l) => l.args.iter().for_each(|e| collect_binding_names(e, out)),
            ExprKind::Set(s) => collect_binding_names(&s.expr, out),
            ExprKind::Return(r) => collect_binding_names(&r.expr, out),
            _ => {}
        }
    }

    fn line_col(src: &str, offset: usize) -> (usize, usize) {
        let before = &src[..offset];
        let line = before.matches('\n').count() + 1;
        let col = offset - before.rfind('\n').map_or(0, |i| i + 1) + 1;
        (line, col)
    }

    /// Asserts each `(key, type)` against [`describe`].
    #[track_caller]
    fn check(src: &str, expected: &[(&str, &str)]) {
        let types = describe(src);
        for (key, ty) in expected {
            assert_eq!(
                types.get(*key).map(String::as_str),
                Some(*ty),
                "`{key}` in:\n{src}\nall types: {types:#?}"
            );
        }
    }

    #[test]
    fn literals_and_primitive_results() {
        check(
            "(define a 1) (define b 1.5) (define c \"s\") (define d #\\a) (define e 'sym)
             (define f '(1 2)) (define g (list 1 2)) (define h (string-length c))
             (define i (string-append c c)) (define j (+ a 1))",
            &[
                ("a", "int"),
                ("b", "float"),
                ("c", "string"),
                ("d", "char"),
                ("e", "symbol"),
                ("f", "list"),
                ("g", "list"),
                ("h", "int"),
                ("i", "string"),
                // A fixnum sum can overflow into a bignum.
                ("j", "number"),
            ],
        );
    }

    #[test]
    fn predicates_narrow_branches() {
        check(
            "(define (g x) (if (string? x) (string-length x) 0))
             (define (q x) (if (not (string? x)) 0 (string-upcase x)))
             (define (r x y) (if (and (string? x) (int? y)) x \"no\"))
             (define (p x) (if (pair? x) (car x) x))",
            &[
                ("x", "any"),
                ("g()", "int"),
                ("x@1:46", "string"),
                // `not` swaps the branches.
                ("x@2:67", "string"),
                ("q()", "int | string"),
                // Both halves of an `and` hold in its then-branch.
                ("x@3:61", "string"),
                ("r()", "string"),
                ("x@4:47", "list | pair"),
                // The else-branch: anything but a pair.
                ("x@4:50", "any except pair"),
            ],
        );
    }

    #[test]
    fn successful_calls_narrow_their_arguments() {
        check(
            "(define (h s) (string-length s) (string-upcase s))
             (define (k v i) (vector-ref v i) i)",
            &[
                // Before the call nothing is known; after it, `s` was a string.
                ("s@1:30", "any"),
                ("s@1:48", "string"),
                ("h()", "string"),
                ("i@2:47", "int"),
                ("k()", "int"),
            ],
        );
    }

    #[test]
    fn mutation_and_shadowing() {
        check(
            "(define (m) (let ([v 1]) (set! v \"s\") v))
             (define (sr) (let ([w 1]) (set! w 2)))
             (define (string-length x) \"shadow\")
             (define (u) (string-length \"a\"))",
            &[
                ("v", "any"),
                ("m()", "any"),
                // `set!` evaluates to the previous value, not void.
                ("sr()", "any"),
                // The user's definition, not the primitive.
                ("u()", "string"),
            ],
        );
    }

    /// A name that is also a builtin's is the builtin only when nothing in
    /// the program binds it: a user definition, or one provided by a required
    /// module, gets none of the builtin's signature.
    #[test]
    fn shadowed_builtins_are_not_builtins() {
        check(
            "(define (length x) 0)
             (define (k v) (length v) v)",
            &[("k()", "any")],
        );

        let dir = std::env::temp_dir().join(format!("steel-types-shadow-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(
            dir.join("lists.scm"),
            "(provide length car) (define (length x) 0) (define (car x) \"mine\")",
        )
        .unwrap();
        let src = "(require \"lists.scm\")
             (define (h y) (length y) y)
             (define (c y) (car y))
             (define (s y) (string-length y) y)";
        let main = dir.join("main.scm");
        std::fs::write(&main, src).unwrap();
        let types = describe_at(src, Some(main));
        let _ = std::fs::remove_dir_all(&dir);
        // The module's `length` takes anything: no narrowing to a list.
        assert_eq!(types.get("h()").map(String::as_str), Some("any"), "{types:#?}");
        // Nor the builtin `car`'s return type.
        assert_eq!(types.get("c()").map(String::as_str), Some("any"), "{types:#?}");
        // An actual builtin still narrows.
        assert_eq!(types.get("s()").map(String::as_str), Some("string"), "{types:#?}");
    }

    #[test]
    fn loops_and_local_functions() {
        check(
            "(define (sum-list xs)
               (let loop ([xs xs] [acc 0])
                 (if (null? xs) acc (loop (cdr xs) (+ acc (car xs))))))
             (define (f s)
               (define n (string-length s))
               (define (helper k) (* k 2))
               (helper n))
             (define (count-up n)
               (let loop ([i 0])
                 (if (< i n) (loop (+ i 1)) i)))",
            &[
                ("sum-list()", "number"),
                ("acc", "number"),
                // Parameters of a local helper come from its call sites.
                ("k", "int"),
                ("f()", "number"),
                ("i", "number"),
                ("count-up()", "number"),
            ],
        );
    }

    #[test]
    fn escaping_and_global_functions_get_no_parameter_types() {
        check(
            "(define (w lst) (map (lambda (z) z) lst))
             (define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))
             (define (use) (fact 5))
             (define (rs . xs) xs)",
            &[
                // Passed to `map`: called from somewhere we cannot see.
                ("z", "any"),
                // Global: callable from outside, whatever `use` passes.
                ("n", "any"),
                ("fact()", "number"),
                ("use()", "number"),
                ("xs", "list"),
            ],
        );
    }

    /// `TY_FILES=a.scm:b.scm`: robustness and coverage over real programs.
    #[test]
    fn stats_over_files() {
        let Ok(files) = std::env::var("TY_FILES") else { return };
        if let Ok(dir) = std::env::var("TY_SOUND_CWD") {
            std::env::set_current_dir(dir).unwrap();
        }
        for file in files.split(':').filter(|f| !f.is_empty()) {
            let src = std::fs::read_to_string(file).unwrap();
            if std::env::var("TY_NORMAL_PATH").is_ok() {
                let mut engine = Engine::new();
                let result = engine.compile_and_run_raw_program_with_path(
                    src.clone(),
                    std::path::PathBuf::from(file),
                );
                if let Err(e) = &result {
                    engine.raise_error(e.clone());
                }
                println!("{file}: normal path {:?}", result.map(|_| ()));
            }
            let mut engine = Engine::new();
            let mut exprs = match engine
                .emit_expanded_ast_without_optimizations(&src, Some(std::path::PathBuf::from(file)))
            {
                Ok(exprs) => exprs,
                Err(e) => {
                    println!("{file}: expand error {e}");
                    continue;
                }
            };
            let analysis = SemanticAnalysis::new(&mut exprs);
            let primitives = PrimitiveSignatures::from_modules(engine.builtin_modules());
            let start = std::time::Instant::now();
            let info = infer_types(&analysis.exprs, &analysis.analysis, &primitives);
            let elapsed = start.elapsed();
            let count = |map: &mut dyn Iterator<Item = TypeSet>| {
                let (mut top, mut never, mut known) = (0, 0, 0);
                for ty in map {
                    if ty.is_top() {
                        top += 1
                    } else if ty.is_bottom() {
                        never += 1
                    } else {
                        known += 1
                    }
                }
                (known, top, never)
            };
            let b = count(&mut info.bindings.values().copied());
            let r = count(&mut info.returns.values().copied());
            let c = count(&mut info.calls.values().copied());
            println!(
                "{file}: {:?} iters={} bindings(known,any,never)={b:?} returns={r:?} calls={c:?}",
                elapsed, info.iterations
            );
            if std::env::var("TY_NEVER").is_ok() {
                let mut names = FxHashMap::default();
                for expr in analysis.exprs.iter() {
                    collect_binding_names(expr, &mut names);
                }
                for (id, ty) in &info.bindings {
                    if ty.is_bottom() {
                        if let Some((name, span)) = names.get(id) {
                            let line = src.get(..span.start as usize).map(|s| s.matches('\n').count() + 1);
                            println!("    never: {name} line {line:?} (source {:?})", span.source_id());
                        }
                    }
                }
            }
        }
    }

    /// Where a check was inserted, for reporting.
    struct Site {
        what: String,
        ty: TypeSet,
        span: crate::parser::span::Span,
    }

    /// Instruments `expr` with `(__tycheck__ value type-bits site)` around every
    /// variable reference with an inferred type narrower than `any`, every
    /// non-tail call, and every parameter (checked on entry). Tail calls are
    /// left alone so loops keep constant stack.
    fn instrument(expr: &mut ExprKind, info: &TypeInfo, sites: &mut Vec<Site>, tail: bool) {
        fn wrap(expr: ExprKind, ty: TypeSet, site: usize) -> ExprKind {
            // A control: with the claim deliberately wrong, violations must show up.
            let ty = if std::env::var("TY_SOUND_CORRUPT").is_ok() {
                ty.without(TypeSet::INT)
            } else {
                ty
            };
            ExprKind::List(crate::parser::ast::List::new(thin_vec![
                ExprKind::ident("__tycheck__"),
                ExprKind::integer_literal(ty.bits() as isize, Default::default()),
                ExprKind::integer_literal(site as isize, Default::default()),
                expr,
            ]))
        }
        let mut site = |sites: &mut Vec<Site>, what: String, ty: TypeSet, span| {
            sites.push(Site { what, ty, span });
            sites.len() - 1
        };
        match expr {
            ExprKind::Atom(atom) if atom.ident().is_some() => {
                if let Some(&ty) = info.references.get(&atom.syn.syntax_object_id) {
                    if !ty.is_top() {
                        let id = site(sites, format!("ref {}", atom.ident().unwrap().resolve()), ty, atom.syn.span);
                        *expr = wrap(expr.clone(), ty, id);
                    }
                }
            }
            ExprKind::If(x) => {
                instrument(&mut x.test_expr, info, sites, false);
                instrument(&mut x.then_expr, info, sites, tail);
                instrument(&mut x.else_expr, info, sites, tail);
            }
            ExprKind::Let(x) => {
                for (_, init) in x.bindings.iter_mut() {
                    instrument(init, info, sites, false);
                }
                instrument(&mut x.body_expr, info, sites, tail);
            }
            ExprKind::Define(x) => instrument(&mut x.body, info, sites, false),
            ExprKind::LambdaFunction(x) => {
                instrument(&mut x.body, info, sites, true);
                let mut checks = Vec::new();
                for arg in &x.args {
                    let ExprKind::Atom(atom) = arg else { continue };
                    let Some(&ty) = info.bindings.get(&atom.syn.syntax_object_id) else { continue };
                    if !ty.is_top() {
                        let id = site(sites, format!("param {}", atom.ident().unwrap().resolve()), ty, atom.syn.span);
                        let mut reference = arg.clone();
                        if let ExprKind::Atom(a) = &mut reference {
                            a.syn.syntax_object_id = crate::parser::parser::SyntaxObjectId::fresh();
                        }
                        checks.push(wrap(reference, ty, id));
                    }
                }
                if !checks.is_empty() {
                    let body = std::mem::take(&mut x.body);
                    checks.push(body);
                    x.body = ExprKind::Begin(Box::new(crate::parser::ast::Begin::new(
                        checks,
                        x.location.clone(),
                    )));
                }
            }
            ExprKind::Begin(x) => {
                let n = x.exprs.len();
                for (i, e) in x.exprs.iter_mut().enumerate() {
                    instrument(e, info, sites, tail && i + 1 == n);
                }
            }
            ExprKind::Set(x) => instrument(&mut x.expr, info, sites, false),
            ExprKind::List(list) => {
                let id = list.syntax_object_id;
                let span = list.location;
                if let Some((operator, args)) = list.args.split_first_mut() {
                    // A variable in operator position stays as it is.
                    if !matches!(operator, ExprKind::Atom(_)) {
                        instrument(operator, info, sites, false);
                    }
                    for arg in args {
                        instrument(arg, info, sites, false);
                    }
                }
                if !tail {
                    if let Some(&ty) = info.calls.get(&id) {
                        if !ty.is_top() && !list.args.is_empty() {
                            let what = format!("call {}", list.args[0].to_string().chars().take(40).collect::<String>());
                            let site = site(sites, what, ty, span);
                            *expr = wrap(expr.clone(), ty, site);
                        }
                    }
                }
            }
            _ => {}
        }
    }

    /// The first bad value seen at each site.
    static VIOLATIONS: std::sync::Mutex<std::collections::BTreeMap<usize, String>> =
        std::sync::Mutex::new(std::collections::BTreeMap::new());
    static CHECKS_RUN: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

    fn tycheck(bits: isize, site: isize, value: SteelVal) -> SteelVal {
        CHECKS_RUN.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        if !TypeSet::from_bits(bits as u32).matches(&value) {
            VIOLATIONS.lock().unwrap().entry(site as usize).or_insert_with(|| {
                let mut shown = value.to_string();
                shown.truncate(60);
                shown
            });
        }
        value
    }

    /// `TY_SOUND_FILES=a.scm:b.scm`: runs each program instrumented with a
    /// check of every inferred type, and fails on any value outside its type.
    #[test]
    fn soundness_check() {
        use crate::steel_vm::register_fn::RegisterFn;
        let Ok(files) = std::env::var("TY_SOUND_FILES") else { return };
        // Benchmarks read their inputs relative to where they are run from.
        if let Ok(dir) = std::env::var("TY_SOUND_CWD") {
            std::env::set_current_dir(dir).unwrap();
        }
        let mut failed = false;
        for file in files.split(':').filter(|f| !f.is_empty()) {
            let src = std::fs::read_to_string(file).unwrap();
            let mut engine = Engine::new();
            let mut exprs = match engine
                .emit_expanded_ast_without_optimizations(&src, Some(std::path::PathBuf::from(file)))
            {
                Ok(exprs) => exprs,
                Err(e) => {
                    println!("{file}: expand error {e}");
                    failed = true;
                    continue;
                }
            };
            let analysis = SemanticAnalysis::new(&mut exprs);
            let primitives = PrimitiveSignatures::from_modules(engine.builtin_modules());
            let info = infer_types(&analysis.exprs, &analysis.analysis, &primitives);

            let mut sites = Vec::new();
            let mut instrumented: Vec<ExprKind> = analysis.exprs.to_vec();
            // A control: the expanded program run as it is, with no checks.
            if std::env::var("TY_SOUND_BASELINE").is_err() {
                for expr in instrumented.iter_mut() {
                    instrument(expr, &info, &mut sites, false);
                }
            }
            if let Ok(dump) = std::env::var("TY_SOUND_DUMP") {
                let program: String = instrumented.iter().map(|e| e.to_pretty(100) + "\n").collect();
                std::fs::write(dump, &program).unwrap();
            }

            // Run it in the same engine that expanded it, so the modules it
            // required are the ones it refers to.
            VIOLATIONS.lock().unwrap().clear();
            CHECKS_RUN.store(0, std::sync::atomic::Ordering::Relaxed);
            engine.register_fn("__tycheck__", tycheck);
            let result =
                engine.compile_and_run_expanded(instrumented, Some(std::path::PathBuf::from(file)));
            let violations = std::mem::take(&mut *VIOLATIONS.lock().unwrap());
            println!(
                "{file}: {} checks inserted, {} executed, run {}, {} violations",
                sites.len(),
                CHECKS_RUN.load(std::sync::atomic::Ordering::Relaxed),
                match &result {
                    Ok(_) => "ok".to_string(),
                    Err(e) => format!("FAILED: {}", e.to_string().chars().take(150).collect::<String>()),
                },
                violations.len()
            );
            for (site, value) in violations {
                let s = &sites[site];
                let line = src.get(..s.span.start as usize).map(|p| p.matches('\n').count() + 1);
                println!("    VIOLATION {} : inferred {} but got {} (line {:?})", s.what, s.ty, value, line);
                failed = true;
            }
            if result.is_err() {
                failed = true;
            }
        }
        assert!(!failed, "soundness check failed");
    }

    #[test]
    fn print_expansion() {
        let Ok(src) = std::env::var("TY_SRC") else { return };
        let path = std::env::var("TY_PATH").ok().map(std::path::PathBuf::from);
        let mut engine = Engine::new();
        let mut exprs = engine
            .emit_expanded_ast_without_optimizations(&src, path)
            .unwrap();
        let analysis = SemanticAnalysis::new(&mut exprs);
        for expr in analysis.exprs.iter() {
            println!("{}", expr.to_pretty(80));
            // With TY_BUILTINS, how analysis classifies each operator.
            if std::env::var("TY_BUILTINS").is_ok() {
                fn ops(e: &ExprKind, a: &crate::compiler::passes::analysis::Analysis) {
                    if let ExprKind::List(l) = e {
                        if let Some(ExprKind::Atom(atom)) = l.args.first() {
                            if let Some(info) = a.info.get(&atom.syn.syntax_object_id) {
                                println!(
                                    "    op {} builtin={} refers_to={:?} kind={:?}",
                                    atom, info.builtin, info.refers_to, info.kind
                                );
                                // What the reference points at.
                                if let Some(def) = info.refers_to.and_then(|id| a.info.get(&id)) {
                                    println!(
                                        "        -> definition {:?}: builtin={} required={} kind={:?}",
                                        info.refers_to.unwrap(),
                                        def.builtin,
                                        def.is_required_identifier,
                                        def.kind
                                    );
                                }
                            }
                        }
                        l.args.iter().for_each(|x| ops(x, a));
                    }
                    if let ExprKind::Define(d) = e {
                        if let ExprKind::Atom(name) = &d.name {
                            let n = name.ident().map(|i| i.resolve()).unwrap_or("");
                            if n == "car" || n == "length" {
                                println!(
                                    "    define {} is {:?} (body: {})",
                                    n,
                                    name.syn.syntax_object_id,
                                    d.body
                                );
                            }
                        }
                        ops(&d.body, a)
                    }
                    if let ExprKind::LambdaFunction(l) = e {
                        ops(&l.body, a)
                    }
                }
                ops(expr, &analysis.analysis);
            }
        }
    }

    #[test]
    fn print_types() {
        let src = std::env::var("TY_SRC").unwrap_or_default();
        for (k, v) in describe(&src) {
            println!("{k}: {v}");
        }
    }
}
