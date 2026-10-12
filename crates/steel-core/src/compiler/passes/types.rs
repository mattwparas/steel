use crate::compiler::passes::analysis::Analysis;
use crate::parser::ast::{Atom, ExprKind, LambdaFunction, List};
use crate::parser::interner::InternedString;
use crate::parser::parser::{SourceId, SyntaxObjectId};
use crate::parser::span::Span;
use crate::parser::tokens::TokenType;
use crate::rvals::SteelVal;
use crate::steel_vm::builtin::{
    get_function_metadata, BuiltInFunctionType, FunctionSignatureMetadata,
};
use crate::steel_vm::engine::ModuleContainer;
use crate::value_type::TypeSet;

mod ty;
use rustc_hash::{FxHashMap, FxHashSet};
use steel_parser::tokens::{IntLiteral, NumberLiteral, RealLiteral};
pub use ty::Ty;

/// The signatures of the native functions a program can refer to, by name,
/// and by the module that provides them.
#[derive(Default)]
pub struct PrimitiveSignatures {
    by_name: FxHashMap<String, FunctionSignatureMetadata>,
    by_module: FxHashMap<(String, String), FunctionSignatureMetadata>,
    /// The function each name in `by_name` is, to tell when two modules bind a
    /// name to different functions.
    named: FxHashMap<String, SteelVal>,
    /// Names the prelude binds. A free name in a program is the prelude's.
    prelude: FxHashSet<String>,
    /// Names two modules bind to different functions, outside the prelude.
    /// Nothing says which one a free name is.
    ambiguous: FxHashSet<String>,
}

impl PrimitiveSignatures {
    /// Every function value bound in `modules`, under every name it is bound to.
    pub fn from_modules(modules: &ModuleContainer) -> Self {
        let mut signatures = Self::default();
        let inner = modules.inner();
        let mut modules: Vec<_> = inner.values().collect();
        // The prelude first, so its bindings take the names.
        modules.sort_by_key(|module| module.name().as_ref() != PRELUDE);
        for module in modules {
            signatures.add_module(module);
        }
        signatures
    }

    /// Adds the functions of `module`, a module the program can require that is
    /// not among the engine's own (one described in `STEEL_LSP_HOME`).
    pub fn add_module(&mut self, module: &crate::steel_vm::builtin::BuiltInModule) {
        let module_name = module.name().to_string();
        let is_prelude = module_name == PRELUDE;
        for name in module.names() {
            // A context function is bound to a wrapper that passes the
            // context, and the host function it calls has the signature.
            let (value, metadata) = match module.context_function(&name) {
                Some(function) => {
                    let metadata = crate::steel_vm::host_signature::declared_in(module, &name)
                        .map(|signature| host_metadata(&name, signature));
                    (function, metadata)
                }
                None => {
                    let Some(value) = module.try_get_ref(&name) else {
                        continue;
                    };
                    let metadata = signature_of(&value);
                    (value, metadata)
                }
            };
            let Some(metadata) = metadata else {
                continue;
            };
            self.by_module
                .insert((module_name.clone(), name.clone()), metadata.clone());
            if is_prelude {
                self.prelude.insert(name.clone());
            } else if self.prelude.contains(&name) || self.ambiguous.contains(&name) {
                continue;
            } else if let Some(other) = self.named.get(&name) {
                if !same_function(other, &value) {
                    self.by_name.remove(&name);
                    self.named.remove(&name);
                    self.ambiguous.insert(name);
                }
                continue;
            }
            self.named.insert(name.clone(), value);
            self.by_name.insert(name, metadata);
        }
    }

    /// The function `name` of the builtin module `module`, as a
    /// `(require-builtin module)` binds it.
    pub fn get_in(&self, module: &str, name: &str) -> Option<&FunctionSignatureMetadata> {
        self.by_module.get(&(module.to_string(), name.to_string()))
    }

    /// `name` may carry the `#%prim.` prefix the expander uses for prelude
    /// functions.
    pub fn get(&self, name: &str) -> Option<&FunctionSignatureMetadata> {
        self.by_name
            .get(name.strip_prefix("#%prim.").unwrap_or(name))
    }
}

/// The module a program's free names are bound from.
const PRELUDE: &str = "steel/base";

/// Whether `a` and `b` are the same native function.
fn same_function(a: &SteelVal, b: &SteelVal) -> bool {
    match (a, b) {
        (SteelVal::FuncV(a), SteelVal::FuncV(b)) => *a as usize == *b as usize,
        (SteelVal::MutFunc(a), SteelVal::MutFunc(b)) => *a as usize == *b as usize,
        (SteelVal::BuiltIn(a), SteelVal::BuiltIn(b)) => *a as usize == *b as usize,
        (SteelVal::BoxedFunction(a), SteelVal::BoxedFunction(b)) => crate::gc::Gc::ptr_eq(a, b),
        _ => false,
    }
}

/// A host function's signature, as a native function's metadata.
fn host_metadata(
    name: &str,
    signature: crate::values::functions::HostSignature,
) -> FunctionSignatureMetadata {
    let fixed = signature.arg_types.fixed.len();
    let arity = match signature.arg_types.rest {
        Some(_) => crate::steel_vm::builtin::Arity::AtLeast(fixed),
        None => crate::steel_vm::builtin::Arity::Exact(fixed),
    };
    FunctionSignatureMetadata {
        name: name.to_string().into(),
        arity,
        is_const: false,
        doc: None,
        return_type: signature.return_type,
        arg_types: Some(signature.arg_types),
        raises: signature.raises,
    }
}

fn signature_of(value: &SteelVal) -> Option<FunctionSignatureMetadata> {
    get_function_metadata(match value {
        SteelVal::FuncV(f) => BuiltInFunctionType::Reference(*f),
        SteelVal::MutFunc(f) => BuiltInFunctionType::Mutable(*f),
        SteelVal::BuiltIn(f) => BuiltInFunctionType::Context(*f),
        // A function registered with `register_fn` carries its own.
        SteelVal::BoxedFunction(f) => {
            return Some(host_metadata(f.name().unwrap_or_default(), f.signature?));
        }
        _ => return None,
    })
}

/// A struct type's descriptor, the host type `make-struct-type` returns first.
fn struct_descriptor() -> Ty {
    Ty::from_value_type(crate::value_type::ValueType::Custom(
        crate::value_type::CustomTypeName::Of(
            core::any::type_name::<crate::values::structs::StructTypeDescriptor>,
        ),
    ))
}

/// What a type predicate tells each branch of an `if`. The then-branch can
/// intersect the tested value's type with `holds`, and the else-branch can
/// remove `excludes` (only the kinds for which the predicate is always true).
fn predicate(name: &str) -> Option<(Ty, Ty)> {
    use Ty as T;
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
        // A struct with a procedure property is callable.
        "procedure?" => (T::PROCEDURE.union(T::OTHER).union(T::STRUCTS), T::PROCEDURE),
        _ => return None,
    })
}

/// Whether a binding named `bound` labels the name `written` in the source.
/// Renaming only adds `#` / `_` prefixes and a numeric suffix (`acc` binds as
/// `##acc6`, a named let's box as `####loop46`). A macro's own binding spanned
/// at the macro use (`or`'s temporary, at `or`) does not match.
fn names_match(bound: &str, written: &str) -> bool {
    let bound = bound.strip_prefix("#%prim.").unwrap_or(bound);
    let bound = bound.trim_start_matches(['#', '_']);
    bound
        .strip_prefix(written.trim_start_matches(['#', '_']))
        .is_some_and(|suffix| suffix.chars().all(|c| c.is_ascii_digit() || c == '_'))
}

/// The result of [`infer_types`].
#[derive(Default, Clone, PartialEq, Debug)]
pub struct TypeInfo {
    /// Binding sites (`define` / `let` names, lambda parameters), by the
    /// syntax object id of the name.
    pub bindings: FxHashMap<SyntaxObjectId, Ty>,
    /// Identifier references, by their syntax object id: what the variable
    /// holds at that point (after narrowing).
    pub references: FxHashMap<SyntaxObjectId, Ty>,
    /// Calls, by the list's syntax object id: what the call returns.
    pub calls: FxHashMap<u32, Ty>,
    /// Lambdas, by their syntax object id: what a call of it returns.
    pub returns: FxHashMap<u32, Ty>,
    /// Parameters, by binding site, with the type the argument must have had for
    /// a call to return normally. This is what the body demands of it, joined
    /// over every normal exit. It says nothing about what callers pass (that is
    /// `bindings`), so it is only for display and cannot be trusted on function
    /// entry.
    pub required: FxHashMap<SyntaxObjectId, Ty>,
    /// Calls to a known lambda, by the list's syntax object id: that lambda.
    pub call_targets: FxHashMap<u32, u32>,
    /// How many passes over the program the fixpoint took.
    pub iterations: usize,
    /// Binding sites known to hold a function (directly, or through the box a
    /// named `let` / `letrec` expands into), with that lambda's id. Its return
    /// type is in `returns`.
    pub functions: FxHashMap<SyntaxObjectId, u32>,
    /// Calls to native functions that cannot return normally, in program order.
    pub failures: Vec<CallFailure>,
    /// Calls to a native function or a known lambda with a number of arguments
    /// it does not take, in program order.
    pub arity_failures: Vec<ArityFailure>,
    /// The program's struct types, indexed as in [`Ty::structure`].
    pub structs: Vec<StructType>,
    /// What each field of a struct type can hold, for the struct types whose
    /// every construction and field write the program shows.
    pub fields: FxHashMap<usize, Vec<Ty>>,
}

impl TypeInfo {
    /// `ty` as shown to a user, with struct types by name.
    pub fn show(&self, ty: Ty) -> String {
        let names: Vec<String> = self.structs.iter().map(|ty| ty.name.clone()).collect();
        ty.show(&names)
    }
}

/// A call with a number of arguments the function never takes.
#[derive(Clone, PartialEq, Debug)]
pub struct ArityFailure {
    /// The function's name as bound.
    pub function: String,
    /// The operator's span.
    pub operator: Span,
    /// What the function takes, as in "expects 2 arguments".
    pub expected: String,
    pub found: usize,
}

/// A call to a native function with an argument that can only hold kinds of
/// value the function does not accept, so the call raises whenever it runs.
#[derive(Clone, PartialEq, Debug)]
pub struct CallFailure {
    /// The function, as the source names it.
    pub function: String,
    /// The operator's span.
    pub operator: Span,
    /// Which argument, from 0, and its span.
    pub argument: usize,
    pub span: Span,
    /// What the function accepts there, as declared and as kinds, and what the
    /// argument can hold, as shown.
    pub declared: String,
    pub expected: Ty,
    pub found: String,
}

/// Infers types for `exprs`, which `analysis` was computed for.
pub fn infer_types(
    exprs: &[ExprKind],
    analysis: &Analysis,
    primitives: &PrimitiveSignatures,
) -> TypeInfo {
    infer_types_with_exports(exprs, analysis, primitives, None)
}

/// [`infer_types`] for a program that other modules can only see through
/// `exports` (see [`exported_names`]). Without that, any of its globals might
/// be used from elsewhere, and struct field types are never known.
pub fn infer_types_with_exports(
    exprs: &[ExprKind],
    analysis: &Analysis,
    primitives: &PrimitiveSignatures,
    exports: Option<&FxHashSet<String>>,
) -> TypeInfo {
    let facts = Facts::collect(exprs, analysis, primitives, exports);
    let mut previous = TypeInfo::default();
    let mut previous_params: FxHashMap<u32, Vec<Ty>> = FxHashMap::default();
    // Every step can only grow a set, and a set can only grow 64 times, so
    // this terminates. The bound guards against a bug making it cycle.
    for iteration in 1..=128 {
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
                functions: facts.functions(),
                structs: facts.structs.clone(),
                ..current
            };
        }
        previous = current;
        previous_params = params;
    }

    // Did not settle, so claim nothing.
    let mut result = previous;
    result.functions = facts.functions();
    result.structs = facts.structs.clone();
    result.failures.clear();
    result.arity_failures.clear();
    for ty in result
        .bindings
        .values_mut()
        .chain(result.references.values_mut())
        .chain(result.calls.values_mut())
        .chain(result.returns.values_mut())
        .chain(result.fields.values_mut().flatten())
    {
        *ty = Ty::TOP;
    }
    result
}

/// What a variable is known to hold, when it is never mutated.
#[derive(Clone, Copy, Debug, PartialEq)]
enum Known {
    /// A lambda, by id.
    Lambda(u32),
    /// A native function, by name.
    Primitive(InternedString),
    /// A function of a builtin module, by module and name: what
    /// `(require-builtin module)` binds, `(%module-get% %-builtin-module-M 'name)`.
    ModuleFunction(InternedString, InternedString),
    /// A box whose only `set-box!` stores this lambda. This is how a named `let`
    /// or `letrec` expands (`(#%box _)`, `(#%set-box! box (λ ...))`, and calls
    /// as `((#%unbox box) args ...)`).
    BoxedLambda(u32),
    /// A piece of the program's struct type `index` (see [`StructType`]).
    Struct(usize, StructPiece),
}

/// What `(make-struct-type 'name fields module)` returns, and what the `struct`
/// macro binds from it: a list of the descriptor, the constructor, the
/// predicate, the getter prototype `(getter struct index)`, and a list of one
/// getter per field.
#[derive(Clone, Copy, Debug, PartialEq)]
enum StructPiece {
    /// The list `make-struct-type` returns.
    Table,
    Descriptor,
    Constructor,
    Predicate,
    GetterPrototype,
    /// `(#%struct-type-key descriptor)`, how the accessors and constructor of a
    /// struct with inline fields name its type.
    Key,
    Getters,
    /// The getter of field `i`.
    Getter(usize),
}

/// A struct type defined at the top level of the program, by a `struct` form
/// or a direct `make-struct-type`.
#[derive(Clone, Debug, PartialEq)]
pub struct StructType {
    pub name: String,
    pub fields: usize,
}

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
    /// A `define` / `let` whose value is this variable, an alias of it.
    AliasInit,
    /// The argument that names the type in `#%mstruct` and the calls like
    /// it, or the descriptor of `#%vtable-update-entry!`.
    DescriptorUse,
    /// Anywhere else, where the value escapes.
    Value,
}

/// Program-wide facts, gathered once before the fixpoint.
#[derive(Default)]
struct Facts {
    /// Bindings that are the target of a `set!`.
    mutated: FxHashSet<SyntaxObjectId>,
    /// Every binding site in the program.
    defined: FxHashSet<SyntaxObjectId>,
    /// Top-level `define`s. These are callable from outside, so their
    /// parameters are never inferred from call sites.
    globals: FxHashSet<SyntaxObjectId>,
    known: FxHashMap<SyntaxObjectId, Known>,
    /// Lambdas by id: (fixed parameter count, has a rest parameter).
    arity: FxHashMap<u32, (usize, bool)>,
    /// Lambdas whose every use is a direct call we can see, so their
    /// parameter types are the union of the argument types at those calls.
    closed: FxHashSet<u32>,
    /// The program's struct types, indexed as in [`Ty::structure`].
    structs: Vec<StructType>,
    /// Lambdas that read or write a field of their first argument, which has
    /// to be of this struct type. A `#:mutable` struct's accessors and setters
    /// are defined this way.
    struct_accessors: FxHashMap<u32, Accessor>,
    /// Lambdas that make a struct of this type from their arguments, as a
    /// `#:mutable` struct's constructor does, and the call inside each that
    /// does it.
    struct_constructors: FxHashMap<u32, usize>,
    /// Lambdas written as the function argument of a standard library `map`,
    /// `filter`, `foldl` or `foldr`, which call them with list elements only.
    list_lambdas: FxHashSet<u32>,
    constructor_bodies: FxHashSet<u32>,
    /// Struct types whose every construction and field write is a direct call
    /// in the program, so their field types are known.
    closed_structs: FxHashSet<usize>,
}

/// A lambda reading or writing field `field` of struct type `ty`.
#[derive(Clone, Copy, Debug, PartialEq)]
struct Accessor {
    ty: usize,
    field: usize,
    set: bool,
}

impl Facts {
    fn functions(&self) -> FxHashMap<SyntaxObjectId, u32> {
        self.known
            .iter()
            .filter_map(|(id, known)| match known {
                Known::Lambda(lambda) | Known::BoxedLambda(lambda) => Some((*id, *lambda)),
                Known::Primitive(_) | Known::ModuleFunction(..) | Known::Struct(..) => None,
            })
            .collect()
    }
    fn collect(
        exprs: &[ExprKind],
        analysis: &Analysis,
        primitives: &PrimitiveSignatures,
        exports: Option<&FxHashSet<String>>,
    ) -> Self {
        let mut collector = Collector {
            analysis,
            primitives,
            exports,
            facts: Facts::default(),
            boxes: FxHashSet::default(),
            box_stores: FxHashMap::default(),
            aliases: FxHashMap::default(),
            uses: FxHashMap::default(),
            bound_lambdas: FxHashMap::default(),
            applied_lambdas: FxHashSet::default(),
            binding_names: FxHashMap::default(),
            unresolved_set_names: FxHashSet::default(),
            global_defines: FxHashMap::default(),
            accessor_candidates: Vec::new(),
            constructor_candidates: Vec::new(),
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
    /// The names the program makes visible to other modules, or None when that
    /// is not known and every global counts as visible.
    exports: Option<&'a FxHashSet<String>>,
    facts: Facts,
    /// Bindings initialized with `(#%box _)`.
    boxes: FxHashSet<SyntaxObjectId>,
    /// The lambda each `(#%set-box! b v)` stores (None when it is not a lambda).
    box_stores: FxHashMap<SyntaxObjectId, Vec<Option<u32>>>,
    /// Bindings initialized with another variable.
    aliases: FxHashMap<SyntaxObjectId, SyntaxObjectId>,
    uses: FxHashMap<SyntaxObjectId, Vec<Position>>,
    /// Lambdas bound directly to a variable (`let`, `define`) or stored in a box.
    bound_lambdas: FxHashMap<u32, SyntaxObjectId>,
    /// Lambdas that are the operator of a call.
    applied_lambdas: FxHashSet<u32>,
    /// Every binding site's name, and the names `set!` targets whose binding
    /// the analysis did not resolve. A `set!` of a global inside a function
    /// defined before the global's `define` has no `refers_to` yet.
    binding_names: FxHashMap<SyntaxObjectId, InternedString>,
    unresolved_set_names: FxHashSet<InternedString>,
    /// The binding sites of each global name. A module can define a global
    /// that its prelude already bound to a builtin.
    global_defines: FxHashMap<InternedString, Vec<SyntaxObjectId>>,
    /// Lambdas shaped like a mutable struct's accessor or setter, with the
    /// binding of the getter prototype they call, the field, and whether they
    /// write it.
    accessor_candidates: Vec<(u32, SyntaxObjectId, usize, bool)>,
    /// Lambdas shaped like a mutable struct's constructor, with the binding of
    /// the descriptor and the call that makes the struct.
    constructor_candidates: Vec<(u32, SyntaxObjectId, u32)>,
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
        if let Some(ident) = name.atom_identifier() {
            self.binding_names.insert(id, *ident);
            if self.facts.globals.contains(&id) {
                self.global_defines.entry(*ident).or_default().push(id);
            }
        }
        match init {
            Some(ExprKind::LambdaFunction(lambda)) => {
                self.facts
                    .known
                    .insert(id, Known::Lambda(lambda.syntax_object_id));
                self.bound_lambdas.insert(lambda.syntax_object_id, id);
                if let Some((getter, field, set)) = self.field_access(lambda) {
                    self.accessor_candidates
                        .push((lambda.syntax_object_id, getter, field, set));
                }
                if let Some((descriptor, call)) = self.construction(lambda) {
                    self.constructor_candidates
                        .push((lambda.syntax_object_id, descriptor, call));
                }
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
            Some(ExprKind::List(list)) => {
                if let Some(known) = self
                    .struct_piece(id, list)
                    .or_else(|| self.module_function(list))
                {
                    self.facts.known.insert(id, known);
                }
            }
            _ => {}
        }
    }

    /// What a binding initialized with `list` holds, when that is a struct type
    /// (a top-level `make-struct-type`) or a piece of one (`list-ref` of it).
    fn struct_piece(&mut self, id: SyntaxObjectId, list: &List) -> Option<Known> {
        let (operator, args) = list.args.split_first()?;
        // The operator has to be the native function, unbound in the program.
        if let ExprKind::Atom(atom) = operator {
            if self.reference(atom).is_some() {
                return None;
            }
        }
        match (operator_name(Some(operator))?, args) {
            ("make-struct-type", [ExprKind::Quote(name), fields, _]) => {
                if !self.facts.globals.contains(&id) {
                    return None;
                }
                let name = name.expr.atom_identifier()?.resolve().to_string();
                let fields = integer_literal(fields)?;
                self.facts.structs.push(StructType { name, fields });
                Some(Known::Struct(
                    self.facts.structs.len() - 1,
                    StructPiece::Table,
                ))
            }
            ("#%struct-type-key", [ExprKind::Atom(source)]) => {
                let source = self.reference(source)?;
                match self.facts.known.get(&source)? {
                    Known::Struct(ty, StructPiece::Descriptor) => {
                        Some(Known::Struct(*ty, StructPiece::Key))
                    }
                    _ => None,
                }
            }
            ("list-ref", [ExprKind::Atom(source), index]) => {
                let index = integer_literal(index)?;
                let source = self.reference(source)?;
                match self.facts.known.get(&source)? {
                    Known::Struct(ty, StructPiece::Table) => Some(Known::Struct(
                        *ty,
                        match index {
                            0 => StructPiece::Descriptor,
                            1 => StructPiece::Constructor,
                            2 => StructPiece::Predicate,
                            3 => StructPiece::GetterPrototype,
                            4 => StructPiece::Getters,
                            _ => return None,
                        },
                    )),
                    Known::Struct(ty, StructPiece::Getters)
                        if index < self.facts.structs[*ty].fields =>
                    {
                        Some(Known::Struct(*ty, StructPiece::Getter(index)))
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    }
    /// `(%module-get% %-builtin-module-M 'name)`, how `(require-builtin M)`
    /// binds each function of a builtin module.
    fn module_function(&self, list: &List) -> Option<Known> {
        let [ExprKind::Atom(operator), ExprKind::Atom(module), ExprKind::Quote(name)] =
            list.args.as_slice()
        else {
            return None;
        };
        // All three have to be the builtins, unbound in the program.
        if self.reference(operator).is_some() || self.reference(module).is_some() {
            return None;
        }
        if !matches!(
            operator.ident()?.resolve(),
            "%module-get%" | "##__module-get"
        ) {
            return None;
        }
        let module = module
            .ident()?
            .resolve()
            .strip_prefix("%-builtin-module-")?;
        let name = name.expr.atom_identifier()?;
        self.primitives.get_in(module, name.resolve())?;
        Some(Known::ModuleFunction(module.into(), *name))
    }

    /// For a lambda shaped like a mutable struct's accessor or setter, the
    /// binding that names the type, the field, and whether it is a setter.
    ///
    /// That is `(λ (this) (#%mstruct-ref key this i))` or
    /// `(λ (this value) (#%mstruct-set! key this i value))`, and `key` has to
    /// turn out to be a type's key.
    fn field_access(&self, lambda: &LambdaFunction) -> Option<(SyntaxObjectId, usize, bool)> {
        if lambda.rest {
            return None;
        }
        let this = binding_site(lambda.args.first()?)?;
        let ExprKind::List(body) = &lambda.body else {
            return None;
        };
        let (operator, args) = body.args.split_first()?;
        let (key, object, index, set) =
            match (operator_name(Some(operator))?, args, lambda.args.len()) {
                ("#%mstruct-ref", [ExprKind::Atom(key), ExprKind::Atom(object), index], 1) => {
                    (key, object, index, false)
                }
                (
                    "#%mstruct-set!",
                    [ExprKind::Atom(key), ExprKind::Atom(object), index, ExprKind::Atom(value)],
                    2,
                ) => {
                    // The value stored is the second parameter.
                    (self.reference(value)? == binding_site(&lambda.args[1])?).then_some(())?;
                    (key, object, index, true)
                }
                _ => return None,
            };
        let index = integer_literal(index)?;
        (self.reference(object)? == this).then_some(())?;
        Some((self.reference(key)?, index, set))
    }

    /// For a lambda shaped like a mutable struct's constructor,
    /// `(λ (a b) (#%mstruct key a b))`, the binding of `key` and the id of
    /// the call.
    fn construction(&self, lambda: &LambdaFunction) -> Option<(SyntaxObjectId, u32)> {
        if lambda.rest {
            return None;
        }
        let ExprKind::List(body) = &lambda.body else {
            return None;
        };
        let (operator, args) = body.args.split_first()?;
        if operator_name(Some(operator))? != "#%mstruct" {
            return None;
        }
        let (ExprKind::Atom(descriptor), fields) = args.split_first()? else {
            return None;
        };
        if fields.len() != lambda.args.len() {
            return None;
        }
        for (field, param) in fields.iter().zip(&lambda.args) {
            let ExprKind::Atom(field) = field else {
                return None;
            };
            (self.reference(field)? == binding_site(param)?).then_some(())?;
        }
        Some((self.reference(descriptor)?, body.syntax_object_id))
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
                self.expr(&define.body, init_position(&define.body));
            }
            ExprKind::Let(let_) => {
                for (name, init) in &let_.bindings {
                    self.bind(name, Some(init));
                    self.expr(init, init_position(init));
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
                    match self.reference(atom) {
                        Some(id) => {
                            self.facts.mutated.insert(id);
                        }
                        None => {
                            if let Some(name) = atom.ident() {
                                self.unresolved_set_names.insert(*name);
                            }
                        }
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
                    Some(
                        "#%vtable-update-entry!"
                        | "#%mstruct"
                        | "#%mstruct-ref"
                        | "#%mstruct-set!"
                        | "#%struct-type-key",
                    ) if matches!(args.first(), Some(ExprKind::Atom(_))) => {
                        self.expr(operator, Position::Operator);
                        self.expr(&args[0], Position::DescriptorUse);
                        for arg in &args[1..] {
                            self.expr(arg, Position::Value);
                        }
                        return;
                    }
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
                if let (Some(name), Some(ExprKind::LambdaFunction(lambda))) =
                    (list_function_name(self.analysis, operator), args.first())
                {
                    let takes = match name {
                        "map" => args.len() - 1,
                        "filter" => 1,
                        _ => 2,
                    };
                    if !lambda.rest && lambda.args.len() == takes {
                        self.facts.list_lambdas.insert(lambda.syntax_object_id);
                    }
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
    /// The struct types whose constructions and field writes are all direct
    /// calls in the program: nothing that makes one or writes a field is
    /// exported, reassigned, or used as a value.
    fn close_structs(&mut self) {
        // What can make a struct of each type or write one of its fields, and
        // the positions it may appear in.
        enum Role {
            Call,
            Descriptor,
            Prototype,
            Key,
        }
        let mut roles: FxHashMap<SyntaxObjectId, (usize, Role)> = FxHashMap::default();
        for (id, known) in &self.facts.known {
            let role = match known {
                Known::Struct(ty, StructPiece::Constructor) => (*ty, Role::Call),
                Known::Struct(ty, StructPiece::Descriptor) => (*ty, Role::Descriptor),
                Known::Struct(ty, StructPiece::GetterPrototype) => (*ty, Role::Prototype),
                Known::Struct(ty, StructPiece::Key) => (*ty, Role::Key),
                Known::Lambda(lambda) => {
                    if let Some(&ty) = self.facts.struct_constructors.get(lambda) {
                        (ty, Role::Call)
                    } else if let Some(accessor) = self.facts.struct_accessors.get(lambda) {
                        if !accessor.set {
                            continue;
                        }
                        (accessor.ty, Role::Call)
                    } else {
                        continue;
                    }
                }
                _ => continue,
            };
            roles.insert(*id, role);
        }

        let mut open = FxHashSet::default();
        // Every call of a getter prototype is inside one of its accessors.
        let mut prototype_calls: FxHashMap<SyntaxObjectId, usize> = FxHashMap::default();
        for &(_, getter, _, _) in &self.accessor_candidates {
            *prototype_calls.entry(getter).or_default() += 1;
        }
        // A key is also named by the constructor.
        let mut key_uses = prototype_calls.clone();
        for &(_, key, _) in &self.constructor_candidates {
            *key_uses.entry(key).or_default() += 1;
        }
        for (id, (ty, role)) in &roles {
            let uses = self.uses.get(id).map(Vec::as_slice).unwrap_or(&[]);
            let allowed = match role {
                Role::Call => uses
                    .iter()
                    .all(|p| matches!(p, Position::Operator | Position::AliasInit)),
                Role::Descriptor => uses
                    .iter()
                    .all(|p| matches!(p, Position::DescriptorUse | Position::AliasInit)),
                Role::Prototype => {
                    uses.iter().all(|p| matches!(p, Position::Operator))
                        && prototype_calls.get(id).copied().unwrap_or(0) == uses.len()
                }
                // Every use of a key is inside an accessor or the constructor.
                Role::Key => {
                    uses.iter().all(|p| matches!(p, Position::DescriptorUse))
                        && key_uses.get(id).copied().unwrap_or(0) == uses.len()
                }
            };
            let exported = match self.exports {
                None => self.facts.globals.contains(id),
                Some(exports) => self
                    .binding_names
                    .get(id)
                    .is_some_and(|name| exports.iter().any(|e| names_match(name.resolve(), e))),
            };
            if !allowed || exported {
                open.insert(*ty);
            }
        }
        // A reassigned alias may have held one of them.
        for (alias, target) in &self.aliases {
            if self.facts.mutated.contains(alias) {
                if let Some((ty, _)) = roles.get(target) {
                    open.insert(*ty);
                }
            }
        }
        self.facts.closed_structs = (0..self.facts.structs.len())
            .filter(|ty| !open.contains(ty))
            .collect();
    }

    fn finish(mut self) -> Facts {
        // For a `set!` we could not tie to its binding, every binding of that
        // name might be the one assigned.
        for (id, name) in &self.binding_names {
            if self.unresolved_set_names.contains(name) {
                self.facts.mutated.insert(*id);
            }
        }
        // A global defined twice holds whichever definition ran last.
        for ids in self.global_defines.values() {
            if ids.len() > 1 {
                self.facts.mutated.extend(ids.iter().copied());
            }
        }

        // Boxed lambdas, where the box's only store is a lambda.
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
        for &(lambda, getter, field, set) in &self.accessor_candidates {
            if let Some(&Known::Struct(ty, StructPiece::Key)) = self.facts.known.get(&getter) {
                if field < self.facts.structs[ty].fields {
                    self.facts
                        .struct_accessors
                        .insert(lambda, Accessor { ty, field, set });
                }
            }
        }
        for &(lambda, descriptor, call) in &self.constructor_candidates {
            if let Some(&Known::Struct(ty, StructPiece::Descriptor | StructPiece::Key)) =
                self.facts.known.get(&descriptor)
            {
                if self.facts.arity.get(&lambda) == Some(&(self.facts.structs[ty].fields, false)) {
                    self.facts.struct_constructors.insert(lambda, ty);
                    self.facts.constructor_bodies.insert(call);
                }
            }
        }
        self.close_structs();
        // A lambda is closed when every way to reach it is a direct call we can see.
        // A box holding a lambda is closed when it is only ever unboxed to be
        // called (plus its one store).
        let box_closed =
            |facts: &Facts, uses: &FxHashMap<SyntaxObjectId, Vec<Position>>, b: &SyntaxObjectId| {
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
                // Bound to a variable, and only called directly or stored into
                // a closed box that holds it.
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
        // So is one passed straight to a standard library list function.
        let list_lambdas: Vec<u32> = self.facts.list_lambdas.iter().copied().collect();
        self.facts.closed.extend(list_lambdas);
        // An immediately applied lambda is only ever called there.
        for lambda in &self.applied_lambdas {
            if !self.bound_lambdas.contains_key(lambda) {
                self.facts.closed.insert(*lambda);
            }
        }

        self.facts
    }
}

/// How a `define` / `let` uses its value: a lone variable is aliased.
fn init_position(init: &ExprKind) -> Position {
    match init {
        ExprKind::Atom(_) => Position::AliasInit,
        _ => Position::Value,
    }
}

/// `map`, `filter`, `foldl` or `foldr` when `operator` names the standard
/// library's: an identifier of that name bound nowhere in the program.
fn list_function_name(analysis: &Analysis, operator: &ExprKind) -> Option<&'static str> {
    let ExprKind::Atom(atom) = operator else {
        return None;
    };
    if reference(analysis, atom).is_some() {
        return None;
    }
    match atom.ident()?.resolve() {
        "map" => Some("map"),
        "filter" => Some("filter"),
        "foldl" => Some("foldl"),
        "foldr" => Some("foldr"),
        _ => None,
    }
}

/// What a call of the native function `name` returns, when what its container
/// arguments hold says more than its declared return type.
fn container_result(name: &str, args: &[Ty]) -> Option<Ty> {
    use TypeSet as K;
    let kinds_of = |tys: &[Ty]| tys.iter().fold(K::BOTTOM, |acc, t| acc.union(t.kinds));
    // A value of these kinds only (so the call only succeeds on them).
    let only = |ty: &Ty, kinds: TypeSet| !ty.is_bottom() && kinds.contains(ty.kinds);
    let element = |ty: &Ty, kinds: TypeSet| {
        (only(ty, kinds) && !ty.elements.is_top()).then(|| Ty::kinds(ty.elements))
    };
    let map = K::HASHMAP;
    let set = K::HASHSET;
    Some(match (name.strip_prefix("#%prim.").unwrap_or(name), args) {
        ("list", _) => Ty::list_of(kinds_of(args)),
        ("cons", [x, tail]) => {
            // Onto a list it is a list, onto anything else a pair.
            let onto_list = !tail.kinds.intersect(K::LIST).is_bottom();
            let onto_other = !tail.kinds.without(K::LIST).is_bottom();
            let mut kinds = K::BOTTOM;
            let mut elements = x.kinds;
            if onto_list {
                kinds = kinds.union(K::LIST);
                elements = elements.union(tail.elements);
            }
            if onto_other {
                kinds = kinds.union(K::PAIR);
            }
            Ty::holding(kinds, elements, K::BOTTOM)
        }
        ("append", _) if args.iter().all(|a| only(a, K::LIST)) => {
            Ty::list_of(args.iter().fold(K::BOTTOM, |acc, a| acc.union(a.elements)))
        }
        ("reverse" | "cdr" | "rest" | "list-tail", [list, ..]) if only(list, K::LIST) => {
            Ty::list_of(list.elements)
        }
        ("car" | "first" | "second" | "third" | "last" | "list-ref", [list, ..]) => {
            element(list, K::LIST.union(K::PAIR))?
        }
        ("range", _) => Ty::list_of(K::INT),
        ("string->list", _) => Ty::list_of(K::CHAR),
        ("immutable-vector", _) => Ty::holding(K::VECTOR, kinds_of(args), K::BOTTOM),
        ("vector-ref", [vector, _]) => element(vector, K::VECTOR)?,
        ("vector->list", [vector, ..]) if only(vector, K::VECTOR) => Ty::list_of(vector.elements),
        ("hash", _) if args.len() % 2 == 0 => {
            let keys = args
                .iter()
                .step_by(2)
                .fold(K::BOTTOM, |acc, t| acc.union(t.kinds));
            let values = args
                .iter()
                .skip(1)
                .step_by(2)
                .fold(K::BOTTOM, |acc, t| acc.union(t.kinds));
            Ty::holding(map, values, keys)
        }
        ("hash-insert", [m, key, value]) if only(m, map) => {
            Ty::holding(map, m.elements.union(value.kinds), m.keys.union(key.kinds))
        }
        ("hash-remove", [m, _]) if only(m, map) => *m,
        ("hash-union", [a, b]) if only(a, map) && only(b, map) => {
            Ty::holding(map, a.elements.union(b.elements), a.keys.union(b.keys))
        }
        ("hash-ref", [m, _]) => element(m, map)?,
        // #f when the key is missing.
        ("hash-try-get", [m, _]) => element(m, map)?.union(Ty::BOOL),
        ("hash-keys->list", [m]) if only(m, map) && !m.keys.is_top() => Ty::list_of(m.keys),
        ("hash-values->list", [m]) if only(m, map) => Ty::list_of(m.elements),
        ("hash-clear", [m]) if only(m, map) => Ty::holding(map, K::BOTTOM, K::BOTTOM),
        ("hashset", _) => Ty::holding(set, kinds_of(args), K::BOTTOM),
        ("hashset-insert", [s, value]) if only(s, set) => {
            Ty::holding(set, s.elements.union(value.kinds), K::BOTTOM)
        }
        ("list->hashset", [list]) if only(list, K::LIST) => {
            Ty::holding(set, list.elements, K::BOTTOM)
        }
        ("hashset->list", [s]) if only(s, set) => Ty::list_of(s.elements),
        // An intersection or difference holds some of what the first holds.
        ("hashset-union" | "hashset-intersection" | "hashset-difference", [a, b])
            if only(a, set) && only(b, set) =>
        {
            Ty::holding(set, a.elements.union(b.elements), K::BOTTOM)
        }
        ("hashset-clear", [s]) if only(s, set) => Ty::holding(set, K::BOTTOM, K::BOTTOM),
        _ => return None,
    })
}

/// The value of a non-negative integer literal.
fn integer_literal(expr: &ExprKind) -> Option<usize> {
    let ExprKind::Atom(atom) = expr else {
        return None;
    };
    match &atom.syn.ty {
        TokenType::Number(number) => match number.resolve() {
            NumberLiteral::Real(RealLiteral::Int(IntLiteral::Small(n))) => usize::try_from(n).ok(),
            _ => None,
        },
        _ => None,
    }
}

/// The names a module's `provide` forms make visible, from its source before
/// expansion. None when that cannot be told: a `provide` anywhere but at the
/// top level (as a macro might write one), or a form it does not know.
pub fn exported_names(source: &[ExprKind]) -> Option<FxHashSet<String>> {
    fn mentions_provide(expr: &ExprKind) -> bool {
        match expr {
            ExprKind::Atom(atom) => atom.ident().is_some_and(|name| name.resolve() == "provide"),
            ExprKind::List(list) => list.args.iter().any(mentions_provide),
            ExprKind::Quote(quote) => mentions_provide(&quote.expr),
            ExprKind::Begin(begin) => begin.exprs.iter().any(mentions_provide),
            ExprKind::Define(define) => mentions_provide(&define.body),
            ExprKind::LambdaFunction(lambda) => mentions_provide(&lambda.body),
            ExprKind::If(if_) => {
                mentions_provide(&if_.test_expr)
                    || mentions_provide(&if_.then_expr)
                    || mentions_provide(&if_.else_expr)
            }
            ExprKind::Let(let_) => {
                let_.bindings
                    .iter()
                    .any(|(_, value)| mentions_provide(value))
                    || mentions_provide(&let_.body_expr)
            }
            ExprKind::Set(set) => mentions_provide(&set.expr),
            ExprKind::Return(return_) => mentions_provide(&return_.expr),
            ExprKind::Vector(vector) => vector.args.iter().any(mentions_provide),
            // A macro could write a `provide` when its definition mentions one.
            ExprKind::Macro(_) | ExprKind::SyntaxRules(_) => expr
                .to_string()
                .split(|c: char| c.is_whitespace() || "()[]".contains(c))
                .any(|token| token == "provide"),
            ExprKind::Require(_) => false,
        }
    }
    fn top_level(expr: &ExprKind, names: &mut FxHashSet<String>) -> Option<()> {
        match expr {
            ExprKind::Begin(begin) => {
                for expr in &begin.exprs {
                    top_level(expr, names)?;
                }
                Some(())
            }
            ExprKind::List(list) if operator_name(list.args.first()) == Some("provide") => {
                for item in &list.args[1..] {
                    let name = match item {
                        ExprKind::Atom(atom) => atom.ident()?,
                        ExprKind::List(form) => {
                            match (operator_name(form.args.first())?, form.args.get(1)) {
                                ("for-syntax" | "contract/out", Some(name)) => {
                                    name.atom_identifier()?
                                }
                                _ => return None,
                            }
                        }
                        _ => return None,
                    };
                    names.insert(name.resolve().to_string());
                }
                Some(())
            }
            other => (!mentions_provide(other)).then_some(()),
        }
    }
    let mut names = FxHashSet::default();
    for expr in source {
        top_level(expr, &mut names)?;
    }
    Some(names)
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

/// Narrowed types of variables at a point in the program. Only holds immutable
/// bindings, since narrowing a variable that can be reassigned would not
/// survive the assignment.
type Env = FxHashMap<SyntaxObjectId, Ty>;

/// What a call's operator is.
enum Callee<'p> {
    Primitive(&'p FunctionSignatureMetadata),
    Lambda(u32),
    /// A piece of the program's struct type (see [`StructPiece`]).
    Struct(usize, StructPiece),
    Unknown,
}

struct Inference<'a> {
    analysis: &'a Analysis,
    primitives: &'a PrimitiveSignatures,
    facts: &'a Facts,
    /// The last iteration's results, for what calls to functions analyzed later
    /// (or recursively) return.
    previous: &'a TypeInfo,
    previous_params: &'a FxHashMap<u32, Vec<Ty>>,
    current: TypeInfo,
    /// Parameter types of closed lambdas, from this iteration's call sites.
    params: FxHashMap<u32, Vec<Ty>>,
}

impl<'a> Inference<'a> {
    /// What binding `id` holds, before any narrowing.
    fn declared(&self, id: SyntaxObjectId) -> Ty {
        if self.facts.mutated.contains(&id) || !self.facts.defined.contains(&id) {
            return Ty::TOP;
        }
        self.current
            .bindings
            .get(&id)
            .or_else(|| self.previous.bindings.get(&id))
            .copied()
            // Not analyzed yet in any iteration. Start optimistic and let the
            // fixpoint grow it.
            .unwrap_or(Ty::BOTTOM)
    }

    fn lookup(&self, id: SyntaxObjectId, env: &Env) -> Ty {
        match env.get(&id) {
            Some(ty) => *ty,
            None => self.declared(id),
        }
    }

    /// Records `ty` for binding `id`. A name can be bound more than once
    /// (a top-level redefinition), and then it can hold either.
    fn bind(&mut self, name: &ExprKind, ty: Ty) {
        if let Some(id) = binding_site(name) {
            let ty = if self.facts.mutated.contains(&id) {
                Ty::TOP
            } else {
                ty
            };
            let entry = self.current.bindings.entry(id).or_insert(Ty::BOTTOM);
            *entry = entry.union(ty);
        }
    }

    /// Narrows an immutable variable in `env`.
    fn narrow(&self, expr: &ExprKind, env: &mut Env, to: impl FnOnce(Ty) -> Ty) {
        let ExprKind::Atom(atom) = expr else { return };
        let Some(id) = reference(self.analysis, atom) else {
            return;
        };
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
                let Some(name) = atom.ident() else {
                    return Callee::Unknown;
                };
                match reference(self.analysis, atom) {
                    Some(id) => match self.facts.known.get(&id) {
                        Some(Known::Lambda(lambda)) => Callee::Lambda(*lambda),
                        Some(Known::Struct(ty, piece)) => Callee::Struct(*ty, *piece),
                        Some(Known::ModuleFunction(module, name)) => self
                            .primitives
                            .get_in(module.resolve(), name.resolve())
                            .map_or(Callee::Unknown, Callee::Primitive),
                        Some(Known::Primitive(name)) => self
                            .primitives
                            .get(name.resolve())
                            .map_or(Callee::Unknown, Callee::Primitive),
                        // Bound in the program to something we cannot see
                        // through, even if it shares a builtin's name (a
                        // module can provide its own `car`).
                        _ => Callee::Unknown,
                    },
                    None => self
                        .free_primitive(name.resolve())
                        .map_or(Callee::Unknown, Callee::Primitive),
                }
            }
            ExprKind::List(list)
                if operator_name(list.args.first()) == Some("#%unbox") && list.args.len() == 2 =>
            {
                let ExprKind::Atom(atom) = &list.args[1] else {
                    return Callee::Unknown;
                };
                match reference(self.analysis, atom).and_then(|id| self.facts.known.get(&id)) {
                    Some(Known::BoxedLambda(lambda)) => Callee::Lambda(*lambda),
                    _ => Callee::Unknown,
                }
            }
            _ => Callee::Unknown,
        }
    }

    fn infer(&mut self, expr: &ExprKind, env: &mut Env) -> Ty {
        match expr {
            ExprKind::Atom(atom) => self.atom(atom, env),
            ExprKind::Quote(quote) => datum_type(&quote.expr),
            // A vector literal's elements are data.
            ExprKind::Vector(_) => datum_type(expr),
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
                Ty::VOID
            }
            ExprKind::LambdaFunction(lambda) => {
                self.lambda(lambda, env);
                Ty::PROCEDURE
            }
            ExprKind::Begin(begin) => {
                let mut ty = Ty::VOID;
                for expr in &begin.exprs {
                    ty = self.infer(expr, env);
                }
                ty
            }
            ExprKind::Set(set) => {
                self.infer(&set.expr, env);
                // `set!` evaluates to the variable's previous value, and an
                // assigned variable can hold anything.
                Ty::TOP
            }
            ExprKind::Return(return_) => {
                self.infer(&return_.expr, env);
                Ty::TOP
            }
            ExprKind::List(list) => {
                if let Some(ExprKind::List(_) | ExprKind::Atom(_) | ExprKind::LambdaFunction(_)) =
                    list.args.first()
                {
                    if let Callee::Lambda(lambda) = self.callee(&list.args[0]) {
                        self.current
                            .call_targets
                            .insert(list.syntax_object_id, lambda);
                    }
                }
                let ty = self.call(list, env);
                self.current.calls.insert(list.syntax_object_id, ty);
                ty
            }
            ExprKind::Macro(_) | ExprKind::SyntaxRules(_) | ExprKind::Require(_) => Ty::VOID,
        }
    }

    fn atom(&mut self, atom: &Atom, env: &Env) -> Ty {
        let ty = match &atom.syn.ty {
            TokenType::Identifier(name) => match reference(self.analysis, atom) {
                Some(id) if self.facts.defined.contains(&id) => self.lookup(id, env),
                // Bound to something outside what we analyzed.
                Some(_) => Ty::TOP,
                None if self.free_primitive(name.resolve()).is_some() => Ty::PROCEDURE,
                None => Ty::TOP,
            },
            TokenType::Number(number) => number_type(&number.resolve()),
            TokenType::StringLiteral(_) => Ty::STRING,
            TokenType::CharacterLiteral(_) => Ty::CHAR,
            TokenType::BooleanLiteral(_) => Ty::BOOL,
            _ => Ty::TOP,
        };
        if atom.ident().is_some() {
            self.current
                .references
                .insert(atom.syn.syntax_object_id, ty);
        }
        ty
    }

    fn lambda(&mut self, lambda: &LambdaFunction, env: &Env) {
        let id = lambda.syntax_object_id;
        let closed = self.facts.closed.contains(&id);
        let fixed = lambda.args.len() - usize::from(lambda.rest);
        for (i, arg) in lambda.args.iter().enumerate() {
            let ty = if lambda.rest && i == fixed {
                Ty::LIST
            } else if closed {
                self.previous_params
                    .get(&id)
                    .and_then(|params| params.get(i))
                    .copied()
                    .unwrap_or(Ty::BOTTOM)
            } else {
                Ty::TOP
            };
            self.bind(arg, ty);
        }
        // Captured variables keep the narrowing they have here. Only immutable
        // ones are ever narrowed, and they hold the same value in the body.
        let mut body_env = env.clone();
        let ty = self.infer(&lambda.body, &mut body_env);
        self.current.returns.insert(id, ty);

        // What the body demanded of each parameter by the time it returned. The
        // environment at the end is the join over every normal exit. An early
        // `return` would be an exit that join does not see.
        if !contains_return(&lambda.body) {
            for arg in &lambda.args {
                let Some(param) = binding_site(arg) else {
                    continue;
                };
                if self.facts.mutated.contains(&param) {
                    continue;
                }
                let required = self.lookup(param, &body_env);
                self.current.required.insert(param, required);
            }
        }
    }

    fn call(&mut self, list: &List, env: &mut Env) -> Ty {
        let Some((operator, args)) = list.args.split_first() else {
            // `()`
            return Ty::LIST;
        };

        let callee = self.callee(operator);
        self.infer(operator, env);
        let arg_types: Vec<Ty> = args.iter().map(|arg| self.infer(arg, env)).collect();

        if let Some(expected) = self.arity_mismatch(&callee, args.len()) {
            // A named let's loop is called through its box, `((#%unbox loop) ...)`.
            let named = match operator {
                ExprKind::List(list) if operator_name(list.args.first()) == Some("#%unbox") => {
                    list.args.get(1)
                }
                _ => Some(operator),
            };
            if let Some(named) = named {
                self.current.arity_failures.push(ArityFailure {
                    function: named
                        .atom_identifier()
                        .map(|name| name.resolve().to_string())
                        .unwrap_or_default(),
                    operator: named.span(),
                    expected,
                    found: args.len(),
                });
            }
        }

        match callee {
            Callee::Primitive(signature) => {
                // A call that returned proves its arguments had the declared types.
                if let Some(declared) = &signature.arg_types {
                    let count_ok = args.len() >= declared.fixed.len()
                        && (declared.rest.is_some() || args.len() == declared.fixed.len());
                    if count_ok {
                        for (i, arg) in args.iter().enumerate() {
                            let Some(declared_ty) = declared.get(i) else {
                                continue;
                            };
                            let ty = Ty::from_arg_type(declared_ty);
                            if ty.is_top() {
                                continue;
                            }
                            self.require(
                                operator,
                                i,
                                arg,
                                arg_types[i],
                                ty,
                                declared_ty.to_string(),
                                env,
                            );
                        }
                    }
                }
                if let Some(ty) = self.mutable_construction(list, operator, args) {
                    self.construct(ty, &arg_types[1..]);
                }
                self.struct_result(operator, args)
                    .or_else(|| container_result(&signature.name, &arg_types))
                    .unwrap_or_else(|| Ty::from_value_type(signature.return_type))
            }
            Callee::Struct(ty, piece) => {
                let this = Ty::structure(ty);
                let name = &self.facts.structs[ty].name;
                match piece {
                    StructPiece::Constructor => {
                        self.construct(ty, &arg_types);
                        this
                    }
                    StructPiece::Predicate => Ty::BOOL,
                    StructPiece::GetterPrototype | StructPiece::Getter(_) => {
                        let takes = if piece == StructPiece::GetterPrototype {
                            2
                        } else {
                            1
                        };
                        if let (Some(arg), true) = (args.first(), args.len() == takes) {
                            let declared = name.clone();
                            self.require(operator, 0, arg, arg_types[0], this, declared, env);
                        }
                        match piece {
                            StructPiece::Getter(field) => self.field(ty, field),
                            _ => Ty::TOP,
                        }
                    }
                    // Calling these raises, but nothing else is claimed.
                    StructPiece::Table
                    | StructPiece::Descriptor
                    | StructPiece::Getters
                    | StructPiece::Key => Ty::TOP,
                }
            }
            Callee::Lambda(lambda) => {
                let takes = self.facts.arity.get(&lambda).map(|&(fixed, _)| fixed);
                let fits = takes == Some(args.len());
                let mut field_read = None;
                if let Some(&accessor) = self.facts.struct_accessors.get(&lambda) {
                    if let (Some(arg), true) = (args.first(), fits) {
                        let ty = accessor.ty;
                        let declared = self.facts.structs[ty].name.clone();
                        self.require(
                            operator,
                            0,
                            arg,
                            arg_types[0],
                            Ty::structure(ty),
                            declared,
                            env,
                        );
                        if accessor.set {
                            self.store_field(ty, accessor.field, arg_types[1]);
                        } else {
                            field_read = Some(self.field(ty, accessor.field));
                        }
                    }
                }
                if let (Some(&ty), true) = (self.facts.struct_constructors.get(&lambda), fits) {
                    self.construct(ty, &arg_types);
                }
                if self.facts.closed.contains(&lambda) {
                    if let Some(&(fixed, rest)) = self.facts.arity.get(&lambda) {
                        if arg_types.len() == fixed || (rest && arg_types.len() >= fixed) {
                            let params = self
                                .params
                                .entry(lambda)
                                .or_insert_with(|| vec![Ty::BOTTOM; fixed]);
                            for (param, ty) in params.iter_mut().zip(&arg_types) {
                                *param = param.union(*ty);
                            }
                        }
                    }
                }
                field_read.unwrap_or_else(|| {
                    self.current
                        .returns
                        .get(&lambda)
                        .or_else(|| self.previous.returns.get(&lambda))
                        .copied()
                        .unwrap_or(Ty::BOTTOM)
                })
            }
            Callee::Unknown => self
                .list_function(operator, args, &arg_types)
                .unwrap_or(Ty::TOP),
        }
    }

    /// What `callee` takes, when that excludes `count` arguments.
    fn arity_mismatch(&self, callee: &Callee, count: usize) -> Option<String> {
        use crate::steel_vm::builtin::Arity;
        let plural = |n: usize| if n == 1 { "argument" } else { "arguments" };
        match callee {
            Callee::Primitive(signature) => match signature.arity {
                Arity::Exact(n) if count != n => Some(format!("{n} {}", plural(n))),
                Arity::AtLeast(n) if count < n => Some(format!("at least {n} {}", plural(n))),
                Arity::AtMost(n) if count > n => Some(format!("at most {n} {}", plural(n))),
                Arity::Range(a, b) if count < a || count > b => {
                    Some(format!("{a} to {b} arguments"))
                }
                _ => None,
            },
            Callee::Lambda(lambda) => match self.facts.arity.get(lambda) {
                Some(&(fixed, false)) if count != fixed => {
                    Some(format!("{fixed} {}", plural(fixed)))
                }
                Some(&(fixed, true)) if count < fixed => {
                    Some(format!("at least {fixed} {}", plural(fixed)))
                }
                _ => None,
            },
            Callee::Struct(ty, piece) => {
                let expected = match piece {
                    StructPiece::Constructor => self.facts.structs[*ty].fields,
                    StructPiece::Predicate | StructPiece::Getter(_) => 1,
                    StructPiece::GetterPrototype => 2,
                    StructPiece::Table
                    | StructPiece::Descriptor
                    | StructPiece::Getters
                    | StructPiece::Key => return None,
                };
                (count != expected).then(|| format!("{expected} {}", plural(expected)))
            }
            Callee::Unknown => None,
        }
    }

    /// A call that returns normally proves argument `index` had type
    /// `required`: narrow it, and record the call as always failing when the
    /// argument can never be of it. `declared` is `required` as the function's
    /// declaration spells it.
    #[allow(clippy::too_many_arguments)]
    fn require(
        &mut self,
        operator: &ExprKind,
        index: usize,
        arg: &ExprKind,
        found: Ty,
        required: Ty,
        declared: String,
        env: &mut Env,
    ) {
        // An argument that cannot produce a value never reaches the call.
        if !found.is_bottom() && found.intersect(required).is_bottom() {
            let structs = self.struct_names();
            self.current.failures.push(CallFailure {
                function: operator_name(Some(operator))
                    .unwrap_or_default()
                    .to_string(),
                operator: operator.span(),
                argument: index,
                span: arg.span(),
                declared,
                expected: required,
                found: found.show(&structs),
            });
        }
        self.narrow(arg, env, |current| current.intersect(required));
    }
    /// What field `field` of struct type `ty` can hold: what every
    /// construction and write of it stored, when those are all known.
    fn field(&self, ty: usize, field: usize) -> Ty {
        if !self.facts.closed_structs.contains(&ty) {
            return Ty::TOP;
        }
        self.previous
            .fields
            .get(&ty)
            .and_then(|fields| fields.get(field))
            .copied()
            // No construction seen yet, in any iteration.
            .unwrap_or(Ty::BOTTOM)
    }

    fn store_field(&mut self, ty: usize, field: usize, value: Ty) {
        if !self.facts.closed_structs.contains(&ty) {
            return;
        }
        let count = self.facts.structs[ty].fields;
        let fields = self
            .current
            .fields
            .entry(ty)
            .or_insert_with(|| vec![Ty::BOTTOM; count]);
        if let Some(slot) = fields.get_mut(field) {
            *slot = slot.union(value);
        }
    }

    /// A struct of type `ty` made from `values`, one per field.
    fn construct(&mut self, ty: usize, values: &[Ty]) {
        if values.len() != self.facts.structs[ty].fields {
            return;
        }
        for (field, value) in values.iter().enumerate() {
            self.store_field(ty, field, *value);
        }
    }

    /// The struct type a `(#%mstruct key fields ...)` call makes, unless it is the one inside a recognized constructor, whose
    /// fields are its parameters (the calls of the constructor are counted).
    fn mutable_construction(
        &self,
        list: &List,
        operator: &ExprKind,
        args: &[ExprKind],
    ) -> Option<usize> {
        if operator_name(Some(operator))? != "#%mstruct"
            || self
                .facts
                .constructor_bodies
                .contains(&list.syntax_object_id)
        {
            return None;
        }
        let ExprKind::Atom(descriptor) = args.first()? else {
            return None;
        };
        match self
            .facts
            .known
            .get(&reference(self.analysis, descriptor)?)?
        {
            Known::Struct(ty, StructPiece::Descriptor | StructPiece::Key) => Some(*ty),
            _ => None,
        }
    }

    fn struct_names(&self) -> Vec<String> {
        self.facts
            .structs
            .iter()
            .map(|ty| ty.name.clone())
            .collect()
    }

    /// What a native function call builds when it takes apart a struct type or
    /// makes a struct: `(list-ref table i)` of a `make-struct-type` result, and
    /// `(#%mstruct key fields ...)`.
    fn struct_result(&self, operator: &ExprKind, args: &[ExprKind]) -> Option<Ty> {
        let known = |expr: &ExprKind| match expr {
            ExprKind::Atom(atom) => {
                reference(self.analysis, atom).and_then(|id| self.facts.known.get(&id).copied())
            }
            _ => None,
        };
        match (operator_name(Some(operator))?, args) {
            ("#%mstruct", [descriptor, ..]) => match known(descriptor)? {
                Known::Struct(ty, StructPiece::Descriptor | StructPiece::Key) => {
                    Some(Ty::structure(ty))
                }
                _ => None,
            },
            ("list-ref", [table, _]) => match known(table)? {
                Known::Struct(_, StructPiece::Table) | Known::Struct(_, StructPiece::Getters) => {
                    // The descriptor is the one piece that is not a procedure
                    // or a list, and it is never called.
                    Some(Ty::PROCEDURE.union(Ty::LIST).union(struct_descriptor()))
                }
                _ => None,
            },
            _ => None,
        }
    }

    /// Narrows `then_env` / `else_env` for the branches of an `if` testing `test`.
    fn test(&self, test: &ExprKind, then_env: &mut Env, else_env: &mut Env) {
        match test {
            // Only #f is false.
            ExprKind::Atom(_) => self.narrow(test, else_env, |ty| ty.intersect(Ty::BOOL)),
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
                // A struct predicate is true only for its own type.
                if let Callee::Struct(ty, StructPiece::Predicate) = self.callee(&list.args[0]) {
                    let this = Ty::structure(ty);
                    self.narrow(&list.args[1], then_env, |ty| ty.intersect(this));
                    self.narrow(&list.args[1], else_env, |ty| ty.without(this));
                }
            }
            // `(and a b)` expands to `(if a b #f)`. When it is true, both were.
            ExprKind::If(if_) if is_false_literal(&if_.else_expr) => {
                let mut ignored = then_env.clone();
                self.test(&if_.test_expr, then_env, &mut ignored);
                let mut ignored = then_env.clone();
                self.test(&if_.then_expr, then_env, &mut ignored);
            }
            _ => {}
        }
    }

    /// A call of the standard library's `map`, `filter`, `foldl` or `foldr`: a
    /// free identifier of that name (see [`list_function_name`]). A lambda
    /// passed straight to one is called with the list's elements (and the
    /// accumulator), which become its parameters' types.
    fn list_function(
        &mut self,
        operator: &ExprKind,
        args: &[ExprKind],
        arg_types: &[Ty],
    ) -> Option<Ty> {
        let name = list_function_name(self.analysis, operator)?;
        let function = args.first()?;
        let rest_types = &arg_types[1..];
        let returns = match self.callee(function) {
            Callee::Lambda(lambda) => {
                let parameters: Option<Vec<Ty>> = match name {
                    "map" => Some(
                        rest_types
                            .iter()
                            .map(|list| Ty::kinds(list.elements))
                            .collect(),
                    ),
                    "filter" => rest_types
                        .first()
                        .map(|list| vec![Ty::kinds(list.elements)]),
                    "foldl" | "foldr" => match rest_types {
                        [init, list] => {
                            let returned = self
                                .current
                                .returns
                                .get(&lambda)
                                .or_else(|| self.previous.returns.get(&lambda))
                                .copied()
                                .unwrap_or(Ty::BOTTOM);
                            Some(vec![Ty::kinds(list.elements), returned.union(*init)])
                        }
                        _ => None,
                    },
                    _ => None,
                };
                if let (Some(parameters), true) =
                    (parameters, self.facts.list_lambdas.contains(&lambda))
                {
                    let slots = self
                        .params
                        .entry(lambda)
                        .or_insert_with(|| vec![Ty::BOTTOM; parameters.len()]);
                    for (slot, ty) in slots.iter_mut().zip(&parameters) {
                        *slot = slot.union(*ty);
                    }
                }
                self.current
                    .returns
                    .get(&lambda)
                    .or_else(|| self.previous.returns.get(&lambda))
                    .copied()
                    .unwrap_or(Ty::BOTTOM)
            }
            Callee::Primitive(signature) => Ty::from_value_type(signature.return_type),
            Callee::Struct(ty, StructPiece::Constructor) => Ty::structure(ty),
            _ => Ty::TOP,
        };
        Some(match (name, rest_types) {
            ("map", _) => Ty::list_of(returns.kinds),
            ("filter", [list]) if TypeSet::LIST.contains(list.kinds) && !list.is_bottom() => {
                Ty::list_of(list.elements)
            }
            ("filter", _) => Ty::LIST,
            ("foldl" | "foldr", [init, _]) => returns.union(*init),
            _ => return None,
        })
    }

    /// The native function a free identifier (bound nowhere in the program)
    /// denotes. `#%prim.<name>` is always the builtin. A bare name is the
    /// builtin only because nothing in the program binds it. The analysis's
    /// own `builtin` flag goes by name alone, and is set for a module's `car`
    /// too.
    fn free_primitive(&self, name: &str) -> Option<&'a FunctionSignatureMetadata> {
        self.primitives.get(name)
    }

    /// Whether `operator` refers to a native function. A user binding that
    /// shares its name does not count.
    fn is_primitive(&self, operator: &ExprKind) -> bool {
        matches!(self.callee(operator), Callee::Primitive(_))
    }

    /// The environment after an `if`, where a variable has either branch's type.
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

/// A type to show at a binding in the source. `span` is the bound name as
/// written, and `label` goes at byte offset `at`, which is right after the name,
/// or for a function defined as `(define (f args ...) ...)` after its header.
#[derive(Debug, Clone, PartialEq)]
pub struct TypeHint {
    pub span: Span,
    pub at: u32,
    pub label: String,
    /// An explanation, where the label alone is not self explanatory.
    pub tooltip: Option<String>,
}

/// Where to show a function's return type. This is after `(f args ...)` when
/// the name heads such a header, otherwise right after the name.
fn return_hint_offset(text: &str, name: Span) -> u32 {
    let bytes = text.as_bytes();
    let start = name.start as usize;
    if start == 0 || bytes.get(start - 1) != Some(&b'(') {
        return name.end;
    }
    let mut depth = 0usize;
    for (i, &b) in bytes.iter().enumerate().skip(start - 1) {
        match b {
            b'(' | b'[' => depth += 1,
            b')' | b']' => {
                depth -= 1;
                if depth == 0 {
                    return (i + 1) as u32;
                }
            }
            // A header only holds names, so stop at strings or comments.
            b'"' | b';' => break,
            _ => {}
        }
    }
    name.end
}

/// Inlay hints for the bindings written in `source`. `: type` goes after a
/// `define` / `let` name or a parameter, `-> type` after the name of a
/// function. There is nothing for `any`, nor for `never` (a parameter of a
/// function nothing calls). Expansion can bind several things under one written
/// name (a named `let` binds a placeholder, a box and the lambda, all spelled
/// `loop`), so there is one hint per name, and a function among them is
/// preferred.
pub fn inlay_hints(
    exprs: &[ExprKind],
    info: &TypeInfo,
    source: SourceId,
    text: &str,
) -> Vec<TypeHint> {
    fn sites(expr: &ExprKind, out: &mut Vec<SyntaxObjectIdAndSpan>) {
        let site = |name: &ExprKind, out: &mut Vec<SyntaxObjectIdAndSpan>| {
            if let ExprKind::Atom(atom) = name {
                if let Some(ident) = atom.ident() {
                    out.push((atom.syn.syntax_object_id, atom.syn.span, *ident));
                }
            }
        };
        match expr {
            ExprKind::Define(d) => {
                site(&d.name, out);
                sites(&d.body, out);
            }
            ExprKind::Let(l) => {
                for (name, value) in &l.bindings {
                    site(name, out);
                    sites(value, out);
                }
                sites(&l.body_expr, out);
            }
            ExprKind::LambdaFunction(l) => {
                for arg in &l.args {
                    site(arg, out);
                }
                sites(&l.body, out);
            }
            ExprKind::If(i) => {
                sites(&i.test_expr, out);
                sites(&i.then_expr, out);
                sites(&i.else_expr, out);
            }
            ExprKind::Begin(b) => b.exprs.iter().for_each(|e| sites(e, out)),
            ExprKind::List(l) => l.args.iter().for_each(|e| sites(e, out)),
            ExprKind::Set(t) => sites(&t.expr, out),
            ExprKind::Return(r) => sites(&r.expr, out),
            _ => {}
        }
    }
    type SyntaxObjectIdAndSpan = (SyntaxObjectId, Span, InternedString);

    let mut all = Vec::new();
    for expr in exprs {
        sites(expr, &mut all);
    }

    // Group what expansion bound under each written name.
    let mut by_span: std::collections::BTreeMap<(u32, u32), Vec<SyntaxObjectId>> =
        std::collections::BTreeMap::new();
    let mut spans = FxHashMap::default();
    for (id, span, name) in all {
        if span.source_id() != Some(source) || span.end <= span.start {
            continue;
        }
        let Some(written) = text.get(span.start as usize..span.end as usize) else {
            continue;
        };
        if !names_match(name.resolve(), written) {
            continue;
        }
        by_span.entry((span.start, span.end)).or_default().push(id);
        spans.insert((span.start, span.end), span);
    }

    let worth_showing = |ty: Ty| !ty.is_top() && !ty.is_bottom();
    let mut hints = Vec::new();
    for (key, ids) in by_span {
        let span = spans[&key];
        let function = ids.iter().find_map(|id| info.functions.get(id));
        let mut at = span.end;
        let mut tooltip = None;
        let label = match function {
            Some(lambda) => match info.returns.get(lambda) {
                Some(&ty) if worth_showing(ty) => {
                    at = return_hint_offset(text, span);
                    format!("-> {}", info.show(ty))
                }
                _ => continue,
            },
            None => {
                let mut types = ids.iter().filter_map(|id| info.bindings.get(id).copied());
                let Some(first) = types.next() else { continue };
                // Show nothing when several things under one name disagree.
                if types.any(|ty| ty != first) {
                    continue;
                }
                // A parameter the body demands more of than its callers are
                // known to pass, shown as `s: string?`.
                let required = ids
                    .iter()
                    .find_map(|id| info.required.get(id).copied())
                    .filter(|&req| req != first && first.contains(req) && worth_showing(req));
                if let Some(required) = required {
                    let shown = info.show(required);
                    let shown = if shown.contains(' ') {
                        format!("({shown})")
                    } else {
                        shown
                    };
                    tooltip = Some(format!(
                        "Expected {}. Calls that return normally pass one, but it is not checked on entry.",
                        info.show(required)
                    ));
                    format!(": {shown}?")
                } else if worth_showing(first) {
                    format!(": {}", info.show(first))
                } else {
                    continue;
                }
            }
        };
        hints.push(TypeHint {
            span,
            at,
            label,
            tooltip,
        });
    }
    hints
}

/// Whether `expr` has an early `return` outside any nested lambda (whose
/// returns are its own).
fn contains_return(expr: &ExprKind) -> bool {
    match expr {
        ExprKind::Return(_) => true,
        ExprKind::LambdaFunction(_) => false,
        ExprKind::If(i) => {
            contains_return(&i.test_expr)
                || contains_return(&i.then_expr)
                || contains_return(&i.else_expr)
        }
        ExprKind::Let(l) => {
            l.bindings.iter().any(|(_, v)| contains_return(v)) || contains_return(&l.body_expr)
        }
        ExprKind::Define(d) => contains_return(&d.body),
        ExprKind::Begin(b) => b.exprs.iter().any(contains_return),
        ExprKind::List(l) => l.args.iter().any(contains_return),
        ExprKind::Set(t) => contains_return(&t.expr),
        _ => false,
    }
}

impl CallFailure {
    pub fn message(&self) -> String {
        format!(
            "`{}` always raises here: argument {} is always {}, but it must be {}",
            self.function,
            self.argument + 1,
            self.found,
            self.declared
        )
    }
}

impl ArityFailure {
    pub fn message(&self, written: &str) -> String {
        format!(
            "ArityMismatch: {written} expects {}, found {}",
            self.expected, self.found
        )
    }
}

/// The arity failures in `source` whose call is written out in `text`, with the
/// operator as written. Calls a macro wrote are left out.
pub fn written_arity_failures<'a>(
    info: &'a TypeInfo,
    source: SourceId,
    text: &'a str,
) -> impl Iterator<Item = (&'a ArityFailure, &'a str)> + 'a {
    let mut seen = FxHashSet::default();
    info.arity_failures.iter().filter_map(move |failure| {
        let span = failure.operator;
        if span.source_id() != Some(source) {
            return None;
        }
        let written = text.get(span.start as usize..span.end as usize)?;
        (names_match(&failure.function, written) && seen.insert(span.start))
            .then_some((failure, written))
    })
}

/// The failures in `source` whose call is written out in `text`, the source's
/// text. A macro can expand into a call that its own tests keep from running,
/// so calls a macro wrote are left out.
pub fn written_call_failures<'a>(
    info: &'a TypeInfo,
    source: SourceId,
    text: &'a str,
) -> impl Iterator<Item = &'a CallFailure> + 'a {
    let mut seen = FxHashSet::default();
    info.failures.iter().filter(move |failure| {
        let written = |span: Span| {
            span.source_id() == Some(source)
                && text.get(span.start as usize..span.end as usize).is_some()
        };
        written(failure.operator)
            && written(failure.span)
            && text
                .get(failure.operator.start as usize..failure.operator.end as usize)
                .is_some_and(|written| names_match(&failure.function, written))
            && seen.insert((failure.span.start, failure.argument))
    })
}

/// The type to show when hovering at byte `offset` of `source` (whose text is
/// `text`), as one line of Scheme-ish notation:
/// - a variable reference: its type at that point, after narrowing
///   (`x: string` inside `(if (string? x) ...)`);
/// - a binding (`define` / `let` name, parameter): its type, and what the body
///   requires of a parameter when that is narrower (`s: any, expected string`);
/// - a function's name, where it is defined or used: its signature,
///   `(f s: string? n) -> number`;
/// - a call's operator, or its opening paren: what the call returns.
pub fn hover_type(
    exprs: &[ExprKind],
    analysis: &Analysis,
    info: &TypeInfo,
    source: SourceId,
    text: &str,
    offset: usize,
) -> Option<String> {
    #[derive(Clone, Copy, PartialEq)]
    enum Role {
        Binding,
        Reference,
        Operator(u32),
    }

    struct Walk<'a> {
        source: SourceId,
        offset: usize,
        // Atoms under the cursor (expansion can put several at one name).
        atoms: Vec<(&'a Atom, Role)>,
        // The innermost call whose paren is at the cursor.
        call_at_paren: Option<u32>,
        lambdas: FxHashMap<u32, &'a LambdaFunction>,
    }

    impl<'a> Walk<'a> {
        fn under(&self, span: Span) -> bool {
            span.source_id() == Some(self.source)
                && (span.start as usize) <= self.offset
                && self.offset < span.end as usize
        }

        fn atom(&mut self, expr: &'a ExprKind, role: Role) {
            if let ExprKind::Atom(atom) = expr {
                if atom.ident().is_some() && self.under(atom.syn.span) {
                    self.atoms.push((atom, role));
                }
            }
        }

        fn expr(&mut self, expr: &'a ExprKind) {
            match expr {
                ExprKind::Atom(_) => self.atom(expr, Role::Reference),
                ExprKind::Define(d) => {
                    self.atom(&d.name, Role::Binding);
                    self.expr(&d.body);
                }
                ExprKind::Let(l) => {
                    for (name, value) in &l.bindings {
                        self.atom(name, Role::Binding);
                        self.expr(value);
                    }
                    self.expr(&l.body_expr);
                }
                ExprKind::LambdaFunction(l) => {
                    self.lambdas.insert(l.syntax_object_id, l);
                    for arg in &l.args {
                        self.atom(arg, Role::Binding);
                    }
                    self.expr(&l.body);
                }
                ExprKind::If(i) => {
                    self.expr(&i.test_expr);
                    self.expr(&i.then_expr);
                    self.expr(&i.else_expr);
                }
                ExprKind::Begin(b) => b.exprs.iter().for_each(|e| self.expr(e)),
                ExprKind::Set(t) => self.expr(&t.expr),
                ExprKind::Return(r) => self.expr(&r.expr),
                ExprKind::List(l) => {
                    if l.location.source_id() == Some(self.source)
                        && l.location.start as usize == self.offset
                    {
                        self.call_at_paren = Some(l.syntax_object_id);
                    }
                    if let Some((operator, args)) = l.args.split_first() {
                        match operator {
                            ExprKind::Atom(_) => {
                                self.atom(operator, Role::Operator(l.syntax_object_id))
                            }
                            _ => self.expr(operator),
                        }
                        args.iter().for_each(|a| self.expr(a));
                    }
                }
                _ => {}
            }
        }
    }

    let mut walk = Walk {
        source,
        offset,
        atoms: Vec::new(),
        call_at_paren: None,
        lambdas: FxHashMap::default(),
    };
    for expr in exprs {
        walk.expr(expr);
    }

    // A name as written, or de-mangled when it is not in this file.
    let name_of = |atom: &Atom| -> String {
        let span = atom.syn.span;
        if span.source_id() == Some(source) {
            if let Some(written) = text.get(span.start as usize..span.end as usize) {
                return written.to_string();
            }
        }
        atom.ident()
            .map(|i| i.resolve().trim_start_matches(['#', '_']).to_string())
            .unwrap_or_default()
    };

    let signature = |name: &str, lambda: u32| -> Option<String> {
        let l = walk.lambdas.get(&lambda)?;
        let fixed = l.args.len() - usize::from(l.rest);
        let mut parts = vec![name.to_string()];
        for (i, arg) in l.args.iter().enumerate() {
            let ExprKind::Atom(atom) = arg else { continue };
            let id = atom.syn.syntax_object_id;
            let ty = info.bindings.get(&id).copied().unwrap_or(Ty::TOP);
            let required = info.required.get(&id).copied();
            let shown = match required {
                Some(req) if req != ty && ty.contains(req) && !req.is_top() && !req.is_bottom() => {
                    format!("{}: {}?", name_of(atom), info.show(req))
                }
                _ if !ty.is_top() && !ty.is_bottom() => {
                    format!("{}: {}", name_of(atom), info.show(ty))
                }
                _ => name_of(atom),
            };
            if i == fixed {
                parts.push(".".to_string());
            }
            parts.push(shown);
        }
        let ret = info.returns.get(&lambda).copied().unwrap_or(Ty::TOP);
        Some(format!("({}) -> {}", parts.join(" "), info.show(ret)))
    };

    // The function a binding or reference denotes, if any.
    let function_of = |atom: &Atom| -> Option<u32> {
        let id = atom.syn.syntax_object_id;
        info.functions
            .get(&id)
            .or_else(|| reference(analysis, atom).and_then(|b| info.functions.get(&b)))
            .copied()
    };

    if !walk.atoms.is_empty() {
        // Prefer a function among several things under one name.
        let (atom, role) = walk
            .atoms
            .iter()
            .find(|(a, _)| function_of(a).is_some())
            .or(walk.atoms.first())
            .copied()?;
        let name = name_of(atom);
        if let Some(lambda) = function_of(atom) {
            let sig = signature(&name, lambda)?;
            return Some(match role {
                Role::Operator(call) => match info.calls.get(&call) {
                    Some(&ty) => format!("{sig}\n; this call: {}", info.show(ty)),
                    None => sig,
                },
                _ => sig,
            });
        }
        let id = atom.syn.syntax_object_id;
        return Some(match role {
            Role::Binding => {
                let ty = info.bindings.get(&id).copied().unwrap_or(Ty::TOP);
                match info.required.get(&id).copied() {
                    Some(req)
                        if req != ty && ty.contains(req) && !req.is_top() && !req.is_bottom() =>
                    {
                        format!("{name}: {}, expected {}", info.show(ty), info.show(req))
                    }
                    _ => format!("{name}: {}", info.show(ty)),
                }
            }
            Role::Reference => {
                let ty = info.references.get(&id).copied().unwrap_or(Ty::TOP);
                format!("{name}: {}", info.show(ty))
            }
            Role::Operator(call) => {
                let ty = info.calls.get(&call).copied().unwrap_or(Ty::TOP);
                format!("({name} ...) -> {}", info.show(ty))
            }
        });
    }

    let call = walk.call_at_paren?;
    let ty = info.calls.get(&call)?;
    Some(format!("-> {}", info.show(*ty)))
}

fn is_false_literal(expr: &ExprKind) -> bool {
    matches!(expr, ExprKind::Atom(atom) if matches!(atom.syn.ty, TokenType::BooleanLiteral(false)))
}

fn number_type(number: &NumberLiteral) -> Ty {
    match number {
        NumberLiteral::Real(RealLiteral::Int(IntLiteral::Small(_))) => Ty::INT,
        NumberLiteral::Real(RealLiteral::Int(IntLiteral::Big(_))) => Ty::OTHER_NUMBER,
        // Normalized, so 4/2 is the fixnum 2.
        NumberLiteral::Real(RealLiteral::Rational(..)) => Ty::INT.union(Ty::OTHER_NUMBER),
        NumberLiteral::Real(RealLiteral::Float(_)) => Ty::FLOAT,
        NumberLiteral::Complex(..) | NumberLiteral::Polar(..) => Ty::NUMBER,
    }
}

/// The type of a quoted datum.
fn datum_type(expr: &ExprKind) -> Ty {
    match expr {
        ExprKind::Atom(atom) => match &atom.syn.ty {
            TokenType::Identifier(_) => Ty::SYMBOL,
            TokenType::Number(number) => number_type(&number.resolve()),
            TokenType::StringLiteral(_) => Ty::STRING,
            TokenType::CharacterLiteral(_) => Ty::CHAR,
            TokenType::BooleanLiteral(_) => Ty::BOOL,
            _ => Ty::TOP,
        },
        // The `car` of an improper list is one of the elements before the dot.
        ExprKind::List(list) if list.improper => Ty::holding(
            TypeSet::PAIR,
            data_kinds(&list.args[..list.args.len().saturating_sub(1)]),
            TypeSet::BOTTOM,
        ),
        ExprKind::List(list) => Ty::list_of(data_kinds(&list.args)),
        ExprKind::Vector(vector) if vector.bytes => Ty::BYTEVECTOR,
        ExprKind::Vector(vector) => {
            Ty::holding(TypeSet::VECTOR, data_kinds(&vector.args), TypeSet::BOTTOM)
        }
        _ => Ty::TOP,
    }
}

/// The kinds of every datum in `data`.
fn data_kinds(data: &[ExprKind]) -> TypeSet {
    data.iter().fold(TypeSet::BOTTOM, |kinds, datum| {
        kinds.union(datum_type(datum).kinds)
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::passes::analysis::SemanticAnalysis;
    use crate::parser::ast::ExprKind;
    use crate::parser::parser::SourceId;
    use crate::steel_vm::engine::Engine;
    use std::collections::BTreeMap;
    use thin_vec::thin_vec;

    /// Infers types for `src`, and describes them by the source text of each
    /// name: `name` for a binding (numbered `name#2`... when a name is bound
    /// more than once), `name()` for what the function bound to it returns,
    /// and `name@line:col` for a narrowed reference.
    fn describe(src: &str) -> BTreeMap<String, String> {
        describe_at(src, &[])
    }

    /// [`describe`] for a program at `path` (so its `require`s resolve).
    /// `modules` are registered with the engine as `(name, source)`.
    fn describe_at(src: &str, modules: &[(&str, &str)]) -> BTreeMap<String, String> {
        let mut engine = Engine::new();
        for (name, source) in modules {
            engine.register_steel_module(name.to_string(), source.to_string());
        }
        let mut exprs = engine
            .emit_expanded_ast_without_optimizations(src, None)
            .unwrap();
        let analysis = SemanticAnalysis::new(&mut exprs);
        let primitives = PrimitiveSignatures::from_modules(engine.builtin_modules());
        let info = infer_types(&analysis.exprs, &analysis.analysis, &primitives);

        let text = |atom: &Atom| -> Option<String> {
            let span = atom.syn.span;
            src.get(span.start as usize..span.end as usize)
                .map(str::to_string)
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
                // For functions, `name()` is the return type of the lambda bound to `name`.
                let named_lambda = match expr {
                    ExprKind::Define(d) => Some((&d.name, &d.body)),
                    ExprKind::Let(l) if l.bindings.len() == 1 => {
                        Some((&l.bindings[0].0, &l.bindings[0].1))
                    }
                    _ => None,
                };
                if let Some((ExprKind::Atom(name), ExprKind::LambdaFunction(lambda))) = named_lambda
                {
                    if let (Some(name), Some(ty)) =
                        (text(name), info.returns.get(&lambda.syntax_object_id))
                    {
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
                    out.insert(
                        atom.syn.syntax_object_id,
                        (ident.resolve().to_string(), atom.syn.span),
                    );
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
                ("f", "(listof int)"),
                ("g", "(listof int)"),
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
                // In the else-branch, anything but a pair.
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
             (define (bump!) (set! counter 5))
             (define counter '())
             (define (string-length x) \"shadow\")
             (define (u) (string-length \"a\"))",
            &[
                // A boxed local has the type of its initial value.
                (
                    "v",
                    if crate::config::config().box_assigned_locals {
                        "int"
                    } else {
                        "any"
                    },
                ),
                ("m()", "any"),
                // `set!` evaluates to the previous value (so this is not void).
                ("sr()", "any"),
                // Assigned by a function defined before the global is, so the
                // analysis leaves that `set!` target unresolved.
                ("counter", "any"),
                // Uses the user's definition instead of the primitive.
                ("u()", "string"),
            ],
        );
    }

    /// A name that is also a builtin's is the builtin only when nothing in
    /// the program binds it. A user definition, or one provided by a required
    /// module, gets none of the builtin's signature.
    #[test]
    fn shadowed_builtins_are_not_builtins() {
        check(
            "(define (length x) 0)
             (define (k v) (length v) v)",
            &[("k()", "any")],
        );

        let src = "(require \"test/lists\")
             (define (h y) (length y) y)
             (define (c y) (car y))
             (define (s y) (string-length y) y)";
        let types = describe_at(
            src,
            &[(
                "test/lists",
                "(provide length car) (define (length x) 0) (define (car x) \"mine\")",
            )],
        );
        // The module's `length` takes anything, so there is no narrowing to a list.
        assert_eq!(
            types.get("h()").map(String::as_str),
            Some("any"),
            "{types:#?}"
        );
        // Nor the builtin `car`'s return type.
        assert_eq!(
            types.get("c()").map(String::as_str),
            Some("any"),
            "{types:#?}"
        );
        // The builtin narrows.
        assert_eq!(
            types.get("s()").map(String::as_str),
            Some("string"),
            "{types:#?}"
        );
    }

    /// The expanded program, and the id of its source.
    fn expand_program(src: &str) -> (Engine, Vec<ExprKind>, SourceId) {
        let mut engine = Engine::new();
        let exprs = engine
            .emit_expanded_ast_without_optimizations(src, None)
            .unwrap();
        let source = exprs.last().and_then(|e| e.span().source_id()).unwrap();
        (engine, exprs, source)
    }

    /// The hints for a program, as `name label` in source order.
    fn hints_for(src: &str) -> Vec<String> {
        let (engine, mut exprs, source) = expand_program(src);
        let analysis = SemanticAnalysis::new(&mut exprs);
        let primitives = PrimitiveSignatures::from_modules(engine.builtin_modules());
        let info = infer_types(&analysis.exprs, &analysis.analysis, &primitives);
        inlay_hints(&analysis.exprs, &info, source, src)
            .into_iter()
            .map(|hint| {
                // `name label`, or `name ... label` when it lands after a header.
                let name = &src[hint.span.start as usize..hint.span.end as usize];
                if hint.at == hint.span.end {
                    format!("{name} {}", hint.label)
                } else {
                    format!(
                        "{} {}",
                        &src[hint.span.start as usize - 1..hint.at as usize],
                        hint.label
                    )
                }
            })
            .collect()
    }

    fn failures_for(src: &str) -> Vec<String> {
        let (engine, mut exprs, source) = expand_program(src);
        let analysis = SemanticAnalysis::new(&mut exprs);
        let primitives = PrimitiveSignatures::from_modules(engine.builtin_modules());
        let info = infer_types(&analysis.exprs, &analysis.analysis, &primitives);
        written_call_failures(&info, source, src)
            .map(|failure| {
                format!(
                    "{} {}",
                    &src[failure.span.start as usize..failure.span.end as usize],
                    failure.message()
                )
            })
            .collect()
    }

    fn arity_failures_for(src: &str) -> Vec<String> {
        let (engine, mut exprs, source) = expand_program(src);
        let analysis = SemanticAnalysis::new(&mut exprs);
        let primitives = PrimitiveSignatures::from_modules(engine.builtin_modules());
        let info = infer_types(&analysis.exprs, &analysis.analysis, &primitives);
        written_arity_failures(&info, source, src)
            .map(|(failure, written)| failure.message(written))
            .collect()
    }

    #[test]
    fn calls_with_the_wrong_number_of_arguments() {
        let failures = arity_failures_for(
            "(define (two a b) (+ a b))
(define (at-least-one a . rest) a)
(define (f)
  (define (helper x) (* x 2))
  (let loop ([i 0] [acc 0])
    (if (> i 3)
        (helper acc 1)
        (loop (+ i 1)))))
(define (g) (two 1))
(define (h) (at-least-one))
(define (k) (hash-insert (hash) 1))
(define (m) (car))
(define (ok) (list (two 1 2) (at-least-one 1 2 3) (hash-insert (hash) 1 2)))",
        );
        assert_eq!(
            failures,
            vec![
                "ArityMismatch: helper expects 1 argument, found 2",
                "ArityMismatch: loop expects 2 arguments, found 1",
                "ArityMismatch: two expects 2 arguments, found 1",
                "ArityMismatch: at-least-one expects at least 1 argument, found 0",
                "ArityMismatch: hash-insert expects 3 arguments, found 2",
                "ArityMismatch: car expects 1 argument, found 0",
            ]
        );
    }

    #[test]
    fn arity_of_shadowed_and_mutated_functions_is_not_checked() {
        let failures = arity_failures_for(
            "(define (two a b) (+ a b))
(set! two (lambda (a) a))
(define (g) (two 1))
(define (h)
  (define (car a b) a)
  (car 1 2))",
        );
        assert_eq!(failures, Vec::<String>::new());
    }

    const STRUCTS: &str = "(struct point (x y))
(struct rect (w h))
(struct cell (v) #:mutable)
(define p (point 1 2))
(define r (rect 1 2))
(define c (cell 5))
";

    #[test]
    fn struct_hints() {
        let hints = hints_for(&format!(
            "{STRUCTS}(define (get q) (point-x q))
(define (cget q) (cell-v q))
(define (either q) (if (point? q) (point-y q) (rect-h q)))
(define (make) (point 1 2))
(define (not-point q) (if (point? q) (rect-w q) 0))"
        ));
        assert_eq!(
            hints,
            vec![
                "p : point",
                "r : rect",
                "c : cell",
                // An accessor takes only its own struct type.
                "q : point?",
                "q : cell?",
                // Each branch demands one of them.
                "q : (point | rect)?",
                "(make) -> point",
                // A point never gets through the first branch.
                "q : (any except point)?",
            ]
        );
    }

    #[test]
    fn struct_failures() {
        let failures = failures_for(&format!(
            "{STRUCTS}(define (a) (point-x r))
(define (b) (point-x \"abc\"))
(define (d) (cell-v 5))
(define (e) (set-cell-v! r 1))
(define (f q) (if (point? q) (rect-w q) 0))
(define (g) (vector-ref p 0))
(define (ok1 q) (if (point? q) (point-x q) (rect-w q)))
(define (ok2) (list (point-x p) (cell-v c) (set-cell-v! c 2) (rect-h r)))"
        ));
        let arity = arity_failures_for(&format!(
            "{STRUCTS}(define (a) (point 1))
(define (b) (point-x))
(define (d) (cell 1 2))
(define (e) (point? p r))
(define (ok) (list (point 1 2) (point-x p) (cell 1) (point? p)))"
        ));
        assert_eq!(
            failures,
            vec![
                "r `point-x` always raises here: argument 1 is always rect, but it must be point",
                "\"abc\" `point-x` always raises here: argument 1 is always string, but it must be point",
                "5 `cell-v` always raises here: argument 1 is always int, but it must be cell",
                "r `set-cell-v!` always raises here: argument 1 is always rect, but it must be cell",
                // The predicate narrowed q to a point.
                "q `rect-w` always raises here: argument 1 is always point, but it must be rect",
                "p `vector-ref` always raises here: argument 1 is always point, but it must be vector|mutable-vector|flat-vector",
            ]
        );
        assert_eq!(
            arity,
            vec![
                "ArityMismatch: point expects 2 arguments, found 1",
                "ArityMismatch: point-x expects 1 argument, found 0",
                "ArityMismatch: cell expects 1 argument, found 2",
                "ArityMismatch: point? expects 1 argument, found 2",
            ]
        );
    }

    #[test]
    fn struct_functions_that_are_redefined_are_not_trusted() {
        let failures = failures_for(&format!(
            "{STRUCTS}(set! point-x car)
(define (a) (point-x r))
(define (b)
  (define (rect-w x) x)
  (rect-w p))"
        ));
        assert_eq!(failures, Vec::<String>::new());

        // A struct type made at run time, by a function, is not one of the
        // program's struct types.
        let src = "(define (make) (make-struct-type 'foo 1 (current-module-relative)))
(define table (make))
(define foo (list-ref table 1))
(define foo-a (list-ref (list-ref table 4) 0))
(define (a) (foo 1 2))
(define (b) (foo-a 5))";
        assert_eq!(failures_for(src), Vec::<String>::new());
        assert_eq!(arity_failures_for(src), Vec::<String>::new());
    }

    #[test]
    fn globals_defined_twice_are_not_trusted() {
        // `match` refers to the builtin `car`, so the module's expansion binds
        // `car` to it before the module's own definition.
        let mut engine = Engine::new();
        engine.register_steel_module(
            "shim".to_string(),
            "(provide g)
(struct mp (a d))
(define (car p) (if (mp? p) (mp-a p) (#%prim.car p)))
(define (h x) (match x [(list a b) a] [_ 0]))
(define (g y) (car y) y)"
                .to_string(),
        );
        let mut exprs = engine
            .emit_expanded_ast_without_optimizations("(require \"shim\")", None)
            .unwrap();
        let analysis = SemanticAnalysis::new(&mut exprs);
        let primitives = PrimitiveSignatures::from_modules(engine.builtin_modules());
        let info = infer_types(&analysis.exprs, &analysis.analysis, &primitives);

        fn find_lambda(expr: &ExprKind, suffix: &str) -> Option<u32> {
            match expr {
                ExprKind::Define(d) => match (&d.name, &d.body) {
                    (ExprKind::Atom(a), ExprKind::LambdaFunction(l))
                        if a.ident()?.resolve().ends_with(suffix) =>
                    {
                        Some(l.syntax_object_id)
                    }
                    _ => None,
                },
                ExprKind::Begin(b) => b.exprs.iter().find_map(|e| find_lambda(e, suffix)),
                _ => None,
            }
        }
        let g = analysis
            .exprs
            .iter()
            .find_map(|e| find_lambda(e, "__g"))
            .unwrap();
        // `car` is the module's own, which also takes structs.
        assert_eq!(info.returns[&g].to_string(), "any");
    }

    #[test]
    fn any_number_of_struct_types() {
        let mut src = String::new();
        for i in 0..200 {
            src.push_str(&format!("(struct s{i} (a))\n"));
        }
        src.push_str("(define last-one (s199 1))\n(define (bad) (s0-a last-one))\n");
        assert_eq!(
            hover_at(&src, "last-one", 0, 1).as_deref(),
            Some("last-one: s199")
        );
        assert_eq!(failures_for(&src).len(), 1);
    }

    #[test]
    fn structs_from_a_required_module() {
        let mut engine = Engine::new();
        engine.register_steel_module(
            "shapes".to_string(),
            "(provide point point-x point?) (struct point (x y))".to_string(),
        );
        let src = "(require \"shapes\")
(define p (point 1 2))
(define (ok) (list (point-x p) (point? p)))";
        let mut exprs = engine
            .emit_expanded_ast_without_optimizations(src, None)
            .unwrap();
        let source = exprs.last().and_then(|e| e.span().source_id()).unwrap();
        let analysis = SemanticAnalysis::new(&mut exprs);
        let primitives = PrimitiveSignatures::from_modules(engine.builtin_modules());
        let info = infer_types(&analysis.exprs, &analysis.analysis, &primitives);
        assert_eq!(written_call_failures(&info, source, src).count(), 0);
        assert_eq!(written_arity_failures(&info, source, src).count(), 0);
    }

    /// With `TY_DUMP_SIGNATURES=path`, writes every native function's declared
    /// signature, one per line and sorted, to compare two builds.
    #[test]
    fn dump_signatures() {
        let Ok(path) = std::env::var("TY_DUMP_SIGNATURES") else {
            return;
        };
        let engine = Engine::new();
        let mut lines = Vec::new();
        for module in engine.builtin_modules().inner().values() {
            for name in module.names() {
                let Some(value) = module.try_get_ref(&name) else {
                    continue;
                };
                if let Some(meta) = signature_of(&value) {
                    lines.push(format!(
                        "{name} | {:?} | {:?} | {:?} | {:?} | {}",
                        meta.arity, meta.return_type, meta.arg_types, meta.raises, meta.is_const
                    ));
                }
            }
        }
        lines.sort();
        lines.dedup();
        std::fs::write(path, lines.join("\n") + "\n").unwrap();
    }

    /// What `failures_for` finds, for a program that exports only `exports`.
    fn failures_exporting(src: &str, exports: &[&str]) -> Vec<String> {
        let (engine, mut exprs, source) = expand_program(src);
        let analysis = SemanticAnalysis::new(&mut exprs);
        let primitives = PrimitiveSignatures::from_modules(engine.builtin_modules());
        let exports: FxHashSet<String> = exports.iter().map(|e| e.to_string()).collect();
        let info = infer_types_with_exports(
            &analysis.exprs,
            &analysis.analysis,
            &primitives,
            Some(&exports),
        );
        written_call_failures(&info, source, src)
            .map(|failure| {
                format!(
                    "{} {}",
                    &src[failure.span.start as usize..failure.span.end as usize],
                    failure.message()
                )
            })
            .collect()
    }

    #[test]
    fn struct_field_types() {
        let src = "(struct point (x y))
(struct cell (v) #:mutable)
(define p (point 1 \"a\"))
(define c (cell 5))
(set-cell-v! c 6)
(define (f q) (string-length (point-x q)))
(define (g) (string-length (cell-v c)))
(define (ok q) (string-length (point-y q)))";
        let expected = vec![
            "(point-x q) `string-length` always raises here: argument 1 is always int, but it must be string",
            "(cell-v c) `string-length` always raises here: argument 1 is always int, but it must be string",
        ];
        assert_eq!(failures_exporting(src, &[]), expected);
        // Without knowing what the program exports, another module might make
        // points of anything.
        assert_eq!(failures_for(src), Vec::<String>::new());
    }

    #[test]
    fn struct_fields_are_unknown_when_construction_can_happen_elsewhere() {
        let src = "(struct point (x y))
(struct cell (v) #:mutable)
(define p (point 1 2))
(define c (cell 5))
(define (f q) (string-length (point-x q)))
(define (g) (string-length (cell-v c)))";
        // Exported constructor, exported setter.
        assert_eq!(
            failures_exporting(src, &["point", "set-cell-v!"]),
            Vec::<String>::new()
        );
        // The constructor escapes as a value, and the setter is reassigned.
        let escaped = format!(
            "{src}
(define makers (list point))
(define setter set-cell-v!)
(set! setter #f)"
        );
        assert_eq!(failures_exporting(&escaped, &[]), Vec::<String>::new());
        // A setter storing something else widens the field.
        let widened = format!("{src}\n(set-cell-v! c \"s\")");
        assert_eq!(
            failures_exporting(&widened, &[]),
            vec!["(point-x q) `string-length` always raises here: argument 1 is always int, but it must be string"]
        );
    }

    #[test]
    fn exported_names_from_provide_forms() {
        let parse = |src: &str| crate::parser::parser::Parser::parse(src).unwrap();
        let names = |src: &str| {
            exported_names(&parse(src)).map(|names| {
                let mut names: Vec<String> = names.into_iter().collect();
                names.sort();
                names
            })
        };
        assert_eq!(
            names("(provide a (for-syntax m) (contract/out f (-> int? int?))) (define a 1)"),
            Some(vec!["a".to_string(), "f".to_string(), "m".to_string()])
        );
        assert_eq!(names("(define a 1)"), Some(vec![]));
        // A provide a macro might write, or a form it does not know.
        assert_eq!(
            names("(define-syntax p (syntax-rules () [(_) (provide a)]))"),
            None
        );
        assert_eq!(names("(provide (all-defined-out))"), None);
    }

    #[test]
    fn container_hints() {
        let hints = hints_for(
            "(define xs (list 1 2 3))
(define ys (cons \"a\" xs))
(define q '(1 2 3))
(define v #(1.5 2.5))
(define h (hash \"a\" 1 \"b\" 2))
(define h2 (hash-insert h 'c #\\x))
(define s (hashset 1 2))
(define first-x (car xs))
(define doubled (map (lambda (x) (* x 2)) xs))
(define evens (filter even? xs))
(define total (foldl (lambda (x acc) (+ x acc)) 0 xs))
(define (build n) (let loop ([i 0] [acc '()]) (if (= i n) acc (loop (+ i 1) (cons i acc)))))
(define keys (hash-keys->list h))
(define looked (hash-ref h \"a\"))",
        );
        assert_eq!(
            hints,
            vec![
                "xs : (listof int)",
                "ys : (listof (int | string))",
                "q : (listof int)",
                "v : (vectorof float)",
                "h : (hash string int)",
                "h2 : (hash (string | symbol) (int | char))",
                "s : (setof int)",
                "first-x : int",
                "doubled : (listof number)",
                // The lambdas passed to map and foldl get the list's elements.
                "x : int",
                "evens : (listof int)",
                "total : number",
                "x : int",
                "acc : number",
                // (+ i 1) is a number, so the accumulated list is too.
                "(build n) -> (listof number)",
                "loop -> (listof number)",
                "i : number",
                "acc : (listof number)",
                "keys : (listof string)",
                "looked : int",
            ]
        );
    }

    #[test]
    fn container_failures() {
        let failures = failures_for(
            "(define xs (list 1 2 3))
(define h (hash \"a\" 1))
(define (a) (string-length (car xs)))
(define (b) (string-length (hash-ref h \"a\")))
(define (c) (symbol->string (first (map (lambda (x) (+ x 1)) xs))))
(define (ok1) (+ (car xs) 1))
(define (ok2 ys) (string-length (car ys)))",
        );
        assert_eq!(
            failures,
            vec![
                "(car xs) `string-length` always raises here: argument 1 is always int, but it must be string",
                "(hash-ref h \"a\") `string-length` always raises here: argument 1 is always int, but it must be string",
                "(first (map (lambda (x) (+ x 1)) xs)) `symbol->string` always raises here: argument 1 is always number, but it must be symbol|list",
            ]
        );
    }

    #[test]
    fn builtin_module_functions() {
        let src = "(require-builtin steel/time)
(require-builtin steel/time as t.)
(define (a) (naive-date-ymd \"x\" 1 2))
(define (b) (t.naive-date-ymd 1 2 \"y\"))
(define (c) (t.local-time/now! 5))
(define (ok) (t.naive-date-ymd 2026 9 27))";
        assert_eq!(
            failures_for(src),
            vec![
                "\"x\" `naive-date-ymd` always raises here: argument 1 is always string, but it must be int",
                "\"y\" `t.naive-date-ymd` always raises here: argument 3 is always string, but it must be int",
                "5 `t.local-time/now!` always raises here: argument 1 is always int, but it must be string|symbol",
            ]
        );
        assert_eq!(
            arity_failures_for(
                "(require-builtin steel/time as t.)
(define (a) (t.naive-date-ymd 1))"
            ),
            vec!["ArityMismatch: t.naive-date-ymd expects 3 arguments, found 1"]
        );
        // A program binding of a builtin module's name is not the builtin.
        assert_eq!(
            failures_for(
                "(require-builtin steel/time)
(define (f)
  (define (naive-date-ymd a b c) a)
  (naive-date-ymd \"x\" 1 2))"
            ),
            Vec::<String>::new()
        );
    }

    #[test]
    fn registered_host_functions() {
        use crate::steel_vm::builtin::BuiltInModule;
        use crate::steel_vm::register_fn::RegisterFn;
        let mut engine = Engine::new();
        let mut module = BuiltInModule::new("host/demo");
        module.register_fn("host-add", |a: isize, b: isize| a + b);
        module.register_fn("host-shout", |s: String| s.to_uppercase());
        module.register_fn("host-mean", |xs: Vec<f64>| xs.iter().sum::<f64>());
        engine.register_module(module);
        let src = "(require-builtin host/demo)
(define (a) (host-add \"x\" 1))
(define (b) (string-length (host-add 1 2)))
(define (c) (+ 1 (host-shout 'sym)))
(define (d) (host-mean 5))
(define (ok) (list (+ (host-add 1 2) 3) (string-length (host-shout \"a\")) (host-mean (list 1.0))))";
        let mut exprs = engine
            .emit_expanded_ast_without_optimizations(src, None)
            .unwrap();
        let source = exprs.last().and_then(|e| e.span().source_id()).unwrap();
        let analysis = SemanticAnalysis::new(&mut exprs);
        let primitives = PrimitiveSignatures::from_modules(engine.builtin_modules());
        let info = infer_types(&analysis.exprs, &analysis.analysis, &primitives);
        let failures: Vec<String> = written_call_failures(&info, source, src)
            .map(|failure| {
                format!(
                    "{} {}",
                    &src[failure.span.start as usize..failure.span.end as usize],
                    failure.message()
                )
            })
            .collect();
        assert_eq!(
            failures,
            vec![
                "\"x\" `host-add` always raises here: argument 1 is always string, but it must be int",
                "(host-add 1 2) `string-length` always raises here: argument 1 is always int, but it must be string",
                // A `String` parameter also takes a symbol, and returns a string.
                "(host-shout 'sym) `+` always raises here: argument 2 is always string, but it must be number",
                "5 `host-mean` always raises here: argument 1 is always int, but it must be list|vector",
            ]
        );
        let arity: Vec<String> = written_arity_failures(&info, source, src)
            .map(|(failure, written)| failure.message(written))
            .collect();
        assert_eq!(arity, Vec::<String>::new());
    }

    #[test]
    fn struct_definitions_raise_nothing() {
        // The expansion passes the descriptor, a host value, to natives.
        let (engine, mut exprs, _) =
            expand_program("(struct point (x y) #:mutable)\n(struct thing (a))\n(define z 1)");
        let analysis = SemanticAnalysis::new(&mut exprs);
        let primitives = PrimitiveSignatures::from_modules(engine.builtin_modules());
        let info = infer_types(&analysis.exprs, &analysis.analysis, &primitives);
        let failures: Vec<String> = info.failures.iter().map(|f| f.message()).collect();
        assert_eq!(failures, Vec::<String>::new());
    }

    #[test]
    fn free_names_are_the_preludes() {
        // A private module binds `mutable-vector->list` to a function of its
        // own host type. A free name is the prelude's function.
        let src = "(define (f) (mutable-vector->list (make-vector 3 0)))";
        assert_eq!(failures_for(src), Vec::<String>::new());
        let src = "(define (f) (mutable-vector->list \"no\"))";
        assert_eq!(failures_for(src).len(), 1);
    }

    #[test]
    fn context_functions() {
        use crate::steel_vm::builtin::BuiltInModule;
        use crate::steel_vm::register_fn::RegisterFn;
        let mut engine = Engine::new();
        engine.register_value("app-context", SteelVal::StringV("app".into()));
        let mut module = BuiltInModule::new("host/ctx");
        module.register_fn_with_ctx("app-context", "ctx-inc", |_: SteelVal, x: isize| x + 1);
        module.register_fn_with_ctx("app-context", "ctx-name", |ctx: String| ctx);
        engine.register_module(module);
        let src = "(require-builtin host/ctx)
(define (a) (ctx-inc \"x\"))
(define (b) (ctx-inc 1 2))
(define (c) (string-length (ctx-inc 1)))
(define (ok) (list (+ 1 (ctx-inc 1)) (string-length (ctx-name))))";
        let mut exprs = engine
            .emit_expanded_ast_without_optimizations(src, None)
            .unwrap();
        let source = exprs.last().and_then(|e| e.span().source_id()).unwrap();
        let analysis = SemanticAnalysis::new(&mut exprs);
        let primitives = PrimitiveSignatures::from_modules(engine.builtin_modules());
        let info = infer_types(&analysis.exprs, &analysis.analysis, &primitives);
        let failures: Vec<String> = written_call_failures(&info, source, src)
            .map(|failure| {
                format!(
                    "{} {}",
                    &src[failure.span.start as usize..failure.span.end as usize],
                    failure.message()
                )
            })
            .collect();
        // The context is passed by the wrapper, so callers give one less.
        assert_eq!(
            failures,
            vec![
                "\"x\" `ctx-inc` always raises here: argument 1 is always string, but it must be int",
                "(ctx-inc 1) `string-length` always raises here: argument 1 is always int, but it must be string",
            ]
        );
        let arity: Vec<String> = written_arity_failures(&info, source, src)
            .map(|(failure, written)| failure.message(written))
            .collect();
        assert_eq!(arity.len(), 1, "{arity:?}");
        assert!(arity[0].contains("ctx-inc"), "{arity:?}");

        let result = engine
            .compile_and_run_raw_program(format!("{src}\n(ok)"))
            .unwrap();
        assert_eq!(result.last().unwrap().to_string(), "(3 3)");
    }

    #[test]
    fn custom_host_types() {
        use crate::steel_vm::builtin::BuiltInModule;
        use crate::steel_vm::register_fn::RegisterFn;
        #[derive(Clone)]
        struct Counter(isize);
        impl crate::rvals::Custom for Counter {}
        #[derive(Clone)]
        struct Gauge(f64);
        impl crate::rvals::Custom for Gauge {}

        let mut engine = Engine::new();
        let mut module = BuiltInModule::new("host/counter");
        module.register_fn("make-counter", || Counter(0));
        module.register_fn("make-gauge", || Gauge(0.0));
        module.register_fn("counter-value", |c: &Counter| c.0);
        module.register_fn("counter-bump!", |c: &mut Counter, by: isize| c.0 += by);
        module.register_fn("counter-copy", |c: Counter| c);
        engine.register_module(module);
        let src = "(require-builtin host/counter)
(define c (make-counter))
(define (a) (counter-value 5))
(define (b) (counter-bump! \"x\" 1))
(define (d) (counter-copy (make-gauge)))
(define (e) (+ 1 c))
(define (ok) (counter-bump! c (counter-value (counter-copy c))))";
        let mut exprs = engine
            .emit_expanded_ast_without_optimizations(src, None)
            .unwrap();
        let source = exprs.last().and_then(|e| e.span().source_id()).unwrap();
        let analysis = SemanticAnalysis::new(&mut exprs);
        let primitives = PrimitiveSignatures::from_modules(engine.builtin_modules());
        let info = infer_types(&analysis.exprs, &analysis.analysis, &primitives);
        let failures: Vec<String> = written_call_failures(&info, source, src)
            .map(|failure| {
                format!(
                    "{} {}",
                    &src[failure.span.start as usize..failure.span.end as usize],
                    failure.message()
                )
            })
            .collect();
        assert_eq!(
            failures,
            vec![
                "5 `counter-value` always raises here: argument 1 is always int, but it must be Counter",
                "\"x\" `counter-bump!` always raises here: argument 1 is always string, but it must be Counter",
                "(make-gauge) `counter-copy` always raises here: argument 1 is always Gauge, but it must be Counter",
                "c `+` always raises here: argument 2 is always Counter, but it must be number",
            ]
        );
        let offset = src.find("define c").unwrap() + "define ".len();
        assert_eq!(
            hover_type(
                &analysis.exprs,
                &analysis.analysis,
                &info,
                source,
                src,
                offset
            )
            .as_deref(),
            Some("c: Counter")
        );
    }

    #[test]
    fn calls_that_always_raise() {
        let failures = failures_for(
            "(define x 10)
(define (f) (string-length x))
(define (g) (+ 1 \"a\"))
(define (h v)
  (if (string? v)
      (+ v 1)
      0))
(define (k)
  (let ([s 10])
    (string-append s \"x\")))",
        );
        assert_eq!(
            failures,
            vec![
                "x `string-length` always raises here: argument 1 is always int, but it must be string",
                "\"a\" `+` always raises here: argument 2 is always string, but it must be number",
                "v `+` always raises here: argument 1 is always string, but it must be number",
                "s `string-append` always raises here: argument 1 is always int, but it must be string",
            ]
        );
    }

    #[test]
    fn calls_that_might_succeed_are_not_reported() {
        let failures = failures_for(
            "(define (a x) (string-length x))
(define (b x)
  (if (int? x)
      (+ x 1)
      (string-length x)))
(define y 1)
(set! y \"s\")
(define (c) (string-length y))
(define (d) (string-length (error \"no\")))
(define (e z) (+ (if z 1 \"s\") 1))
(define (f)
  (define (string-length s) 0)
  (string-length 5))",
        );
        assert_eq!(failures, Vec::<String>::new());
    }

    #[test]
    fn inlay_hints_for_bindings_and_functions() {
        let hints = hints_for(
            "(define (f s)
  (define n (string-length s))
  (define (helper k) (* k 2))
  (helper n))
(define (sum-list xs)
  (let loop ([xs xs] [acc 0])
    (if (null? xs) acc (loop (cdr xs) (+ acc (car xs))))))
(define x 5)
(define (g a b) (or (string? a) b))",
        );
        assert_eq!(
            hints,
            vec![
                "(f s) -> number",
                // f's body requires a string of s (string-length).
                "s : string?",
                "n : int",
                "(helper k) -> number",
                "k : int",
                "(sum-list xs) -> number",
                // The named let shows its return type, and not the placeholder
                // `loop` is bound to before the lambda is stored.
                "loop -> number",
                // `null?` is true of an empty vector, `car` and `cdr` take lists
                // and pairs.
                "xs : (list | pair | vector | mutable-vector)?",
                "acc : number",
                "x : int",
                // Nothing on `or` (its binding belongs to the macro), and
                // nothing for `g`, which returns #t or `b` (`any`).
            ],
            "{hints:#?}"
        );
    }

    /// Hover text at the first occurrence of `needle` (offset into it).
    fn hover_at(src: &str, needle: &str, nth: usize, into: usize) -> Option<String> {
        let (engine, mut exprs, source) = expand_program(src);
        let analysis = SemanticAnalysis::new(&mut exprs);
        let primitives = PrimitiveSignatures::from_modules(engine.builtin_modules());
        let info = infer_types(&analysis.exprs, &analysis.analysis, &primitives);
        let offset = src.match_indices(needle).nth(nth).unwrap().0 + into;
        hover_type(
            &analysis.exprs,
            &analysis.analysis,
            &info,
            source,
            src,
            offset,
        )
    }

    #[test]
    fn hover_types() {
        let src = "(define (g x) (if (string? x) (string-length x) 0))
(define (h s) (string-length s) (string-upcase s))
(define (use) (string-append (h \"a\") \"b\"))";
        // A reference narrowed by the branch it is in, and not before it.
        // ("x)" occurs as the parameter, in the test, and in the branch.)
        assert_eq!(hover_at(src, "x)", 2, 0).as_deref(), Some("x: string"));
        assert_eq!(hover_at(src, "x)", 1, 0).as_deref(), Some("x: any"));
        // A function's name shows its signature, with the required type.
        assert_eq!(
            hover_at(src, "h s", 0, 0).as_deref(),
            Some("(h s: string?) -> string")
        );
        // A call to it shows the signature and what this call returns.
        assert_eq!(
            hover_at(src, "(h \"a\")", 0, 1).as_deref(),
            Some("(h s: string?) -> string\n; this call: string")
        );
        // A builtin's call, from its operator or its paren.
        assert_eq!(
            hover_at(src, "string-append", 0, 3).as_deref(),
            Some("(string-append ...) -> string")
        );
        assert_eq!(
            hover_at(src, "(string-append", 0, 0).as_deref(),
            Some("-> string")
        );
        // A parameter is any, but the body requires a string.
        assert_eq!(
            hover_at(src, "s)", 0, 0).as_deref(),
            Some("s: any, expected string")
        );
    }

    #[test]
    fn inlay_hints_for_required_parameter_types() {
        let hints = hints_for(
            "(define (h s) (string-length s) (string-upcase s))
(define (g x) (if (string? x) (string-length x) 0))
(define (both v i) (list (vector-ref v i) (+ i 1)))
(define (either y) (if (string? y) (string-length y) (vector-length y)))
(define (helper-user n)
  (define (helper k) (* k 2))
  (helper n))",
        );
        assert_eq!(
            hints,
            vec![
                "(h s) -> string",
                // Every normal return went through (string-length s).
                "s : string?",
                "(g x) -> int",
                // Only one branch demands a string, so nothing is required of x.
                "(both v i) -> list",
                // vector-ref takes any of the three vectors; a flat vector is
                // `other`.
                "v : (vector | mutable-vector | other)?",
                "i : int?",
                // Each branch demands something of y, so the join requires one
                // or the other.
                "(either y) -> int",
                "y : (string | vector | mutable-vector | other)?",
                "(helper-user n) -> number",
                "(helper k) -> number",
                // n is a global function's parameter (any), so the call site
                // says nothing of k, and the body needs a number. When the call
                // site knows more, as with `k : int` above, that is shown.
                "k : number?",
            ],
            "{hints:#?}"
        );
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
                // Passed to `map`, so called from somewhere we cannot see.
                ("z", "any"),
                // Global, so callable from outside whatever `use` passes.
                ("n", "any"),
                ("fact()", "number"),
                ("use()", "number"),
                ("xs", "list"),
            ],
        );
    }

    /// Checks robustness and coverage over real programs, given as
    /// `TY_FILES=a.scm:b.scm`.
    #[test]
    fn stats_over_files() {
        let Ok(files) = std::env::var("TY_FILES") else {
            return;
        };
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
            // What the file exports, from its `provide` forms, as the language
            // server sees it.
            let exports = crate::parser::parser::Parser::parse(&src)
                .ok()
                .and_then(|source| exported_names(&source));
            let info = infer_types_with_exports(
                &analysis.exprs,
                &analysis.analysis,
                &primitives,
                exports.as_ref(),
            );
            let elapsed = start.elapsed();
            for failure in &info.failures {
                println!(
                    "{file}: always raises: {} at {:?}",
                    failure.message(),
                    failure.span
                );
            }
            for failure in &info.arity_failures {
                println!(
                    "{file}: arity: {} at {:?}",
                    failure.message(&failure.function),
                    failure.operator
                );
            }
            let count = |map: &mut dyn Iterator<Item = Ty>| {
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
                "{file}: {:?} iters={} bindings(known,any,never)={b:?} returns={r:?} calls={c:?} structs={} closed={} fields={:?}",
                elapsed,
                info.iterations,
                info.structs.len(),
                info.fields.len(),
                {
                    let mut fields: Vec<String> = info
                        .fields
                        .iter()
                        .map(|(ty, fields)| {
                            let shown: Vec<String> = fields.iter().map(|t| info.show(*t)).collect();
                            format!("{}({})", info.structs[*ty].name, shown.join(", "))
                        })
                        .collect();
                    fields.sort();
                    fields
                }
            );
            if std::env::var("TY_NEVER").is_ok() {
                let mut names = FxHashMap::default();
                for expr in analysis.exprs.iter() {
                    collect_binding_names(expr, &mut names);
                }
                for (id, ty) in &info.bindings {
                    if ty.is_bottom() {
                        if let Some((name, span)) = names.get(id) {
                            let line = src
                                .get(..span.start as usize)
                                .map(|s| s.matches('\n').count() + 1);
                            println!(
                                "    never: {name} line {line:?} (source {:?})",
                                span.source_id()
                            );
                        }
                    }
                }
            }
        }
    }

    /// Where a check was inserted, for reporting.
    struct Site {
        what: String,
        ty: Ty,
        span: crate::parser::span::Span,
    }

    /// Instruments `expr` with `(__tycheck__ value type-bits site)` around every
    /// variable reference with an inferred type narrower than `any`, every
    /// non-tail call, and every parameter (checked on entry). Tail calls are
    /// left alone so loops keep constant stack.
    fn instrument(
        expr: &mut ExprKind,
        info: &TypeInfo,
        lambda_params: &FxHashMap<u32, Vec<SyntaxObjectId>>,
        sites: &mut Vec<Site>,
        tail: bool,
        enclosing: Option<u32>,
    ) {
        fn wrap(expr: ExprKind, ty: Ty, site: usize) -> ExprKind {
            // A control. With the claim made wrong on purpose, violations must show up.
            let ty = if std::env::var("TY_SOUND_CORRUPT").is_ok() {
                ty.without(Ty::INT)
            } else if std::env::var("TY_SOUND_CORRUPT_STRUCTS").is_ok() {
                ty.without(Ty::STRUCTS)
            } else if std::env::var("TY_SOUND_CORRUPT_ELEMENTS").is_ok() {
                Ty {
                    elements: ty.elements.without(TypeSet::INT),
                    ..ty
                }
            } else {
                ty
            };
            let bits =
                |set: TypeSet| ExprKind::integer_literal(set.bits() as isize, Default::default());
            ExprKind::List(List::new(thin_vec![
                ExprKind::ident("__tycheck__"),
                bits(ty.kinds),
                bits(ty.elements),
                bits(ty.keys),
                ExprKind::integer_literal(site as isize, Default::default()),
                expr,
            ]))
        }
        let site = |sites: &mut Vec<Site>, what: String, ty: Ty, span| {
            sites.push(Site { what, ty, span });
            sites.len() - 1
        };
        match expr {
            ExprKind::Atom(atom) if atom.ident().is_some() => {
                if let Some(&ty) = info.references.get(&atom.syn.syntax_object_id) {
                    if !ty.is_top() {
                        let id = site(
                            sites,
                            format!("ref {}", atom.ident().unwrap().resolve()),
                            ty,
                            atom.syn.span,
                        );
                        *expr = wrap(expr.clone(), ty, id);
                    }
                }
            }
            ExprKind::If(x) => {
                instrument(
                    &mut x.test_expr,
                    info,
                    lambda_params,
                    sites,
                    false,
                    enclosing,
                );
                instrument(
                    &mut x.then_expr,
                    info,
                    lambda_params,
                    sites,
                    tail,
                    enclosing,
                );
                instrument(
                    &mut x.else_expr,
                    info,
                    lambda_params,
                    sites,
                    tail,
                    enclosing,
                );
            }
            ExprKind::Let(x) => {
                for (_, init) in x.bindings.iter_mut() {
                    instrument(init, info, lambda_params, sites, false, enclosing);
                }
                instrument(
                    &mut x.body_expr,
                    info,
                    lambda_params,
                    sites,
                    tail,
                    enclosing,
                );
            }
            ExprKind::Define(x) => {
                instrument(&mut x.body, info, lambda_params, sites, false, enclosing)
            }
            ExprKind::LambdaFunction(x) => {
                instrument(
                    &mut x.body,
                    info,
                    lambda_params,
                    sites,
                    true,
                    Some(x.syntax_object_id),
                );
                let mut checks = Vec::new();
                for arg in &x.args {
                    let ExprKind::Atom(atom) = arg else { continue };
                    let Some(&ty) = info.bindings.get(&atom.syn.syntax_object_id) else {
                        continue;
                    };
                    if !ty.is_top() {
                        let id = site(
                            sites,
                            format!("param {}", atom.ident().unwrap().resolve()),
                            ty,
                            atom.syn.span,
                        );
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
                    instrument(e, info, lambda_params, sites, tail && i + 1 == n, enclosing);
                }
            }
            ExprKind::Set(x) => {
                instrument(&mut x.expr, info, lambda_params, sites, false, enclosing)
            }
            ExprKind::List(list) => {
                let id = list.syntax_object_id;
                let span = list.location;
                let original_args: Vec<ExprKind> = list.args.iter().skip(1).cloned().collect();
                if let Some((operator, args)) = list.args.split_first_mut() {
                    // A variable in operator position stays as it is.
                    if !matches!(operator, ExprKind::Atom(_)) {
                        instrument(operator, info, lambda_params, sites, false, enclosing);
                    }
                    for arg in args {
                        instrument(arg, info, lambda_params, sites, false, enclosing);
                    }
                }
                // What the callee requires of its arguments holds once it has
                // returned, so check it after the call with
                // `(let ([r (f a)]) (check a) r)`. That takes a tail call out of
                // tail position, so skip it for a lambda calling itself (a
                // loop), which would then grow the stack.
                let target = info.call_targets.get(&id).copied();
                let self_call = target.is_some() && target == enclosing;
                let mut required_checks = Vec::new();
                if !self_call {
                    if let Some(params) = target.and_then(|l| lambda_params.get(&l)) {
                        for (arg, param) in original_args.iter().zip(params) {
                            let (ExprKind::Atom(atom), Some(&req)) =
                                (arg, info.required.get(param))
                            else {
                                continue;
                            };
                            if req.is_top() || atom.ident().is_none() {
                                continue;
                            }
                            let what = format!(
                                "required {} of {}",
                                atom.ident().unwrap().resolve(),
                                list.args[0]
                                    .to_string()
                                    .chars()
                                    .take(30)
                                    .collect::<String>()
                            );
                            let id = site(sites, what, req, atom.syn.span);
                            let mut reference = arg.clone();
                            if let ExprKind::Atom(a) = &mut reference {
                                a.syn.syntax_object_id =
                                    crate::parser::parser::SyntaxObjectId::fresh();
                            }
                            required_checks.push(wrap(reference, req, id));
                        }
                    }
                }
                if !tail {
                    if let Some(&ty) = info.calls.get(&id) {
                        if !ty.is_top() && !list.args.is_empty() {
                            let what = format!(
                                "call {}",
                                list.args[0]
                                    .to_string()
                                    .chars()
                                    .take(40)
                                    .collect::<String>()
                            );
                            let site = site(sites, what, ty, span);
                            *expr = wrap(expr.clone(), ty, site);
                        }
                    }
                }
                {
                    if !required_checks.is_empty() {
                        static N: std::sync::atomic::AtomicUsize =
                            std::sync::atomic::AtomicUsize::new(0);
                        let tmp = crate::parser::interner::InternedString::from(format!(
                            "__req_tmp{}",
                            N.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
                        ));
                        required_checks.push(ExprKind::atom(tmp));
                        let call = std::mem::take(expr);
                        let location = crate::parser::parser::SyntaxObject::default(
                            crate::parser::tokens::TokenType::Let,
                        );
                        *expr = ExprKind::Let(Box::new(crate::parser::ast::Let::new(
                            vec![(ExprKind::atom(tmp), call)],
                            ExprKind::Begin(Box::new(crate::parser::ast::Begin::new(
                                required_checks,
                                location.clone(),
                            ))),
                            location,
                        )));
                    }
                }
            }
            _ => {}
        }
    }

    /// The struct type names of the program being checked, by index.
    static STRUCT_NAMES: std::sync::Mutex<Vec<String>> = std::sync::Mutex::new(Vec::new());

    /// Whether `value` is of `ty`: its kind, and for a container, what it holds.
    fn matches_ty(ty: Ty, value: &SteelVal) -> bool {
        if !matches_with_structs(ty.kinds, value) {
            return false;
        }
        // Only the first elements of a large container, so programs that keep
        // building longer lists stay linear.
        fn all(set: TypeSet, values: impl Iterator<Item = SteelVal>) -> bool {
            set.is_top()
                || values
                    .into_iter()
                    .take(256)
                    .all(|v| matches_with_structs(set, &v))
        }
        // Containers are immutable, so one that held the right kinds once
        // always does. Checking each at every reference would be quadratic in
        // programs that build large lists.
        let identity = match value {
            // A list's length is a walk, but its node pointer names the view.
            SteelVal::ListV(list) => Some((list.as_ptr_usize(), 0)),
            SteelVal::VectorV(vector) => {
                Some((crate::gc::Gc::as_ptr(&vector.0) as usize, vector.len()))
            }
            SteelVal::HashMapV(map) => Some((crate::gc::Gc::as_ptr(&map.0) as usize, map.len())),
            SteelVal::HashSetV(set) => Some((crate::gc::Gc::as_ptr(&set.0) as usize, set.len())),
            _ => None,
        };
        let key = identity.map(|(address, len)| (address, len, ty.elements.bits(), ty.keys.bits()));
        if let Some(key) = key {
            if CHECKED.with(|checked| checked.borrow().contains_key(&key)) {
                return true;
            }
        }
        let holds = match value {
            SteelVal::ListV(list) => all(ty.elements, list.iter().cloned()),
            SteelVal::Pair(pair) => all(ty.elements, std::iter::once(pair.car())),
            SteelVal::VectorV(vector) => all(ty.elements, vector.iter().cloned()),
            SteelVal::HashSetV(set) => all(ty.elements, set.iter().cloned()),
            SteelVal::HashMapV(map) => {
                all(ty.keys, map.keys().cloned()) && all(ty.elements, map.values().cloned())
            }
            _ => true,
        };
        if let (Some(key), true) = (key, holds) {
            CHECKED.with(|checked| {
                let mut checked = checked.borrow_mut();
                if checked.len() > 100_000 {
                    checked.clear();
                }
                // Held so the address is not reused by another container.
                checked.insert(key, value.clone());
            });
        }
        holds
    }

    std::thread_local! {
        /// Containers already found to hold what a claim says, by identity,
        /// length, and the claimed element and key kinds.
        static CHECKED: std::cell::RefCell<FxHashMap<(usize, usize, u64, u64), SteelVal>> =
            std::cell::RefCell::new(FxHashMap::default());
    }

    /// `ty.matches(value)`, where a struct matches a struct type of the program
    /// by name. The kind of a struct depends on the program, so
    /// [`TypeSet::of`] only says `other`.
    fn matches_with_structs(ty: TypeSet, value: &SteelVal) -> bool {
        if ty.matches(value) {
            return true;
        }
        let SteelVal::CustomStruct(s) = value else {
            return false;
        };
        let names = STRUCT_NAMES.lock().unwrap();
        names
            .iter()
            .enumerate()
            .any(|(index, name)| name.as_str() == s.name().resolve() && ty.has_struct(index))
    }

    /// The first bad value seen at each site.
    static VIOLATIONS: std::sync::Mutex<std::collections::BTreeMap<usize, String>> =
        std::sync::Mutex::new(std::collections::BTreeMap::new());
    static CHECKS_RUN: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

    fn tycheck(
        kinds: isize,
        elements: isize,
        keys: isize,
        site: isize,
        value: SteelVal,
    ) -> SteelVal {
        CHECKS_RUN.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let ty = Ty {
            kinds: TypeSet::from_bits(kinds as u64),
            elements: TypeSet::from_bits(elements as u64),
            keys: TypeSet::from_bits(keys as u64),
        };
        if !matches_ty(ty, &value) {
            VIOLATIONS
                .lock()
                .unwrap()
                .entry(site as usize)
                .or_insert_with(|| {
                    // Printing can itself panic (the cycle printer on some boxes),
                    // but the kind is always safe.
                    let text = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                        let mut shown = value.to_string();
                        shown.truncate(60);
                        shown
                    }))
                    .unwrap_or_else(|_| "<unprintable>".to_string());
                    format!("{} ({})", text, TypeSet::of(&value))
                });
        }
        value
    }

    /// Runs each program in `TY_SOUND_FILES=a.scm:b.scm` instrumented with a
    /// check of every inferred type, and fails on any value outside its type.
    #[test]
    fn soundness_check() {
        use crate::steel_vm::register_fn::RegisterFn;
        let Ok(files) = std::env::var("TY_SOUND_FILES") else {
            return;
        };
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
            // What the file exports, from its `provide` forms, as the language
            // server sees it.
            let exports = crate::parser::parser::Parser::parse(&src)
                .ok()
                .and_then(|source| exported_names(&source));
            let info = infer_types_with_exports(
                &analysis.exprs,
                &analysis.analysis,
                &primitives,
                exports.as_ref(),
            );
            *STRUCT_NAMES.lock().unwrap() = info.structs.iter().map(|ty| ty.name.clone()).collect();

            if std::env::var("TY_SOUND_DEBUG").is_ok() {
                let narrow_required = info.required.values().filter(|t| !t.is_top()).count();
                let targets_with_required = info
                    .call_targets
                    .values()
                    .filter(|l| info.returns.contains_key(l))
                    .count();
                println!(
                    "    debug: required entries {} (narrower than any: {}), call_targets {} ({} to lambdas with a return), lists recorded {}",
                    info.required.len(),
                    narrow_required,
                    info.call_targets.len(),
                    targets_with_required,
                    info.calls.len()
                );
            }
            let mut sites = Vec::new();
            // Instrument the analyzed expressions themselves. Cloning an AST
            // gives every syntax object a fresh id, and every lookup into
            // `info` (keyed by those ids) would miss.
            drop(analysis);
            let mut instrumented: Vec<ExprKind> = exprs;
            // A control, with the expanded program run as it is and no checks.
            if std::env::var("TY_SOUND_BASELINE").is_err() {
                let mut lambda_params = FxHashMap::default();
                fn collect_params(e: &ExprKind, out: &mut FxHashMap<u32, Vec<SyntaxObjectId>>) {
                    if let ExprKind::LambdaFunction(l) = e {
                        // Only the fixed parameters line up with arguments. A
                        // rest parameter collects all the others into a list.
                        let fixed = l.args.len() - usize::from(l.rest);
                        let ids = l.args[..fixed].iter().filter_map(binding_site).collect();
                        out.insert(l.syntax_object_id, ids);
                    }
                    match e {
                        ExprKind::If(i) => {
                            collect_params(&i.test_expr, out);
                            collect_params(&i.then_expr, out);
                            collect_params(&i.else_expr, out);
                        }
                        ExprKind::Let(l) => {
                            l.bindings.iter().for_each(|(_, v)| collect_params(v, out));
                            collect_params(&l.body_expr, out);
                        }
                        ExprKind::Define(d) => collect_params(&d.body, out),
                        ExprKind::LambdaFunction(l) => collect_params(&l.body, out),
                        ExprKind::Begin(b) => b.exprs.iter().for_each(|x| collect_params(x, out)),
                        ExprKind::List(l) => l.args.iter().for_each(|x| collect_params(x, out)),
                        ExprKind::Set(t) => collect_params(&t.expr, out),
                        _ => {}
                    }
                }
                for expr in instrumented.iter() {
                    collect_params(expr, &mut lambda_params);
                }
                if std::env::var("TY_SOUND_DEBUG").is_ok() {
                    for (lambda, params) in &lambda_params {
                        for p in params {
                            if let Some(req) = info.required.get(p).filter(|t| !t.is_top()) {
                                let targeted =
                                    info.call_targets.values().filter(|l| *l == lambda).count();
                                println!("    debug: lambda {lambda} param {p:?} requires {req}; call sites resolving to it: {targeted}");
                            }
                        }
                    }
                    let known: FxHashSet<u32> = lambda_params.keys().copied().collect();
                    let missing = info
                        .call_targets
                        .values()
                        .filter(|l| !known.contains(l))
                        .count();
                    println!(
                        "    debug: call targets not in lambda_params: {missing} of {}",
                        info.call_targets.len()
                    );
                }
                for expr in instrumented.iter_mut() {
                    instrument(expr, &info, &lambda_params, &mut sites, false, None);
                }
            }
            if let Ok(dump) = std::env::var("TY_SOUND_DUMP") {
                let program: String = instrumented
                    .iter()
                    .map(|e| e.to_pretty(100) + "\n")
                    .collect();
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
            let kinds = |prefix: &str| sites.iter().filter(|s| s.what.starts_with(prefix)).count();
            println!(
                "{file}: {} checks inserted (ref {} / param {} / call {} / required {}), {} executed, run {}, {} violations",
                sites.len(),
                kinds("ref "),
                kinds("param "),
                kinds("call "),
                kinds("required"),
                CHECKS_RUN.load(std::sync::atomic::Ordering::Relaxed),
                match &result {
                    Ok(_) => "ok".to_string(),
                    Err(e) => format!("FAILED: {}", e.to_string().chars().take(150).collect::<String>()),
                },
                violations.len()
            );
            for (site, value) in violations {
                let s = &sites[site];
                let line = src
                    .get(..s.span.start as usize)
                    .map(|p| p.matches('\n').count() + 1);
                println!(
                    "    VIOLATION {} : inferred {} but got {} (line {:?})",
                    s.what, s.ty, value, line
                );
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
        let Ok(src) = std::env::var("TY_SRC") else {
            return;
        };
        let path = std::env::var("TY_PATH").ok().map(std::path::PathBuf::from);
        let mut engine = Engine::new();
        let mut exprs = engine
            .emit_expanded_ast_without_optimizations(&src, path)
            .unwrap();
        let analysis = SemanticAnalysis::new(&mut exprs);
        for expr in analysis.exprs.iter() {
            println!("{}", expr.to_pretty(80));
            // With TY_BUILTINS, print how analysis classifies each operator.
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
                                    n, name.syn.syntax_object_id, d.body
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
