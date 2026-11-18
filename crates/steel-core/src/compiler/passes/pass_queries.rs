//! What a Steel pass can ask about the program it was given
//! (STEEL_PASSES_IN_STEEL_PLAN.md, phase 3).
//!
//! While a pass runs, the program it was given and the bridge's forms are in
//! a thread local. The analysis (`Analysis`, keyed by syntax id) is made the
//! first time the pass asks something, from that program, so it describes
//! exactly what the pass sees. Passes that ask nothing don't pay for it. A
//! pass that rewrites the program and wants fresh facts returns its result;
//! the next pass gets a new analysis.

use alloc::format;
use alloc::vec::Vec;
use core::cell::RefCell;
use core::marker::PhantomData;

use rustc_hash::FxHashSet;
use steel_parser::ast::ExprKind;
use steel_parser::interner::InternedString;
use steel_parser::parser::SyntaxObjectId;

use crate::compiler::passes::analysis::{
    bound_names, struct_predicate_names, Analysis, IdentifierStatus, SemanticInformation, BOOLEAN_PRIMITIVES,
    REORDERABLE_PRIMITIVES,
};
use crate::compiler::passes::syntax_bridge::SyntaxBridge;
use crate::gc::Gc;
use crate::rerrs::{ErrorKind, SteelErr};
use crate::rvals::{Result, SteelVal};
use crate::steel_vm::builtin::BuiltInModule;
use crate::steel_vm::register_fn::RegisterFn;

// The program a pass was given
enum Program {
    // As ExprKind already: the first pass at a point, or one on its own
    Given(*const [ExprKind]),
    // Syntax an earlier pass in a chain returned, decoded the first time
    // something is asked
    Pending(SteelVal),
    Decoded(Vec<ExprKind>),
}

struct Facts {
    // Valid while the `FactsGuard` that put them here lives, which borrows
    // them
    program: Program,
    bridge: *mut SyntaxBridge,
    analysis: Option<Analysis>,
    struct_predicates: Option<FxHashSet<InternedString>>,
    bound: Option<FxHashSet<InternedString>>,
}

impl Facts {
    fn bridge(&self) -> &SyntaxBridge {
        // Safety: see `bridge`
        unsafe { &*self.bridge }
    }

    // A program a pass returned as syntax that can't be decoded gives no facts
    fn exprs(&mut self) -> &[ExprKind] {
        if let Program::Pending(syntax) = &self.program {
            // Safety: see `bridge`
            let bridge = unsafe { &mut *self.bridge };
            let decoded = bridge.decode_detached(syntax).unwrap_or_default();
            self.program = Program::Decoded(decoded);
        }
        match &self.program {
            // Safety: see `program`
            Program::Given(exprs) => unsafe { &**exprs },
            Program::Decoded(exprs) => exprs,
            Program::Pending(_) => unreachable!(),
        }
    }

    fn form_node(&self, key: u32) -> Option<(u32, Option<bool>)> {
        self.bridge().form_node(key)
    }

    fn analysis(&mut self) -> &Analysis {
        if self.analysis.is_none() {
            let exprs = self.exprs();
            let mut analysis = Analysis::from_exprs(exprs);
            analysis.populate_captures(exprs);
            self.analysis = Some(analysis);
        }
        self.analysis.as_ref().unwrap()
    }

    fn bound(&mut self) -> &FxHashSet<InternedString> {
        if self.bound.is_none() {
            self.bound = Some(bound_names(self.exprs()));
        }
        self.bound.as_ref().unwrap()
    }

    fn struct_predicates(&mut self) -> &FxHashSet<InternedString> {
        if self.struct_predicates.is_none() {
            self.struct_predicates = Some(struct_predicate_names(self.exprs()));
        }
        self.struct_predicates.as_ref().unwrap()
    }
}

std::thread_local! {
    static FACTS: RefCell<Option<Facts>> = const { RefCell::new(None) };
}

/// Makes a program the one queries answer about until it's dropped
pub(crate) struct FactsGuard<'a> {
    previous: Option<Facts>,
    _program: PhantomData<&'a mut SyntaxBridge>,
}

fn install<'a>(program: Program, bridge: &'a mut SyntaxBridge) -> FactsGuard<'a> {
    let facts = Facts {
        program,
        bridge: bridge as *mut SyntaxBridge,
        analysis: None,
        struct_predicates: None,
        bound: None,
    };
    FactsGuard {
        previous: FACTS.with(|current| current.replace(Some(facts))),
        _program: PhantomData,
    }
}

/// Queries answer about `exprs`, which `bridge` encoded
pub(crate) fn enter<'a>(exprs: &'a [ExprKind], bridge: &'a mut SyntaxBridge) -> FactsGuard<'a> {
    install(Program::Given(exprs as *const [ExprKind]), bridge)
}

/// Queries answer about `syntax`, what an earlier pass in a chain returned
/// from syntax `bridge` encoded. Only nodes from the encoded program have ids
/// the analysis knows: what an earlier pass wrote answers #f.
pub(crate) fn enter_pending<'a>(syntax: &SteelVal, bridge: &'a mut SyntaxBridge) -> FactsGuard<'a> {
    install(Program::Pending(syntax.clone()), bridge)
}

impl Drop for FactsGuard<'_> {
    fn drop(&mut self) {
        let previous = self.previous.take();
        FACTS.with(|current| *current.borrow_mut() = previous);
    }
}

fn with_facts<T>(f: impl FnOnce(&mut Facts) -> T) -> Result<T> {
    FACTS.with(|facts| match &mut *facts.borrow_mut() {
        Some(facts) => Ok(f(facts)),
        None => Err(SteelErr::new(
            ErrorKind::Generic,
            "the program can only be asked about while a pass runs".into(),
        )),
    })
}

// The id a query is about: syntax from the program (an atom's own id, a
// form's node id), or an id a query gave back
fn id_of(facts: &Facts, value: &SteelVal) -> Option<u32> {
    match value {
        SteelVal::IntV(id) => u32::try_from(*id).ok(),
        SteelVal::SyntaxObject(s) => {
            let key = s.id()?;
            match facts.form_node(key) {
                Some((0, _)) => None,
                Some((node, _)) => Some(node),
                None => Some(key),
            }
        }
        _ => None,
    }
}

fn symbol(name: &str) -> SteelVal {
    SteelVal::SymbolV(name.into())
}

fn hash(entries: Vec<(&str, SteelVal)>) -> SteelVal {
    let map: crate::values::HashMap<SteelVal, SteelVal> =
        entries.into_iter().map(|(k, v)| (symbol(k), v)).collect();
    SteelVal::HashMapV(Gc::new(map).into())
}

fn kind_name(kind: &IdentifierStatus) -> &'static str {
    match kind {
        IdentifierStatus::Global => "global",
        IdentifierStatus::Local => "local",
        IdentifierStatus::LocallyDefinedFunction => "local-function",
        IdentifierStatus::LetVar => "let",
        IdentifierStatus::Captured => "captured",
        IdentifierStatus::Free => "free",
        IdentifierStatus::HeapAllocated => "heap",
    }
}

fn optional_id(id: Option<SyntaxObjectId>) -> SteelVal {
    id.map(|id| SteelVal::IntV(id.0 as isize))
        .unwrap_or(SteelVal::BoolV(false))
}

fn binding_hash(id: u32, info: &SemanticInformation, binding: Option<&SemanticInformation>) -> SteelVal {
    let binding = binding.unwrap_or(info);
    hash(vec![
        ("id", SteelVal::IntV(id as isize)),
        ("kind", symbol(kind_name(&info.kind))),
        // Of the binding a reference refers to, or of this one
        ("binding-kind", symbol(kind_name(&binding.kind))),
        ("refers-to", optional_id(info.refers_to)),
        // Of the binding a reference refers to, or of this one
        ("uses", SteelVal::IntV(binding.usage_count as isize)),
        ("set!", SteelVal::BoolV(binding.set_bang)),
        ("last-use", SteelVal::BoolV(info.last_usage)),
        ("builtin", SteelVal::BoolV(info.builtin)),
        (
            "builtin-name",
            info.builtin_name
                .map(|name| symbol(name.resolve()))
                .unwrap_or(SteelVal::BoolV(false)),
        ),
        ("escapes", SteelVal::BoolV(info.escapes)),
        (
            "captured",
            SteelVal::BoolV(info.captured_from_enclosing || binding.captured_from_enclosing),
        ),
        ("shadowed", SteelVal::BoolV(info.is_shadowed)),
    ])
}

/// `(binding-info id-or-syntax)`: what the analysis knows about an identifier,
/// as a hash, or #f
fn binding_info(value: SteelVal) -> Result<SteelVal> {
    with_facts(|facts| {
        let Some(id) = id_of(facts, &value) else {
            return SteelVal::BoolV(false);
        };
        let analysis = facts.analysis();
        let Some(info) = analysis.info.get(&SyntaxObjectId(id)) else {
            return SteelVal::BoolV(false);
        };
        let binding = info
            .refers_to
            .map(|binding| analysis.resolve_reference(binding))
            .and_then(|binding| analysis.info.get(&binding));
        binding_hash(id, info, binding)
    })
}

/// `(function-info lambda)`: a lambda form's arguments, captured variables
/// and whether it escapes, as a hash, or #f
fn function_info(value: SteelVal) -> Result<SteelVal> {
    with_facts(|facts| {
        let rest = match &value {
            SteelVal::SyntaxObject(s) => s
                .id()
                .and_then(|key| facts.form_node(key))
                .and_then(|(_, rest)| rest),
            _ => None,
        };
        let Some(id) = id_of(facts, &value) else {
            return SteelVal::BoolV(false);
        };
        let analysis = facts.analysis();
        let Some(info) = analysis.function_info.get(&id) else {
            return SteelVal::BoolV(false);
        };
        let names = |vars: &[(InternedString, crate::compiler::passes::analysis::ScopeInfo)]| {
            SteelVal::ListV(vars.iter().map(|(name, _)| symbol(name.resolve())).collect())
        };
        hash(vec![
            ("id", SteelVal::IntV(id as isize)),
            ("arguments", names(info.arguments())),
            ("captures", names(info.captured_vars())),
            ("escapes", SteelVal::BoolV(info.escapes)),
            ("rest", SteelVal::BoolV(rest.unwrap_or(false))),
            ("depth", SteelVal::IntV(info.depth as isize)),
        ])
    })
}

/// `(lambda-rest? form)`: whether a lambda from the program takes a rest
/// argument, its last
fn lambda_rest(value: SteelVal) -> Result<bool> {
    with_facts(|facts| match &value {
        SteelVal::SyntaxObject(s) => s
            .id()
            .and_then(|key| facts.form_node(key))
            .and_then(|(_, rest)| rest)
            .unwrap_or(false),
        _ => false,
    })
}

/// `(syntax-id stx)`: the id the analysis knows `stx` by, or #f
fn syntax_id(value: SteelVal) -> Result<SteelVal> {
    with_facts(|facts| {
        id_of(facts, &value)
            .map(|id| SteelVal::IntV(id as isize))
            .unwrap_or(SteelVal::BoolV(false))
    })
}

// The name an identifier has in the program (not the written one syntax-e
// gives), or a symbol's
fn raw_name(value: &SteelVal) -> Option<InternedString> {
    match value {
        SteelVal::SymbolV(name) => Some(InternedString::from(name.as_str())),
        SteelVal::SyntaxObject(s) => match &s.syntax {
            SteelVal::SymbolV(name) => Some(InternedString::from(name.as_str())),
            _ => None,
        },
        _ => None,
    }
}

/// `(struct-predicate? name)`: whether the program defines `name` (a symbol
/// or identifier) as a struct's predicate
fn struct_predicate(value: SteelVal) -> Result<bool> {
    let Some(name) = raw_name(&value) else {
        return Ok(false);
    };
    with_facts(|facts| facts.struct_predicates().contains(&name))
}

/// `(bound-name? name)`: whether anything in the program binds the name
/// (define, let, lambda), whichever binding a given identifier refers to.
/// Goes by names, so it holds up where the analysis is unsure: inlined copies
/// can share an id, and then `binding-info` describes one of them.
fn bound_name(value: SteelVal) -> Result<bool> {
    let Some(name) = raw_name(&value) else {
        return Ok(false);
    };
    with_facts(|facts| facts.bound().contains(&name))
}

/// `(identifier-name stx)`: an identifier's name in the program, which
/// `syntax-e` doesn't always give: it gives the name as written, without a
/// module's prefix. #f for anything else.
fn identifier_name(value: SteelVal) -> SteelVal {
    match &value {
        SteelVal::SyntaxObject(s) => match &s.syntax {
            SteelVal::SymbolV(_) => s.syntax.clone(),
            _ => SteelVal::BoolV(false),
        },
        SteelVal::SymbolV(_) => value,
        _ => SteelVal::BoolV(false),
    }
}

/// `(fresh-name prefix)`: a symbol no program has, `prefix` and a number
fn fresh_name(prefix: SteelVal) -> Result<SteelVal> {
    let SteelVal::SymbolV(prefix) = prefix else {
        return Err(SteelErr::new(
            ErrorKind::TypeMismatch,
            format!("fresh-name: expects a symbol, found {prefix}"),
        ));
    };
    Ok(symbol(&format!("{}{}", prefix.as_str(), SyntaxObjectId::fresh().0)))
}

/// `(reorderable-primitive? name)`: whether calls to the primitive `name`
/// (with or without `#%prim.`) have no effects, so arguments using it can be
/// evaluated in another order (REORDERABLE_PRIMITIVES)
fn reorderable_primitive(name: SteelVal) -> bool {
    match name {
        SteelVal::SymbolV(name) => {
            let name = name.as_str();
            REORDERABLE_PRIMITIVES.contains(&name.strip_prefix("#%prim.").unwrap_or(name))
        }
        _ => false,
    }
}

/// `(compiler-setting name)`: the compiler settings passes follow (config.rs),
/// by name: `hoist-car-all`. #f for any other name.
fn compiler_setting(name: SteelVal) -> SteelVal {
    let config = crate::config::config();
    match &name {
        SteelVal::SymbolV(name) if name.as_str() == "hoist-car-all" => {
            SteelVal::BoolV(config.hoist_car_all)
        }
        _ => SteelVal::BoolV(false),
    }
}

/// `(boolean-primitive? name)`: whether the primitive `name` (with or without
/// `#%prim.`) only ever answers #t or #f
fn boolean_primitive(name: SteelVal) -> bool {
    match name {
        SteelVal::SymbolV(name) => {
            let name = name.as_str();
            BOOLEAN_PRIMITIVES.contains(&name.strip_prefix("#%prim.").unwrap_or(name))
        }
        _ => false,
    }
}

pub(crate) fn register(module: &mut BuiltInModule) {
    module
        .register_fn("binding-info", binding_info)
        .register_fn("function-info", function_info)
        .register_fn("lambda-rest?", lambda_rest)
        .register_fn("syntax-id", syntax_id)
        .register_fn("struct-predicate?", struct_predicate)
        .register_fn("bound-name?", bound_name)
        .register_fn("boolean-primitive?", boolean_primitive)
        .register_fn("identifier-name", identifier_name)
        .register_fn("fresh-name", fresh_name)
        .register_fn("reorderable-primitive?", reorderable_primitive)
        .register_fn("compiler-setting", compiler_setting);
}
