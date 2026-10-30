use core::sync::atomic::{AtomicBool, Ordering};
use std::collections::HashSet;

use quickscope::ScopeSet;
use rustc_hash::{FxBuildHasher, FxHashMap, FxHashSet};
use smallvec::SmallVec;
use steel_parser::ast::{
    self, parse_lambda, Begin, BEGIN, DEFINE_SYNTAX, QUOTE, SYNTAX_CASE, SYNTAX_RULES,
};
use steel_parser::parser::{lower_macro_and_require_definitions, SourceId};
use steel_parser::tokens::NumberLiteral;

use crate::compiler::modules::{RequiredModuleMacros, PRELUDE_MODULES};
use crate::compiler::passes::mangle::NameMangler;
use crate::compiler::passes::VisitorMutRefUnit;
use crate::compiler::program::{DEFINE, LAMBDA, LAMBDA_FN, LAMBDA_SYMBOL, LET};
use crate::parser::ast::ExprKind;
use crate::parser::parser::{SyntaxObject, SyntaxObjectId};
use crate::steel_vm::engine::ModuleContainer;
use crate::{compiler::program::AS_KEYWORD, parser::tokens::TokenType};
use crate::{compiler::program::REQUIRE_BUILTIN, rvals::Result};

use steel_parser::expr_list;
use thin_vec::{thin_vec, ThinVec};

use super::visitors::VisitorMutRef;
use super::{
    ast::{Atom, Define, LambdaFunction, List, Quote},
    interner::InternedString,
    kernel::Kernel,
};

use crate::parser::expander::SteelMacro;

static REST_ARG: &str = "##%list-args";

fn take_macro_defs<B: BodyForms>(
    body: &mut B,
    start: usize,
    found: &mut Vec<ExprKind>,
) -> Result<()> {
    let mut index = start;

    while index < body.forms().len() {
        match &mut body.forms()[index] {
            ExprKind::List(l) if defines_local_macro(l) => {
                found.push(lower_macro_and_require_definitions(
                    body.remove_form(index),
                )?);

                continue;
            }
            ExprKind::Begin(b) => take_macro_defs(&mut b.exprs, 0, found)?,
            ExprKind::List(l) if is_begin(l) => take_macro_defs(&mut l.args, 1, found)?,
            _ => {}
        }

        index += 1;
    }

    Ok(())
}

fn binder_name(syn: &SyntaxObject) -> Option<InternedString> {
    match &syn.ty {
        TokenType::Identifier(name) => Some(*name),
        _ => None,
    }
}

// Tells two definitions of a name in a body apart: the name plus the scopes of the expansions
// that introduced it. Use-site scopes don't count, so two macro uses given the same name define
// it twice, as in Racket.
type LocalKey = (InternedString, steel_parser::scopes::ScopeSetId);

fn local_key(syn: &SyntaxObject) -> Option<LocalKey> {
    Some((binder_name(syn)?, syn.scopes.introductions()))
}

struct Depth(usize);

impl Depth {
    fn depth(&self) -> usize {
        self.0
    }

    fn push_layer(&mut self) {
        self.0 += 1;
    }

    fn pop_layer(&mut self) {
        self.0 = self.0.saturating_sub(1).max(1);
    }
}

fn parameter_name(param: &ExprKind) -> Option<&InternedString> {
    match param {
        ExprKind::List(l) => l.first_ident(),
        param => param.atom_identifier(),
    }
}

fn defines_macro(expr: &ExprKind) -> bool {
    matches!(expr, ExprKind::List(l) if defines_local_macro(l))
}

pub(crate) fn lift_macro_defs_from_begins(exprs: &mut Vec<ExprKind>) -> Result<()> {
    let mut found = Vec::new();
    let mut index = 0;

    while index < exprs.len() {
        // Splice a top-level begin that defines macros into the top level, so its forms see
        // the macros defined before them
        let spliced = match &mut exprs[index] {
            ExprKind::Begin(b) if b.exprs.iter().any(defines_macro) => {
                Some(core::mem::take(&mut b.exprs))
            }
            ExprKind::List(l) if is_begin(l) && l.args[1..].iter().any(defines_macro) => {
                Some(l.args.drain(1..).collect())
            }
            _ => None,
        };

        if let Some(forms) = spliced {
            let forms = forms
                .into_iter()
                .map(|form| {
                    if defines_macro(&form) {
                        Ok(lower_macro_and_require_definitions(form)?)
                    } else {
                        Ok(form)
                    }
                })
                .collect::<Result<Vec<_>>>()?;

            exprs.splice(index..=index, forms);
            continue;
        }

        match &mut exprs[index] {
            ExprKind::Begin(b) => take_macro_defs(&mut b.exprs, 0, &mut found)?,
            ExprKind::List(l) if is_begin(l) => take_macro_defs(&mut l.args, 1, &mut found)?,
            _ => {}
        }

        let lifted = found.len();
        exprs.splice(index..index, found.drain(..));

        index += lifted + 1;
    }

    Ok(())
}

// Returns the names of the macros it defined
pub(crate) fn define_generated_macros(
    expr: &mut ExprKind,
    macro_map: &mut FxHashMap<InternedString, SteelMacro>,
    globals: GlobalMap,
) -> Result<Vec<InternedString>> {
    rename_template_macros(core::slice::from_mut(expr), true);

    let mut names = Vec::new();

    // Expand again so the code after a definition can use the new macro
    while define_generated_macro_forms(expr, macro_map, &mut names)? {
        expand(expr, macro_map, globals)?;
        rename_template_macros(core::slice::from_mut(expr), true);
    }

    Ok(names)
}

// Returns whether it defined any macros
fn define_generated_macro_forms(
    expr: &mut ExprKind,
    macro_map: &mut FxHashMap<InternedString, SteelMacro>,
    names: &mut Vec<InternedString>,
) -> Result<bool> {
    let mut defined = false;

    match expr {
        ExprKind::List(l) if defines_local_macro(l) => {
            let definition = core::mem::replace(
                expr,
                ExprKind::Begin(Box::new(Begin::new(
                    Vec::new(),
                    SyntaxObject::default(TokenType::Begin),
                ))),
            );

            if let ExprKind::Macro(m) = lower_macro_and_require_definitions(definition)? {
                let generated_macro = SteelMacro::parse_from_ast_macro(m)?;
                names.push(*generated_macro.name());
                macro_map.insert(*generated_macro.name(), generated_macro);
                defined = true;
            }
        }
        ExprKind::Begin(b) => {
            for expr in b.exprs.iter_mut() {
                defined |= define_generated_macro_forms(expr, macro_map, names)?;
            }
        }
        ExprKind::List(l) if is_begin(l) => {
            for expr in l.args.iter_mut().skip(1) {
                defined |= define_generated_macro_forms(expr, macro_map, names)?;
            }
        }
        _ => {}
    }

    Ok(defined)
}

// Collects the names of the macros a form defines at its top level, including inside begins
pub(crate) fn defined_macro_names(expr: &ExprKind, names: &mut FxHashSet<InternedString>) {
    match expr {
        ExprKind::Begin(b) => b.exprs.iter().for_each(|x| defined_macro_names(x, names)),
        ExprKind::List(l) if l.is_define_syntax() => match l.get(1) {
            Some(ExprKind::List(signature)) => names.extend(signature.first_ident()),
            other => names.extend(other.and_then(ExprKind::atom_identifier)),
        },
        ExprKind::List(l) if is_begin(l) => l
            .args
            .iter()
            .skip(1)
            .for_each(|x| defined_macro_names(x, names)),
        _ => {}
    }
}

// Collects the macros of either kind a body defines at its top level, including inside begins.
// The kernel leaves uses of the syntax-rules ones to the syntax-rules expander.
fn defined_procedural_macros(expr: &ExprKind, names: &mut Vec<InternedString>) {
    match expr {
        ExprKind::Begin(b) => b
            .exprs
            .iter()
            .for_each(|x| defined_procedural_macros(x, names)),
        // A syntax-rules definition already parsed
        ExprKind::Macro(m) => {
            if let Some(syn) = m.name.atom_syntax_object() {
                if let TokenType::Identifier(name) = syn.ty {
                    names.push(name);
                }
            }
        }
        ExprKind::List(l) if l.is_define_syntax() => {
            let name = match l.get(1) {
                Some(ExprKind::List(signature)) => signature.first(),
                other => other,
            };

            if let Some(syn) = name.and_then(ExprKind::atom_syntax_object) {
                if let TokenType::Identifier(name) = syn.ty {
                    names.push(name);
                }
            }
        }
        ExprKind::List(l) if is_begin(l) => l
            .args
            .iter()
            .skip(1)
            .for_each(|x| defined_procedural_macros(x, names)),
        _ => {}
    }
}

pub(crate) fn defined_procedural_macro_names(
    expr: &ExprKind,
    names: &mut FxHashSet<InternedString>,
) {
    match expr {
        ExprKind::Begin(b) => b
            .exprs
            .iter()
            .for_each(|x| defined_procedural_macro_names(x, names)),
        ExprKind::List(l) if l.is_define_syntax() && !defines_local_macro(l) => {
            if let Some(ExprKind::SyntaxRules(_)) = l.get(2) {
                return;
            }

            match l.get(1) {
                Some(ExprKind::List(signature)) => names.extend(signature.first_ident()),
                other => names.extend(other.and_then(ExprKind::atom_identifier)),
            }
        }
        ExprKind::List(l) if is_begin(l) => l
            .args
            .iter()
            .skip(1)
            .for_each(|x| defined_procedural_macro_names(x, names)),
        _ => {}
    }
}

// Moves the macro definitions into the macro map, returning their names
pub fn extract_macro_defs(
    exprs: &mut Vec<ExprKind>,
    macro_map: &mut FxHashMap<InternedString, SteelMacro>,
    redefinitions: &mut MacroRedefinitions,
) -> Result<Vec<InternedString>> {
    lift_macro_defs_from_begins(exprs)?;

    let mut error = None;
    let mut defined = Vec::new();
    // Procedural macros seen so far. A later syntax-rules macro with the same name replaces
    // one from that point on
    let mut procedural = FxHashSet::default();

    exprs.retain_mut(|expr| {
        if let ExprKind::List(l) = expr {
            if l.is_define_syntax() {
                procedural.extend(procedural_macro_name(l));
            }
        }

        if let ExprKind::Macro(_) = expr {
            // Replace with dummy begin value so we don't have to copy
            // everything other for every macro definition
            let mut taken_expr = ExprKind::Begin(Box::new(Begin::new(
                Vec::new(),
                SyntaxObject::default(TokenType::Begin),
            )));

            core::mem::swap(expr, &mut taken_expr);

            if let ExprKind::Macro(m) = taken_expr {
                match SteelMacro::parse_from_ast_macro(m) {
                    Ok(mut generated_macro) => {
                        // Free names in its templates refer to the top-level globals
                        generated_macro.mark_definition_resolved("top-level");
                        let name = *generated_macro.name();
                        defined.push(name);

                        if let Some(marker) = redefinitions.define(
                            macro_map,
                            name,
                            generated_macro,
                            procedural.contains(&name),
                        ) {
                            *expr = marker;
                            return true;
                        }
                    }
                    Err(e) => {
                        error = Some(e);
                        return false;
                    }
                }
            } else {
                unreachable!();
            }

            return false;
        }

        true
    });

    if let Some(e) = error {
        return Err(e);
    }

    Ok(defined)
}

// How deeply macros can expand into each other. A long `and` or `cond` needs a lot of depth,
// so with stacker the stack grows as needed.
#[cfg(feature = "stacker")]
pub(crate) const MAX_EXPANSION_DEPTH: usize = 32768;
#[cfg(not(feature = "stacker"))]
pub(crate) const MAX_EXPANSION_DEPTH: usize = 512;

// How many expansions the first pass over a body form does before leaving the rest to the
// body's expansion
const MAX_HEAD_EXPANSIONS: usize = 256;

#[derive(Copy, Clone, Debug)]
pub enum GlobalMap<'a> {
    Map(&'a FxHashMap<InternedString, usize>),
    Set(&'a FxHashSet<InternedString>),
}

impl<'a> GlobalMap<'a> {
    pub fn contains(&self, i: &InternedString) -> bool {
        match self {
            // TODO: Fix this so that we can actually leverage it!
            GlobalMap::Map(_) => false,
            GlobalMap::Set(hash_set) => hash_set.contains(i),
        }
    }

    pub fn actually_contains(&self, i: &InternedString) -> bool {
        match self {
            // TODO: Fix this so that we can actually leverage it!
            GlobalMap::Map(m) => m.contains_key(i),
            GlobalMap::Set(hash_set) => hash_set.contains(i),
        }
    }
}

pub fn expand(
    expr: &mut ExprKind,
    map: &FxHashMap<InternedString, SteelMacro>,
    globals: GlobalMap,
) -> Result<ScopeSet<InternedString, FxBuildHasher>> {
    let exclusions = HashSet::new();
    let mut expander = Expander {
        depth: 0,
        map: MacroMap::Shared(map),
        overlays: Vec::new(),
        changed: false,
        in_scope_values: ScopeSet::default(),
        source_id: SourceId::none(),
        exclusions: &exclusions,
        globals,
        local_macros: LocalMacros::default(),
        top_level: true,
        body_form: false,
        foreign: false,
        kernel: None,
        define_top_level_procedural: false,
        defined_procedural: Vec::new(),
        head_only: false,
        head_expanded: false,
        head_covered: false,
        prelude_only: false,
        head_body: true,
        scope_chain: ScopeChain::default(),
        prelude_base: 0,
    };
    expander.visit(expr)?;

    Ok(expander.in_scope_values)
}

// Expands both the syntax-rules macros in `map` and the kernel's procedural macros, each where
// it's used. Returns whether any macro expanded.
pub(crate) fn expand_with_kernel(
    expr: &mut ExprKind,
    map: &FxHashMap<InternedString, SteelMacro>,
    globals: GlobalMap,
    kernel: KernelExpander<'_>,
) -> Result<bool> {
    expand_with_kernel_defining(expr, map, globals, kernel, true)
}

// Like `expand_with_kernel`, but if `define_top_level` is false, procedural macros defined at
// the top level are left for later
pub(crate) fn expand_with_kernel_defining(
    expr: &mut ExprKind,
    map: &FxHashMap<InternedString, SteelMacro>,
    globals: GlobalMap,
    kernel: KernelExpander<'_>,
    define_top_level: bool,
) -> Result<bool> {
    let mut expander = Expander::new(map, &NO_EXCLUSIONS, globals).with_kernel(kernel);
    expander.define_top_level_procedural = define_top_level;
    expander.expand(expr)?;
    Ok(expander.changed)
}

// Expands a required module's syntax-rules macros, whose templates refer to names in that
// module. Returns whether anything expanded.
pub(crate) fn expand_foreign(
    expr: &mut ExprKind,
    map: &FxHashMap<InternedString, SteelMacro>,
    globals: GlobalMap,
) -> Result<bool> {
    let mut expander = Expander::new(map, &NO_EXCLUSIONS, globals).foreign();
    expander.expand(expr)?;
    Ok(expander.changed)
}

pub fn expand_with_source_id(
    expr: &mut ExprKind,
    map: &FxHashMap<InternedString, SteelMacro>,
    exclusions: &HashSet<InternedString>,
    source_id: Option<SourceId>,
    globals: GlobalMap,
) -> Result<bool> {
    let mut expander = Expander {
        depth: 0,
        map: MacroMap::Shared(map),
        overlays: Vec::new(),
        changed: false,
        in_scope_values: ScopeSet::default(),
        source_id,
        exclusions,
        globals,
        local_macros: LocalMacros::default(),
        top_level: true,
        body_form: false,
        foreign: false,
        kernel: None,
        define_top_level_procedural: false,
        defined_procedural: Vec::new(),
        head_only: false,
        head_expanded: false,
        head_covered: false,
        prelude_only: false,
        head_body: true,
        scope_chain: ScopeChain::default(),
        prelude_base: 0,
    };

    expander.visit(expr)?;

    Ok(expander.changed)
}

pub(crate) fn is_lambda_head(head: &ExprKind) -> bool {
    match head {
        ExprKind::Atom(Atom {
            syn:
                SyntaxObject {
                    ty: TokenType::Lambda,
                    ..
                },
        }) => true,
        ExprKind::Atom(Atom {
            syn:
                SyntaxObject {
                    ty: TokenType::Identifier(i),
                    ..
                },
        }) => *i == *LAMBDA || *i == *LAMBDA_SYMBOL || *i == *LAMBDA_FN,
        _ => false,
    }
}

pub(crate) fn is_let_head(head: &ExprKind) -> bool {
    match head {
        ExprKind::Atom(Atom {
            syn: SyntaxObject {
                ty: TokenType::Let, ..
            },
        }) => true,
        ExprKind::Atom(Atom {
            syn:
                SyntaxObject {
                    ty: TokenType::Identifier(i),
                    ..
                },
        }) => *i == *LET,
        _ => false,
    }
}

// The parser gives `define` its own token, while code built from data has it as an identifier
pub(crate) fn is_define(l: &List) -> bool {
    l.first()
        .and_then(ExprKind::atom_syntax_object)
        .is_some_and(|head| head.ty == TokenType::Define)
        || l.first_ident() == Some(&*DEFINE)
}

pub(crate) fn is_begin(l: &List) -> bool {
    l.is_begin() || l.first_ident().is_some_and(|x| *x == *BEGIN)
}

// A procedural macro replaces a syntax-rules one of its name for the forms after it
pub(crate) fn forget_replaced_macro(map: &mut FxHashMap<InternedString, SteelMacro>, l: &List) {
    if !l.is_define_syntax() {
        return;
    }

    let Some(name) = procedural_macro_name(l) else {
        return;
    };

    if let Some(previous) = map.remove(&name) {
        if previous.is_prelude() {
            PRELUDE_ALIASES.store(true, Ordering::Relaxed);
            map.entry(prelude_macro_alias(&name)).or_insert(previous);
        }
    }
}

fn procedural_name_span(l: &List) -> Option<steel_parser::span::Span> {
    match l.get(1)? {
        ExprKind::List(header) => header.first()?.atom_syntax_object().map(|x| x.span),
        name => name.atom_syntax_object().map(|x| x.span),
    }
}

pub(crate) fn procedural_macro_name(l: &List) -> Option<InternedString> {
    match l.args.get(1)? {
        ExprKind::List(signature) => signature.first_ident().copied(),
        ExprKind::Atom(_)
            if !l
                .get(2)
                .and_then(ExprKind::list)
                .is_some_and(List::is_syntax_rules) =>
        {
            l.args[1].atom_identifier().copied()
        }
        _ => None,
    }
}

// R6RS `(define-syntax name (identifier-syntax template))`: a bare `name` expands to the
// template, and `(name args ...)` to `(template args ...)`. Lowers it to a transformer.
fn lower_identifier_syntax(l: &mut List) {
    let Some(ExprKind::List(value)) = l.args.get_mut(2) else {
        return;
    };
    if value
        .first_ident()
        .is_none_or(|x| IDENTIFIER_SYNTAX.with(|name| x != name))
    {
        return;
    }
    if value.args.len() == 3 {
        lower_identifier_syntax_clauses(l);
        return;
    }
    if value.args.len() != 2 {
        return;
    }

    let template = value.args.pop().unwrap();
    let span = value.location;
    let id = |name: &str| ExprKind::ident_with_span(name, span);
    let list = |args: Vec<ExprKind>| ExprKind::List(List::new(args.into()));
    let (stx, args) = ("#%identifier-syntax-stx", "#%identifier-syntax-args");

    l.args[2] = list(vec![
        id("lambda"),
        list(vec![id(stx)]),
        list(vec![
            id("syntax-case"),
            id(stx),
            list(vec![]),
            list(vec![
                list(vec![id("_"), id(args), id("...")]),
                list(vec![
                    id("syntax"),
                    list(vec![template.clone(), id(args), id("...")]),
                ]),
            ]),
            list(vec![id("_"), list(vec![id("syntax"), template])]),
        ]),
    ]);
}

// `(identifier-syntax [id template] [(set! id2 pattern) template2])` lowers to a set! transformer
fn lower_identifier_syntax_clauses(l: &mut List) {
    let Some(ExprKind::List(value)) = l.args.get_mut(2) else {
        return;
    };
    let (Some(ExprKind::List(plain)), Some(ExprKind::List(set))) =
        (value.args.get(1), value.args.get(2))
    else {
        return;
    };
    let (Some(id), Some(template)) = (plain.args.first(), plain.args.get(1)) else {
        return;
    };
    if plain.args.len() != 2 || !matches!(id, ExprKind::Atom(_)) || set.args.len() != 2 {
        return;
    }
    let (set_pattern, set_template) = (set.args[0].clone(), set.args[1].clone());
    let (id, template) = (id.clone(), template.clone());

    let span = value.location;
    let ident = |name: &str| ExprKind::ident_with_span(name, span);
    let list = |args: Vec<ExprKind>| ExprKind::List(List::new(args.into()));
    let (stx, args) = ("#%identifier-syntax-stx", "#%identifier-syntax-args");

    l.args[2] = list(vec![
        ident("make-set!-transformer"),
        list(vec![
            ident("lambda"),
            list(vec![ident(stx)]),
            list(vec![
                ident("syntax-case"),
                ident(stx),
                list(vec![ident("set!")]),
                list(vec![set_pattern, list(vec![ident("syntax"), set_template])]),
                list(vec![
                    list(vec![id.clone(), ident(args), ident("...")]),
                    list(vec![
                        ident("syntax"),
                        list(vec![template.clone(), ident(args), ident("...")]),
                    ]),
                ]),
                list(vec![
                    id.clone(),
                    list(vec![ident("identifier?"), list(vec![ident("syntax"), id])]),
                    list(vec![ident("syntax"), template]),
                ]),
            ]),
        ]),
    ]);
}

fn defines_local_macro(l: &List) -> bool {
    l.is_define_syntax()
        && l.get(2)
            .and_then(ExprKind::list)
            .is_some_and(List::is_syntax_rules)
}

// Names paired with the introduction scopes of the expansion that wrote them
type TemplateNames = Vec<(InternedString, steel_parser::scopes::ScopeSetId)>;

// At the top level a macro's name is global, so any name a template wrote counts, kernel macro
// templates included. In a body, local macro names are resolved by their scopes.
fn template_macro_names(forms: &[ExprKind], names: &mut TemplateNames, global: bool) {
    for form in forms {
        match form {
            // Procedural ones too, `(define-syntax (name stx) ...)` or `(define-syntax name ...)`
            ExprKind::List(l)
                if defines_local_macro(l)
                    || l.is_define_syntax() && procedural_macro_name(l).is_some() =>
            {
                let name = match l.get(1) {
                    Some(ExprKind::List(header)) => header.first(),
                    name => name,
                };
                if let Some(syn) = name.and_then(ExprKind::atom_syntax_object) {
                    if let TokenType::Identifier(name) = syn.ty {
                        // Skip names that came from the macro's input, not its template
                        let written = if global {
                            syn.scopes.from_template()
                        } else {
                            syn.scopes.introduced() && syn.unresolved
                        };
                        if written && !name.resolve().starts_with("##") {
                            names.push((name, syn.scopes.introductions()));
                        }
                    }
                }
            }
            ExprKind::Begin(b) => template_macro_names(&b.exprs, names, global),
            ExprKind::List(l) if is_begin(l) => template_macro_names(&l.args[1..], names, global),
            _ => {}
        }
    }
}

struct RenameTemplateMacros<'a> {
    names: &'a [(InternedString, steel_parser::scopes::ScopeSetId)],
}

impl VisitorMutRefUnit for RenameTemplateMacros<'_> {
    fn visit_atom(&mut self, a: &mut Atom) {
        if let TokenType::Identifier(name) = &mut a.syn.ty {
            if !self.names.iter().any(|x| x.0 == *name) {
                return;
            }
            let introductions = a.syn.scopes.introductions();
            if self.names.contains(&(*name, introductions.clone())) {
                let written = name.resolve();
                *name = format!(
                    "##{}#{}",
                    written.trim_start_matches("##"),
                    introductions.serial()
                )
                .into();
            }
        }
    }

    fn visit_quote(&mut self, _quote: &mut Quote) {}
}

// Renames the variables a template defines in the begin-for-syntax forms it writes, so each
// expansion gets its own
pub(crate) fn rename_template_definitions(forms: &mut [ExprKind]) {
    fn collect(form: &ExprKind, names: &mut TemplateNames) {
        let ExprKind::List(l) = form else {
            if let ExprKind::Define(d) = form {
                collect_name(&d.name, names);
            }
            return;
        };

        if l.is_define_syntax() {
            return;
        }

        let head = l
            .first()
            .and_then(ExprKind::atom_syntax_object)
            .map(|x| &x.ty);

        match head {
            Some(TokenType::Begin) => l.args.iter().skip(1).for_each(|x| collect(x, names)),
            Some(TokenType::Identifier(name))
                if matches!(name.resolve(), "begin" | "begin-for-syntax") =>
            {
                l.args.iter().skip(1).for_each(|x| collect(x, names))
            }
            Some(TokenType::Define) => match l.get(1) {
                Some(ExprKind::List(f)) => {
                    if let Some(name) = f.first() {
                        collect_name(name, names)
                    }
                }
                Some(name) => collect_name(name, names),
                None => {}
            },
            _ => {}
        }
    }

    fn collect_name(name: &ExprKind, names: &mut TemplateNames) {
        if let Some(syn) = name.atom_syntax_object() {
            if let TokenType::Identifier(name) = syn.ty {
                // A name the template wrote, not one the use passed in
                if syn.scopes.from_template() {
                    names.push((name, syn.scopes.introductions()));
                }
            }
        }
    }

    let mut names = Vec::new();
    forms.iter().for_each(|x| collect(x, &mut names));

    if names.is_empty() {
        return;
    }

    let mut rename = RenameTemplateMacros { names: &names };

    for form in forms {
        rename.visit(form);
    }
}

pub(crate) fn template_macro_written_name(name: &str) -> Option<&str> {
    let (written, mark) = name.strip_prefix("##")?.rsplit_once('#')?;
    (!written.is_empty() && !mark.is_empty() && mark.bytes().all(|x| x.is_ascii_digit()))
        .then_some(written)
}

pub(crate) fn rename_template_macros(forms: &mut [ExprKind], global: bool) {
    let mut names = Vec::new();
    template_macro_names(forms, &mut names, global);

    if names.is_empty() {
        return;
    }

    let mut rename = RenameTemplateMacros { names: &names };

    for form in forms {
        rename.visit(form);
    }
}

trait BodyForms {
    fn forms(&mut self) -> &mut [ExprKind];
    fn remove_form(&mut self, index: usize) -> ExprKind;
}

impl BodyForms for Vec<ExprKind> {
    fn forms(&mut self) -> &mut [ExprKind] {
        self
    }

    fn remove_form(&mut self, index: usize) -> ExprKind {
        self.remove(index)
    }
}

impl BodyForms for ThinVec<ExprKind> {
    fn forms(&mut self) -> &mut [ExprKind] {
        self
    }

    fn remove_form(&mut self, index: usize) -> ExprKind {
        self.remove(index)
    }
}

struct LocalMacro {
    name: InternedString,
    // None for a procedural macro, which the kernel expands
    expander: Option<SteelMacro>,
    // How many scopes were open where the macro was defined
    depth: usize,
    // Span of its name, which tells two definitions of the same name apart
    name_span: Option<steel_parser::span::Span>,
    // The scopes of its name at the definition
    scopes: steel_parser::scopes::ScopeSetId,
}

thread_local! {
    // How many compiles are running on this thread. They nest when transformer code evaluates
    // code. What the outermost compile recorded can be dropped once it finishes.
    static COMPILES: core::cell::Cell<usize> = const { core::cell::Cell::new(0) };

    // Introduction scopes of expansions of required modules' macros. Names those expansions
    // wrote resolve where the macros are defined.
    static FOREIGN_EXPANSIONS: core::cell::RefCell<FxHashSet<steel_parser::scopes::Scope>> =
        core::cell::RefCell::new(FxHashSet::default());

    // Procedural macros the code being expanded can use. Their uses get their arguments as
    // written
    static PROCEDURAL_HEADS: core::cell::RefCell<FxHashSet<InternedString>> =
        core::cell::RefCell::new(FxHashSet::default());
}

// A procedural macro definition, or a begin with one
fn procedural_definition(expr: &ExprKind) -> bool {
    match expr {
        ExprKind::List(l) if is_begin(l) => l.args[1..].iter().any(procedural_definition),
        ExprKind::Begin(b) => b.exprs.iter().any(procedural_definition),
        ExprKind::List(l) => l.is_define_syntax() && procedural_macro_name(l).is_some(),
        expr => kernel_local_macro_entry(expr).is_some(),
    }
}

// The kernel's name for a macro defined inside a function
fn fresh_local_macro_name(name: InternedString) -> InternedString {
    static COUNT: core::sync::atomic::AtomicUsize = core::sync::atomic::AtomicUsize::new(0);
    let n = COUNT.fetch_add(1, core::sync::atomic::Ordering::Relaxed);
    InternedString::from(format!("{name}%local{n}"))
}

pub(crate) struct ProceduralHeads(FxHashSet<InternedString>);

impl ProceduralHeads {
    // Uses `names` plus the procedural macros `forms` define, restoring the old set on drop
    pub(crate) fn set(mut names: FxHashSet<InternedString>, forms: &[ExprKind]) -> Self {
        for form in forms {
            defined_procedural_macro_names(form, &mut names);
        }

        ProceduralHeads(PROCEDURAL_HEADS.with(|x| x.replace(names)))
    }

    // Adds the macros a form defined, once it's expanded
    pub(crate) fn add(&self, form: &ExprKind) {
        PROCEDURAL_HEADS.with(|x| defined_procedural_macro_names(form, &mut x.borrow_mut()));
    }
}

impl Drop for ProceduralHeads {
    fn drop(&mut self) {
        let previous = core::mem::take(&mut self.0);
        PROCEDURAL_HEADS.with(|x| *x.borrow_mut() = previous);
    }
}

// Whether a name is one of the procedural macros the module can use
pub(crate) fn is_procedural_head(name: &InternedString) -> bool {
    PROCEDURAL_HEADS.with(|x| x.borrow().contains(name))
}

// Scopes of the binding forms around the expander's position, innermost last. When the
// expander reaches an identifier it adds the scopes of the forms entered after the identifier
// was made: all of them for written code, and for macro output only the binding forms that
// expansion introduced.
#[derive(Default)]
pub(crate) struct ScopeChain {
    scopes: Vec<steel_parser::scopes::Scope>,
    // A path id for each prefix of the chain, so a slice can be keyed by the paths at its ends
    paths: Vec<u32>,
    // Variables bound by the surrounding forms, with their binders' scopes, and the names each
    // level of the chain bound so they can be removed when it's left
    binders: FxHashMap<InternedString, Vec<steel_parser::scopes::ScopeSetId>>,
    bound: Vec<Vec<InternedString>>,

    // Top-level variables, resolved by name like globals
    top_level: FxHashSet<InternedString>,
    // Those templates defined, by the introduction scopes of the expansions that wrote them
    template_top_level: FxHashMap<InternedString, Vec<steel_parser::scopes::ScopeSetId>>,
}

thread_local! {
    // Each binding form's scope, keyed by its keyword's syntax object
    static FORM_SCOPES: core::cell::RefCell<
        FxHashMap<(usize, u8), steel_parser::scopes::Scope>,
    > = core::cell::RefCell::new(FxHashMap::default());
}

thread_local! {
    // Use-site scope of the last expansion, for the body that form is in
    static LAST_USE_SITE: core::cell::Cell<Option<steel_parser::scopes::Scope>> =
        const { core::cell::Cell::new(None) };
}

thread_local! {
    // For each body, the use-site scopes of the macro uses among its forms. The body's
    // definitions drop these from their names. Keyed by the body's scope, since a body keeps
    // the same scope in every pass that expands it.
    static BODY_USE_SITES: core::cell::RefCell<
        FxHashMap<steel_parser::scopes::Scope, Vec<steel_parser::scopes::Scope>>,
    > = core::cell::RefCell::new(FxHashMap::default());
}

thread_local! {
    // Variable binders visible at each procedural macro call in progress, innermost last
    static BINDER_TABLES: core::cell::RefCell<
        Vec<FxHashMap<InternedString, Vec<steel_parser::scopes::ScopeSetId>>>,
    > = const { core::cell::RefCell::new(Vec::new()) };
}

// The local variable (as its binder's scopes) that an identifier passed to the current
// procedural macro refers to, found the same way `meaning` does. None for a global or free name.
pub(crate) fn local_binding(
    name: InternedString,
    scopes: &steel_parser::scopes::ScopeSetId,
) -> Option<steel_parser::scopes::ScopeSetId> {
    BINDER_TABLES.with(|x| {
        x.borrow()
            .last()?
            .get(&name)?
            .iter()
            .filter(|x| x.is_subset(scopes))
            .max_by_key(|x| x.len())
            .cloned()
    })
}

// What each module's top-level names refer to: its own definitions, and for each import the
// module it comes from and its name there. Kept by module name.
#[derive(Default)]
pub(crate) struct ModuleBindings {
    pub(crate) own: FxHashSet<InternedString>,
    pub(crate) imports: FxHashMap<InternedString, (InternedString, InternedString)>,
}

thread_local! {
    static MODULE_BINDINGS: core::cell::RefCell<FxHashMap<InternedString, ModuleBindings>> =
        core::cell::RefCell::new(FxHashMap::default());
    // The module being compiled, and the module of the macro whose patterns are being matched
    static CURRENT_MODULE: core::cell::Cell<Option<InternedString>> =
        const { core::cell::Cell::new(None) };
    static LITERAL_MODULE: core::cell::Cell<Option<InternedString>> =
        const { core::cell::Cell::new(None) };
}

// Added to what's known, since the top level is compiled a piece at a time
pub(crate) fn record_module_bindings(module: InternedString, bindings: ModuleBindings) {
    MODULE_BINDINGS.with(|x| {
        let mut modules = x.borrow_mut();
        let entry = modules.entry(module).or_default();
        entry.own.extend(bindings.own);
        entry.imports.extend(bindings.imports);
    })
}

thread_local! {
    // The scope chain where the syntax-rules macro being expanded is used
    static USE_CHAIN: core::cell::Cell<*const ScopeChain> =
        const { core::cell::Cell::new(core::ptr::null()) };
}

// Makes the scope chain at a macro use visible to its matching until it's dropped
pub(crate) struct UseBinders(*const ScopeChain);

impl UseBinders {
    fn set(chain: &ScopeChain) -> Self {
        UseBinders(USE_CHAIN.with(|x| x.replace(chain)))
    }
}

impl Drop for UseBinders {
    fn drop(&mut self) {
        USE_CHAIN.with(|x| x.set(self.0));
    }
}

thread_local! {
    // The scopes of a local macro's definition, which its literals have
    static LITERAL_SCOPES: core::cell::RefCell<Option<steel_parser::scopes::ScopeSetId>> =
        const { core::cell::RefCell::new(None) };
}

// The local an identifier with these scopes refers to where the macro is used
fn use_site_binder(
    name: &InternedString,
    scopes: &steel_parser::scopes::ScopeSetId,
) -> Option<steel_parser::scopes::ScopeSetId> {
    let chain = USE_CHAIN.with(|x| x.get());
    if chain.is_null() {
        return None;
    }
    // Set from a chain that lives, unchanged, for the whole expansion
    let chain = unsafe { &*chain };
    crate::compiler::passes::hygiene::largest_subset(
        chain
            .binders
            .get(name)
            .into_iter()
            .flatten()
            .map(|x| (x, x.clone())),
        scopes,
    )
}

// Whether a local macro's literal and an identifier of its use refer to the same local
pub(crate) fn literal_local_matches(literal: &InternedString, syn: &SyntaxObject) -> bool {
    let Some(literal_scopes) = LITERAL_SCOPES.with(|x| x.borrow().clone()) else {
        return false;
    };
    let chain = USE_CHAIN.with(|x| x.get());
    if chain.is_null() {
        return false;
    }
    let Some(name) = syn.ty.identifier() else {
        return false;
    };
    // Set from a chain that lives, unchanged, for the whole expansion
    let scopes = unsafe { &*chain }.stamped(syn);
    match (
        use_site_binder(name, &scopes),
        use_site_binder(literal, &literal_scopes),
    ) {
        (Some(x), Some(y)) => name == literal && x == y,
        _ => false,
    }
}

// Whether a local binds the identifier where the macro is used, or None outside an expansion
pub(crate) fn use_site_local(syn: &SyntaxObject) -> Option<bool> {
    let chain = USE_CHAIN.with(|x| x.get());
    if chain.is_null() {
        return None;
    }
    // Set from a chain that lives, unchanged, for the whole expansion
    let chain = unsafe { &*chain };
    let name = syn.ty.identifier()?;
    // The input isn't stamped yet
    let scopes = chain.stamped(syn);
    Some(
        chain
            .binders
            .get(name)
            .is_some_and(|x| x.iter().any(|x| x.is_subset(&scopes))),
    )
}

pub(crate) fn current_module() -> Option<InternedString> {
    CURRENT_MODULE.with(|x| x.get())
}

// Sets the module being compiled until it's dropped
pub(crate) struct CurrentModule(Option<InternedString>);

impl CurrentModule {
    pub(crate) fn set(module: InternedString) -> Self {
        CurrentModule(CURRENT_MODULE.with(|x| x.replace(Some(module))))
    }
}

impl Drop for CurrentModule {
    fn drop(&mut self) {
        CURRENT_MODULE.with(|x| x.set(self.0));
    }
}

pub(crate) fn literal_module() -> Option<InternedString> {
    LITERAL_MODULE.with(|x| x.get())
}

// Sets the module a macro's literals refer to names in until it's dropped
pub(crate) struct LiteralModule(Option<InternedString>);

impl LiteralModule {
    pub(crate) fn set(module: Option<InternedString>) -> Self {
        LiteralModule(LITERAL_MODULE.with(|x| x.replace(module)))
    }
}

impl Drop for LiteralModule {
    fn drop(&mut self) {
        LITERAL_MODULE.with(|x| x.set(self.0));
    }
}

// The global binding a name has in a module, following imports to where it's defined: the
// defining module and the name there, or no module when nothing defines it. Unknown modules
// give `None`.
pub(crate) fn global_binding(
    module: InternedString,
    name: InternedString,
) -> Option<(Option<InternedString>, InternedString)> {
    MODULE_BINDINGS.with(|x| {
        let bindings = x.borrow();
        let (mut module, mut name) = (module, name);
        bindings.get(&module)?;
        // Imports can chain through re-exports
        for _ in 0..32 {
            let Some(entry) = bindings.get(&module) else {
                return Some((Some(module), name));
            };
            if entry.own.contains(&name) {
                return Some((Some(module), name));
            }
            match entry.imports.get(&name) {
                Some((from, original)) => {
                    module = *from;
                    name = *original;
                }
                None => return Some((None, name)),
            }
        }
        Some((None, name))
    })
}

// Whether two names, each in its module, have the same global binding. None when either
// module isn't known.
pub(crate) fn same_global_binding(
    a: (InternedString, InternedString),
    b: (InternedString, InternedString),
) -> Option<bool> {
    Some(global_binding(a.0, a.1)? == global_binding(b.0, b.1)?)
}

// Every name a procedural macro was registered under, in any environment. A pass without the
// kernel leaves forms headed by these to the kernel's pass.
static REGISTERED_PROCEDURAL: once_cell::sync::Lazy<std::sync::RwLock<FxHashSet<InternedString>>> =
    once_cell::sync::Lazy::new(Default::default);

pub(crate) fn registered_procedural(name: InternedString) {
    REGISTERED_PROCEDURAL.write().unwrap().insert(name);
}

pub(crate) fn made_use_site(scope: Option<steel_parser::scopes::Scope>) {
    LAST_USE_SITE.with(|x| x.set(scope));
}

// What an identifier refers to, resolved by its scopes as in Racket
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum Meaning {
    Variable,
    // Index into `LocalMacros`
    LocalMacro(usize),
    // A function's procedural macro the kernel defined, by its kernel name
    KernelMacro(InternedString),
    // Nothing local: a global, a global macro, or free
    Unbound,
}

impl ScopeChain {
    // A form gets the same scope each time the expander enters it, which can happen over
    // several rounds. The form is keyed by its keyword's syntax object, which a copy of the
    // form doesn't share. `which` is 0 for the form's scope, 1 for its body's.
    fn enter(&mut self, form: usize, which: u8) {
        // Don't reuse a scope older than the enclosing one (a macro may have moved the form
        // into a newer scope), so the chain stays in creation order
        let inner = self.scopes.last().copied();
        let scope = FORM_SCOPES.with(|x| {
            let mut forms = x.borrow_mut();
            match forms.get(&(form, which)) {
                Some(scope) if inner.is_none_or(|inner| inner.time() < scope.time()) => *scope,
                _ => {
                    let scope = steel_parser::scopes::Scope::fresh();
                    forms.insert((form, which), scope);
                    scope
                }
            }
        });
        self.scopes.push(scope);
        self.bound.push(Vec::new());
        let parent = self.paths.last().copied().unwrap_or(0);
        let path = PATHS.with(|x| {
            let mut paths = x.borrow_mut();
            let next = paths.len() as u32 + 1;
            *paths.entry((parent, scope)).or_insert(next)
        });
        self.paths.push(path);
    }

    fn leave(&mut self) {
        self.scopes.pop();
        self.paths.pop();
        for name in self.bound.pop().into_iter().flatten() {
            if let Some(binders) = self.binders.get_mut(&name) {
                binders.pop();
                if binders.is_empty() {
                    self.binders.remove(&name);
                }
            }
        }
    }

    // The scope `enter` will give a form
    fn scope_for(&self, form: usize, which: u8) -> steel_parser::scopes::Scope {
        FORM_SCOPES.with(|x| {
            *x.borrow_mut()
                .entry((form, which))
                .or_insert_with(steel_parser::scopes::Scope::fresh)
        })
    }

    fn stamp(&mut self, syn: &mut SyntaxObject) {
        syn.scopes = self.stamped(syn);
    }

    // The scopes an identifier has at the expander's current position
    fn stamped(&self, syn: &SyntaxObject) -> steel_parser::scopes::ScopeSetId {
        if syn.ty.identifier().is_none() {
            return syn.scopes.clone();
        }
        self.stamped_any(syn)
    }

    // Like `stamped`, but for literals too, which `datum->syntax` can use as context
    fn stamped_any(&self, syn: &SyntaxObject) -> steel_parser::scopes::ScopeSetId {
        let birth = syn.scopes.birth();
        let from = self.scopes.partition_point(|x| x.time() <= birth);
        let Some(top) = self
            .scopes
            .last()
            .copied()
            .filter(|_| from < self.scopes.len())
        else {
            return syn.scopes.clone();
        };
        let _ = top;
        // Cache the slice of the chain, keyed by the paths at its two ends
        let scopes = &self.scopes;
        let key = (
            if from == 0 { 0 } else { self.paths[from - 1] },
            *self.paths.last().unwrap(),
        );
        let suffix = SUFFIXES.with(|x| {
            x.borrow_mut()
                .entry(key)
                .or_insert_with(|| steel_parser::scopes::ScopeSetId::of_sorted(&scopes[from..]))
                .clone()
        });
        syn.scopes.union(&suffix)
    }

    // Stamps a variable's binder and records its scopes so references can find it
    fn bind(&mut self, syn: &mut SyntaxObject) {
        self.stamp(syn);
        self.record(syn);
    }

    fn bind_expr(&mut self, expr: &mut ExprKind) {
        if let ExprKind::Atom(a) = expr {
            self.bind(&mut a.syn);
        }
    }

    // Records a define's name, as a body variable or a top-level one
    fn define(&mut self, syn: &mut SyntaxObject) {
        if self.scopes.is_empty() {
            // A global a template defines is the template's, which the hygiene pass names
            if let Some(name) = syn.ty.identifier() {
                if syn.scopes.introduced() {
                    self.template_top_level
                        .entry(*name)
                        .or_default()
                        .push(syn.scopes.introductions());
                } else {
                    self.top_level.insert(*name);
                }
            }
        } else {
            // Drop the use-site scopes of the body's macro uses that produced it, so code
            // written elsewhere in the body can refer to it
            self.stamp(syn);
            syn.scopes = self.without_body_use_sites(core::mem::take(&mut syn.scopes));
            self.record(syn);
        }
    }

    // Whether an identifier with these scopes refers to a top-level variable
    fn is_top_level(
        &self,
        name: &InternedString,
        scopes: &steel_parser::scopes::ScopeSetId,
    ) -> bool {
        (self.top_level.contains(name) && !scopes.introduced())
            || self
                .template_top_level
                .get(name)
                .is_some_and(|x| x.iter().any(|x| x.is_subset(scopes)))
    }

    fn define_expr(&mut self, expr: &mut ExprKind) {
        if let ExprKind::Atom(a) = expr {
            self.define(&mut a.syn);
        }
    }

    fn without_body_use_sites(
        &self,
        scopes: steel_parser::scopes::ScopeSetId,
    ) -> steel_parser::scopes::ScopeSetId {
        match self.scopes.last() {
            Some(body) => BODY_USE_SITES.with(|x| match x.borrow().get(body) {
                Some(uses) => scopes.without(uses),
                None => scopes,
            }),
            None => scopes,
        }
    }

    // Records that the last expansion was one of this body's forms
    fn body_use(&mut self) {
        if let (Some(scope), Some(body)) = (LAST_USE_SITE.with(|x| x.take()), self.scopes.last()) {
            BODY_USE_SITES.with(|x| x.borrow_mut().entry(*body).or_default().push(scope));
        }
    }

    // Records a body's definition before the body's forms are expanded
    fn record_definition(&mut self, syn: &SyntaxObject) {
        let mut syn = syn.clone();
        syn.scopes = self.without_body_use_sites(self.stamped(&syn));
        self.record(&syn);
    }

    // Records a body's variable before the body's forms are expanded
    fn record(&mut self, syn: &SyntaxObject) {
        if let Some(name) = syn.ty.identifier() {
            let scopes = self.stamped(syn);
            let Some(bound) = self.bound.last_mut() else {
                return;
            };
            // Only once per level, since a form entered again in a later round binds it again
            let binders = self.binders.entry(*name).or_default();
            if binders.last() == Some(&scopes) && bound.contains(name) {
                return;
            }
            binders.push(scopes);
            bound.push(*name);
        }
    }

    fn stamp_expr(&mut self, expr: &mut ExprKind) {
        if let ExprKind::Atom(a) = expr {
            self.stamp(&mut a.syn);
        }
    }

    // Stamps every identifier in a form, for a local macro definition whose templates keep
    // their meaning from where it's defined
    fn stamp_all(&mut self, expr: &mut ExprKind) {
        struct StampAll<'a>(&'a mut ScopeChain);
        impl VisitorMutRefUnit for StampAll<'_> {
            fn visit_vector(&mut self, v: &mut crate::parser::ast::Vector) {
                for expr in v.args.iter_mut() {
                    self.visit(expr);
                }
            }

            fn visit_atom(&mut self, a: &mut Atom) {
                self.0.stamp(&mut a.syn);
            }
        }
        StampAll(self).visit(expr);
    }

    // Stamps the local macros a body defines, before they're parsed
    fn stamp_local_macro_definitions<'b>(
        &mut self,
        forms: impl IntoIterator<Item = &'b mut ExprKind>,
    ) {
        let forms: Vec<&mut ExprKind> = forms.into_iter().collect();
        // let-syntax's macros are only visible in its body, the `(let () body ...)` after them,
        // so their names get that let's scopes and their own transformers don't see them
        let inner = forms
            .first()
            .is_some_and(|x| is_let_syntax_marker(x))
            .then(|| forms.last())
            .flatten()
            .and_then(|x| x.list())
            .filter(|x| x.first().is_some_and(is_let_head))
            .map(form_key)
            .map(|key| (self.scope_for(key, 0), self.scope_for(key, 1)));
        for form in forms {
            if let (Some((scope, body_scope)), ExprKind::List(l)) = (inner, &mut *form) {
                if l.is_define_syntax() {
                    self.stamp_all(form);
                    let ExprKind::List(l) = form else {
                        unreachable!()
                    };
                    let name = match l.args.get_mut(1) {
                        Some(ExprKind::List(signature)) => signature.args.first_mut(),
                        other => other,
                    };
                    if let Some(ExprKind::Atom(a)) = name {
                        a.syn.scopes = a.syn.scopes.add(scope).add(body_scope);
                    }
                    continue;
                }
            }
            match form {
                ExprKind::List(l) if defines_local_macro(l) => self.stamp_all(form),
                // Just the name of a procedural one
                ExprKind::List(l) if l.is_define_syntax() => match l.args.get_mut(1) {
                    Some(ExprKind::List(signature)) => {
                        if let Some(name) = signature.args.first_mut() {
                            self.stamp_expr(name);
                        }
                    }
                    Some(name) => self.stamp_expr(name),
                    None => {}
                },
                ExprKind::List(l) if is_begin(l) => {
                    self.stamp_local_macro_definitions(l.args.iter_mut().skip(1))
                }
                ExprKind::Begin(b) => self.stamp_local_macro_definitions(b.exprs.iter_mut()),
                _ => {}
            }
        }
    }
}

// Key for the scopes the expander gives a list form: its keyword's syntax object id
fn form_key(l: &List) -> usize {
    l.first()
        .and_then(ExprKind::atom_syntax_object)
        .map_or(l.syntax_object_id as usize, |x| {
            x.syntax_object_id.0 as usize
        })
}

// How a procedural macro is used: as a form's head, as a bare identifier, or as the target of
// a `set!`, which only a set! transformer accepts
#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum UseKind {
    Form,
    Identifier,
    Set,
}

// The input a procedural macro gets: the whole form, or the bare identifier
pub(crate) fn use_input(l: &mut List, identifier: bool) -> ExprKind {
    let l = core::mem::replace(l, List::new(ThinVec::new()));
    match l.args.len() {
        1 if identifier => l.args.into_iter().next().unwrap(),
        _ => ExprKind::List(l),
    }
}

thread_local! {
    static BINDING_FORMS: FxHashSet<InternedString> = [
        "provide",
        "require",
        "for-syntax",
        "only-in",
        "except-in",
        "prefix-in",
        "rename-in",
        "struct-out",
        "all-from-out",
        "#%require-dylib",
    ]
    .into_iter()
    .map(InternedString::from)
    .collect();
    static IDENTIFIER_SYNTAX: InternedString = "identifier-syntax".into();
}

// Forms whose identifiers name what they provide or require, rather than being uses
fn names_bindings(l: &List) -> bool {
    l.first_ident()
        .is_some_and(|head| BINDING_FORMS.with(|x| x.contains(head)))
}

fn procedural_use(l: &List, in_scope: &ScopeSet<InternedString, FxBuildHasher>) -> bool {
    l.first()
        .is_some_and(|head| names_procedural(head, in_scope))
}

fn names_procedural(expr: &ExprKind, in_scope: &ScopeSet<InternedString, FxBuildHasher>) -> bool {
    let ExprKind::Atom(Atom { syn }) = expr else {
        return false;
    };

    let TokenType::Identifier(s) = &syn.ty else {
        return false;
    };

    (syn.unresolved || !in_scope.contains(s))
        && (PROCEDURAL_HEADS.with(|x| x.borrow().contains(s)) || {
            // Also check its name in the module that defines it
            let registered = REGISTERED_PROCEDURAL.read().unwrap();
            registered.contains(s)
                || s.resolve()
                    .rsplit_once("__%#__")
                    .is_some_and(|(_, name)| registered.contains(&InternedString::from(name)))
        })
}

// Records an expansion of a required module's macro by its introduction scope
pub(crate) fn record_foreign(introduction: steel_parser::scopes::Scope) {
    FOREIGN_EXPANSIONS.with(|x| x.borrow_mut().insert(introduction));
}

// Whether a required module's macro wrote an identifier with these scopes
pub(crate) fn is_foreign(scopes: &steel_parser::scopes::ScopeSetId) -> bool {
    FOREIGN_EXPANSIONS.with(|x| {
        let foreign = x.borrow();
        !foreign.is_empty()
            && scopes.introduced()
            && scopes
                .scopes()
                .iter()
                .any(|x| x.is_introduction() && foreign.contains(x))
    })
}

thread_local! {
    static PATHS: core::cell::RefCell<FxHashMap<(u32, steel_parser::scopes::Scope), u32>> =
        core::cell::RefCell::new(FxHashMap::default());
}
thread_local! {
    static SUFFIXES: core::cell::RefCell<
        FxHashMap<(u32, u32), steel_parser::scopes::ScopeSetId>,
    > = core::cell::RefCell::new(FxHashMap::default());
}

pub(crate) struct CompileScope;

impl CompileScope {
    pub(crate) fn enter() -> Self {
        COMPILES.with(|x| x.set(x.get() + 1));
        CompileScope
    }
}

impl Drop for CompileScope {
    fn drop(&mut self) {
        let outermost = COMPILES.with(|x| {
            x.set(x.get() - 1);
            x.get() == 0
        });

        // Clear the per-compile state its passes share. Foreign expansions stay, since what
        // they wrote can be expanded in a later compile
        if outermost {
            FORM_SCOPES.with(|x| x.borrow_mut().clear());
            PATHS.with(|x| x.borrow_mut().clear());
            SUFFIXES.with(|x| x.borrow_mut().clear());
            BODY_USE_SITES.with(|x| x.borrow_mut().clear());
            steel_parser::scopes::trim_caches();
        }
    }
}

static MACRO_REDEFINITION: &str = "#%macro-redefinition:";

// Marks where code redefines a macro. Forms after it use the new definition, forms before it
// the earlier one.
fn macro_redefinition_marker(index: usize) -> ExprKind {
    ExprKind::Quote(Box::new(Quote::new(
        ExprKind::ident(&format!("{MACRO_REDEFINITION}{index}")),
        SyntaxObject::default(TokenType::Quote),
    )))
}

fn macro_redefinition(expr: &ExprKind) -> Option<usize> {
    let ExprKind::Quote(q) = expr else {
        return None;
    };

    q.expr
        .atom_identifier()?
        .resolve()
        .strip_prefix(MACRO_REDEFINITION)?
        .parse()
        .ok()
}

// Macros the code defines more than once, held until their markers are reached
#[derive(Default)]
pub(crate) struct MacroRedefinitions {
    defined: FxHashSet<InternedString>,
    macros: Vec<Option<SteelMacro>>,
}

impl MacroRedefinitions {
    // Defines a macro, or if it was already defined, holds it and returns a marker
    pub(crate) fn define(
        &mut self,
        map: &mut FxHashMap<InternedString, SteelMacro>,
        name: InternedString,
        m: SteelMacro,
        replaces: bool,
    ) -> Option<ExprKind> {
        if self.defined.insert(name) && !replaces {
            define_macro(map, name, m);
            return None;
        }

        self.macros.push(Some(m));
        Some(macro_redefinition_marker(self.macros.len() - 1))
    }

    // Defines a marker's macro and replaces the marker with an empty begin
    pub(crate) fn reached(
        &mut self,
        map: &mut FxHashMap<InternedString, SteelMacro>,
        expr: &mut ExprKind,
    ) -> bool {
        let Some(index) = macro_redefinition(expr) else {
            return false;
        };

        if let Some(m) = self.macros.get_mut(index).and_then(Option::take) {
            let name = *m.name();
            define_macro(map, name, m);
        }

        *expr = ExprKind::Begin(Box::new(Begin::new(
            Vec::new(),
            SyntaxObject::default(TokenType::Begin),
        )));

        true
    }

    // Defines whatever is still held once the code is done
    pub(crate) fn finish(&mut self, map: &mut FxHashMap<InternedString, SteelMacro>) {
        for m in self.macros.iter_mut().filter_map(Option::take) {
            let name = *m.name();
            define_macro(map, name, m);
        }
    }
}

// The name a prelude macro is kept under when a definition replaces it
pub(crate) fn prelude_macro_alias(name: &InternedString) -> InternedString {
    thread_local! {
        static ALIASES: core::cell::RefCell<FxHashMap<InternedString, InternedString>> =
            core::cell::RefCell::new(FxHashMap::default());
    }
    ALIASES.with(|aliases| {
        *aliases
            .borrow_mut()
            .entry(*name)
            .or_insert_with(|| format!("#%prelude-macro.{}", name.resolve()).into())
    })
}

// Removes a macro the code shadows, keeping a prelude macro for the prelude's templates
pub(crate) fn remove_macro(
    map: &mut FxHashMap<InternedString, SteelMacro>,
    name: &InternedString,
) -> Option<SteelMacro> {
    let previous = map.remove(name)?;

    if previous.is_prelude() {
        PRELUDE_ALIASES.store(true, Ordering::Relaxed);
        map.entry(prelude_macro_alias(name))
            .or_insert_with(|| previous.clone());
    }

    Some(previous)
}

// Defines a macro, keeping a prelude macro it replaces for the prelude's templates
pub(crate) fn define_macro(
    map: &mut FxHashMap<InternedString, SteelMacro>,
    name: InternedString,
    m: SteelMacro,
) {
    if let Some(previous) = map.insert(name, m) {
        if previous.is_prelude() {
            PRELUDE_ALIASES.store(true, Ordering::Relaxed);
            map.entry(prelude_macro_alias(&name)).or_insert(previous);
        }
    }
}

// Whether any definition has replaced a prelude macro
static PRELUDE_ALIASES: AtomicBool = AtomicBool::new(false);

pub(crate) fn is_local_macro_definition(expr: &ExprKind) -> bool {
    matches!(expr, ExprKind::List(l) if defines_local_macro(l))
        || kernel_local_macro(expr).is_some()
}

static KERNEL_LOCAL_MACRO: &str = "#%kernel-local-macro";

// Left where a function defines a procedural macro, so the hygiene pass knows which of the
// function's locals the macro can see
fn kernel_local_macro_marker(
    name: InternedString,
    span: steel_parser::span::Span,
    scopes: steel_parser::scopes::ScopeSetId,
) -> ExprKind {
    let mut name = SyntaxObject::new(TokenType::Identifier(name), span);
    name.scopes = scopes;
    ExprKind::Quote(Box::new(Quote::new(
        ExprKind::List(List::new(thin_vec![
            ExprKind::ident_with_span(KERNEL_LOCAL_MACRO, span),
            ExprKind::Atom(Atom::new(name)),
        ])),
        SyntaxObject::new(TokenType::Quote, span),
    )))
}

// Recovers a local macro's written name from the kernel's name for it
fn written_kernel_local_name(internal: InternedString) -> InternedString {
    internal
        .resolve()
        .rsplit_once("%local")
        .map_or(internal, |(written, _)| written.into())
}

// A marker's macro: its kernel name, the span of its definition, and its name's scopes
fn kernel_local_macro_entry(
    expr: &ExprKind,
) -> Option<(
    InternedString,
    steel_parser::span::Span,
    steel_parser::scopes::ScopeSetId,
)> {
    let ExprKind::Quote(q) = expr else {
        return None;
    };

    match q.expr.list()?.args.as_slice() {
        [head, name] if head.atom_identifier()?.resolve() == KERNEL_LOCAL_MACRO => {
            let syn = name.atom_syntax_object()?;
            match syn.ty {
                TokenType::Identifier(name) => Some((name, syn.span, syn.scopes.clone())),
                _ => None,
            }
        }
        _ => None,
    }
}

pub(crate) fn kernel_local_macro(expr: &ExprKind) -> Option<InternedString> {
    let ExprKind::Quote(q) = expr else {
        return None;
    };

    match q.expr.list()?.args.as_slice() {
        [head, name] if head.atom_identifier()?.resolve() == KERNEL_LOCAL_MACRO => {
            name.atom_identifier().copied()
        }
        _ => None,
    }
}

// The scopes of a procedural macro's definition: those of the first identifier after its name,
// since `define-syntax` itself is a keyword without scopes
pub(crate) fn definition_scopes(l: &List) -> steel_parser::scopes::ScopeSetId {
    struct First(Option<steel_parser::scopes::ScopeSetId>);
    impl crate::compiler::passes::VisitorMutUnitRef<'_> for First {
        fn visit_atom(&mut self, a: &'_ Atom) {
            if self.0.is_none() && a.syn.ty.identifier().is_some() {
                self.0 = Some(a.syn.scopes.clone());
            }
        }
    }
    use crate::compiler::passes::VisitorMutUnitRef;
    let mut first = First(None);
    match l.get(1) {
        Some(ExprKind::List(signature)) => {
            for arg in signature.args.iter().skip(1) {
                first.visit(arg);
            }
        }
        _ => {}
    }
    for form in l.args.iter().skip(2) {
        first.visit(form);
    }
    first.0.unwrap_or_default()
}

// A name as the code wrote it, without the `##` prefix added to a template's binders
fn written_name(name: &str) -> &str {
    name.strip_prefix("##").unwrap_or(name)
}

// The innermost target of a define: its name, or the signature that starts with it, looking
// through a curried define's `((f a) b)`
fn define_target(target: &ExprKind) -> &ExprKind {
    let mut target = target;
    while let ExprKind::List(signature) = target {
        match signature.first() {
            Some(inner @ ExprKind::List(_)) => target = inner,
            _ => break,
        }
    }
    target
}

// Rewrites `(define ((f a) b) body ...)` to `(define (f a) (lambda (b) body ...))`
pub(crate) fn uncurry_define(l: &mut List) {
    while let Some(ExprKind::List(signature)) = l.args.get(1) {
        // Leave one without a body for the parser to report
        if !matches!(signature.first(), Some(ExprKind::List(_))) || l.args.len() < 3 {
            break;
        }

        let ExprKind::List(mut signature) = core::mem::take(&mut l.args[1]) else {
            unreachable!()
        };
        let inner = signature.args.remove(0);
        let span = match define_target(&inner) {
            ExprKind::List(f) => f.first().and_then(ExprKind::atom_syntax_object),
            name => name.atom_syntax_object(),
        }
        .map(|x| x.span)
        .unwrap_or_default();

        let mut lambda = thin_vec![
            ExprKind::Atom(Atom::new(SyntaxObject::new(
                TokenType::Identifier(*LAMBDA),
                span,
            ))),
            ExprKind::List(List::new_maybe_improper(
                core::mem::take(&mut signature.args),
                signature.improper,
            )),
        ];
        lambda.extend(l.args.drain(2..));

        l.args[1] = inner;
        l.args.push(ExprKind::List(List::new(lambda)));
    }
}

impl LocalMacro {
    fn expand(
        &self,
        expr: List,
        span: steel_parser::span::Span,
        scope: &ScopeSet<InternedString, FxBuildHasher>,
        globals: GlobalMap,
    ) -> Result<(ExprKind, steel_parser::scopes::Scope)> {
        let mut bound_since = ScopeSet::default();

        for name in scope.iter() {
            if scope
                .height_of(name)
                .is_some_and(|height| height >= self.depth)
            {
                bound_since.define(*name);
            }
        }

        self.expander
            .as_ref()
            .expect("the kernel expands a procedural macro")
            .expand_marked(expr, span, &bound_since, globals)
    }
}

pub(crate) fn unmangled(name: &str) -> &str {
    use crate::compiler::modules::{MANGLER_PREFIX, MANGLER_SEPARATOR};

    match name.find(MANGLER_SEPARATOR) {
        Some(i) if name.starts_with(MANGLER_PREFIX) => &name[i + MANGLER_SEPARATOR.len()..],
        _ => name,
    }
}

// let-syntax marks the start of the body that defines its macros with `'#%let-syntax`
fn is_let_syntax_marker(form: &ExprKind) -> bool {
    matches!(form, ExprKind::Quote(q) if q.expr.atom_identifier().is_some_and(|x| x.resolve() == "#%let-syntax"))
}

fn ends_with_definition(form: &ExprKind) -> bool {
    match form {
        ExprKind::List(l) if defines_local_macro(l) => true,
        ExprKind::Begin(b) => b.exprs.last().map_or(true, ends_with_definition),
        ExprKind::List(l) if is_begin(l) => l.args[1..].last().map_or(true, ends_with_definition),
        _ => false,
    }
}

// Whether a body form defines a macro, directly or inside a begin
fn defines_body_macro(form: &ExprKind) -> bool {
    match form {
        ExprKind::List(l) if defines_local_macro(l) || l.is_define_syntax() => true,
        ExprKind::Begin(b) => b.exprs.iter().any(defines_body_macro),
        ExprKind::List(l) if is_begin(l) => l.args[1..].iter().any(defines_body_macro),
        _ => false,
    }
}

#[derive(Default)]
struct LocalMacros {
    macros: Vec<LocalMacro>,
}

impl LocalMacros {
    fn define_from<B: BodyForms>(
        &mut self,
        body: &mut B,
        start: usize,
        depth: usize,
    ) -> Result<()> {
        rename_template_macros(body.forms().get_mut(start..).unwrap_or_default(), false);
        self.define_body_macros(body, start, depth)
    }

    fn define_body_macros<B: BodyForms>(
        &mut self,
        body: &mut B,
        start: usize,
        depth: usize,
    ) -> Result<()> {
        for form in body.forms().iter_mut().skip(start) {
            match form {
                ExprKind::List(l) if defines_local_macro(l) => {
                    let span = l
                        .first()
                        .and_then(ExprKind::atom_syntax_object)
                        .map(|x| x.span);

                    let written = l.get(1).and_then(ExprKind::atom_syntax_object);
                    let name_span = written.map(|x| x.span);

                    // Already defined by the body this begin is part of. One written by a
                    // different expansion is a different macro, even from the same template.
                    let introductions = written.map(|x| x.scopes.introductions());
                    let defined = l.get(1).and_then(ExprKind::atom_identifier).and_then(|x| {
                        self.macros.iter().rev().find(|m| {
                            m.name == *x
                                && m.depth == depth
                                && Some(m.scopes.introductions()) == introductions
                        })
                    });

                    if let Some(defined) = defined {
                        if defined.name_span == name_span
                            || defined.expander.as_ref().map(|x| x.span()) == span
                                && defined.name_span.is_none()
                        {
                            continue;
                        }

                        // A body defines a name once
                        stop!(BadSyntax => format!("define-syntax: duplicate binding name: {}", written_name(&defined.name.to_string()));
                            name_span.unwrap_or_default());
                    }

                    if let ExprKind::Macro(m) =
                        lower_macro_and_require_definitions(ExprKind::List(l.clone()))?
                    {
                        let expander = SteelMacro::parse_from_ast_macro(m)?;

                        let scopes = written
                            .map(|x| x.scopes.without_use_sites())
                            .unwrap_or_default();
                        self.macros.push(LocalMacro {
                            name: *expander.name(),
                            expander: Some(expander),
                            depth,
                            name_span,
                            scopes,
                        });
                    }
                }
                ExprKind::List(l) if l.is_define_syntax() => {
                    if let Some(name) = procedural_macro_name(l) {
                        let written = match l.get(1) {
                            Some(ExprKind::List(f)) => f.first(),
                            other => other,
                        }
                        .and_then(ExprKind::atom_syntax_object);
                        let scopes = written
                            .map(|x| x.scopes.without_use_sites())
                            .unwrap_or_default();

                        // Already defined by the body this begin is part of
                        let defined = self
                            .macros
                            .iter()
                            .any(|x| x.name == name && x.depth == depth && x.expander.is_none());

                        if !defined {
                            self.macros.push(LocalMacro {
                                name,
                                expander: None,
                                depth,
                                name_span: None,
                                scopes,
                            });
                        }
                    }
                }
                // A marker where the kernel defined one in an earlier pass
                form if kernel_local_macro_entry(form).is_some() => {
                    let (internal, _, scopes) = kernel_local_macro_entry(form).unwrap();
                    let name = written_kernel_local_name(internal);
                    let defined = self
                        .macros
                        .iter()
                        .any(|x| x.name == name && x.depth == depth && x.expander.is_none());

                    if !defined {
                        self.macros.push(LocalMacro {
                            name,
                            expander: None,
                            depth,
                            name_span: None,
                            scopes,
                        });
                    }
                }
                // A begin in a body is part of that body
                ExprKind::Begin(b) => self.define_body_macros(&mut b.exprs, 0, depth)?,
                ExprKind::List(l) if is_begin(l) => {
                    self.define_body_macros(&mut l.args, 1, depth)?
                }
                _ => {}
            }
        }

        Ok(())
    }

    fn enter<B: BodyForms>(
        &mut self,
        body: &mut B,
        start: usize,
        scope: &ScopeSet<InternedString, FxBuildHasher>,
    ) -> Result<usize> {
        self.define_in(body, start, scope, true)
    }

    // A begin's forms are part of the surrounding body, so the macros it defines stay until
    // that body ends, and it's the body that needs an expression after them
    fn splice<B: BodyForms>(
        &mut self,
        body: &mut B,
        start: usize,
        scope: &ScopeSet<InternedString, FxBuildHasher>,
    ) -> Result<()> {
        self.define_in(body, start, scope, false).map(|_| ())
    }

    fn define_in<B: BodyForms>(
        &mut self,
        body: &mut B,
        start: usize,
        scope: &ScopeSet<InternedString, FxBuildHasher>,
        needs_expression: bool,
    ) -> Result<usize> {
        let visible = self.macros.len();

        self.define_from(body, start, scope.depth())?;

        if self.macros.len() == visible {
            return Ok(visible);
        }

        if needs_expression
            && body
                .forms()
                .iter()
                .skip(start)
                .last()
                .map_or(true, ends_with_definition)
        {
            let span = self.macros[visible..]
                .iter()
                .find_map(|x| x.expander.as_ref().map(|x| x.span()))
                .unwrap_or_default();
            self.macros.truncate(visible);
            stop!(BadSyntax => "define-syntax in a body needs an expression after it"; span);
        }

        Ok(visible)
    }

    // A function or let body, which can't end with a macro definition of either kind
    fn enter_body<B: BodyForms>(
        &mut self,
        body: &mut B,
        start: usize,
        scope: &ScopeSet<InternedString, FxBuildHasher>,
    ) -> Result<usize> {
        if let Some(ExprKind::List(last)) = body.forms().iter().skip(start).last() {
            if last.is_define_syntax() {
                stop!(BadSyntax => "define-syntax in a body needs an expression after it"; last.location);
            }
        }

        self.enter(body, start, scope)
    }

    fn enter_expr(
        &mut self,
        body: &mut ExprKind,
        scope: &ScopeSet<InternedString, FxBuildHasher>,
    ) -> Result<usize> {
        match body {
            ExprKind::Begin(b) => self.enter_body(&mut b.exprs, 0, scope),
            ExprKind::List(l) if is_begin(l) => self.enter_body(&mut l.args, 1, scope),
            ExprKind::List(l) if l.is_define_syntax() => {
                stop!(BadSyntax => "define-syntax in a body needs an expression after it"; l.location)
            }
            _ => Ok(self.macros.len()),
        }
    }

    fn leave(&mut self, visible: usize) {
        self.macros.truncate(visible);
    }

    // Defines the macro a body form expanded into, so the rest of the body sees it. `written`
    // is whether the form already defined one before it was expanded.
    fn define_expanded(
        &mut self,
        form: &mut ExprKind,
        written: bool,
        scope: &ScopeSet<InternedString, FxBuildHasher>,
    ) -> Result<()> {
        if written || !defines_body_macro(form) {
            return Ok(());
        }

        let mut forms = vec![form.clone()];
        self.define_from(&mut forms, 0, scope.depth())?;
        *form = forms.pop().unwrap();

        Ok(())
    }
}

// Hooks for visiting a let's body. What it expands first counts toward the body's expansion
// depth.
trait BodyFirst {
    fn procedural_first(&mut self, _forms: &mut [ExprKind]) -> Result<usize> {
        Ok(0)
    }

    fn end_body(&mut self, _depth: usize) {}

    // Called for a let's binders and body, after its values are visited
    fn enter_let_body(&mut self, _l: &mut List, _bindings_index: usize) {}

    fn leave_let_body(&mut self) {}
}

impl BodyFirst for Expander<'_> {
    fn procedural_first(&mut self, forms: &mut [ExprKind]) -> Result<usize> {
        let depth = self.define_procedural_first(forms)?;
        self.depth += depth;
        Ok(depth)
    }

    fn end_body(&mut self, depth: usize) {
        self.depth -= depth;
    }

    fn enter_let_body(&mut self, l: &mut List, bindings_index: usize) {
        self.scope_chain.enter(form_key(l), 0);
        if bindings_index == 2 {
            if let Some(name) = l.args.get_mut(1) {
                self.scope_chain.bind_expr(name);
            }
        }
        if let Some(ExprKind::List(bindings)) = l.args.get_mut(bindings_index) {
            for binding in bindings.args.iter_mut() {
                if let ExprKind::List(binding) = binding {
                    if let Some(binder) = binding.args.first_mut() {
                        self.scope_chain.bind_expr(binder);
                    }
                }
            }
        }
        self.scope_chain.enter(form_key(l), 1);
        if let Some(body) = l.args.get_mut(bindings_index + 1..) {
            self.scope_chain
                .stamp_local_macro_definitions(body.iter_mut());
        }
    }

    fn leave_let_body(&mut self) {
        self.scope_chain.leave();
        self.scope_chain.leave();
    }
}

fn visit_let_form<V: VisitorMutRef<Output = Result<()>> + BodyFirst>(
    visitor: &mut V,
    scope: fn(&mut V) -> Option<&mut ScopeSet<InternedString, FxBuildHasher>>,
    // The name a binder is recorded under in the scope
    key: fn(&SyntaxObject) -> Option<InternedString>,
    local_macros: fn(
        &mut V,
    ) -> Option<(&mut LocalMacros, &ScopeSet<InternedString, FxBuildHasher>)>,
    // Called before each form of the body
    body_form: fn(&mut V),
    l: &mut List,
) -> Result<()> {
    let named = l.args.get(1).and_then(|x| x.atom_identifier()).is_some();
    let name = l
        .args
        .get(1)
        .and_then(ExprKind::atom_syntax_object)
        .and_then(key);
    let bindings_index = if named { 2 } else { 1 };

    if let Some(scope) = scope(visitor) {
        scope.push_layer();

        if let Some(name) = name {
            scope.define(name);
        }
    }

    if let Some(ExprKind::List(bindings)) = l.args.get_mut(bindings_index) {
        for arg in &mut bindings.args {
            if let ExprKind::List(binding) = arg {
                let ident = binding
                    .first()
                    .and_then(ExprKind::atom_syntax_object)
                    .and_then(key);

                if let (Some(ident), Some(scope)) = (ident, scope(visitor)) {
                    scope.define(ident);
                }

                if let Some(expr) = binding
                    .args
                    .get_mut(0)
                    .filter(|x| !matches!(**x, ExprKind::Atom(_)))
                {
                    visitor.visit(expr)?;
                }

                if let Some(expr) = binding.args.get_mut(1) {
                    visitor.visit(expr)?;
                }
            }
        }
    }

    visitor.enter_let_body(l, bindings_index);

    let visible = match local_macros(visitor) {
        Some((local_macros, scope)) => {
            Some(local_macros.enter_body(&mut l.args, bindings_index + 1, scope)?)
        }
        None => None,
    };

    if let Some(body_args) = l.args.get_mut(bindings_index + 1..) {
        let depth = visitor.procedural_first(body_args)?;

        for body_expr in body_args {
            let written = defines_body_macro(body_expr);
            body_form(visitor);
            visitor.visit(body_expr)?;

            if let Some((local_macros, scope)) = local_macros(visitor) {
                local_macros.define_expanded(body_expr, written, scope)?;
            }
        }

        visitor.end_body(depth);
    }

    if let Some(visible) = visible {
        if let Some((local_macros, _)) = local_macros(visitor) {
            local_macros.leave(visible);
        }
    }

    visitor.leave_let_body();

    if let Some(scope) = scope(visitor) {
        scope.pop_layer();
    }

    Ok(())
}

pub struct Expander<'a> {
    pub(crate) map: MacroMap<'a>,
    // The macros of the modules the code requires
    pub(crate) overlays: Vec<RequiredMacroMap<'a>>,
    exclusions: &'a HashSet<InternedString>,
    pub(crate) globals: GlobalMap<'a>,
    pub(crate) changed: bool,
    // We're going to actually check if the macro is in scope
    in_scope_values: ScopeSet<InternedString, FxBuildHasher>,
    source_id: Option<SourceId>,
    depth: usize,
    local_macros: LocalMacros,
    top_level: bool,
    // The form being visited is a body form, so a begin's definitions belong to the body
    body_form: bool,
    // Expanding a required module's macros for the code that required it
    foreign: bool,
    // Expands procedural macros too, where they're used
    pub(crate) kernel: Option<KernelExpander<'a>>,
    // Expand only the form's own macro use, once, and record whether it did
    head_only: bool,
    head_expanded: bool,
    // The form is in a body whose first pass already went through it
    head_covered: bool,
    // The first pass is over a body's forms, not those of a begin in an expression
    head_body: bool,
    // Expanding transformer code, which only sees the prelude's phase 0 macros plus the ones it
    // defines itself (`local_macros` from `prelude_base` on)
    prelude_only: bool,
    prelude_base: usize,
    // Define procedural macros that an expansion produces at the top level right where they're
    // made, and record their names for the code that checks definitions
    pub(crate) define_top_level_procedural: bool,
    pub(crate) defined_procedural: Vec<(InternedString, steel_parser::span::Span)>,
    pub(crate) scope_chain: ScopeChain,
}

#[derive(Debug)]
pub struct RequiredMacroMap<'a> {
    pub(crate) map: &'a mut RequiredModuleMacros,
    pub(crate) changed: bool,
}

// An expander that also expands required modules' macros, and owns its macro map so a
// top-level define-syntax can replace entries
pub type ExpanderMany<'a> = Expander<'a>;

// An expander's macros: a shared read-only map, or its own
pub(crate) enum MacroMap<'a> {
    Shared(&'a FxHashMap<InternedString, SteelMacro>),
    Own(&'a mut FxHashMap<InternedString, SteelMacro>),
}

impl MacroMap<'_> {
    pub(crate) fn get(&self, name: &InternedString) -> Option<&SteelMacro> {
        self.macros().get(name)
    }

    fn contains_key(&self, name: &InternedString) -> bool {
        self.macros().contains_key(name)
    }

    pub(crate) fn macros(&self) -> &FxHashMap<InternedString, SteelMacro> {
        match self {
            MacroMap::Shared(map) => map,
            MacroMap::Own(map) => map,
        }
    }

    // Only an expander with its own macros changes them
    pub(crate) fn macros_mut(&mut self) -> Option<&mut FxHashMap<InternedString, SteelMacro>> {
        match self {
            MacroMap::Shared(_) => None,
            MacroMap::Own(map) => Some(map),
        }
    }
}

static NO_EXCLUSIONS: once_cell::sync::Lazy<HashSet<InternedString>> =
    once_cell::sync::Lazy::new(HashSet::new);

impl<'a> Expander<'a> {
    pub fn many(
        map: &'a mut FxHashMap<InternedString, SteelMacro>,
        overlays: Vec<RequiredMacroMap<'a>>,
        globals: GlobalMap<'a>,
    ) -> Self {
        Self {
            map: MacroMap::Own(map),
            overlays,
            changed: false,
            in_scope_values: ScopeSet::default(),
            source_id: SourceId::none(),
            depth: 0,
            exclusions: &NO_EXCLUSIONS,
            globals,
            local_macros: LocalMacros::default(),
            top_level: true,
            body_form: false,
            foreign: false,
            kernel: None,
            define_top_level_procedural: false,
            defined_procedural: Vec::new(),
            head_only: false,
            head_expanded: false,
            head_covered: false,
            prelude_only: false,
            head_body: true,
            scope_chain: ScopeChain::default(),
            prelude_base: 0,
        }
    }
}

impl<'a> Expander<'a> {
    pub fn new(
        map: &'a FxHashMap<InternedString, SteelMacro>,
        exclusions: &'a HashSet<InternedString>,
        globals: GlobalMap<'a>,
    ) -> Self {
        Self {
            map: MacroMap::Shared(map),
            overlays: Vec::new(),
            changed: false,
            in_scope_values: ScopeSet::default(),
            source_id: SourceId::none(),
            depth: 0,
            exclusions,
            globals,
            local_macros: LocalMacros::default(),
            top_level: true,
            body_form: false,
            foreign: false,
            kernel: None,
            define_top_level_procedural: false,
            defined_procedural: Vec::new(),
            head_only: false,
            head_expanded: false,
            head_covered: false,
            prelude_only: false,
            head_body: true,
            scope_chain: ScopeChain::default(),
            prelude_base: 0,
        }
    }

    // Also expand procedural macros
    pub(crate) fn with_kernel(mut self, kernel: KernelExpander<'a>) -> Self {
        self.kernel = Some(kernel);
        self
    }

    // For a required module's macros
    pub(crate) fn foreign(mut self) -> Self {
        self.foreign = true;
        self
    }

    pub fn expand(&mut self, expr: &mut ExprKind) -> Result<()> {
        self.top_level = true;
        self.visit(expr)?;

        // Definitions in the code hide macros with the same names
        if let Some(map) = self.map.macros_mut() {
            for i in self.in_scope_values.iter() {
                remove_macro(map, i);
            }

            self.in_scope_values.clear_all();
        }

        Ok(())
    }
}

impl<'a> Expander<'a> {
    // A bare identifier that names a macro. A procedural macro is given the identifier, while
    // a syntax-rules macro has no pattern for it, so that's an error.
    fn visit_identifier(
        &mut self,
        expr: &mut ExprKind,
        top_level: bool,
        body_form: bool,
    ) -> Result<()> {
        // Identifiers in transformer code are phase 1
        if self.prelude_only {
            return Ok(());
        }

        let Some(syn) = expr.atom_syntax_object() else {
            return Ok(());
        };
        if syn.ty.identifier().is_none() {
            return Ok(());
        }
        if let ExprKind::Atom(a) = expr {
            self.scope_chain.stamp(&mut a.syn);
        }
        let Some(syn) = expr.atom_syntax_object() else {
            return Ok(());
        };
        // Most identifiers name no procedural macro at all
        let procedural = syn.ty.identifier().is_some_and(|s| {
            self.kernel
                .as_ref()
                .is_some_and(|kernel| kernel.may_name_macro(s))
                || self
                    .overlays
                    .iter()
                    .any(|x| x.map.provided_kernel_macros.contains_key(s))
        });
        if procedural {
            let mut l = List::new(thin_vec::thin_vec![core::mem::take(expr)]);
            let used = self.kernel_use(&mut l, top_level, body_form, UseKind::Identifier);
            match used {
                Ok(Some(expanded)) => {
                    *expr = expanded;
                    return Ok(());
                }
                Ok(None) => *expr = l.args.pop().unwrap_or_default(),
                Err(e) => return Err(e),
            }
        }

        // Only check when all macros are being expanded, since a pass that expands just some
        // of them can't recognize uses of the others
        if let Some(syn) = expr.atom_syntax_object().filter(|_| self.kernel.is_some()) {
            if self.names_rules_macro(syn) {
                stop!(BadSyntax => format!("{}: bad syntax", syn.ty); syn.span);
            }
        }
        Ok(())
    }

    // Whether a bare identifier names a syntax-rules macro, resolved as a form head would be
    fn names_rules_macro(&self, syn: &SyntaxObject) -> bool {
        let Some(s) = syn.ty.identifier() else {
            return false;
        };
        // Names written by the prelude's or a required module's macros refer to where those
        // macros are defined
        if syn.unresolved
            || syn.scopes.contains(steel_parser::scopes::Scope::prelude())
            || is_foreign(&syn.scopes)
            || self.exclusions.contains(s)
        {
            return false;
        }

        match self.meaning(syn) {
            Meaning::LocalMacro(i) => return self.local_macros.macros[i].expander.is_some(),
            Meaning::Unbound => {}
            _ => return false,
        }

        self.overlays
            .iter()
            .any(|x| x.map.in_scope_macros.contains_key(s))
            || self.map.get(s).is_some_and(|m| {
                self.source_id.is_none() || syn.span.source_id() == m.location.source_id()
            })
    }

    fn scoped_decision(&self, l: &List, kind: UseKind) -> Option<ScopedDecision> {
        let named = if kind == UseKind::Set {
            l.get(1)
        } else {
            l.first()
        };
        let head = named.and_then(ExprKind::atom_syntax_object)?;
        Some(match self.meaning(head) {
            Meaning::Variable => ScopedDecision::Variable,
            Meaning::KernelMacro(internal) => ScopedDecision::Kernel(internal),
            Meaning::LocalMacro(i) => {
                let local = &self.local_macros.macros[i];
                if local.expander.is_some() {
                    ScopedDecision::Rules
                } else if local.depth <= 1
                    && self.kernel.as_ref().is_some_and(|kernel| {
                        kernel.environment.zip(kernel.map.as_ref()).is_some_and(
                            |(environment, map)| map.has_own_macro(environment, &local.name),
                        )
                    })
                {
                    // A top-level macro, which the kernel has defined by now
                    ScopedDecision::Unbound
                } else {
                    ScopedDecision::Placeholder(local.name, local.scopes.clone())
                }
            }
            Meaning::Unbound => ScopedDecision::Unbound,
        })
    }

    // What an identifier means by its scopes: the variable or local macro whose binder's
    // scopes are the largest subset of the identifier's
    fn meaning(&self, syn: &SyntaxObject) -> Meaning {
        let Some(name) = syn.ty.identifier() else {
            return Meaning::Unbound;
        };
        // Fast path: most names have no local binding
        let local = self.scope_chain.binders.contains_key(name)
            || self.local_macros.macros.iter().any(|x| x.name == *name)
            || self
                .kernel
                .as_ref()
                .is_some_and(|kernel| kernel.local_macros.iter().any(|x| x.0 == *name));
        if !local {
            return if self.scope_chain.is_top_level(name, &syn.scopes) {
                Meaning::Variable
            } else {
                Meaning::Unbound
            };
        }
        let variables = self
            .scope_chain
            .binders
            .get(name)
            .into_iter()
            .flatten()
            .map(|x| (x.clone(), Meaning::Variable));
        let macros = self
            .local_macros
            .macros
            .iter()
            .enumerate()
            .filter(|(_, x)| x.name == *name)
            .map(|(i, x)| (x.scopes.clone(), Meaning::LocalMacro(i)));
        // Also the kernel's local macros, which a macro expansion may have defined
        let kernel_macros = self
            .kernel
            .as_ref()
            .into_iter()
            .flat_map(|kernel| kernel.local_macros.iter())
            .filter(|x| x.0 == *name)
            .filter(|x| {
                !self
                    .local_macros
                    .macros
                    .iter()
                    .any(|m| m.name == x.0 && m.scopes == x.3)
            })
            .map(|x| (x.3.clone(), Meaning::KernelMacro(x.2)));
        let candidates: Vec<_> = variables.chain(macros).chain(kernel_macros).collect();
        // Ties go to the last one, since a later definition replaces an earlier one. Racket
        // reports a reference as ambiguous when no candidate's scopes contain all the others'.
        let Some(largest) = crate::compiler::passes::hygiene::largest_subset(
            candidates
                .iter()
                .map(|(scopes, meaning)| (scopes, *meaning)),
            &syn.scopes,
        ) else {
            // Top-level variables are resolved by name like globals, and only hide names
            // written here, not ones a macro introduced
            if self.scope_chain.is_top_level(name, &syn.scopes) {
                return Meaning::Variable;
            }
            return Meaning::Unbound;
        };
        largest
    }

    // Whether a name refers to a macro here, local or global, that a use of it would expand
    fn names_macro(&self, syn: &SyntaxObject) -> bool {
        let Some(s) = syn.ty.identifier() else {
            return false;
        };
        match self.meaning(syn) {
            // A procedural one only counts when the kernel is present, which a later pass has
            Meaning::LocalMacro(i) if self.local_macros.macros[i].expander.is_some() => {
                return true
            }
            Meaning::LocalMacro(_) | Meaning::KernelMacro(_) => return self.kernel.is_some(),
            Meaning::Variable => return false,
            _ if self.exclusions.contains(s) => return false,
            _ => {}
        }

        let syntax_rules = self
            .overlays
            .iter()
            .any(|x| x.map.in_scope_macros.contains_key(s))
            || self.map.get(s).is_some_and(|m| {
                self.source_id.is_none() || syn.span.source_id() == m.location.source_id()
            });
        let procedural = self
            .overlays
            .iter()
            .any(|x| x.map.provided_kernel_macros.contains_key(s))
            || self.kernel.as_ref().is_some_and(|kernel| {
                kernel.required.iter().any(|x| x.provided.contains_key(s))
                    || kernel
                        .map
                        .as_ref()
                        .is_some_and(|map| map.names_prefixed_procedural(s))
                    || kernel
                        .environment
                        .zip(kernel.map.as_ref())
                        .is_some_and(|(environment, map)| map.has_own_macro(environment, s))
            });

        syntax_rules || procedural
    }

    // For a bare identifier `l` is a list of just it, and for a `set!` it's the whole form
    fn kernel_use(
        &mut self,
        l: &mut List,
        top_level: bool,
        body_form: bool,
        kind: UseKind,
    ) -> Result<Option<ExprKind>> {
        let head_only = core::mem::take(&mut self.head_only);
        // Resolve the use's name by its scopes, for the kernel to act on
        let Some(scoped) = self.scoped_decision(l, kind) else {
            return Ok(None);
        };
        let Some(kernel) = self.kernel.as_mut() else {
            return Ok(None);
        };
        let named = if kind == UseKind::Set {
            l.get(1)
        } else {
            l.first()
        };
        let Some(s) = named.and_then(|x| match x {
            ExprKind::Atom(Atom {
                syn:
                    SyntaxObject {
                        ty: TokenType::Identifier(s),
                        ..
                    },
            }) => Some(*s),
            _ => None,
        }) else {
            return Ok(None);
        };

        // A procedural macro is given its input with the scopes it has here
        if kernel.may_name_macro(&s)
            || self
                .overlays
                .iter()
                .any(|x| x.map.provided_kernel_macros.contains_key(&s))
        {
            for arg in l.args.iter_mut() {
                self.scope_chain.stamp_all(arg);
            }
        }
        let Some(kernel) = self.kernel.as_mut() else {
            return Ok(None);
        };

        // The required modules that provide a procedural macro of the name, in order
        let mut required: Vec<RequiredKernelMacros> = self
            .overlays
            .iter_mut()
            .filter(|x| x.map.provided_kernel_macros.contains_key(&s))
            .map(|x| RequiredKernelMacros {
                environment: &x.map.module_name,
                provided: &x.map.provided_kernel_macros,
                mangler: &mut x.map.name_mangler,
            })
            .collect();

        // Let the transformer look up what its input's names refer to here
        BINDER_TABLES.with(|x| {
            x.borrow_mut()
                .push(core::mem::take(&mut self.scope_chain.binders))
        });
        let used = if required.is_empty() {
            let mut own = core::mem::take(&mut kernel.required);
            let used = kernel.expand_use(s, l, &mut own, kind, scoped);
            kernel.required = own;
            used
        } else {
            kernel.expand_use(s, l, &mut required, kind, scoped)
        };
        drop(required);
        if let Some(binders) = BINDER_TABLES.with(|x| x.borrow_mut().pop()) {
            self.scope_chain.binders = binders;
        }

        let Some(used) = used? else {
            return Ok(None);
        };
        self.changed = true;
        if body_form || head_only {
            self.scope_chain.body_use();
        }

        match used {
            KernelUse::Done(expr) => Ok(Some(expr)),
            KernelUse::Expanded { expr, .. } if head_only => {
                self.head_expanded = true;
                Ok(Some(expr))
            }
            KernelUse::Expanded {
                mut expr,
                definition,
            } => {
                self.depth += 1;
                self.top_level = top_level;
                self.body_form = body_form;
                let result = self.visit(&mut expr);
                self.depth -= 1;
                result?;

                // A local macro definition expands to void, so leave a marker in its body
                if let Some((name, span, scopes)) = definition {
                    if let ExprKind::Atom(_) = expr {
                        expr = kernel_local_macro_marker(name, span, scopes);
                    }
                }

                Ok(Some(expr))
            }
        }
    }

    // Defines a body's procedural macros before its other forms, so they're visible in the
    // whole body
    fn define_procedural_first(&mut self, forms: &mut [ExprKind]) -> Result<usize> {
        // Without the kernel, the body's variables still hide macros in all of it
        if self.kernel.is_none() {
            if self.in_scope_values.depth() > 1 {
                self.define_body_variables(forms)?;
            }
            return Ok(0);
        }

        // The body's variables hide macros throughout it, and one named like a macro of the
        // body is a duplicate. The body's macros can see them.
        let in_body = self.in_body();
        if in_body {
            self.define_body_variables(forms)?;
        }

        for form in forms.iter_mut().filter(|x| procedural_definition(x)) {
            self.body_form = true;
            self.visit(form)?;
        }

        // Then expand each form's head until it's a definition or an expression, so what a
        // macro defines is visible in the whole body too

        // The most expansions any one form needed, which counts as nesting depth
        let mut deepest = 0;

        for form in forms.iter_mut() {
            let mut budget = MAX_HEAD_EXPANSIONS;
            self.head_pass(form, false, in_body, &mut budget)?;
            deepest = deepest.max(MAX_HEAD_EXPANSIONS - budget);
        }

        Ok(deepest)
    }

    // A body form's first pass. Definitions a macro produced apply to the whole body, and a
    // begin's forms are handled in order after its own macros.
    fn head_pass(
        &mut self,
        form: &mut ExprKind,
        generated: bool,
        in_body: bool,
        budget: &mut usize,
    ) -> Result<bool> {
        let expanded = self.expand_head_from(form, budget)?;
        let generated = generated || expanded;

        // Give the definitions a macro produced the scopes their names have here
        if expanded {
            self.scope_chain
                .stamp_local_macro_definitions(core::iter::once(&mut *form));
        }

        if let ExprKind::List(l) = form {
            if is_begin(l) {
                if generated {
                    if in_body {
                        self.define_body_variables(&l.args[1..])?;
                    }
                    self.local_macros
                        .splice(&mut l.args, 1, &self.in_scope_values)?;
                }

                let mut changed = expanded;
                for form in l.args[1..].iter_mut() {
                    changed |= crate::compiler::passes::with_stack(|| {
                        self.head_pass(form, generated, in_body, budget)
                    })?;
                }
                return Ok(changed);
            }
        }

        if !generated {
            return Ok(false);
        }

        if in_body {
            self.define_body_variables(core::slice::from_mut(form))?;
        }

        if procedural_definition(form) {
            self.body_form = true;
            self.visit(form)?;
        } else if defines_body_macro(form) {
            self.define_generated_macros(form)?;
        }

        Ok(expanded)
    }

    // In a function's body, not at the top level
    fn in_body(&self) -> bool {
        self.kernel.as_ref().is_some_and(|x| x.locals.depth() > 1)
    }

    // Expands the prelude's macros in transformer code, since phase 1 has them too. The code's
    // own macros are phase 0. `definition` is the define-syntax form; for
    // `(define-syntax (name stx) body ...)` its forms from the third on are the body.
    fn expand_prelude_macros(&mut self, definition: &mut ThinVec<ExprKind>) -> Result<()> {
        let kernel = self.kernel.take();
        let prelude_only = core::mem::replace(&mut self.prelude_only, true);
        let base = core::mem::replace(&mut self.prelude_base, self.local_macros.macros.len());
        // Transformer code has its own phase 1 top level
        let top_level = core::mem::take(&mut self.scope_chain.top_level);
        let template_top_level = core::mem::take(&mut self.scope_chain.template_top_level);

        // Its definitions are the transformer's, which don't hide the code's macros
        self.in_scope_values.push_layer();

        // Macros the body defines are visible in all of it
        let result = self
            .local_macros
            .enter(definition, 2, &self.in_scope_values)
            .and_then(|visible| {
                let result = definition
                    .iter_mut()
                    .skip(2)
                    .try_for_each(|x| self.visit(x));
                self.local_macros.leave(visible);
                result
            });

        self.in_scope_values.pop_layer();

        self.scope_chain.top_level = top_level;
        self.scope_chain.template_top_level = template_top_level;
        self.prelude_base = base;
        self.prelude_only = prelude_only;
        self.kernel = kernel;
        result
    }

    // Visits the `#,` holes of a quasisyntax template, which are code
    fn visit_unsyntax_holes(&mut self, expr: &mut ExprKind) -> Result<()> {
        match expr {
            ExprKind::List(l)
                if matches!(
                    super::kernel::form_name(l),
                    Some("#%unsyntax" | "#%unsyntax-splicing" | "unsyntax" | "unsyntax-splicing")
                ) =>
            {
                l.args.iter_mut().skip(1).try_for_each(|x| self.visit(x))
            }
            ExprKind::List(l) => l
                .args
                .iter_mut()
                .try_for_each(|x| self.visit_unsyntax_holes(x)),
            ExprKind::Vector(v) => v
                .args
                .iter_mut()
                .try_for_each(|x| self.visit_unsyntax_holes(x)),
            ExprKind::Quote(q) => self.visit_unsyntax_holes(&mut q.expr),
            _ => Ok(()),
        }
    }

    // Defines the syntax-rules macros in a body form that a macro produced. A begin's belong to
    // the body, as if it were spliced.
    fn define_generated_macros(&mut self, form: &mut ExprKind) -> Result<()> {
        match form {
            ExprKind::List(l) if is_begin(l) => {
                self.local_macros
                    .splice(&mut l.args, 1, &self.in_scope_values)
            }
            ExprKind::Begin(b) => self
                .local_macros
                .splice(&mut b.exprs, 0, &self.in_scope_values),
            form => self
                .local_macros
                .define_expanded(form, false, &self.in_scope_values),
        }
    }

    // Records the variables a body defines, which hide macros with their names throughout the
    // body. One a macro introduced only hides what that expansion wrote, which the kernel tracks.
    fn define_body_variables(&mut self, forms: &[ExprKind]) -> Result<()> {
        fn written_name(name: &ExprKind) -> Option<InternedString> {
            name.atom_syntax_object()
                .filter(|x| !x.scopes.introduced())
                .and_then(|x| x.ty.identifier().copied())
        }

        for form in forms {
            match form {
                ExprKind::Define(d) => {
                    if let Some(syn) = d.name.atom_syntax_object() {
                        self.scope_chain.record_definition(syn);
                    }
                    if let Some(name) = written_name(&d.name) {
                        self.in_scope_values.define(name);
                    }
                    if let Some(kernel) = self.kernel.as_mut() {
                        kernel.defined(
                            &d.name,
                            self.head_body,
                            Some(d.location.syntax_object_id),
                        )?;
                    }
                }
                ExprKind::Begin(b) => self.define_body_variables(&b.exprs)?,
                ExprKind::List(l) if is_begin(l) => self.define_body_variables(&l.args[1..])?,
                ExprKind::List(l)
                    if l.args.len() > 1
                        && matches!(
                            l.first(),
                            Some(ExprKind::Atom(Atom {
                                syn: SyntaxObject {
                                    ty: TokenType::Define,
                                    ..
                                },
                            }))
                        ) =>
                {
                    let target = define_target(&l.args[1]);
                    let binder = match target {
                        ExprKind::List(signature) => signature.first(),
                        name => Some(name),
                    };
                    if let Some(syn) = binder.and_then(ExprKind::atom_syntax_object) {
                        self.scope_chain.record_definition(syn);
                    }
                    let name = match target {
                        ExprKind::List(signature) => signature.first().and_then(written_name),
                        name => written_name(name),
                    };
                    if let Some(name) = name {
                        self.in_scope_values.define(name);
                    }
                    if let Some(kernel) = self.kernel.as_mut() {
                        kernel.defined(
                            target,
                            self.head_body,
                            l.first()
                                .and_then(ExprKind::atom_syntax_object)
                                .map(|x| x.syntax_object_id),
                        )?;
                    }
                }
                _ => {}
            }
        }

        Ok(())
    }

    // Expands a body form's own macro use until it's something else, such as a begin. A form
    // that keeps expanding past the budget is left for the body's expansion, which reports it.
    fn expand_head_from(&mut self, form: &mut ExprKind, budget: &mut usize) -> Result<bool> {
        let mut expanded = false;

        loop {
            match form {
                ExprKind::List(l) if is_begin(l) => return Ok(expanded),
                ExprKind::List(l) => {
                    let Some(
                        head @ ExprKind::Atom(Atom {
                            syn:
                                SyntaxObject {
                                    ty: TokenType::Identifier(s),
                                    ..
                                },
                        }),
                    ) = l.first()
                    else {
                        return Ok(expanded);
                    };

                    if [*LAMBDA_SYMBOL, *LAMBDA, *QUOTE, *SYNTAX_CASE, *SYNTAX_RULES].contains(s)
                        || *s == *DEFINE_SYNTAX
                        || is_let_head(head)
                        || defines_local_macro(l)
                    {
                        return Ok(expanded);
                    }
                }
                _ => return Ok(expanded),
            }

            if *budget == 0 {
                return Ok(expanded);
            }
            *budget -= 1;

            self.head_only = true;
            self.head_expanded = false;
            self.body_form = true;
            self.visit(form)?;
            self.head_only = false;

            if !core::mem::take(&mut self.head_expanded) {
                return Ok(expanded);
            }
            expanded = true;
        }
    }

    // A begin in a body is part of it, so what it defines stays for the rest of the body.
    // One in an expression is its own body.
    fn visit_begin_forms(
        &mut self,
        begin: &mut super::ast::Begin,
        body_form: bool,
        covered: bool,
    ) -> Result<()> {
        let visible = if body_form {
            self.local_macros
                .splice(&mut begin.exprs, 0, &self.in_scope_values)?;
            None
        } else {
            Some(
                self.local_macros
                    .enter(&mut begin.exprs, 0, &self.in_scope_values)?,
            )
        };

        // The body's first pass already went through a begin spliced into it
        let depth = if body_form && covered {
            0
        } else {
            let head_body = core::mem::replace(&mut self.head_body, body_form);
            let depth = self.define_procedural_first(&mut begin.exprs);
            self.head_body = head_body;
            depth?
        };
        self.depth += depth;

        for expr in begin.exprs.iter_mut() {
            let written = defines_body_macro(expr);
            self.body_form = body_form;
            self.head_covered = self.kernel.is_some();
            self.visit(expr)?;

            if body_form || visible.is_some() {
                self.local_macros
                    .define_expanded(expr, written, &self.in_scope_values)?;
            }
        }

        self.depth -= depth;

        if let Some(visible) = visible {
            self.local_macros.leave(visible);
        }

        Ok(())
    }

    fn visit_expr(&mut self, expr: &mut ExprKind) -> Result<()> {
        if self.depth > MAX_EXPANSION_DEPTH {
            stop!(BadSyntax => "macro expansion doesn't finish";
                crate::parser::span_visitor::get_span(expr));
        }

        let head_only = core::mem::take(&mut self.head_only);
        let covered = core::mem::take(&mut self.head_covered);

        let top_level = core::mem::replace(&mut self.top_level, false);
        let body_form = core::mem::replace(&mut self.body_form, false);

        if top_level {
            match expr {
                ExprKind::Begin(b) => {
                    let visible = self.local_macros.macros.len();
                    let depth = self.in_scope_values.depth();
                    self.local_macros.define_from(&mut b.exprs, 0, depth)?;

                    for expr in b.exprs.iter_mut() {
                        self.top_level = true;
                        self.body_form = true;
                        self.visit(expr)?;
                    }

                    self.local_macros.leave(visible);
                    return Ok(());
                }
                ExprKind::List(l) if is_begin(l) => {
                    let visible = self.local_macros.macros.len();
                    let depth = self.in_scope_values.depth();
                    self.local_macros.define_from(&mut l.args, 1, depth)?;

                    for expr in l.args.iter_mut().skip(1) {
                        self.top_level = true;
                        self.body_form = true;
                        self.visit(expr)?;
                    }

                    self.local_macros.leave(visible);
                    return Ok(());
                }
                _ => {}
            }
        }

        if let Some(kernel) = self.kernel.as_mut() {
            match expr {
                ExprKind::Quote(_) => kernel.revive(expr),
                // A syntax-rules definition that a procedural macro produced
                ExprKind::Macro(_) => {
                    kernel.macro_definition(expr);
                    self.top_level = top_level;
                    self.body_form = body_form;
                    return self.visit_expr(expr);
                }
                _ => {}
            }
        }

        match expr {
            ExprKind::If(f) => self.visit_if(f),
            ExprKind::Define(d) => self.visit_define(d),
            ExprKind::LambdaFunction(l) => self.visit_lambda_function(l),
            ExprKind::Begin(b) => self.visit_begin_forms(b, body_form, covered),
            ExprKind::Return(r) => self.visit_return(r),
            ExprKind::Let(l) => self.visit_let(l),
            ExprKind::Quote(q) => self.visit_quote(q),
            ExprKind::Macro(m) => self.visit_macro(m),
            ExprKind::Atom(_) => self.visit_identifier(expr, top_level, body_form),
            ExprKind::List(l) => {
                // Stamp the head's scopes before deciding what it refers to
                if let Some(ExprKind::Atom(a)) = l.args.first_mut() {
                    self.scope_chain.stamp(&mut a.syn);
                }

                if defines_local_macro(l) {
                    if let Some(kernel) = self.kernel.as_mut() {
                        kernel.rules_defined(l)?;
                    }
                    self.scope_chain.stamp_all(expr);
                    return Ok(());
                }

                // A macro's name can't be set
                if let Some(target) = l.args.get_mut(1) {
                    self.scope_chain.stamp_expr(target);
                }
                if let (Some(ExprKind::Atom(head)), Some(ExprKind::Atom(target))) =
                    (l.first(), l.get(1))
                {
                    let set = head.syn.ty == TokenType::Set
                        || head.ident().is_some_and(|x| {
                            x.resolve() == "set!" && !self.in_scope_values.contains(x)
                        });
                    if set && self.names_macro(&target.syn) {
                        let (ty, span) = (target.syn.ty.clone(), target.syn.span);
                        // A set! transformer takes the whole form
                        if let Some(expanded) =
                            self.kernel_use(l, top_level, body_form, UseKind::Set)?
                        {
                            *expr = expanded;
                            return Ok(());
                        }
                        stop!(BadSyntax => format!("set!: cannot mutate syntax identifier: {}", ty); span);
                    }
                }

                // `define` as a plain identifier, as in code built from syntax objects, defines too
                if let Some(ExprKind::Atom(Atom { syn })) = l.args.first_mut() {
                    if syn.ty == TokenType::Identifier(*DEFINE)
                        && !self.in_scope_values.contains(&DEFINE)
                    {
                        syn.ty = TokenType::Define;
                    }
                }

                // In transformer code a template is output, not code to expand (apart from its
                // holes), and a with-syntax pattern isn't code either
                if self.prelude_only {
                    match super::kernel::form_name(l) {
                        Some("syntax" | "quote-syntax" | "#%expand-syntax-case") => return Ok(()),
                        Some("quasisyntax" | "#%quasisyntax") => {
                            return l
                                .args
                                .iter_mut()
                                .skip(1)
                                .try_for_each(|x| self.visit_unsyntax_holes(x));
                        }
                        Some("with-syntax")
                            if !self.in_scope_values.contains(&"with-syntax".into()) =>
                        {
                            if let Some(ExprKind::List(bindings)) = l.args.get_mut(1) {
                                for binding in bindings.args.iter_mut() {
                                    if let ExprKind::List(binding) = binding {
                                        for expr in binding.args.iter_mut().skip(1) {
                                            self.visit(expr)?;
                                        }
                                    }
                                }
                            }
                            return l.args.iter_mut().skip(2).try_for_each(|x| self.visit(x));
                        }
                        _ => {}
                    }
                }

                match l.first() {
                    // TODO: Come back to this?
                    Some(ExprKind::Atom(
                        ident @ Atom {
                            syn:
                                SyntaxObject {
                                    ty: TokenType::Identifier(s),
                                    ..
                                },
                        },
                    )) if *s == *LAMBDA_SYMBOL || *s == *LAMBDA => {
                        if let ExprKind::LambdaFunction(mut lambda) =
                            parse_lambda(ident.clone(), l.args.clone())?
                        {
                            self.visit_lambda_function(&mut lambda)?;

                            *expr = ExprKind::LambdaFunction(lambda);

                            return Ok(());

                            // return self.visit_lambda_function(&mut lambda);

                            // return self.visit_lambda_function(lambda);
                        } else {
                            unreachable!()
                        }
                    }
                    Some(ExprKind::Atom(Atom {
                        syn:
                            SyntaxObject {
                                ty: TokenType::Quote,
                                ..
                            },
                    })) => return Ok(()),

                    Some(ExprKind::Atom(Atom {
                        syn:
                            SyntaxObject {
                                ty: TokenType::Identifier(i),
                                ..
                            },
                    })) if *i == *QUOTE => return Ok(()),

                    Some(ExprKind::Atom(Atom {
                        syn:
                            SyntaxObject {
                                ty: TokenType::Identifier(i),
                                ..
                            },
                    })) if *i == *SYNTAX_CASE && !self.in_scope_values.contains(i) => {
                        if let Some(args) = l.args.get_mut(3..) {
                            for arg in args {
                                if let ExprKind::List(l) = arg {
                                    if let Some(rest) = l.args.get_mut(1..) {
                                        for arg in rest {
                                            self.visit(arg)?;
                                        }
                                    }
                                } else {
                                    self.visit(arg)?;
                                }
                            }
                        }
                        return Ok(());
                    }

                    Some(head) if is_let_head(head) => {
                        let hidden = match self.kernel.as_mut() {
                            Some(kernel) => {
                                let named =
                                    l.args.get(1).and_then(|x| x.atom_identifier()).is_some();
                                let bindings_index = if named { 2 } else { 1 };
                                let body = l.args.get(bindings_index + 1..).unwrap_or_default();
                                Some(kernel.enter_scope(body))
                            }
                            None => None,
                        };

                        let result = visit_let_form(
                            self,
                            |v| Some(&mut v.in_scope_values),
                            binder_name,
                            |v| Some((&mut v.local_macros, &v.in_scope_values)),
                            |v| {
                                v.body_form = true;
                                v.head_covered = v.kernel.is_some();
                            },
                            l,
                        );

                        if let (Some(kernel), Some(hidden)) = (self.kernel.as_mut(), hidden) {
                            kernel.leave_scope(hidden);
                        }
                        return result;
                    }

                    Some(ExprKind::Atom(Atom {
                        syn:
                            SyntaxObject {
                                ty: TokenType::Begin,
                                ..
                            },
                    })) => {
                        // A begin in a body splices into it, one in an expression is its own body
                        let visible = if body_form {
                            self.local_macros
                                .splice(&mut l.args, 1, &self.in_scope_values)?;
                            None
                        } else {
                            Some(
                                self.local_macros
                                    .enter(&mut l.args, 1, &self.in_scope_values)?,
                            )
                        };

                        // The body's first pass already went through a begin spliced into it
                        let depth = if body_form && covered {
                            0
                        } else {
                            let head_body = core::mem::replace(&mut self.head_body, body_form);
                            let depth = self.define_procedural_first(&mut l.args[1..]);
                            self.head_body = head_body;
                            depth?
                        };
                        self.depth += depth;

                        for expr in l.args.iter_mut() {
                            let written = defines_body_macro(expr);
                            self.body_form = body_form;
                            self.head_covered = self.kernel.is_some();
                            self.visit(expr)?;

                            self.local_macros.define_expanded(
                                expr,
                                written,
                                &self.in_scope_values,
                            )?;
                        }

                        self.depth -= depth;

                        if let Some(visible) = visible {
                            self.local_macros.leave(visible);
                        }

                        return Ok(());
                    }

                    Some(ExprKind::Atom(Atom {
                        syn:
                            SyntaxObject {
                                ty: TokenType::Identifier(i),
                                ..
                            },
                    })) if *i == *SYNTAX_RULES && !self.in_scope_values.contains(i) => {
                        if let Some(args) = l.args.get_mut(2..) {
                            for arg in args {
                                self.visit(arg)?;
                            }
                        }
                        return Ok(());
                    }
                    // If we're in define-syntax, don't expand the list of the name
                    Some(ExprKind::Atom(Atom {
                        syn:
                            SyntaxObject {
                                ty: TokenType::Identifier(i),
                                ..
                            },
                    })) if *i == *DEFINE_SYNTAX && !self.in_scope_values.contains(i) => {
                        // Scopes of the name at its definition. The kernel gives them to
                        // identifiers a function's macro templates write
                        match l.args.get_mut(1) {
                            Some(ExprKind::List(signature)) => {
                                if let Some(name) = signature.args.first_mut() {
                                    self.scope_chain.stamp_expr(name);
                                }
                            }
                            Some(name) => self.scope_chain.stamp_expr(name),
                            None => {}
                        }
                        if !IDENTIFIER_SYNTAX.with(|x| self.in_scope_values.contains(x)) {
                            lower_identifier_syntax(l);
                        }
                        // The kernel defines a procedural macro where it appears: in a function,
                        // or at the top level in order with the other forms. One a macro produced
                        // at the top level is recorded for the definition checks.
                        let local = self.kernel.as_ref().is_some_and(|x| x.locals.depth() > 1);
                        let made = l
                            .first()
                            .and_then(ExprKind::atom_syntax_object)
                            .is_some_and(|x| x.scopes.introduced());
                        if (local || self.define_top_level_procedural)
                            && procedural_macro_name(l).is_some()
                        {
                            let defined = procedural_macro_name(l).zip(procedural_name_span(l));
                            // It replaces a syntax-rules macro with its name for later forms
                            if let Some(map) = self.map.macros_mut().filter(|_| top_level) {
                                forget_replaced_macro(map, l);
                            }
                            self.expand_prelude_macros(&mut l.args)?;
                            if let Some(expanded) =
                                self.kernel_use(l, top_level, body_form, UseKind::Form)?
                            {
                                if !local && made {
                                    self.defined_procedural.extend(defined);
                                }
                                *expr = expanded;
                                return Ok(());
                            }
                        }

                        // The name isn't a macro use, even when it names a macro. What the
                        // transformer defines is its own, and hides none of the code's macros.
                        if let Some(arg) = l.args.get_mut(2) {
                            self.in_scope_values.push_layer();
                            let result = self.visit(arg);
                            self.in_scope_values.pop_layer();
                            result?;
                        }

                        // A procedural macro replaces a syntax-rules one of its name for the
                        // forms after it
                        if let Some(map) = self.map.macros_mut().filter(|_| top_level) {
                            forget_replaced_macro(map, l);
                        }

                        return Ok(());
                    }

                    Some(ExprKind::Atom(Atom {
                        syn:
                            SyntaxObject {
                                ty: TokenType::DefineSyntax,
                                ..
                            },
                    })) if !self.in_scope_values.contains(&DEFINE_SYNTAX) => {
                        // Scopes of the name at its definition. The kernel gives them to
                        // identifiers a function's macro templates write
                        match l.args.get_mut(1) {
                            Some(ExprKind::List(signature)) => {
                                if let Some(name) = signature.args.first_mut() {
                                    self.scope_chain.stamp_expr(name);
                                }
                            }
                            Some(name) => self.scope_chain.stamp_expr(name),
                            None => {}
                        }
                        if !IDENTIFIER_SYNTAX.with(|x| self.in_scope_values.contains(x)) {
                            lower_identifier_syntax(l);
                        }
                        // The kernel defines a function's procedural macros here, and also ones a
                        // macro produced at the top level
                        let top_level_definition =
                            self.define_top_level_procedural && top_level && !body_form
                                || self.define_top_level_procedural
                                    && self.kernel.as_ref().is_some_and(|x| x.locals.depth() <= 1);
                        if (self.kernel.as_ref().is_some_and(|x| x.locals.depth() > 1)
                            || top_level_definition && self.kernel.is_some())
                            && procedural_macro_name(l).is_some()
                        {
                            let defined = procedural_macro_name(l).zip(procedural_name_span(l));
                            let local = self.kernel.as_ref().is_some_and(|x| x.locals.depth() > 1);
                            let span = l
                                .first()
                                .and_then(ExprKind::atom_syntax_object)
                                .map(|x| x.span)
                                .unwrap_or_default();
                            l.args[0] = ExprKind::Atom(Atom::new(SyntaxObject::new(
                                TokenType::Identifier(*DEFINE_SYNTAX),
                                span,
                            )));
                            if let Some(map) = self.map.macros_mut().filter(|_| top_level) {
                                forget_replaced_macro(map, l);
                            }
                            self.expand_prelude_macros(&mut l.args)?;
                            if let Some(expanded) =
                                self.kernel_use(l, top_level, body_form, UseKind::Form)?
                            {
                                if !local {
                                    self.defined_procedural.extend(defined);
                                }
                                *expr = expanded;
                                return Ok(());
                            }
                        }

                        // What the transformer defines is its own
                        if let Some(arg) = l.args.get_mut(2) {
                            self.in_scope_values.push_layer();
                            let result = self.visit(arg);
                            self.in_scope_values.pop_layer();
                            result?;
                        }

                        if let Some(map) = self.map.macros_mut().filter(|_| top_level) {
                            forget_replaced_macro(map, l);
                        }

                        return Ok(());
                    }

                    // Stupid hack to get around the token types
                    Some(ExprKind::Atom(
                        ident @ Atom {
                            syn:
                                SyntaxObject {
                                    ty: TokenType::Lambda,
                                    ..
                                },
                        },
                    )) if l.args.len() >= 3 => {
                        if let ExprKind::LambdaFunction(mut lambda) =
                            parse_lambda(ident.clone(), core::mem::take(&mut l.args))?
                        {
                            if l.improper {
                                lambda.rest = true;
                            }

                            self.visit_lambda_function(&mut lambda)?;

                            *expr = ExprKind::LambdaFunction(lambda);

                            return Ok(());
                        } else {
                            unreachable!()
                        }
                    }

                    Some(ExprKind::Atom(Atom {
                        syn:
                            SyntaxObject {
                                ty: TokenType::Define,
                                ..
                            },
                    })) if l.args.len() > 1 => {
                        uncurry_define(l);

                        // Record the defined name. A function's parameters and body get the
                        // function's scopes, as with a lambda.
                        let function_scopes =
                            matches!(&l.args[1], ExprKind::List(s) if s.first_ident().is_some());
                        let key = form_key(l);
                        match &mut l.args[1] {
                            ExprKind::List(signature) if function_scopes => {
                                if let Some(name) = signature.args.first_mut() {
                                    self.scope_chain.define_expr(name);
                                }
                                self.scope_chain.enter(key, 0);
                                for parameter in signature.args.iter_mut().skip(1) {
                                    match parameter {
                                        ExprKind::List(default) => {
                                            if let Some(name) = default.args.first_mut() {
                                                self.scope_chain.bind_expr(name);
                                            }
                                        }
                                        parameter => self.scope_chain.bind_expr(parameter),
                                    }
                                }
                                self.scope_chain.enter(key, 1);
                                self.scope_chain
                                    .stamp_local_macro_definitions(l.args[2..].iter_mut());
                            }
                            name => self.scope_chain.define_expr(name),
                        }

                        if let Some(kernel) = self.kernel.as_mut() {
                            let form = l
                                .first()
                                .and_then(ExprKind::atom_syntax_object)
                                .map(|x| x.syntax_object_id);
                            kernel.defined(&l.args[1], body_form, form)?;
                        }

                        // A global a template defines doesn't hide a macro of its name from
                        // the rest of the code
                        let hides = |syn: Option<&SyntaxObject>| {
                            !(top_level && syn.is_some_and(|x| x.scopes.introduced()))
                        };
                        let is_function = match &l.args[1] {
                            ExprKind::List(l) if l.first_ident().is_some() => {
                                if hides(l.first().and_then(ExprKind::atom_syntax_object)) {
                                    self.in_scope_values.define(*l.first_ident().unwrap());
                                }
                                true
                            }

                            ExprKind::Atom(a) if a.ident().is_some() => {
                                if hides(Some(&a.syn)) {
                                    self.in_scope_values.define(*a.ident().unwrap());
                                }
                                false
                            }
                            _ => false,
                        };

                        if is_function {
                            self.in_scope_values.push_layer();

                            if let ExprKind::List(signature) = &l.args[1] {
                                for arg in signature.args.iter().skip(1) {
                                    if let Some(ident) = parameter_name(arg) {
                                        self.in_scope_values.define(*ident);
                                    }
                                }
                            }
                        }

                        let visible = if is_function {
                            Some(self.local_macros.enter_body(
                                &mut l.args,
                                2,
                                &self.in_scope_values,
                            )?)
                        } else {
                            None
                        };

                        let hidden = match (self.kernel.as_mut(), &l.args[1]) {
                            (Some(kernel), ExprKind::List(signature)) if is_function => {
                                Some(kernel.enter_scope(&l.args[2..]))
                            }
                            _ => None,
                        };

                        let mut depth = 0;
                        if is_function {
                            // The parameters' defaults, `[k (f 4)]`, are expressions too
                            if let ExprKind::List(signature) = &mut l.args[1] {
                                for parameter in signature.args.iter_mut().skip(1) {
                                    if let ExprKind::List(default) = parameter {
                                        for expr in default.args.iter_mut().skip(1) {
                                            self.visit(expr)?;
                                        }
                                    }
                                }
                            }

                            depth = self.define_procedural_first(&mut l.args[2..])?;
                            self.depth += depth;
                        }

                        for expr in l.args[2..].iter_mut() {
                            let written = defines_body_macro(expr);
                            self.body_form = is_function;
                            self.head_covered = is_function && self.kernel.is_some();
                            self.visit(expr)?;

                            if is_function {
                                self.local_macros.define_expanded(
                                    expr,
                                    written,
                                    &self.in_scope_values,
                                )?;
                            }
                        }

                        self.depth -= depth;

                        if let Some(visible) = visible {
                            self.local_macros.leave(visible);
                        }

                        if let (Some(kernel), Some(hidden)) = (self.kernel.as_mut(), hidden) {
                            kernel.leave_scope(hidden);
                        }

                        if is_function {
                            self.in_scope_values.pop_layer();
                        }
                        if function_scopes {
                            self.scope_chain.leave();
                            self.scope_chain.leave();
                        }

                        return Ok(());
                    }

                    Some(ExprKind::Atom(Atom {
                        syn:
                            SyntaxObject {
                                ty: TokenType::Identifier(s),
                                span: sp,
                                unresolved,
                                ..
                            },
                    })) => {
                        // A head from a prelude template refers to the prelude's macro, which is
                        // kept under an alias when a macro of the same name is defined here. The
                        // same goes for a head a required module's procedural macro wrote.
                        let mut found_index = None;
                        let prelude_template = l
                            .first()
                            .and_then(ExprKind::atom_syntax_object)
                            .is_some_and(|x| {
                                x.scopes.contains(steel_parser::scopes::Scope::prelude())
                                    || is_foreign(&x.scopes)
                            });
                        let alias = (prelude_template && PRELUDE_ALIASES.load(Ordering::Relaxed))
                            .then(|| prelude_macro_alias(s))
                            .filter(|alias| self.map.contains_key(alias));
                        // Otherwise use the prelude's own macro, when no alias was kept here
                        let prelude_macros = (prelude_template && alias.is_none())
                            .then(crate::steel_vm::engine::default_prelude_macros);
                        let prelude_macro = prelude_macros.as_ref().and_then(|x| x.get(s));
                        let s = alias.as_ref().unwrap_or(s);

                        let prelude_only = self.prelude_only;

                        // Resolve the head by its scopes
                        let meaning = l
                            .first()
                            .and_then(ExprKind::atom_syntax_object)
                            .map_or(Meaning::Unbound, |head| self.meaning(head));
                        let local = match meaning {
                            Meaning::LocalMacro(i)
                                if self.local_macros.macros[i].expander.is_some() =>
                            {
                                Some(&self.local_macros.macros[i])
                            }
                            _ => None,
                        };
                        let procedural = match meaning {
                            Meaning::LocalMacro(i) => {
                                self.local_macros.macros[i].expander.is_none()
                            }
                            Meaning::KernelMacro(_) => true,
                            _ => false,
                        };

                        // At the top level a use refers to a procedural macro defined before it,
                        // even if a syntax-rules macro of the same name is defined after it
                        let top_level_procedural = local.is_none()
                            && self.kernel.as_ref().is_some_and(|kernel| {
                                kernel.environment == Some("top-level")
                                    && kernel
                                        .map
                                        .as_ref()
                                        .is_some_and(|map| map.has_own_macro("top-level", s))
                            });

                        if procedural || top_level_procedural {
                            // The kernel expands it
                        } else if let Some(local) = local {
                            let span = *sp;

                            let binders = UseBinders::set(&self.scope_chain);
                            let literals = LITERAL_SCOPES
                                .with(|x| x.borrow_mut().replace(local.scopes.clone()));
                            let expanded = local.expand(
                                List::new_maybe_improper(core::mem::take(&mut l.args), l.improper),
                                span,
                                &self.in_scope_values,
                                self.globals,
                            );
                            LITERAL_SCOPES.with(|x| *x.borrow_mut() = literals);
                            drop(binders);
                            let (mut expanded, introduction) = expanded?;
                            if self.prelude_only {
                                crate::parser::kernel::rename_template_binders(
                                    &mut expanded,
                                    introduction,
                                );
                            }
                            if body_form || head_only {
                                self.scope_chain.body_use();
                            }
                            self.changed = true;

                            if head_only {
                                self.head_expanded = true;
                                *expr = expanded;
                                return Ok(());
                            }

                            self.depth += 1;

                            self.body_form = body_form;
                            self.visit(&mut expanded)?;

                            self.depth -= 1;

                            *expr = expanded;

                            return Ok(());
                        } else if let Some(m) = prelude_macro.or_else(|| {
                            // Look in the required modules' macros, then the code's own
                            self.overlays
                                .iter()
                                .enumerate()
                                .filter(|(_, x)| {
                                    !prelude_only
                                        || PRELUDE_MODULES.contains(&x.map.module_name.as_str())
                                })
                                .find_map(|(index, x)| {
                                    let found = x.map.in_scope_macros.get(s);
                                    if found.is_some() {
                                        found_index = Some(index);
                                    }

                                    found
                                })
                                .or_else(|| (!prelude_only).then(|| self.map.get(s)).flatten())
                        }) {
                            // Interweave the macro expansion, unless a local hides it
                            if !self.exclusions.contains(s) {
                                let visible = self.source_id.is_none()
                                    || sp.source_id() == m.location.source_id();
                                let expands =
                                    meaning == Meaning::Unbound && (*unresolved || visible);
                                if expands {
                                    let span = *sp;

                                    let binders = UseBinders::set(&self.scope_chain);
                                    let (mut expanded, introduction) = m.expand_marked(
                                        List::new_maybe_improper(
                                            core::mem::take(&mut l.args),
                                            l.improper,
                                        ),
                                        span,
                                        &self.in_scope_values,
                                        self.globals,
                                    )?;
                                    drop(binders);
                                    // Transformer code becomes data, without the scopes
                                    if self.prelude_only {
                                        crate::parser::kernel::rename_template_binders(
                                            &mut expanded,
                                            introduction,
                                        );
                                    }
                                    self.changed = true;
                                    if body_form || head_only {
                                        self.scope_chain.body_use();
                                    }

                                    // Names a required module's macro wrote refer to where that
                                    // macro is defined
                                    if let Some(index) = found_index {
                                        self.overlays[index].changed = true;
                                    }

                                    if self.foreign || found_index.is_some() {
                                        record_foreign(introduction);
                                    }

                                    if head_only {
                                        self.head_expanded = true;
                                        *expr = expanded;
                                        return Ok(());
                                    }

                                    self.depth += 1;
                                    self.top_level = top_level;

                                    self.body_form = body_form;
                                    self.visit(&mut expanded)?;

                                    self.depth -= 1;

                                    *expr = expanded;

                                    return Ok(());
                                }
                            }

                            // let expanded = m.expand(l.clone(), *sp)?;
                            // self.changed = true;
                            // return self.visit(expanded);
                        }
                    }
                    // Some(other) => {
                    //     println!("543 - DIDN'T MATCH: {}", other);
                    //     // println!("{:?}", other);
                    // }
                    _ => {}
                }

                // A procedural macro gets its arguments unexpanded, since macros expand from the
                // outside in
                self.head_only = head_only;
                if let Some(expanded) = self.kernel_use(l, top_level, body_form, UseKind::Form)? {
                    *expr = expanded;
                    return Ok(());
                }

                if head_only {
                    return Ok(());
                }

                // Without the kernel, procedural macro uses are left for a later pass
                if self.kernel.is_none() && procedural_use(l, &self.in_scope_values) {
                    return Ok(());
                }

                // The head isn't a bare identifier use, and neither are the names a provide or
                // require lists
                let names = names_bindings(l);
                for (i, expr) in l.args.iter_mut().enumerate() {
                    if let ExprKind::Atom(a) = expr {
                        if i == 0 || names {
                            self.scope_chain.stamp(&mut a.syn);
                            continue;
                        }
                    }

                    // A head that expands to just a procedural macro's name is an identifier use
                    // of that macro, not a use of the form it heads. Keep it as written for a
                    // later pass with the kernel.
                    if i == 0 && self.kernel.is_none() && matches!(expr, ExprKind::List(_)) {
                        let written = expr.clone();
                        self.visit(expr)?;
                        if names_procedural(expr, &self.in_scope_values) {
                            *expr = written;
                        }
                        continue;
                    }

                    self.visit(expr)?;
                }

                Ok(())

                // l.args = l
                //     .args
                //     .into_iter()
                //     .map(|e| self.visit(e))
                //     .collect::<Result<Vec<_>>>()?;
            }
            ExprKind::SyntaxRules(s) => self.visit_syntax_rules(s),
            ExprKind::Set(s) => {
                self.scope_chain.stamp_expr(&mut s.variable);
                let macro_target = match &s.variable {
                    ExprKind::Atom(target) if self.names_macro(&target.syn) => {
                        Some((target.syn.ty.clone(), target.syn.span))
                    }
                    _ => None,
                };
                let Some((ty, span)) = macro_target else {
                    return self.visit_set(s);
                };

                // A set! transformer takes the whole form
                let ExprKind::Set(s) = core::mem::take(expr) else {
                    unreachable!()
                };
                let s = *s;
                let mut l = List::new(thin_vec::thin_vec![
                    ExprKind::Atom(Atom::new(s.location)),
                    s.variable,
                    s.expr
                ]);
                if let Some(expanded) =
                    self.kernel_use(&mut l, top_level, body_form, UseKind::Set)?
                {
                    *expr = expanded;
                    return Ok(());
                }
                stop!(BadSyntax => format!("set!: cannot mutate syntax identifier: {}", ty); span)
            }
            ExprKind::Require(r) => self.visit_require(r),
            ExprKind::Vector(v) => self.visit_vector(v),
        }
    }
}

impl<'a> VisitorMutRef for Expander<'a> {
    type Output = Result<()>;

    // Grow the stack as needed, since expansions nest as deep as macros expand into each other
    fn visit(&mut self, expr: &mut ExprKind) -> Self::Output {
        crate::compiler::passes::with_stack(|| self.visit_expr(expr))
    }

    fn visit_if(&mut self, f: &mut super::ast::If) -> Self::Output {
        self.visit(&mut f.test_expr)?;
        self.visit(&mut f.then_expr)?;
        self.visit(&mut f.else_expr)?;
        Ok(())
    }

    fn visit_define(&mut self, define: &mut super::ast::Define) -> Self::Output {
        self.scope_chain.define_expr(&mut define.name);
        if let Some(ident) = define.name.atom_identifier() {
            self.in_scope_values.define(*ident);
        }

        if let Some(kernel) = self.kernel.as_mut() {
            kernel.defined(&define.name, false, Some(define.location.syntax_object_id))?;
        }

        self.visit(&mut define.body)?;
        Ok(())
    }

    fn visit_lambda_function(
        &mut self,
        lambda_function: &mut super::ast::LambdaFunction,
    ) -> Self::Output {
        // The parameters and body get the lambda's scope, and the body (a definition context)
        // also gets one of its own
        self.scope_chain
            .enter(lambda_function.location.syntax_object_id.0 as usize, 0);
        for parameter in lambda_function.args.iter_mut() {
            match parameter {
                ExprKind::List(default) => {
                    if let Some(name) = default.args.first_mut() {
                        self.scope_chain.bind_expr(name);
                    }
                }
                parameter => self.scope_chain.bind_expr(parameter),
            }
        }
        duplicate_binder(
            lambda_function.args.iter().map(|x| match x {
                ExprKind::List(default) => default.args.first().unwrap_or(x),
                x => x,
            }),
            "lambda: duplicate argument name",
        )?;
        self.scope_chain
            .enter(lambda_function.location.syntax_object_id.0 as usize, 1);
        self.scope_chain
            .stamp_local_macro_definitions([&mut lambda_function.body]);

        self.in_scope_values.push_layer();

        for value in &lambda_function.args {
            if let Some(ident) = parameter_name(value) {
                self.in_scope_values.define(*ident);
            }
        }

        // Enter the kernel's locals too, before visiting the parameters' defaults
        let hidden = self
            .kernel
            .as_mut()
            .map(|kernel| kernel.enter_scope(core::slice::from_ref(&lambda_function.body)));

        // Visit a parameter's default, not its name
        for value in &mut lambda_function.args {
            if !matches!(value, ExprKind::Atom(_)) {
                self.visit(value)?;
            }
        }

        let visible = self
            .local_macros
            .enter_expr(&mut lambda_function.body, &self.in_scope_values)?;

        self.body_form = true;
        self.visit(&mut lambda_function.body)?;

        if let (Some(kernel), Some(hidden)) = (self.kernel.as_mut(), hidden) {
            kernel.leave_scope(hidden);
        }

        self.local_macros.leave(visible);

        self.scope_chain.leave();
        self.scope_chain.leave();
        self.in_scope_values.pop_layer();

        Ok(())
    }

    fn visit_begin(&mut self, begin: &mut super::ast::Begin) -> Self::Output {
        self.visit_begin_forms(begin, false, false)
    }

    fn visit_return(&mut self, r: &mut super::ast::Return) -> Self::Output {
        self.visit(&mut r.expr)
    }

    fn visit_quote(&mut self, _quote: &mut super::ast::Quote) -> Self::Output {
        // println!("Visiting quote with : {:?}", quote);
        // quote.expr = self.visit(quote.expr)?;
        // Ok(ExprKind::Quote(quote))
        Ok(())
    }

    fn visit_macro(&mut self, m: &mut super::ast::Macro) -> Self::Output {
        stop!(BadSyntax => format!("unexpected macro definition in expand visitor: {}", m); m.location.span)
    }

    fn visit_atom(&mut self, _a: &mut Atom) -> Self::Output {
        // Ok(ExprKind::Atom(a))
        Ok(())
    }

    fn visit_syntax_rules(&mut self, l: &mut super::ast::SyntaxRules) -> Self::Output {
        stop!(Generic => "unexpected syntax-rules definition"; l.location.span)
    }

    fn visit_set(&mut self, s: &mut super::ast::Set) -> Self::Output {
        if let ExprKind::Atom(target) = &s.variable {
            if self.names_macro(&target.syn) {
                stop!(BadSyntax => format!("set!: cannot mutate syntax identifier: {}", target.syn.ty);
                    target.syn.span);
            }
        }
        if !matches!(s.variable, ExprKind::Atom(_)) {
            self.visit(&mut s.variable)?;
        }
        self.scope_chain.stamp_expr(&mut s.variable);
        self.visit(&mut s.expr)?;
        Ok(())
    }

    fn visit_require(&mut self, _s: &mut super::ast::Require) -> Self::Output {
        // Ok(ExprKind::Require(s))
        Ok(())
    }

    fn visit_let(&mut self, l: &mut super::ast::Let) -> Self::Output {
        self.in_scope_values.push_layer();

        for (binding, _) in l.bindings.iter_mut() {
            if let Some(ident) = binding.atom_identifier() {
                self.in_scope_values.define(*ident);
            }
        }

        for (_, expr) in l.bindings.iter_mut() {
            self.visit(expr)?;
        }

        // The binders and body get the let's scopes, not the values, as with `lambda`
        self.scope_chain
            .enter(l.location.syntax_object_id.0 as usize, 0);
        for (binder, _) in l.bindings.iter_mut() {
            self.scope_chain.bind_expr(binder);
        }
        duplicate_binder(l.bindings.iter().map(|x| &x.0), "let: duplicate identifier")?;
        self.scope_chain
            .enter(l.location.syntax_object_id.0 as usize, 1);
        self.scope_chain
            .stamp_local_macro_definitions([&mut l.body_expr]);

        let visible = self
            .local_macros
            .enter_expr(&mut l.body_expr, &self.in_scope_values)?;

        let hidden = self
            .kernel
            .as_mut()
            .map(|kernel| kernel.enter_scope(core::slice::from_ref(&l.body_expr)));

        self.body_form = true;
        self.visit(&mut l.body_expr)?;

        if let (Some(kernel), Some(hidden)) = (self.kernel.as_mut(), hidden) {
            kernel.leave_scope(hidden);
        }

        self.local_macros.leave(visible);

        self.scope_chain.leave();
        self.scope_chain.leave();
        self.in_scope_values.pop_layer();

        Ok(())
    }

    fn visit_list(&mut self, _l: &mut List) -> Self::Output {
        Ok(())
    }

    fn visit_vector(&mut self, _v: &mut super::ast::Vector) -> Self::Output {
        Ok(())
    }
}

/// The procedural macros a required module provides, keyed by the names they're required under
pub struct RequiredKernelMacros<'a> {
    pub environment: &'a str,
    pub provided: &'a FxHashMap<InternedString, InternedString>,
    pub mangler: &'a mut NameMangler,
}

// What a use's name refers to by its scopes, for the kernel
#[derive(Clone, PartialEq, Eq)]
enum ScopedDecision {
    Variable,
    // A function's procedural macro the kernel defined, by its kernel name
    Kernel(InternedString),
    // A macro a body defines, by its name and its name's scopes
    Placeholder(InternedString, steel_parser::scopes::ScopeSetId),
    // A function's syntax-rules macro, which hides a global procedural one
    Rules,
    Unbound,
}

enum KernelUse {
    // Nothing more to expand in it
    Done(ExprKind),
    // A macro's output, still to be expanded, with the marker a function's macro definition
    // leaves
    Expanded {
        expr: ExprKind,
        definition: Option<(
            InternedString,
            steel_parser::span::Span,
            steel_parser::scopes::ScopeSetId,
        )>,
    },
}

pub struct KernelExpander<'a> {
    pub(crate) map: Option<&'a mut Kernel>,
    pub(crate) changed: bool,
    builtin_modules: ModuleContainer,
    environment: Option<&'a str>,
    // The names a required module's macros are used under, mapped to the macros they name
    allowed_macros: Option<&'a FxHashMap<InternedString, InternedString>>,
    mangler: Option<&'a mut NameMangler>,
    // How many function and let bodies deep, 1 at the top
    locals: Depth,
    required: Vec<RequiredKernelMacros<'a>>,
    // The macros the enclosing functions define, with the scope depth they're defined at and
    // the kernel's name for them
    local_macros: Vec<(
        InternedString,
        usize,
        InternedString,
        steel_parser::scopes::ScopeSetId,
    )>,
    // The procedural macros the enclosing bodies define, which hide imported ones in them
    local_hidden: Vec<(InternedString, usize)>,
    // Where each of `local_macros` is defined, keyed by its kernel name
    local_definitions: FxHashMap<InternedString, steel_parser::span::Span>,
    // The variables the enclosing bodies define (not their parameters), keyed by `LocalKey`
    // and depth, with the span and form that defined them
    local_variables:
        FxHashMap<(LocalKey, usize), (steel_parser::span::Span, Option<SyntaxObjectId>)>,
}

impl<'a> KernelExpander<'a> {
    pub fn new(map: Option<&'a mut Kernel>, builtin_modules: ModuleContainer) -> Self {
        Self {
            map,
            changed: false,
            builtin_modules,
            environment: None,
            allowed_macros: None,
            mangler: None,
            locals: Depth(1),
            local_macros: Vec::new(),
            required: Vec::new(),
            local_hidden: Vec::new(),
            local_definitions: FxHashMap::default(),
            local_variables: FxHashMap::default(),
        }
    }

    // Expands a required module's macros in that module's environment, limited to `allowed`
    // when given. Their output introduces the module's names.
    pub(crate) fn for_module(
        map: Option<&'a mut Kernel>,
        environment: &'a str,
        allowed: Option<&'a FxHashMap<InternedString, InternedString>>,
        mangler: &'a mut NameMangler,
    ) -> Self {
        Self {
            environment: Some(environment),
            allowed_macros: allowed,
            mangler: Some(mangler),
            ..Self::new(map, ModuleContainer::default())
        }
    }

    // Adds the procedural macros required modules provide
    pub(crate) fn with_required(mut self, required: Vec<RequiredKernelMacros<'a>>) -> Self {
        self.required = required;
        self
    }

    // For an environment's procedural macros
    pub(crate) fn in_environment(
        map: Option<&'a mut Kernel>,
        builtin_modules: ModuleContainer,
        environment: &'a str,
    ) -> Self {
        Self {
            environment: Some(environment),
            ..Self::new(map, builtin_modules)
        }
    }

    fn leave_local_macros(&mut self) {
        let depth = self.locals.depth();
        self.local_macros.retain(|x| x.1 <= depth);
        self.local_variables.retain(|x, _| x.1 <= depth);
    }

    // Only a body's own defines are checked and recorded, not ones inside the arguments of a
    // form left unexpanded
    fn define_local(
        &mut self,
        name: &ExprKind,
        body: bool,
        form: Option<SyntaxObjectId>,
    ) -> Result<()> {
        let Some(syn) = name.atom_syntax_object() else {
            return Ok(());
        };
        let Some(key) = local_key(syn) else {
            return Ok(());
        };

        let depth = self.locals.depth();
        if depth > 1 && body {
            // A body can define a name only once, including as a macro
            if let Some(defined) = self
                .local_macros
                .iter()
                .find(|x| x.0 == key.0 && x.1 == depth && x.3.introductions() == key.1)
                .and_then(|x| self.local_definitions.get(&x.2))
            {
                let span = if defined.start > syn.span.start {
                    *defined
                } else {
                    syn.span
                };
                stop!(BadSyntax => format!("define: duplicate binding name: {}", written_name(&syn.ty.to_string())); span);
            }

            // Or as a variable. The same define visited again has the same span and form, but
            // two defines a macro made can share a name's span, so the form is checked too.
            if let Some((defined, defined_form)) = self.local_variables.get(&(key.clone(), depth)) {
                if *defined != syn.span || *defined_form != form {
                    let span = if defined.start > syn.span.start {
                        *defined
                    } else {
                        syn.span
                    };
                    stop!(BadSyntax => format!("define: duplicate binding name: {}", written_name(&syn.ty.to_string())); span);
                }
            }

            self.local_variables
                .entry((key, depth))
                .or_insert((syn.span, form));
        }

        Ok(())
    }

    // Expands a use of a procedural macro, leaving its output for the caller to expand. None
    // if it isn't one.
    fn expand_use(
        &mut self,
        s: InternedString,
        l: &mut List,
        required: &mut [RequiredKernelMacros<'_>],
        kind: UseKind,
        // What the name refers to by its scopes
        scoped: ScopedDecision,
    ) -> Result<Option<KernelUse>> {
        let identifier = kind == UseKind::Identifier;
        let form = kind == UseKind::Form;
        let named = if kind == UseKind::Set {
            l.get(1)
        } else {
            l.first()
        };
        let head = named.and_then(ExprKind::atom_syntax_object);

        let shadowed = scoped == ScopedDecision::Variable;
        // Index into `local_macros`
        let local_index = match scoped {
            ScopedDecision::Kernel(internal) => {
                self.local_macros.iter().position(|x| x.2 == internal)
            }
            ScopedDecision::Placeholder(name, ref scopes) => self
                .local_macros
                .iter()
                .position(|x| x.0 == name && x.3 == *scopes),
            _ => None,
        };
        // A macro a body defines whose definition isn't expanded yet
        let pending_local = match scoped {
            ScopedDecision::Placeholder(name, scopes) => !self
                .local_macros
                .iter()
                .any(|x| x.0 == name && x.3 == scopes),
            ScopedDecision::Rules => true,
            _ => false,
        };
        let head_scopes = head.map(|head| head.scopes.clone());
        // Written by a required module's macro, so it resolves where that macro is defined
        let foreign_name = head.is_some_and(|head| {
            self.map
                .as_ref()
                .is_some_and(|map| map.is_foreign(&head.scopes))
        });
        // The environment's own macro with this name hides an imported one
        let own_macro = !foreign_name
            && (local_index.is_some()
                || pending_local
                || self.map.as_ref().is_some_and(|map| {
                    self.environment
                        .is_some_and(|env| map.has_own_macro(env, &s))
                }));

        // A macro a function defines hides the module's macro with the same name
        let local_macro = local_index.is_some() || pending_local;

        // A module's code can name the macros it defines and imports by its prefix, private
        // ones too
        if let Some(map) = self.map.as_mut().filter(|_| !shadowed && !local_macro) {
            if let Some(expanded) = map.expand_prefixed_macro(&s, l, self.environment, kind)? {
                self.changed = true;
                return Ok(Some(KernelUse::Expanded {
                    expr: expanded,
                    definition: None,
                }));
            }
        }

        // A macro from one of the required modules, searched in the order they're required
        let index = required
            .iter()
            .position(|x| x.provided.contains_key(&s))
            .filter(|_| !shadowed && !own_macro);

        if let (Some(index), Some(map)) = (index, self.map.as_mut()) {
            let required = &mut required[index];
            let name = required.provided[&s];

            // A `set!` of it expands only if it's a set! transformer
            if map.contains_syntax_object_macro(&name, Some(required.environment))
                && (kind != UseKind::Set || map.is_set_transformer(&name))
            {
                let expanded = map.expand_syntax_object(
                    &name,
                    use_input(l, identifier),
                    required.environment,
                    Some(&mut *required.mangler),
                )?;
                self.changed = true;

                return Ok(Some(KernelUse::Expanded {
                    expr: expanded,
                    definition: None,
                }));
            }
        }

        let local_name = local_index.map(|i| self.local_macros[i].2);

        let name = match self.allowed_macros {
            _ if shadowed => None,
            _ if local_name.is_some() => local_name,
            // A macro a body here defines, whose definition isn't expanded yet
            _ if pending_local => None,
            // A foreign name doesn't refer to the environment's own macro, unless the same
            // template defined it
            _ if foreign_name
                && self.map.as_ref().is_some_and(|map| {
                    self.environment
                        .is_some_and(|env| map.has_own_macro(env, &s))
                        && !head_scopes.is_some_and(|scopes| map.defined_by_template_of(&s, scopes))
                }) =>
            {
                None
            }
            Some(allowed) => allowed.get(&s).copied(),
            None => Some(s),
        };

        // A macro a function defines is only visible in that function
        let local_macros = &self.local_macros;
        let out_of_scope = |map: &Kernel, name: &InternedString| {
            map.is_local_macro(name) && !local_macros.iter().any(|x| x.2 == *name)
        };

        if let (Some(map), Some(name)) = (&mut self.map, name) {
            if map
                .contains_syntax_object_macro(&name, self.environment.as_ref().map(|x| x.as_ref()))
                && !out_of_scope(map, &name)
                && (kind != UseKind::Set || map.is_set_transformer(&name))
            {
                let mut local_definition = None;

                // A macro defined in a function can refer to its locals
                if self.locals.depth() > 1 && name.resolve() == "define-syntax" && form {
                    let written = match l.args.get(1) {
                        Some(ExprKind::List(f)) => f.first(),
                        other => other,
                    }
                    .and_then(ExprKind::atom_syntax_object);
                    let key = written.and_then(local_key);
                    let written_scopes = written.map(|x| x.scopes.without_use_sites());
                    let defined = written.and_then(|x| match x.ty {
                        TokenType::Identifier(name) => Some(name),
                        _ => None,
                    });

                    if let Some(defined) = defined {
                        // A body can only define a name once
                        let depth = self.locals.depth();
                        let here = written.map(|x| x.span).unwrap_or(l.location);
                        let duplicate = if self
                            .local_macros
                            .iter()
                            .any(|x| Some((x.0, x.3.introductions())) == key && x.1 == depth)
                        {
                            Some(here)
                        } else {
                            // Reported at the later of the two, as Racket does
                            key.and_then(|key| self.local_variables.get(&(key, depth)))
                                .map(|(x, _)| if x.start > here.start { *x } else { here })
                        };
                        if let Some(span) = duplicate {
                            stop!(BadSyntax => format!("define-syntax: duplicate binding name: {}", written_name(&defined.to_string())); span);
                        }

                        // Give it a fresh name so it doesn't replace the module's macro of
                        // the same name
                        let internal = fresh_local_macro_name(defined);
                        let written = match l.args.get_mut(1) {
                            Some(ExprKind::List(f)) => f.args.first_mut(),
                            other => other,
                        };
                        if let Some(ExprKind::Atom(a)) = written {
                            a.syn.ty = TokenType::Identifier(internal);
                        }

                        // Names its templates write resolve where the definition is written,
                        // which may be inside the template that made the definition, not just
                        // where the name came from
                        let written_with = definition_scopes(l).without_use_sites();
                        let scopes = written_scopes
                            .clone()
                            .unwrap_or_default()
                            .union(&written_with);
                        map.define_local_macro(internal, scopes);
                        self.local_macros.push((
                            defined,
                            self.locals.depth(),
                            internal,
                            written_scopes.clone().unwrap_or_default(),
                        ));
                        // The hygiene pass finds what the macro captures by this name, which is
                        // the macro's own
                        local_definition =
                            Some((internal, l.location, written_scopes.unwrap_or_default()));
                        self.local_definitions.insert(internal, l.location);
                    }
                }

                // For a macro a template defines outside a function, names its templates write
                // resolve where that template is
                if self.locals.depth() <= 1 && name.resolve() == "define-syntax" && form {
                    let written = match l.args.get(1) {
                        Some(ExprKind::List(f)) => f.first(),
                        other => other,
                    }
                    .and_then(ExprKind::atom_identifier);
                    let scopes = definition_scopes(l).without_use_sites();
                    if let Some(defined) = written.filter(|_| scopes.introduced()) {
                        map.define_macro_scopes(*defined, scopes);
                    }
                }

                let input = use_input(l, identifier);

                let expanded = map.expand_syntax_object(
                    &name,
                    input,
                    self.environment
                        .as_ref()
                        .map(|x| x.as_ref())
                        .unwrap_or("default"),
                    self.mangler.as_deref_mut(),
                )?;
                self.changed = true;

                return Ok(Some(KernelUse::Expanded {
                    expr: expanded,
                    definition: local_definition,
                }));
            }
        }

        if s == *REQUIRE_BUILTIN && form {
            match &l.args[1..] {
                [ExprKind::Atom(Atom {
                    syn:
                        SyntaxObject {
                            ty: TokenType::StringLiteral(s),
                            span,
                            ..
                        },
                })] => {
                    // TODO: if it can't be found, the module needs to be marked as `MaybeDylib`
                    // and use the binds that are listed in the dylib require spec, something like:
                    // (require-builtin steel/obviouslydylib/sqlite (only-in ... ... ...)) <-
                    // Then, we can _attempt_ to load the dylib at runtime. If we can't we move on, and
                    // otherwise we can error if the identifiers are not lining up.
                    if let Some(module) = self.builtin_modules.get(s.resolve()) {
                        return Ok(Some(KernelUse::Done(module.to_syntax(None))));
                    } else {
                        stop!(BadSyntax => format!("require-builtin: module not found: {}", s); *span);
                    }
                }

                [ExprKind::Atom(Atom {
                    syn:
                        SyntaxObject {
                            ty: TokenType::Identifier(s),
                            span,
                            ..
                        },
                })] => {
                    if let Some(module) = self.builtin_modules.get(s.resolve()) {
                        return Ok(Some(KernelUse::Done(module.to_syntax(None))));
                    } else {
                        stop!(BadSyntax => format!("require-builtin: module not found: {}", s); *span);
                    }
                }

                [ExprKind::Atom(Atom {
                    syn:
                        SyntaxObject {
                            ty: TokenType::StringLiteral(s),
                            span,
                            ..
                        },
                }), ExprKind::Atom(Atom {
                    syn:
                        SyntaxObject {
                            ty: TokenType::Identifier(az),
                            ..
                        },
                }), ExprKind::Atom(Atom {
                    syn:
                        SyntaxObject {
                            ty: TokenType::Identifier(prefix),
                            ..
                        },
                })] if *az == *AS_KEYWORD => {
                    if let Some(module) = self.builtin_modules.get(s.resolve()) {
                        return Ok(Some(KernelUse::Done(
                            module.to_syntax(Some(prefix.resolve())),
                        )));
                    } else {
                        stop!(BadSyntax => format!("require-builtin: module not found: {}", s); *span);
                    }
                }

                [ExprKind::Atom(Atom {
                    syn:
                        SyntaxObject {
                            ty: TokenType::Identifier(s),
                            span,
                            ..
                        },
                }), ExprKind::Atom(Atom {
                    syn:
                        SyntaxObject {
                            ty: TokenType::Identifier(az),
                            ..
                        },
                }), ExprKind::Atom(Atom {
                    syn:
                        SyntaxObject {
                            ty: TokenType::Identifier(prefix),
                            ..
                        },
                })] if *az == *AS_KEYWORD => {
                    if let Some(module) = self.builtin_modules.get(s.resolve()) {
                        return Ok(Some(KernelUse::Done(
                            module.to_syntax(Some(prefix.resolve())),
                        )));
                    } else {
                        stop!(BadSyntax => format!("require-builtin: module not found: {}", s); *span);
                    }
                }

                _ => {
                    stop!(ArityMismatch => "require-builtin malformed - follows the pattern (require-builtin \"<module>\") or (require-builtin \"<module>\" as <prefix>"; l.location)
                }
            }
        }

        Ok(None)
    }

    // Enters a function or let body. The procedural macros the body defines hide others with
    // the same names in it. Returns what `leave_scope` takes.
    fn enter_scope(&mut self, body: &[ExprKind]) -> usize {
        self.locals.push_layer();

        let hidden = self.local_hidden.len();
        let mut names = Vec::new();
        for form in body {
            defined_procedural_macros(form, &mut names);
        }
        let depth = self.locals.depth();
        self.local_hidden
            .extend(names.into_iter().map(|name| (name, depth)));
        hidden
    }

    fn leave_scope(&mut self, hidden: usize) {
        self.local_hidden.truncate(hidden);
        self.locals.pop_layer();
        self.leave_local_macros();
    }

    // Whether a name could refer to a procedural macro here, before working out which one
    fn may_name_macro(&self, s: &InternedString) -> bool {
        self.local_macros.iter().any(|x| x.0 == *s)
            || self
                .map
                .as_ref()
                .is_some_and(|map| map.names_prefixed_procedural(s))
            || self.local_hidden.iter().any(|x| x.0 == *s)
            || self.required.iter().any(|x| x.provided.contains_key(s))
            || self.map.as_ref().is_some_and(|map| {
                let name = self.allowed_macros.and_then(|x| x.get(s)).unwrap_or(s);
                map.may_have_macro(name)
            })
    }

    // Records a variable a define introduces, including `f` in `(define (f ...) ...)`. At the
    // top level it replaces a procedural macro of the same name.
    fn defined(&mut self, name: &ExprKind, body: bool, form: Option<SyntaxObjectId>) -> Result<()> {
        let name = match name {
            ExprKind::List(f) => match f.args.first() {
                Some(name) => name,
                None => return Ok(()),
            },
            name => name,
        };
        self.define_local(name, body, form)?;

        if self.locals.depth() <= 1 && self.environment.as_deref() == Some("top-level") {
            if let (Some(map), Some(name)) = (self.map.as_mut(), name.atom_identifier()) {
                map.forget_macro("top-level", name);
            }
        }

        Ok(())
    }

    // Restores a function's macro from the marker an earlier pass left where it was defined
    fn revive(&mut self, expr: &ExprKind) {
        if let Some((internal, span, scopes)) = kernel_local_macro_entry(expr) {
            if !self.local_macros.iter().any(|x| x.2 == internal) {
                let written = written_kernel_local_name(internal);
                self.local_macros
                    .push((written, self.locals.depth(), internal, scopes));
                self.local_definitions.insert(internal, span);
            }
        }
    }

    // Checks a function's syntax-rules macro definition against the kernel's locals
    fn rules_defined(&mut self, l: &List) -> Result<()> {
        // A body can define a name only once, including as a macro
        if let Some(name) = l.get(1).and_then(ExprKind::atom_syntax_object) {
            let depth = self.locals.depth();
            if let Some((defined, _)) = local_key(name)
                .filter(|_| depth > 1)
                .and_then(|key| self.local_variables.get(&(key, depth)))
            {
                // Report it at the later of the two, named after that one's form
                let (span, form) = if defined.start > name.span.start {
                    (*defined, "define")
                } else {
                    (name.span, "define-syntax")
                };
                stop!(BadSyntax => format!("{form}: duplicate binding name: {}", written_name(&name.ty.to_string())); span);
            }
        }

        Ok(())
    }

    // Turns a syntax-rules definition a procedural macro produced back into a list
    fn macro_definition(&mut self, expr: &mut ExprKind) {
        let ExprKind::Macro(m) = core::mem::replace(expr, ExprKind::empty()) else {
            unreachable!()
        };
        *expr = ExprKind::List(crate::parser::tryfrom_visitor::macro_as_list(*m));
    }
}

fn expr_usize(value: usize) -> ExprKind {
    ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Number(
        NumberLiteral::Real(steel_parser::tokens::RealLiteral::Int(
            steel_parser::tokens::IntLiteral::Small(value as _),
        ))
        .into(),
    ))))
}

fn evaluates_eagerly(default: &ExprKind) -> bool {
    matches!(default, ExprKind::Atom(_) | ExprKind::Quote(_))
}

struct MentionsAny<'a> {
    names: &'a [InternedString],
    found: bool,
}

impl<'a> crate::compiler::passes::VisitorMutUnitRef<'a> for MentionsAny<'a> {
    fn visit_atom(&mut self, a: &'a Atom) {
        if let Some(ident) = a.ident() {
            self.found |= self.names.contains(ident);
        }
    }
}

fn mentions_any(expr: &ExprKind, names: &[InternedString]) -> bool {
    use crate::compiler::passes::VisitorMutUnitRef;

    let mut finder = MentionsAny {
        names,
        found: false,
    };
    finder.visit(expr);
    finder.found
}

// The position of `name` among the parameters as written in the parameter list
// e.g. (a b c), b => 1
fn written_at(parameters: &[ExprKind], name: &ExprKind) -> usize {
    let name = name.atom_identifier();
    parameters
        .iter()
        .position(|parameter| match parameter {
            ExprKind::List(l) => l.first().and_then(|x| x.atom_identifier()) == name,
            parameter => parameter.atom_identifier() == name,
        })
        .unwrap_or(usize::MAX)
}

// Requirements:
// - Don't actually implement using a hashmap. Most likely
//   not necessary, and we don't need to introduce _another_
//   allocation into a kwargs dictionary.
//
// - We can just provide this as a function where the default
//   value is provided to the plist-get function
//   if the value is not there.
//
// As a result, expansion with default arguments should look like this:
//
// (define (test-kwargs #:foo [foo 10] #:bar [bar 20])
//  (+ foo bar))
//
// (define test-kwargs (lambda args
//     (let [(foo (plist-try-get args 10))
//           (bar (plist-try-get args 20))]
//        (+ foo bar)))
//
// This gives us the ability to fetch args quickly - just one linear scan,
// no additional allocation beyond the rest args that are already getting allocated.
//
// Handling args with non defaults just means we'll just plist-get instead of plist-try-get
// with a default value. That more or less means we'll only do a handful of scans.
// Removes the procedural macro definitions written at the top level, since they're loaded
// before the code is expanded
pub(crate) fn strip_loaded_procedural_definitions(form: &mut ExprKind) {
    match form {
        ExprKind::Begin(b) => b
            .exprs
            .iter_mut()
            .for_each(strip_loaded_procedural_definitions),
        ExprKind::List(l) if is_begin(l) => l.args[1..]
            .iter_mut()
            .for_each(strip_loaded_procedural_definitions),
        ExprKind::List(l) if l.is_define_syntax() && procedural_macro_name(l).is_some() => {
            *form = ExprKind::ident("void");
        }
        _ => {}
    }
}

// Expands the keyword and default arguments of the functions in a lowered expression. A
// function takes its name from the define it's the value of.
pub(crate) fn expand_keyword_arguments(expr: &mut ExprKind) -> Result<()> {
    struct KeywordArguments {
        define_context: Option<ExprKind>,
    }

    impl VisitorMutRef for KeywordArguments {
        type Output = Result<()>;

        fn visit(&mut self, expr: &mut ExprKind) -> Self::Output {
            // Only a lambda that's the define's value has its name
            if !matches!(expr, ExprKind::LambdaFunction(_)) {
                self.define_context = None;
            }

            crate::compiler::passes::with_stack(|| match expr {
                ExprKind::If(f) => self.visit_if(f),
                ExprKind::Define(d) => self.visit_define(d),
                ExprKind::LambdaFunction(l) => self.visit_lambda_function(l),
                ExprKind::Begin(b) => self.visit_begin(b),
                ExprKind::Return(r) => self.visit_return(r),
                ExprKind::Let(l) => self.visit_let(l),
                ExprKind::List(l) => self.visit_list(l),
                ExprKind::Set(s) => self.visit_set(s),
                ExprKind::Quote(_)
                | ExprKind::Macro(_)
                | ExprKind::Atom(_)
                | ExprKind::SyntaxRules(_)
                | ExprKind::Require(_)
                | ExprKind::Vector(_) => Ok(()),
            })
        }

        fn visit_if(&mut self, f: &mut super::ast::If) -> Self::Output {
            self.visit(&mut f.test_expr)?;
            self.visit(&mut f.then_expr)?;
            self.visit(&mut f.else_expr)
        }

        fn visit_define(&mut self, define: &mut super::ast::Define) -> Self::Output {
            self.define_context = Some(define.name.clone());
            let result = self.visit(&mut define.body);
            self.define_context = None;
            result
        }

        fn visit_lambda_function(
            &mut self,
            lambda: &mut super::ast::LambdaFunction,
        ) -> Self::Output {
            // Only the define's own function has its name
            let name = self.define_context.take();
            for arg in lambda.args.iter_mut() {
                self.visit(arg)?;
            }
            self.visit(&mut lambda.body)?;
            expand_keyword_and_default_arguments(lambda, &name)
        }

        fn visit_begin(&mut self, begin: &mut super::ast::Begin) -> Self::Output {
            begin.exprs.iter_mut().try_for_each(|x| self.visit(x))
        }

        fn visit_return(&mut self, r: &mut super::ast::Return) -> Self::Output {
            self.visit(&mut r.expr)
        }

        fn visit_quote(&mut self, _: &mut super::ast::Quote) -> Self::Output {
            Ok(())
        }

        fn visit_macro(&mut self, _: &mut super::ast::Macro) -> Self::Output {
            Ok(())
        }

        fn visit_atom(&mut self, _: &mut Atom) -> Self::Output {
            Ok(())
        }

        fn visit_list(&mut self, l: &mut List) -> Self::Output {
            self.define_context = None;
            l.args.iter_mut().try_for_each(|x| self.visit(x))
        }

        fn visit_syntax_rules(&mut self, _: &mut super::ast::SyntaxRules) -> Self::Output {
            Ok(())
        }

        fn visit_set(&mut self, s: &mut super::ast::Set) -> Self::Output {
            self.visit(&mut s.expr)
        }

        fn visit_require(&mut self, _: &mut super::ast::Require) -> Self::Output {
            Ok(())
        }

        fn visit_let(&mut self, l: &mut super::ast::Let) -> Self::Output {
            self.define_context = None;
            for (_, value) in l.bindings.iter_mut() {
                self.visit(value)?;
            }
            self.visit(&mut l.body_expr)
        }

        fn visit_vector(&mut self, _: &mut super::ast::Vector) -> Self::Output {
            Ok(())
        }
    }

    KeywordArguments {
        define_context: None,
    }
    .visit(expr)
}

fn expand_keyword_and_default_arguments(
    lambda_function: &mut super::ast::LambdaFunction,
    define_name: &Option<ExprKind>,
) -> Result<()> {
    let is_rest = lambda_function.rest;

    // First, lets find the pairs of keyword args. These could be default or not.
    // The order of keyword args doesn't matter, and they can be interwoven with
    // non key word arguments.

    // kwargs - These are things that are of the format:
    // #:foo [foo 10] or #:foo foo.
    let mut keyword_map: SmallVec<[(&ExprKind, &ExprKind); 6]> = SmallVec::new();

    // A call can pass keywords before positional arguments, so when there are keywords every
    // positional argument is looked up in the arguments given
    let has_keywords = lambda_function.args.iter().any(|x| {
        matches!(
            x,
            ExprKind::Atom(Atom {
                syn: SyntaxObject {
                    ty: TokenType::Keyword(_),
                    ..
                }
            })
        )
    });

    let mut non_keyword_or_default_args: SmallVec<[ExprKind; 6]> = lambda_function
        .args
        .iter()
        .take_while(|_| !has_keywords)
        .take_while(|x| {
            !matches!(
                x,
                ExprKind::Atom(Atom {
                    syn: SyntaxObject {
                        ty: TokenType::Keyword(_),
                        ..
                    }
                }) | ExprKind::List(_)
            )
        })
        .cloned()
        .collect();

    enum MaybeDefault<'a> {
        Rest(&'a ExprKind),
        Positional(&'a ExprKind),
        DefaultArg(&'a ExprKind, &'a ExprKind),
    }

    // Fetch positional args this way as well
    let mut positional_args: Vec<MaybeDefault> = Vec::new();
    let mut iter = lambda_function.args.iter().peekable();
    let mut seen_default_or_kwarg = has_keywords;
    let mut seen_default = false;

    let mut required_positional_arg_count = 0;
    let mut optional_positional_arg_count = 0;

    let mut required_keyword_arg_count = 0;
    let mut optional_keyword_arg_count = 0;

    while let Some(next) = iter.next() {
        let is_keyword = matches!(
            next,
            ExprKind::Atom(Atom {
                syn: SyntaxObject {
                    ty: TokenType::Keyword(_),
                    ..
                }
            })
        );

        let is_default_arg = matches!(next, ExprKind::List(_));
        seen_default_or_kwarg = seen_default_or_kwarg || is_default_arg;

        if is_keyword {
            let value = iter.next();
            match value {
                Some(v) => {
                    if matches!(v, ExprKind::List(_)) {
                        optional_keyword_arg_count += 1;
                    } else {
                        required_keyword_arg_count += 1;
                    }

                    keyword_map.push((next, v));
                }
                None => {
                    stop!(BadSyntax => format!("keyword arg missing variable name: {}", next); lambda_function.location.span)
                }
            }

            seen_default_or_kwarg = true;
        } else if seen_default_or_kwarg {
            match next {
                ExprKind::Atom(a) => {
                    if is_rest && iter.peek().is_none() {
                        positional_args.push(MaybeDefault::Rest(next))
                    } else {
                        if seen_default {
                            stop!(BadSyntax => "positional arg without default found after default argument"; a.syn.span)
                        }

                        required_positional_arg_count += 1;
                        positional_args.push(MaybeDefault::Positional(next))
                    }
                }
                ExprKind::List(l) => {
                    if l.len() != 2 {
                        stop!(BadSyntax => "malformed default argument"; lambda_function.location.span)
                    }

                    seen_default = true;

                    optional_positional_arg_count += 1;
                    positional_args.push(MaybeDefault::DefaultArg(
                        l.first().unwrap(),
                        l.get(1).unwrap(),
                    ));
                }
                _ => {
                    stop!(BadSyntax => "Internal compiler error")
                }
            }
        }
    }

    // Bail out if theres no keyword args or default arguments to expand
    if keyword_map.is_empty()
        && positional_args
            .iter()
            .all(|x| matches!(x, MaybeDefault::Positional(_)))
    {
        return Ok(());
    }

    let positional_arg_iter = positional_args
        .into_iter()
        .enumerate()
        .map(|(index, x)| match x {
            MaybeDefault::Rest(p) => {
                let mut p = p.clone();
                if let Some(var) = p.atom_syntax_object_mut() {
                    var.introduced_via_macro = true;
                }

                let expression = expr_list![
                    // TODO: this should actually be the call site span? How do we anchor against
                    // the calling function? Tail calls seem to mess that up - how do we reserve that
                    // information?
                    ExprKind::ident("#%prim.plist-get-positional-arg-list",),
                    ExprKind::ident(REST_ARG),
                    expr_usize(index),
                ];

                Ok((p.clone(), expression))
            }

            MaybeDefault::Positional(p) => {
                let mut p = p.clone();
                if let Some(var) = p.atom_syntax_object_mut() {
                    var.introduced_via_macro = true;
                }

                let expression = expr_list![
                    // TODO: this should actually be the call site span? How do we anchor against
                    // the calling function? Tail calls seem to mess that up - how do we reserve that
                    // information?
                    ExprKind::ident("#%prim.plist-get-positional-arg",),
                    ExprKind::ident(REST_ARG),
                    expr_usize(index),
                ];

                Ok((p.clone(), expression))
            }
            MaybeDefault::DefaultArg(p, default_value) => {
                let mut p = p.clone();
                if let Some(var) = p.atom_syntax_object_mut() {
                    var.introduced_via_macro = true;
                }

                let expression = if evaluates_eagerly(default_value) {
                    expr_list![
                        // TODO: this should actually be the call site span? How do we anchor against
                        // the calling function? Tail calls seem to mess that up - how do we reserve that
                        // information?
                        ExprKind::ident("#%prim.plist-try-get-positional-arg",),
                        ExprKind::ident(REST_ARG),
                        expr_usize(index),
                        default_value.clone()
                    ]
                } else {
                    // The default only runs if the argument is missing
                    ExprKind::If(Box::new(ast::If::new(
                        expr_list![
                            ExprKind::ident("#%prim.plist-has-positional-arg?"),
                            ExprKind::ident(REST_ARG),
                            expr_usize(index),
                        ],
                        expr_list![
                            ExprKind::ident("#%prim.plist-get-positional-arg"),
                            ExprKind::ident(REST_ARG),
                            expr_usize(index),
                        ],
                        default_value.clone(),
                        SyntaxObject::default(TokenType::If),
                    )))
                };

                Ok((p.clone(), expression))
            }
        });

    // The keywords a call can give
    let declared_keywords: ThinVec<ExprKind> = keyword_map.iter().map(|x| x.0.clone()).collect();

    let bindings = keyword_map
        .into_iter()
        .map(|x| {
            let keyword = x.0;
            let original_var_name = x.1;
            let mut required_kwarg = false;

            // This is a bit wasteful... come back to this
            let (mut var_name, expr) = if let ExprKind::List(l) = original_var_name {
                if l.len() != 2 {
                    stop!(BadSyntax => "Missing default argument for keyword"; 
                        lambda_function.location.span)
                }

                (l[0].clone(), l[1].clone())
            } else {
                required_kwarg = true;

                (original_var_name.clone(), original_var_name.clone())
            };

            if let Some(var) = var_name.atom_syntax_object_mut() {
                var.introduced_via_macro = true;
            }

            let mut expression = if required_kwarg {
                expr_list![
                    // TODO: this should actually be the call site span? How do we anchor against
                    // the calling function? Tail calls seem to mess that up - how do we reserve that
                    // information?
                    ExprKind::ident("#%prim.plist-get-kwarg",),
                    ExprKind::ident(REST_ARG),
                    keyword.clone(),
                    ExprKind::Quote(Box::new(Quote::new(
                        define_name
                            .clone()
                            .unwrap_or_else(|| ExprKind::ident("anonymous function")),
                        lambda_function.location.clone()
                    )))
                ]
            } else if evaluates_eagerly(&expr) {
                expr_list![
                    ExprKind::ident("#%prim.plist-try-get"),
                    ExprKind::ident(REST_ARG),
                    keyword.clone(),
                    expr
                ]
            } else {
                // The default only runs if the keyword is missing
                let quoted = keyword.clone();
                ExprKind::If(Box::new(ast::If::new(
                    expr_list![
                        ExprKind::ident("#%prim.plist-has-kwarg?"),
                        ExprKind::ident(REST_ARG),
                        quoted.clone(),
                    ],
                    expr_list![
                        ExprKind::ident("#%prim.plist-try-get"),
                        ExprKind::ident(REST_ARG),
                        quoted,
                        ExprKind::bool_lit(false),
                    ],
                    expr,
                    SyntaxObject::default(TokenType::If),
                )))
            };

            if let ExprKind::List(l) = &mut expression {
                l.location = lambda_function.location.span;
            }

            Ok((var_name, expression))
        })
        .chain(positional_arg_iter)
        .collect::<Result<Vec<_>>>()?;

    // if let Some(rest_arg) =

    // TODO: Pick up all keyword args before the start
    non_keyword_or_default_args.push(ExprKind::ident(REST_ARG));

    // Note: When a default uses an earlier argument,
    // the arguments are bound one after another so that we get proper ordering.
    let names: SmallVec<[InternedString; 6]> = bindings
        .iter()
        .filter_map(|x| x.0.atom_identifier().copied())
        .collect();
    let sequential = bindings.iter().any(|x| mentions_any(&x.1, &names));

    let inner_application = if sequential {
        let mut in_order: Vec<_> = bindings.iter().collect();
        in_order.sort_by_key(|x| written_at(&lambda_function.args, &x.0));

        in_order
            .into_iter()
            .rev()
            .fold(lambda_function.body.clone(), |body, (name, value)| {
                ExprKind::List(List::new(thin_vec![
                    ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
                        thin_vec![name.clone()],
                        body,
                        SyntaxObject::default(TokenType::Lambda),
                    ))),
                    value.clone(),
                ]))
            })
    } else {
        let mut inner_application =
            thin_vec![ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
                bindings.iter().map(|x| x.0.clone()).collect(),
                lambda_function.body.clone(),
                SyntaxObject::default(TokenType::Lambda),
            )))];

        inner_application.extend(bindings.iter().map(|x| x.1.clone()));

        ExprKind::List(List::new(inner_application))
    };

    // A keyword it doesn't take, or one given twice, is an error, as in Racket
    let function_name = define_name
        .clone()
        .unwrap_or_else(|| ExprKind::ident("anonymous function"));
    let keyword_check = expr_list![
        ExprKind::ident("#%prim.plist-check-keywords"),
        ExprKind::ident(REST_ARG),
        ExprKind::Quote(Box::new(Quote::new(
            ExprKind::List(List::new(declared_keywords)),
            lambda_function.location.clone(),
        ))),
        ExprKind::Quote(Box::new(Quote::new(
            function_name,
            lambda_function.location.clone(),
        ))),
    ];

    // TODO: This check has to be a little bit better.
    // For this to behave, we have to check the arity of the positional arguments,
    // as well as the non positional args. To do so, we should
    // just count the length of kwargs provided, as well as the length
    // of the positional args.
    //
    // Required positional and keyword args will count for 1 and 2 spaces
    // at the callsite, respectively.
    let arity_check_condition = ExprKind::If(Box::new(ast::If::new(
        ExprKind::List(List::new(thin_vec![
            ExprKind::ident("#%prim.plist-validate-args"),
            ExprKind::ident(REST_ARG),
            expr_usize(required_keyword_arg_count),
            expr_usize(required_positional_arg_count),
            expr_usize(optional_keyword_arg_count),
            expr_usize(optional_positional_arg_count),
            ExprKind::bool_lit(is_rest),
        ])),
        inner_application,
        expr_list![
            ExprKind::ident("#%prim.error-with-span"),
            expr_list![ExprKind::ident("#%prim.current-function-span")],
            ExprKind::string_lit(format!(
                "Optional args - arity mismatch: {}",
                define_name
                    .clone()
                    .unwrap_or_else(|| ExprKind::ident("anonymous function")),
            ))
        ],
        SyntaxObject::default(TokenType::If),
    )));

    // Check keywords before arity, so an unexpected keyword is what gets reported
    let arity_check_condition = ExprKind::If(Box::new(ast::If::new(
        keyword_check,
        arity_check_condition,
        ExprKind::bool_lit(false),
        SyntaxObject::default(TokenType::If),
    )));

    *lambda_function = LambdaFunction::new_with_rest_arg(
        non_keyword_or_default_args.into_iter().collect(),
        arity_check_condition,
        SyntaxObject::default(TokenType::Lambda),
    );

    Ok(())
}

// VisitorMutRef
fn _define_quoted_ast_node(ast_name: ExprKind, expanded_expr: &ExprKind) -> ExprKind {
    ExprKind::Define(Box::new(Define::new(
        ast_name,
        ExprKind::Quote(Box::new(Quote::new(
            expanded_expr.clone(),
            SyntaxObject::default(TokenType::Quote),
        ))),
        SyntaxObject::default(TokenType::Define),
    )))
}

#[cfg(test)]
mod expansion_tests {
    use steel_parser::span::Span;

    use super::*;

    use crate::parser::expander::MacroCase;
    use crate::parser::expander::MacroPattern;

    use crate::parser::ast::{Begin, If, List};
    use crate::parser::expander::PatternList;
    use crate::parser::tokens::TokenType;

    fn atom_identifier(s: &str) -> ExprKind {
        ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Identifier(
            s.into(),
        ))))
    }

    fn ellipses() -> ExprKind {
        ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Ellipses)))
    }

    #[test]
    fn test_basic_expansion() {
        // (define-syntax when
        //     (syntax-rules ()
        //       [(when a b ...)
        //        (if a (begin b ...) void)]))
        let m = SteelMacro::new(
            "when".into(),
            Vec::new(),
            vec![MacroCase::new(
                PatternList::new(vec![
                    MacroPattern::Syntax("when".into(), false),
                    MacroPattern::Single("a".into()),
                    MacroPattern::Many(MacroPattern::Single("b".into()).into()),
                ]),
                If::new(
                    atom_identifier("a"),
                    Begin::new(
                        vec![atom_identifier("b"), ellipses()],
                        SyntaxObject::default(TokenType::Begin),
                    )
                    .into(),
                    atom_identifier("void"),
                    SyntaxObject::default(TokenType::If),
                )
                .into(),
            )],
            Span::default(),
        );

        let mut map = FxHashMap::default();
        map.insert("when".into(), m);

        let mut input: ExprKind = List::new(thin_vec![
            atom_identifier("when"),
            atom_identifier("blagh"),
            atom_identifier("do-thing"),
        ])
        .into();

        let expected: ExprKind = If::new(
            atom_identifier("blagh"),
            Begin::new(
                vec![atom_identifier("do-thing")],
                SyntaxObject::default(TokenType::Begin),
            )
            .into(),
            atom_identifier("void"),
            SyntaxObject::default(TokenType::If),
        )
        .into();

        expand(&mut input, &map, GlobalMap::Set(&FxHashSet::default())).unwrap();

        assert_eq!(expected, input)
    }
}

// Two binders of one form that are the same identifier, the same name with the same scopes,
// as Racket reports
fn duplicate_binder<'a>(binders: impl Iterator<Item = &'a ExprKind>, message: &str) -> Result<()> {
    let mut seen: Vec<&SyntaxObject> = Vec::new();
    for binder in binders {
        let Some(syn) = binder.atom_syntax_object() else {
            continue;
        };
        let Some(name) = syn.ty.identifier() else {
            continue;
        };
        if seen
            .iter()
            .any(|x| x.ty.identifier() == Some(name) && x.scopes == syn.scopes)
        {
            stop!(BadSyntax => format!("{}: {}", message, written_name(name.resolve())); syn.span);
        }
        seen.push(syn);
    }
    Ok(())
}
