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
use crate::parser::parser::{ExpansionMark, SyntaxObject};
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

// The name of a parameter, which can be `[name default]` too
// The syntax of a parameter's name
fn parameter_syntax(param: &ExprKind) -> Option<&SyntaxObject> {
    match param {
        ExprKind::List(l) => l.first().and_then(ExprKind::atom_syntax_object),
        param => param.atom_syntax_object(),
    }
}

fn binder_name(syn: &SyntaxObject) -> Option<InternedString> {
    match &syn.ty {
        TokenType::Identifier(name) => Some(*name),
        _ => None,
    }
}

// A local of the kernel expander. One a macro introduced only hides what the same expansion
// wrote, so it's kept with its mark.
fn kernel_local_key(syn: &SyntaxObject) -> Option<InternedString> {
    let name = binder_name(syn)?;

    if syn.mark == ExpansionMark::NONE {
        Some(name)
    } else {
        // Made once for each name and mark, as every binder a template wrote needs it
        thread_local! {
            static KEYS: core::cell::RefCell<FxHashMap<(InternedString, u32), InternedString>> =
                core::cell::RefCell::new(FxHashMap::default());
        }
        Some(KEYS.with(|keys| {
            *keys
                .borrow_mut()
                .entry((name, syn.mark.0))
                .or_insert_with(|| format!("{}#%mark{}", name.resolve(), syn.mark.0).into())
        }))
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
        // A begin at the top level that defines macros splices into the top level, so its
        // forms see the macros defined before them
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

// The names of the macros it defined
pub(crate) fn define_generated_macros(
    expr: &mut ExprKind,
    macro_map: &mut FxHashMap<InternedString, SteelMacro>,
    globals: GlobalMap,
) -> Result<Vec<InternedString>> {
    rename_template_macros(core::slice::from_mut(expr));

    let mut names = Vec::new();

    // The code after a definition uses the macro
    while define_generated_macro_forms(expr, macro_map, &mut names)? {
        expand(expr, macro_map, globals)?;
        rename_template_macros(core::slice::from_mut(expr));
    }

    Ok(names)
}

// Whether it defined any
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

// The names of the macros a form defines at its top, in begins too
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

// The names of the procedural macros a form defines at its top, in begins too
// The macros a body defines, of either kind, with the mark of each name. The kernel leaves a
// use of a syntax-rules one to the syntax-rules expander.
fn defined_procedural_macros(expr: &ExprKind, names: &mut Vec<(InternedString, ExpansionMark)>) {
    match expr {
        ExprKind::Begin(b) => b
            .exprs
            .iter()
            .for_each(|x| defined_procedural_macros(x, names)),
        // A syntax-rules definition already parsed
        ExprKind::Macro(m) => {
            if let Some(syn) = m.name.atom_syntax_object() {
                if let TokenType::Identifier(name) = syn.ty {
                    names.push((name, syn.mark));
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
                    names.push((name, syn.mark));
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

// The template marks in an expression
fn template_marks(expr: &ExprKind) -> FxHashSet<ExpansionMark> {
    struct Marks(FxHashSet<ExpansionMark>);

    impl<'a> crate::compiler::passes::VisitorMutUnitRef<'a> for Marks {
        fn visit_atom(&mut self, a: &'a Atom) {
            if a.syn.mark.is_template() {
                self.0.insert(a.syn.mark);
            }
        }
    }

    let mut marks = Marks(FxHashSet::default());
    crate::compiler::passes::VisitorMutUnitRef::visit(&mut marks, expr);
    marks.0
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
    // Procedural macros so far, which a syntax-rules macro of the same name replaces from
    // where it is
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
                        // Its free names mean the globals, where it's defined
                        generated_macro.mark_definition_resolved();
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

// How deep macros can expand into each other, which a long `and` or `cond` needs. Compiling
// grows its stack where it can.
#[cfg(feature = "stacker")]
pub(crate) const MAX_EXPANSION_DEPTH: usize = 32768;
#[cfg(not(feature = "stacker"))]
pub(crate) const MAX_EXPANSION_DEPTH: usize = 512;

// How many expansions the first pass over a body form makes before it leaves the rest to the
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
        prelude_base: 0,
    };
    expander.visit(expr)?;

    Ok(expander.in_scope_values)
}

// `expand`, saying whether any macro expanded
// Expands both kinds of macros, each where it's used: the syntax-rules ones of `map`, and the
// kernel's
pub(crate) fn expand_with_kernel(
    expr: &mut ExprKind,
    map: &FxHashMap<InternedString, SteelMacro>,
    globals: GlobalMap,
    kernel: KernelExpander<'_>,
) -> Result<bool> {
    expand_with_kernel_defining(expr, map, globals, kernel, true)
}

// The same, with the procedural macros the top level defines left for later when not
// `define_top_level`
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

// Expands a required module's syntax-rules macros, whose templates mean names where the
// module is. Whether anything expanded.
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

// The name a define-syntax form gives a macro the kernel expands
// Where the name of a procedural macro definition is written
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

fn defines_local_macro(l: &List) -> bool {
    l.is_define_syntax()
        && l.get(2)
            .and_then(ExprKind::list)
            .is_some_and(List::is_syntax_rules)
}

fn template_macro_names(forms: &[ExprKind], names: &mut Vec<(InternedString, ExpansionMark)>) {
    for form in forms {
        match form {
            // A procedural one too, as `(define-syntax (name stx) ...)` or `(define-syntax name ...)`
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
                        // A name from the macro's input isn't the template's
                        if syn.mark.is_template()
                            && syn.unresolved
                            && !name.resolve().starts_with("##")
                        {
                            names.push((name, syn.mark));
                        }
                    }
                }
            }
            ExprKind::Begin(b) => template_macro_names(&b.exprs, names),
            ExprKind::List(l) if is_begin(l) => template_macro_names(&l.args[1..], names),
            _ => {}
        }
    }
}

struct RenameTemplateMacros<'a> {
    names: &'a [(InternedString, ExpansionMark)],
}

impl VisitorMutRefUnit for RenameTemplateMacros<'_> {
    fn visit_atom(&mut self, a: &mut Atom) {
        let mark = a.syn.mark;

        if let TokenType::Identifier(name) = &mut a.syn.ty {
            if self.names.contains(&(*name, mark)) {
                let written = name.resolve();
                *name = format!("##{}#{}", written.trim_start_matches("##"), mark.0).into();
            }
        }
    }

    fn visit_quote(&mut self, _quote: &mut Quote) {}
}

// The variables a template defines in begin-for-syntax forms it made, which are its own
// for each expansion
pub(crate) fn rename_template_definitions(forms: &mut [ExprKind]) {
    fn collect(form: &ExprKind, names: &mut Vec<(InternedString, ExpansionMark)>) {
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

    fn collect_name(name: &ExprKind, names: &mut Vec<(InternedString, ExpansionMark)>) {
        if let Some(syn) = name.atom_syntax_object() {
            if let TokenType::Identifier(name) = syn.ty {
                // syntax-rules writes the names its template binds with a `##`
                if syn.mark.is_template() && name.resolve().starts_with("##") {
                    names.push((name, syn.mark));
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

pub(crate) fn rename_template_macros(forms: &mut [ExprKind]) {
    let mut names = Vec::new();
    template_macro_names(forms, &mut names);

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
    // Scopes open where the macro was defined
    depth: usize,
    // The mark of its name, from the expansion that defined it if a macro did
    mark: ExpansionMark,
    // Where its name is written, which tells two definitions of a name apart
    name_span: Option<steel_parser::span::Span>,
    // Where a procedural one is defined, which what the kernel expanded it to names
    definition: Option<steel_parser::span::Span>,
    // The local macros defined with it, as a range of the stack, which its templates see
    // unless it's from let-syntax
    group: (usize, usize),
    recursive: bool,
}

thread_local! {
    // Each expansion of a local macro, by the mark its template's identifiers get, with that
    // macro's name and where it's defined. What a template wrote means the local macros
    // visible where the macro is defined, and every expansion pass needs to know that.
    static LOCAL_EXPANSIONS: core::cell::RefCell<
        FxHashMap<ExpansionMark, (InternedString, steel_parser::span::Span)>,
    > = core::cell::RefCell::new(FxHashMap::default());

    // How many compiles are running on this thread, one inside another when transformer code
    // evaluates code. What the outermost one recorded isn't needed once it's done.
    static COMPILES: core::cell::Cell<usize> = const { core::cell::Cell::new(0) };

    // The marks of what required modules' procedural macros introduced where they're used.
    // Those names mean what they do where the macros are defined.
    static FOREIGN_MARKS: core::cell::RefCell<FxHashSet<ExpansionMark>> =
        core::cell::RefCell::new(FxHashSet::default());

    // The procedural macros the code being expanded can use, which get their arguments as
    // they're written
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

// The procedural macros of the code being expanded, until it's dropped
// The name the kernel has a macro a function defines by
fn fresh_local_macro_name(name: InternedString) -> InternedString {
    static COUNT: core::sync::atomic::AtomicUsize = core::sync::atomic::AtomicUsize::new(0);
    let n = COUNT.fetch_add(1, core::sync::atomic::Ordering::Relaxed);
    InternedString::from(format!("{name}%local{n}"))
}

pub(crate) struct ProceduralHeads(FxHashSet<InternedString>);

impl ProceduralHeads {
    // With those the forms define themselves
    pub(crate) fn set(mut names: FxHashSet<InternedString>, forms: &[ExprKind]) -> Self {
        for form in forms {
            defined_procedural_macro_names(form, &mut names);
        }

        ProceduralHeads(PROCEDURAL_HEADS.with(|x| x.replace(names)))
    }

    // The macros a form defined, once it's expanded
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

// A use of a procedural macro, which the syntax-rules expanders leave for the kernel
fn procedural_use(l: &List, in_scope: &ScopeSet<InternedString, FxBuildHasher>) -> bool {
    let Some(ExprKind::Atom(Atom { syn })) = l.first() else {
        return false;
    };

    let TokenType::Identifier(s) = &syn.ty else {
        return false;
    };

    (syn.unresolved || !in_scope.contains(s)) && PROCEDURAL_HEADS.with(|x| x.borrow().contains(s))
}

// The function's syntax-rules macro whose expansion has this mark, by name and where it's
// defined
fn local_expansion(mark: ExpansionMark) -> Option<(InternedString, steel_parser::span::Span)> {
    LOCAL_EXPANSIONS.with(|x| x.borrow().get(&mark).copied())
}

pub(crate) fn record_foreign_mark(mark: ExpansionMark) {
    FOREIGN_MARKS.with(|x| x.borrow_mut().insert(mark));
}

pub(crate) fn is_foreign_mark(mark: ExpansionMark) -> bool {
    FOREIGN_MARKS.with(|x| x.borrow().contains(&mark))
}

// A mark that resolves as `outer` does, where `outer` is an expansion of a local macro
pub(crate) fn inherit_local_expansion(outer: ExpansionMark, fresh: ExpansionMark) {
    LOCAL_EXPANSIONS.with(|x| {
        let mut x = x.borrow_mut();
        if let Some(defining) = x.get(&outer).copied() {
            x.insert(fresh, defining);
        }
    });
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

        if outermost {
            LOCAL_EXPANSIONS.with(|x| x.borrow_mut().clear());
            FOREIGN_MARKS.with(|x| x.borrow_mut().clear());
        }
    }
}

static LOCAL_MACRO_CAPTURE: &str = "##local-macro##";

static LOCAL_MACRO_CAPTURES: AtomicBool = AtomicBool::new(false);

pub(crate) fn local_macro_capture(
    macro_name: &InternedString,
    name: &InternedString,
) -> InternedString {
    LOCAL_MACRO_CAPTURES.store(true, Ordering::Relaxed);

    let name = name.resolve();

    format!(
        "{}{}#{}{}",
        LOCAL_MACRO_CAPTURE,
        name.len(),
        name,
        macro_name.resolve()
    )
    .into()
}

pub(crate) fn local_macro_captured(name: &str) -> Option<(&str, &str)> {
    let (len, rest) = name.strip_prefix(LOCAL_MACRO_CAPTURE)?.split_once('#')?;
    let len = len.parse::<usize>().ok()?;

    Some((rest.get(len..)?, rest.get(..len)?))
}

pub(crate) fn any_local_macro_captures() -> bool {
    LOCAL_MACRO_CAPTURES.load(Ordering::Relaxed)
}

static MACRO_REDEFINITION: &str = "#%macro-redefinition:";

// Where code defines a macro again. The forms after it use the new definition, the ones
// before it the earlier one.
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

// The macros code defines a second time or more, with markers where they're defined
#[derive(Default)]
pub(crate) struct MacroRedefinitions {
    defined: FxHashSet<InternedString>,
    macros: Vec<Option<SteelMacro>>,
}

impl MacroRedefinitions {
    // Defines a macro, or keeps it for its marker when the code defined it already
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

    // Defines the macro of a marker, which is then no code at all
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

    // What isn't defined yet, once the code is done
    pub(crate) fn finish(&mut self, map: &mut FxHashMap<InternedString, SteelMacro>) {
        for m in self.macros.iter_mut().filter_map(Option::take) {
            let name = *m.name();
            define_macro(map, name, m);
        }
    }
}

// Where a definition of the same name keeps a prelude macro
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

// Left where a function defines a macro the kernel expands, so the hygiene pass knows
// which of the function's locals the macro can see
fn kernel_local_macro_marker(
    name: InternedString,
    span: steel_parser::span::Span,
    mark: ExpansionMark,
) -> ExprKind {
    let mut name = SyntaxObject::new(TokenType::Identifier(name), span);
    name.mark = mark;
    ExprKind::Quote(Box::new(Quote::new(
        ExprKind::List(List::new(thin_vec![
            ExprKind::ident_with_span(KERNEL_LOCAL_MACRO, span),
            ExprKind::Atom(Atom::new(name)),
        ])),
        SyntaxObject::new(TokenType::Quote, span),
    )))
}

// The name a function's macro is written with, from the kernel's name for it
fn written_kernel_local_name(internal: InternedString) -> InternedString {
    internal
        .resolve()
        .rsplit_once("%local")
        .map_or(internal, |(written, _)| written.into())
}

// The macro a marker is left for, by its kernel name, its name's mark and where it's defined
fn kernel_local_macro_entry(
    expr: &ExprKind,
) -> Option<(InternedString, ExpansionMark, steel_parser::span::Span)> {
    let ExprKind::Quote(q) = expr else {
        return None;
    };

    match q.expr.list()?.args.as_slice() {
        [head, name] if head.atom_identifier()?.resolve() == KERNEL_LOCAL_MACRO => {
            let syn = name.atom_syntax_object()?;
            match syn.ty {
                TokenType::Identifier(name) => Some((name, syn.mark, syn.span)),
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

struct CaptureLocals<'a> {
    scope: &'a ScopeSet<InternedString, FxBuildHasher>,
    body_defines: &'a [InternedString],
    macro_name: InternedString,
    // The procedural macros of the bodies around here, by name and depth, which hide the
    // variables of their names in scope where they're defined
    procedural: &'a [(InternedString, usize)],
}

impl VisitorMutRefUnit for CaptureLocals<'_> {
    fn visit_atom(&mut self, a: &mut Atom) {
        // Another macro's template wrote it, so it means what it does there, not a local here
        if !a.syn.unresolved || a.syn.mark.is_template() {
            return;
        }

        let TokenType::Identifier(name) = &mut a.syn.ty else {
            return;
        };

        // Height 0 is the top level
        let height = self.scope.height_of(name);
        let hidden = self
            .procedural
            .iter()
            .any(|(macro_name, depth)| macro_name == name && height.is_some_and(|h| h < *depth));
        let local = !hidden
            && (height.is_some_and(|height| height > 0)
                || self.scope.depth() > 1 && self.body_defines.contains(name));

        if local {
            *name = local_macro_capture(&self.macro_name, name);
            a.syn.unresolved = false;
        }
    }
}

// What a define defines by: its name, or the signature that starts with it, through a curried
// define's `((f a) b)`
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

// `(define ((f a) b) body ...)` as `(define (f a) (lambda (b) body ...))`
pub(crate) fn uncurry_define(l: &mut List) {
    while let Some(ExprKind::List(signature)) = l.args.get(1) {
        // One without a body is the parser's to report
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

fn body_defines(forms: &[ExprKind], names: &mut Vec<InternedString>) {
    for form in forms {
        match form {
            ExprKind::Define(d) => names.extend(d.name.atom_identifier().copied()),
            ExprKind::Begin(b) => body_defines(&b.exprs, names),
            ExprKind::List(l) if is_begin(l) => body_defines(&l.args[1..], names),
            ExprKind::List(l)
                if matches!(
                    l.first(),
                    Some(ExprKind::Atom(Atom {
                        syn: SyntaxObject {
                            ty: TokenType::Define,
                            ..
                        },
                    }))
                ) =>
            {
                match l.get(1).map(define_target) {
                    Some(ExprKind::List(signature)) => {
                        names.extend(signature.first_ident().copied())
                    }
                    Some(name) => names.extend(name.atom_identifier().copied()),
                    None => {}
                }
            }
            _ => {}
        }
    }
}

impl LocalMacro {
    fn shadowed(&self, scope: &ScopeSet<InternedString, FxBuildHasher>) -> bool {
        scope
            .height_of(&self.name)
            .is_some_and(|height| height >= self.depth)
    }

    fn expand(
        &self,
        expr: List,
        span: steel_parser::span::Span,
        scope: &ScopeSet<InternedString, FxBuildHasher>,
        globals: GlobalMap,
    ) -> Result<(ExprKind, ExpansionMark)> {
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

// let-syntax starts the body that defines its macros with `'#%let-syntax`
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

// A body form that defines a macro, written or as a begin of them
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
    fn find(&self, name: &InternedString) -> Option<&LocalMacro> {
        self.macros.iter().rev().find(|x| x.name == *name)
    }

    // What a macro defined outside any function introduces only means the local macros
    // that same expansion defines
    fn visible(defined: ExpansionMark, used: ExpansionMark) -> bool {
        (!used.is_definition_resolved() || defined == used)
            // One a template named is only for what templates wrote
            && !(used == ExpansionMark::NONE && defined != ExpansionMark::NONE)
    }

    // One defined from `base` on, not one around it of the same name
    fn find_marked_since(
        &self,
        name: &InternedString,
        mark: ExpansionMark,
        base: usize,
    ) -> Option<&LocalMacro> {
        let found = self.find_marked(name, mark)?;
        let index = self.macros.iter().position(|x| core::ptr::eq(x, found))?;
        (index >= base).then_some(found)
    }

    fn find_marked(&self, name: &InternedString, mark: ExpansionMark) -> Option<&LocalMacro> {
        let limit = self.visible_to(mark);

        self.macros
            .iter()
            .enumerate()
            .rev()
            .find(|(i, x)| {
                x.name == *name
                    && Self::visible(x.mark, mark)
                    // Or one that same expansion defines
                    && (*i < limit || x.mark == mark)
            })
            .map(|(_, x)| x)
    }

    // How many local macros what a local macro's template wrote sees: those defined where
    // that macro is, with it and the others it's defined with unless it's from let-syntax
    fn visible_to(&self, mark: ExpansionMark) -> usize {
        if !mark.is_template() {
            return self.macros.len();
        }

        let Some((name, span)) = LOCAL_EXPANSIONS.with(|x| x.borrow().get(&mark).copied()) else {
            return self.macros.len();
        };

        // A module's names are mangled after an earlier pass defined them
        let name = unmangled(name.resolve());

        self.macros
            .iter()
            .rev()
            .find(|x| {
                unmangled(x.name.resolve()) == name
                    && x.expander.as_ref().map(|x| x.span()).or(x.definition) == Some(span)
            })
            .map_or(self.macros.len(), |x| {
                if x.recursive {
                    x.group.1
                } else {
                    x.group.0
                }
            })
    }

    fn expanded(
        &mut self,
        local: (InternedString, Option<steel_parser::span::Span>),
        mark: ExpansionMark,
    ) {
        if let (name, Some(span)) = local {
            LOCAL_EXPANSIONS.with(|x| x.borrow_mut().insert(mark, (name, span)));
        }
    }

    // Whether a procedural macro of a body is the innermost macro of this name, so it isn't
    // expanded here
    fn procedural_hides(&self, name: &InternedString, mark: ExpansionMark) -> bool {
        self.find_marked(name, mark)
            .is_some_and(|x| x.expander.is_none())
    }

    fn define_from<B: BodyForms>(
        &mut self,
        body: &mut B,
        start: usize,
        depth: usize,
    ) -> Result<()> {
        rename_template_macros(body.forms().get_mut(start..).unwrap_or_default());
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

                    // Already defined by the body this begin is part of
                    let defined = l
                        .get(1)
                        .and_then(ExprKind::atom_identifier)
                        .and_then(|x| self.find(x).filter(|m| m.depth == depth));

                    if let Some(defined) = defined {
                        if defined.name_span == name_span
                            || defined.expander.as_ref().map(|x| x.span()) == span
                                && defined.name_span.is_none()
                        {
                            continue;
                        }

                        // A body defines a name once
                        stop!(BadSyntax => format!("define-syntax: duplicate binding name: {}", defined.name);
                            name_span.unwrap_or_default());
                    }

                    if let ExprKind::Macro(m) =
                        lower_macro_and_require_definitions(ExprKind::List(l.clone()))?
                    {
                        let mark = l
                            .get(1)
                            .and_then(ExprKind::atom_syntax_object)
                            .map_or(ExpansionMark::NONE, |x| x.mark);

                        let expander = SteelMacro::parse_from_ast_macro(m)?;

                        self.macros.push(LocalMacro {
                            name: *expander.name(),
                            expander: Some(expander),
                            depth,
                            mark,
                            name_span,
                            definition: None,
                            group: (0, 0),
                            recursive: true,
                        });
                    }
                }
                ExprKind::List(l) if l.is_define_syntax() => {
                    if let Some(name) = procedural_macro_name(l) {
                        let mark = match l.get(1) {
                            Some(ExprKind::List(f)) => f.first(),
                            other => other,
                        }
                        .and_then(ExprKind::atom_syntax_object)
                        .map_or(ExpansionMark::NONE, |x| x.mark);

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
                                mark,
                                name_span: None,
                                definition: Some(l.location),
                                group: (0, 0),
                                recursive: true,
                            });
                        }
                    }
                }
                // Where the kernel defined one in an earlier pass
                form if kernel_local_macro_entry(form).is_some() => {
                    let (internal, mark, span) = kernel_local_macro_entry(form).unwrap();
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
                            mark,
                            name_span: None,
                            definition: Some(span),
                            group: (0, 0),
                            recursive: true,
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

    // A begin's forms are part of the body around it, so the macros it defines stay
    // until that body ends, and the body is what needs an expression after them
    fn splice<B: BodyForms>(
        &mut self,
        body: &mut B,
        start: usize,
        scope: &ScopeSet<InternedString, FxBuildHasher>,
    ) -> Result<()> {
        self.define_in(body, start, scope, false).map(|_| ())
    }

    fn procedural_macros(&self) -> Vec<(InternedString, usize)> {
        self.macros
            .iter()
            .filter(|x| x.expander.is_none())
            .map(|x| (x.name, x.depth))
            .collect()
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

        let recursive = !body.forms().get(start).is_some_and(is_let_syntax_marker);
        let mut group = (visible, self.macros.len());

        // What a begin in a body defines joins the macros that body defines, which can
        // refer to them
        if !needs_expression {
            let depth = scope.depth();
            let enclosing = self.macros[..visible]
                .iter()
                .rev()
                .find(|x| x.depth == depth && x.recursive && x.group.1 == visible)
                .map(|x| x.group);

            if let Some(enclosing) = enclosing {
                group = (enclosing.0, self.macros.len());

                for local in &mut self.macros[..visible] {
                    if local.group == enclosing {
                        local.group = group;
                    }
                }
            }
        }

        for local in &mut self.macros[visible..] {
            local.group = group;
            local.recursive = recursive;
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

        let mut names = Vec::new();
        body_defines(body.forms().get(start..).unwrap_or_default(), &mut names);
        let procedural = self.procedural_macros();

        for local in &mut self.macros[visible..] {
            let Some(expander) = local.expander.as_mut() else {
                continue;
            };

            let mut capture = CaptureLocals {
                scope,
                body_defines: &names,
                macro_name: local.name,
                procedural: &procedural,
            };

            for template in expander.exprs_mut() {
                VisitorMutRefUnit::visit(&mut capture, template);
            }
        }

        Ok(visible)
    }

    // A function or let body, which can't end with a definition of a macro, whichever kind
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

    // A body form a macro expanded to a definition of a macro, which the rest of the body
    // sees. `written` is whether the form defined one before it was expanded.
    // A macro an expansion defined in a body, which captures the body's locals as a written one
    // does: those in `scope`, and the body's `defines`
    fn define_expanded(
        &mut self,
        form: &mut ExprKind,
        written: bool,
        scope: &ScopeSet<InternedString, FxBuildHasher>,
        defines: &[InternedString],
    ) -> Result<()> {
        if written || !defines_body_macro(form) {
            return Ok(());
        }

        let visible = self.macros.len();
        let mut forms = vec![form.clone()];
        self.define_from(&mut forms, 0, scope.depth())?;
        *form = forms.pop().unwrap();

        let group = (visible, self.macros.len());
        let procedural = self.procedural_macros();
        for local in &mut self.macros[visible..] {
            local.group = group;

            let Some(expander) = local.expander.as_mut() else {
                continue;
            };
            let mut capture = CaptureLocals {
                scope,
                body_defines: defines,
                macro_name: local.name,
                procedural: &procedural,
            };
            for template in expander.exprs_mut() {
                VisitorMutRefUnit::visit(&mut capture, template);
            }
        }

        Ok(())
    }
}

// What a let's body visits first, which counts toward how deep the body's expansion is
trait BodyFirst {
    fn procedural_first(&mut self, _forms: &mut [ExprKind]) -> Result<usize> {
        Ok(0)
    }

    fn end_body(&mut self, _depth: usize) {}
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
}

fn visit_let_form<V: VisitorMutRef<Output = Result<()>> + BodyFirst>(
    visitor: &mut V,
    scope: fn(&mut V) -> Option<&mut ScopeSet<InternedString, FxBuildHasher>>,
    // The name a binder is in the scope by
    key: fn(&SyntaxObject) -> Option<InternedString>,
    local_macros: fn(
        &mut V,
    ) -> Option<(&mut LocalMacros, &ScopeSet<InternedString, FxBuildHasher>)>,
    // Before each form of the body
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

                if let Some(expr) = binding.args.get_mut(0) {
                    visitor.visit(expr)?;
                }

                if let Some(expr) = binding.args.get_mut(1) {
                    visitor.visit(expr)?;
                }
            }
        }
    }

    let visible = match local_macros(visitor) {
        Some((local_macros, scope)) => {
            Some(local_macros.enter_body(&mut l.args, bindings_index + 1, scope)?)
        }
        None => None,
    };

    if let Some(body_args) = l.args.get_mut(bindings_index + 1..) {
        let depth = visitor.procedural_first(body_args)?;
        let mut defines = Vec::new();
        body_defines(body_args, &mut defines);

        for body_expr in body_args {
            let written = defines_body_macro(body_expr);
            body_form(visitor);
            visitor.visit(body_expr)?;

            if let Some((local_macros, scope)) = local_macros(visitor) {
                local_macros.define_expanded(body_expr, written, scope, &defines)?;
            }
        }

        visitor.end_body(depth);
    }

    if let Some(visible) = visible {
        if let Some((local_macros, _)) = local_macros(visitor) {
            local_macros.leave(visible);
        }
    }

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
    // The form being visited is one of a body's, where a begin's definitions are the body's
    body_form: bool,
    // The macros are a required module's, expanded for the code that required it
    foreign: bool,
    // The procedural macros, which it expands where they're used too
    pub(crate) kernel: Option<KernelExpander<'a>>,
    // Expand only the form's own macro use, once, and whether it did
    head_only: bool,
    head_expanded: bool,
    // The form is one of a body whose first pass went through it
    head_covered: bool,
    // Expanding transformer code, which only gets the prelude's macros at phase 0, and the
    // macros it defines itself: those of `local_macros` from `prelude_base` on
    prelude_only: bool,
    prelude_base: usize,
    // A procedural macro a macro made at the top level is defined where it's made, and its name
    // kept for the code that checks definitions
    pub(crate) define_top_level_procedural: bool,
    pub(crate) defined_procedural: Vec<(InternedString, steel_parser::span::Span)>,
}

#[derive(Debug)]
pub struct RequiredMacroMap<'a> {
    pub(crate) map: &'a mut RequiredModuleMacros,
    pub(crate) changed: bool,
}

// One that also expands the macros of the modules the code requires, with macros of its own a
// top level define-syntax can replace
pub type ExpanderMany<'a> = Expander<'a>;

// The macros an expander has: ones only read, or its own
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
            prelude_base: 0,
        }
    }

    // With the procedural macros too
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

        // What the code defines hides macros of those names
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
    // A use of a procedural macro, expanded where it is, with what it made expanded too
    fn kernel_use(
        &mut self,
        l: &mut List,
        top_level: bool,
        body_form: bool,
    ) -> Result<Option<ExprKind>> {
        let head_only = core::mem::take(&mut self.head_only);
        let Some(kernel) = self.kernel.as_mut() else {
            return Ok(None);
        };
        let Some(s) = l.first().and_then(|x| match x {
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

        let used = if required.is_empty() {
            let mut own = core::mem::take(&mut kernel.required);
            let used = kernel.expand_use(s, l, &mut own);
            kernel.required = own;
            used
        } else {
            kernel.expand_use(s, l, &mut required)
        };
        drop(required);

        let Some(used) = used? else {
            return Ok(None);
        };
        self.changed = true;

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

                // The definition expands to void, in the body the macro is defined in
                if let Some((name, span, mark)) = definition {
                    if let ExprKind::Atom(_) = expr {
                        expr = kernel_local_macro_marker(name, span, mark);
                    }
                }

                Ok(Some(expr))
            }
        }
    }

    // A body's procedural macro definitions, before its other forms, so they're the whole
    // body's
    fn define_procedural_first(&mut self, forms: &mut [ExprKind]) -> Result<usize> {
        // Without the kernel, the body's variables still hide macros in all of it
        if self.kernel.is_none() {
            if self.in_scope_values.depth() > 1 {
                self.define_body_variables(forms)?;
            }
            return Ok(0);
        }

        for form in forms.iter_mut().filter(|x| procedural_definition(x)) {
            self.body_form = true;
            self.visit(form)?;
        }

        // The body's variables hide macros in all of it, and one named like the body's
        // macro is a duplicate
        let in_body = self.in_body();
        if in_body {
            self.define_body_variables(forms)?;
        }

        // Then each form's macro uses, until it's a definition or an expression, so what a
        // macro defines is the whole body's too

        // The most expansions one form made, which its expansion counts as nesting
        let mut deepest = 0;
        let mut defines = Vec::new();
        let mut changed = true;

        for i in 0..forms.len() {
            if core::mem::take(&mut changed) {
                defines.clear();
                body_defines(forms, &mut defines);
            }

            let mut budget = MAX_HEAD_EXPANSIONS;
            changed = self.head_pass(&mut forms[i], false, in_body, &defines, &mut budget)?;
            deepest = deepest.max(MAX_HEAD_EXPANSIONS - budget);
        }

        Ok(deepest)
    }

    // A body form's first pass. What a macro made defines what it defines for the whole
    // body, a begin's forms in order after its own macros.
    fn head_pass(
        &mut self,
        form: &mut ExprKind,
        generated: bool,
        in_body: bool,
        defines: &[InternedString],
        budget: &mut usize,
    ) -> Result<bool> {
        let expanded = self.expand_head_from(form, budget)?;
        let generated = generated || expanded;

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
                        self.head_pass(form, generated, in_body, defines, budget)
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
            self.define_generated_macros(form, defines)?;
        }

        Ok(expanded)
    }

    // In a function's body, not at the top level
    fn in_body(&self) -> bool {
        self.kernel.as_ref().is_some_and(|x| x.locals.depth() > 1)
    }

    // The prelude's macros in transformer code, as phase 1 has them too. The code's own
    // macros are phase 0's.
    // `definition` is the define-syntax, whose forms from the third on are the transformer's
    // body when it's `(define-syntax (name stx) body ...)`
    fn expand_prelude_macros(&mut self, definition: &mut ThinVec<ExprKind>) -> Result<()> {
        let kernel = self.kernel.take();
        let prelude_only = core::mem::replace(&mut self.prelude_only, true);
        let base = core::mem::replace(&mut self.prelude_base, self.local_macros.macros.len());

        // The macros the body defines are the whole body's
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

        self.prelude_base = base;
        self.prelude_only = prelude_only;
        self.kernel = kernel;
        result
    }

    // The `#,` holes of a quasisyntax template, which are code
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

    // The syntax-rules macros a body form a macro made defines. A begin's are the body's, as
    // when it's spliced.
    fn define_generated_macros(
        &mut self,
        form: &mut ExprKind,
        defines: &[InternedString],
    ) -> Result<()> {
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
                .define_expanded(form, false, &self.in_scope_values, defines),
        }
    }

    // The variables a body defines, which hide macros of their names in all of it. One a
    // macro introduced only hides what that expansion wrote, which the kernel tells apart.
    fn define_body_variables(&mut self, forms: &[ExprKind]) -> Result<()> {
        fn written_name(name: &ExprKind) -> Option<InternedString> {
            name.atom_syntax_object()
                .filter(|x| x.mark == ExpansionMark::NONE)
                .and_then(|x| x.ty.identifier().copied())
        }

        for form in forms {
            match form {
                ExprKind::Define(d) => {
                    if let Some(name) = written_name(&d.name) {
                        self.in_scope_values.define(name);
                    }
                    if let Some(kernel) = self.kernel.as_mut() {
                        kernel.defined(&d.name, true)?;
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
                    let name = match target {
                        ExprKind::List(signature) => signature.first().and_then(written_name),
                        name => written_name(name),
                    };
                    if let Some(name) = name {
                        self.in_scope_values.define(name);
                    }
                    if let Some(kernel) = self.kernel.as_mut() {
                        kernel.defined(target, true)?;
                    }
                }
                _ => {}
            }
        }

        Ok(())
    }

    // Expand a body form's own macro use until it's something else, a begin too. One that
    // keeps making more is left for the body's expansion, which reports it.
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

        // The body's first pass went through one spliced into it
        let depth = if body_form && covered {
            0
        } else {
            self.define_procedural_first(&mut begin.exprs)?
        };
        self.depth += depth;
        let mut defines = Vec::new();
        body_defines(&begin.exprs, &mut defines);

        for expr in begin.exprs.iter_mut() {
            let written = defines_body_macro(expr);
            self.body_form = body_form;
            self.head_covered = self.kernel.is_some();
            self.visit(expr)?;

            if body_form || visible.is_some() {
                self.local_macros.define_expanded(
                    expr,
                    written,
                    &self.in_scope_values,
                    &defines,
                )?;
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
                // A syntax-rules definition a procedural macro made
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
            ExprKind::Atom(a) => self.visit_atom(a),
            ExprKind::List(l) => {
                if defines_local_macro(l) {
                    if let Some(kernel) = self.kernel.as_mut() {
                        kernel.rules_defined(l)?;
                    }
                    return Ok(());
                }

                // A macro's name can't be set
                if let (Some(kernel), Some(ExprKind::Atom(head)), Some(ExprKind::Atom(target))) =
                    (self.kernel.as_ref(), l.first(), l.get(1))
                {
                    let set = head.syn.ty == TokenType::Set
                        || head.ident().is_some_and(|x| x.resolve() == "set!");
                    if set && kernel.names_local_macro(&target.syn) {
                        stop!(BadSyntax => format!("set!: cannot mutate syntax identifier: {}", target.syn.ty);
                            target.syn.span);
                    }
                }

                // `define` written as a name, as code made from syntax objects has it, defines
                if let Some(ExprKind::Atom(Atom { syn })) = l.args.first_mut() {
                    if syn.ty == TokenType::Identifier(*DEFINE)
                        && !self.in_scope_values.contains(&DEFINE)
                    {
                        syn.ty = TokenType::Define;
                    }
                }

                // A template in transformer code is the code it makes, apart from its holes,
                // and a with-syntax pattern isn't code
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
                                let name = named
                                    .then(|| l.args.get(1).and_then(ExprKind::atom_syntax_object))
                                    .flatten();
                                let binders = match l.args.get(bindings_index) {
                                    Some(ExprKind::List(bindings)) => bindings
                                        .args
                                        .iter()
                                        .filter_map(|x| x.list())
                                        .filter_map(|x| x.first())
                                        .filter_map(ExprKind::atom_syntax_object)
                                        .collect::<Vec<_>>(),
                                    _ => Vec::new(),
                                };
                                let body = l.args.get(bindings_index + 1..).unwrap_or_default();
                                Some(kernel.enter_scope(name.into_iter().chain(binders), body))
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
                        // A begin in a body splices into it, one in an expression is its own
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

                        // The body's first pass went through one spliced into it
                        let depth = if body_form && covered {
                            0
                        } else {
                            self.define_procedural_first(&mut l.args[1..])?
                        };
                        self.depth += depth;
                        let mut defines = Vec::new();
                        body_defines(&l.args[1..], &mut defines);

                        for expr in l.args.iter_mut() {
                            let written = defines_body_macro(expr);
                            self.body_form = body_form;
                            self.head_covered = self.kernel.is_some();
                            self.visit(expr)?;

                            self.local_macros.define_expanded(
                                expr,
                                written,
                                &self.in_scope_values,
                                &defines,
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
                        // A procedural macro, which the kernel defines where it is: a function's,
                        // and one at the top level, in order with the other forms. One a macro
                        // made there is kept for the checks of what the code defines.
                        let local = self.kernel.as_ref().is_some_and(|x| x.locals.depth() > 1);
                        let made = l
                            .first()
                            .and_then(ExprKind::atom_syntax_object)
                            .is_some_and(|x| x.mark != ExpansionMark::NONE);
                        if (local || self.define_top_level_procedural)
                            && procedural_macro_name(l).is_some()
                        {
                            let defined = procedural_macro_name(l).zip(procedural_name_span(l));
                            // It replaces a syntax-rules macro of its name for the forms after it
                            if let Some(map) = self.map.macros_mut().filter(|_| top_level) {
                                forget_replaced_macro(map, l);
                            }
                            self.expand_prelude_macros(&mut l.args)?;
                            if let Some(expanded) = self.kernel_use(l, top_level, body_form)? {
                                if !local && made {
                                    self.defined_procedural.extend(defined);
                                }
                                *expr = expanded;
                                return Ok(());
                            }
                        }

                        // The name isn't a macro use, even when it names a macro
                        if let Some(arg) = l.args.get_mut(2) {
                            self.visit(arg)?;
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
                        // A function's procedural macro, which the kernel defines here, and one a
                        // macro made at the top level
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
                            if let Some(expanded) = self.kernel_use(l, top_level, body_form)? {
                                if !local {
                                    self.defined_procedural.extend(defined);
                                }
                                *expr = expanded;
                                return Ok(());
                            }
                        }

                        if let Some(arg) = l.args.get_mut(2) {
                            self.visit(arg)?;
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

                        if let Some(kernel) = self.kernel.as_mut() {
                            kernel.defined(&l.args[1], body_form)?;
                        }

                        let is_function = match &l.args[1] {
                            ExprKind::List(l) if l.first_ident().is_some() => {
                                self.in_scope_values.define(*l.first_ident().unwrap());
                                true
                            }

                            ExprKind::Atom(a) if a.ident().is_some() => {
                                self.in_scope_values.define(*a.ident().unwrap());
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
                                Some(kernel.enter_scope(
                                    signature.args.iter().skip(1).filter_map(parameter_syntax),
                                    &l.args[2..],
                                ))
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
                        let mut defines = Vec::new();
                        body_defines(&l.args[2..], &mut defines);

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
                                    &defines,
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

                        return Ok(());
                    }

                    Some(ExprKind::Atom(Atom {
                        syn:
                            SyntaxObject {
                                ty: TokenType::Identifier(s),
                                span: sp,
                                unresolved,
                                mark,
                                ..
                            },
                    })) => {
                        // A prelude template means the prelude's macro, which a macro of
                        // the same name defined here keeps under another name. So does what a
                        // required module's procedural macro wrote.
                        let mut found_index = None;
                        let prelude_template = mark.is_prelude() || is_foreign_mark(*mark);
                        let alias = (prelude_template && PRELUDE_ALIASES.load(Ordering::Relaxed))
                            .then(|| prelude_macro_alias(s))
                            .filter(|alias| self.map.contains_key(alias));
                        // Or the prelude's, where nothing here keeps it
                        let prelude_macros = (prelude_template && alias.is_none())
                            .then(crate::steel_vm::engine::default_prelude_macros);
                        let prelude_macro = prelude_macros.as_ref().and_then(|x| x.get(s));
                        let s = alias.as_ref().unwrap_or(s);

                        let prelude_only = self.prelude_only;
                        let local = if prelude_only {
                            self.local_macros
                                .find_marked_since(s, *mark, self.prelude_base)
                        } else {
                            self.local_macros.find_marked(s, *mark)
                        };

                        // At the top level a procedural macro defined before the use is the one
                        // it means, as a syntax-rules one of the name defined after it is
                        let top_level_procedural = local.is_none()
                            && self.kernel.as_ref().is_some_and(|kernel| {
                                kernel.environment == Some("top-level")
                                    && kernel
                                        .map
                                        .as_ref()
                                        .is_some_and(|map| map.has_own_macro("top-level", s))
                            });

                        if self.local_macros.procedural_hides(s, *mark) || top_level_procedural {
                            // The kernel expands it
                        } else if let Some(local) = local {
                            // A template means the macro where it's defined, not a variable
                            // of the same name where it's used
                            if *unresolved || !local.shadowed(&self.in_scope_values) {
                                let span = *sp;
                                let defining =
                                    (local.name, local.expander.as_ref().map(|x| x.span()));

                                let (mut expanded, mark) = local.expand(
                                    List::new_maybe_improper(
                                        core::mem::take(&mut l.args),
                                        l.improper,
                                    ),
                                    span,
                                    &self.in_scope_values,
                                    self.globals,
                                )?;
                                self.local_macros.expanded(defining, mark);
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
                            }
                        } else if let Some(m) = prelude_macro.or_else(|| {
                            // A required module's macro, then the code's own
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
                            // Interweave the macro expansion
                            if !self.exclusions.contains(s) {
                                // If this macro has been overwritten by any local value, respect
                                // the local binding and do not expand the macro, unless the
                                // identifier came from a macro template
                                let visible = self.source_id.is_none()
                                    || sp.source_id() == m.location.source_id();

                                if *unresolved || visible && !self.in_scope_values.contains(s) {
                                    let span = *sp;

                                    let (mut expanded, mark) = m.expand_marked(
                                        List::new_maybe_improper(
                                            core::mem::take(&mut l.args),
                                            l.improper,
                                        ),
                                        span,
                                        &self.in_scope_values,
                                        self.globals,
                                    )?;
                                    self.changed = true;

                                    // What a required module's macro wrote means names where the
                                    // macro is defined
                                    if let Some(index) = found_index {
                                        self.overlays[index].changed = true;
                                    }

                                    if self.foreign || found_index.is_some() {
                                        record_foreign_mark(mark);
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

                // A procedural macro gets its arguments as they're written, as the macros
                // expand from the outside in
                self.head_only = head_only;
                if let Some(expanded) = self.kernel_use(l, top_level, body_form)? {
                    *expr = expanded;
                    return Ok(());
                }

                if head_only {
                    return Ok(());
                }

                // Without the kernel here, it expands them later
                if self.kernel.is_none() && procedural_use(l, &self.in_scope_values) {
                    return Ok(());
                }

                for expr in l.args.iter_mut() {
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
            ExprKind::Set(s) => self.visit_set(s),
            ExprKind::Require(r) => self.visit_require(r),
            ExprKind::Vector(v) => self.visit_vector(v),
        }
    }
}

impl<'a> VisitorMutRef for Expander<'a> {
    type Output = Result<()>;

    // Expansions nest as deep as macros expand into each other
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
        if let Some(ident) = define.name.atom_identifier() {
            self.in_scope_values.define(*ident);
        }

        if let Some(kernel) = self.kernel.as_mut() {
            kernel.defined(&define.name, false)?;
        }

        self.visit(&mut define.body)?;
        Ok(())
    }

    fn visit_lambda_function(
        &mut self,
        lambda_function: &mut super::ast::LambdaFunction,
    ) -> Self::Output {
        self.in_scope_values.push_layer();

        for value in &lambda_function.args {
            if let Some(ident) = parameter_name(value) {
                self.in_scope_values.define(*ident);
            }
        }

        // The kernel's locals too, before what the parameters' defaults use
        let hidden = self.kernel.as_mut().map(|kernel| {
            kernel.enter_scope(
                lambda_function.args.iter().filter_map(parameter_syntax),
                core::slice::from_ref(&lambda_function.body),
            )
        });

        for value in &mut lambda_function.args {
            self.visit(value)?;
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
        if let (Some(kernel), ExprKind::Atom(target)) = (self.kernel.as_ref(), &s.variable) {
            if kernel.names_local_macro(&target.syn) {
                stop!(BadSyntax => format!("set!: cannot mutate syntax identifier: {}", target.syn.ty);
                    target.syn.span);
            }
        }
        self.visit(&mut s.variable)?;
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

        for (binding, expr) in l.bindings.iter_mut() {
            self.visit(binding)?;
            self.visit(expr)?;
        }

        let visible = self
            .local_macros
            .enter_expr(&mut l.body_expr, &self.in_scope_values)?;

        let hidden = self.kernel.as_mut().map(|kernel| {
            kernel.enter_scope(
                l.bindings
                    .iter()
                    .filter_map(|(x, _)| x.atom_syntax_object()),
                core::slice::from_ref(&l.body_expr),
            )
        });

        self.body_form = true;
        self.visit(&mut l.body_expr)?;

        if let (Some(kernel), Some(hidden)) = (self.kernel.as_mut(), hidden) {
            kernel.leave_scope(hidden);
        }

        self.local_macros.leave(visible);

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

/// The procedural macros a required module provides, by the names they're required as
pub struct RequiredKernelMacros<'a> {
    pub environment: &'a str,
    pub provided: &'a FxHashMap<InternedString, InternedString>,
    pub mangler: &'a mut NameMangler,
}

// A macro use the kernel expanded
enum KernelUse {
    // Nothing more to expand in it
    Done(ExprKind),
    // What a macro made, to expand, with the marker a function's macro definition leaves
    Expanded {
        expr: ExprKind,
        definition: Option<(InternedString, steel_parser::span::Span, ExpansionMark)>,
    },
}

pub struct KernelExpander<'a> {
    pub(crate) map: Option<&'a mut Kernel>,
    pub(crate) changed: bool,
    builtin_modules: ModuleContainer,
    environment: Option<&'a str>,
    // The names a required module's macros are used by, with the macros they name
    allowed_macros: Option<&'a FxHashMap<InternedString, InternedString>>,
    mangler: Option<&'a mut NameMangler>,
    // Local variables, which hide macros of the same name
    locals: ScopeSet<InternedString, FxBuildHasher>,
    required: Vec<RequiredKernelMacros<'a>>,
    // The macros the functions around here define, with the scope depth they're defined in
    // and the name the kernel has them by
    local_macros: Vec<(InternedString, usize, InternedString, ExpansionMark)>,
    // The marks what a function's macro wrote has, with which of `local_macros` it sees: those
    // where it's defined, as how many were defined before it and the scope depth it's in, whose
    // macros it sees wherever they're defined in the body
    local_expansions: FxHashMap<ExpansionMark, (usize, usize)>,
    // Which of `local_macros` a function's syntax-rules macro sees, the same way, by its name and
    // where it's defined
    rules_sees: FxHashMap<(InternedString, steel_parser::span::Span), (usize, usize)>,
    // The procedural macros the bodies around here define, which hide imported ones in them
    local_hidden: Vec<(InternedString, ExpansionMark, usize)>,
    // Where each of `local_macros` is defined, by its kernel name
    local_definitions: FxHashMap<InternedString, steel_parser::span::Span>,
    // The variables the bodies around here define, not their parameters, by key and depth,
    // with where
    local_variables: FxHashMap<(InternedString, usize), steel_parser::span::Span>,
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
            locals: ScopeSet::default(),
            local_macros: Vec::new(),
            local_expansions: FxHashMap::default(),
            rules_sees: FxHashMap::default(),
            required: Vec::new(),
            local_hidden: Vec::new(),
            local_definitions: FxHashMap::default(),
            local_variables: FxHashMap::default(),
        }
    }

    // For the macros of a required module, where it defines them: those named in `allowed`
    // when given, introducing the module's names
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

    // With the procedural macros required modules provide
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

    // An internal define is a local, a top level one isn't
    // The macros of a scope that's done
    // Whether a use whose head has `used` means a macro a function defined with a name marked
    // `defined`, at `index` in `local_macros`. As for syntax-rules locals: one a template named
    // is only for what that expansion wrote, and a written one is for written code and what the
    // function's own macros wrote, not a template of a macro from outside.
    fn sees_local(
        &self,
        defined: ExpansionMark,
        used: ExpansionMark,
        index: Option<usize>,
        depth: usize,
    ) -> bool {
        if defined != ExpansionMark::NONE {
            return defined == used;
        }

        if used == ExpansionMark::NONE {
            return true;
        }

        // What a macro of the function wrote sees those where that macro is defined
        let sees = self.local_expansions.get(&used).copied().or_else(|| {
            let (name, span) = local_expansion(used)?;
            Some(
                self.rules_sees
                    .get(&(name, span))
                    .copied()
                    .unwrap_or((usize::MAX, usize::MAX)),
            )
        });
        // Those defined before it, and any in a body it's in, as a body's macros are its whole
        // body's. One not defined yet might be, as what's a body isn't known before it's expanded.
        match (sees, index) {
            (Some((count, sees_depth)), Some(index)) => index < count || depth <= sees_depth,
            (Some(_), None) => true,
            (None, _) => false,
        }
    }

    fn leave_local_macros(&mut self) {
        let depth = self.locals.depth();
        self.local_macros.retain(|x| x.1 <= depth);
        self.local_variables.retain(|x, _| x.1 <= depth);
    }

    // Only a body's own define, not one in the arguments of a form left unexpanded, is checked
    // and kept as the body's
    fn define_local(&mut self, name: &ExprKind, body: bool) -> Result<()> {
        let Some(syn) = name.atom_syntax_object() else {
            return Ok(());
        };
        let Some(key) = kernel_local_key(syn) else {
            return Ok(());
        };

        let depth = self.locals.depth();
        if depth > 1 && !body {
            self.locals.define(key);
        } else if depth > 1 {
            // A body defines a name once, a macro of it too
            let written = syn.ty.identifier().copied();
            if let Some(defined) = self
                .local_macros
                .iter()
                .find(|x| Some(x.0) == written && x.1 == depth && x.3 == syn.mark)
                .and_then(|x| self.local_definitions.get(&x.2))
            {
                let span = if defined.start > syn.span.start {
                    *defined
                } else {
                    syn.span
                };
                stop!(BadSyntax => format!("define: duplicate binding name: {}", syn.ty); span);
            }

            // Or a variable, which the same define visited again isn't
            if let Some(defined) = self.local_variables.get(&(key, depth)) {
                if *defined != syn.span {
                    let span = if defined.start > syn.span.start {
                        *defined
                    } else {
                        syn.span
                    };
                    stop!(BadSyntax => format!("define: duplicate binding name: {}", syn.ty); span);
                }
            }

            self.locals.define(key);
            self.local_variables.entry((key, depth)).or_insert(syn.span);
        }

        Ok(())
    }

    // A use of a macro the kernel expands: the macro it means, expanded, for the caller to
    // expand what it made. None if it isn't one.
    fn expand_use(
        &mut self,
        s: InternedString,
        l: &mut List,
        required: &mut [RequiredKernelMacros<'_>],
    ) -> Result<Option<KernelUse>> {
        let head = l.first().and_then(ExprKind::atom_syntax_object);

        // A local of the same name hides the macro, unless a template wrote it
        // A local a macro introduced only hides what the same expansion wrote
        let shadowed = head.is_some_and(|head| {
            self.locals.contains(&s) && !head.unresolved && !head.mark.is_template()
                || head.mark != ExpansionMark::NONE
                    && kernel_local_key(head).is_some_and(|key| self.locals.contains(&key))
        });

        // A name a required module's macro introduced is resolved where that
        // macro is defined
        let foreign_name = head.is_some_and(|head| {
            self.map
                .as_ref()
                .is_some_and(|map| map.is_foreign_mark(head.mark))
        });

        // The innermost macro of the name a function here defines that this
        // use means
        let head_mark = head.map_or(ExpansionMark::NONE, |head| head.mark);
        let local_index = self
            .local_macros
            .iter()
            .enumerate()
            .rev()
            .find(|(i, x)| x.0 == s && self.sees_local(x.3, head_mark, Some(*i), x.1))
            .map(|(i, _)| i)
            .filter(|_| !foreign_name);
        // A macro a body defines hides a variable of its scope or one around it, its
        // parameters too, but not one an inner scope binds
        let macro_depth = local_index.map(|i| self.local_macros[i].1).or_else(|| {
            self.local_hidden
                .iter()
                .rev()
                .find(|x| x.0 == s && self.sees_local(x.1, head_mark, None, x.2))
                .map(|x| x.2)
        });
        let shadowed = shadowed
            && !head.zip(macro_depth).is_some_and(|(head, depth)| {
                let height = self
                    .locals
                    .height_of(&s)
                    .max(kernel_local_key(head).and_then(|key| self.locals.height_of(&key)));
                height.is_some_and(|height| height < depth)
            });

        // One a body here defines, before its definition is expanded
        let pending_local = !foreign_name
            && self
                .local_hidden
                .iter()
                .filter(|x| {
                    !self
                        .local_macros
                        .iter()
                        .any(|m| m.0 == x.0 && m.1 == x.2 && m.3 == x.1)
                })
                .any(|x| x.0 == s && self.sees_local(x.1, head_mark, None, x.2));

        // The environment's own macro of the name hides an imported one
        let own_macro = !foreign_name
            && (local_index.is_some()
                || pending_local
                || self.map.as_ref().is_some_and(|map| {
                    self.environment
                        .is_some_and(|env| map.has_own_macro(env, &s))
                }));

        // A macro a function defines hides the module's of its name
        let local_macro = local_index.is_some() || pending_local;

        // A module's code names the macros it defines and imports by its
        // prefix, private ones too
        if let Some(map) = self.map.as_mut().filter(|_| !shadowed && !local_macro) {
            if let Some(expanded) = map.expand_prefixed_macro(&s, l, self.environment)? {
                self.changed = true;
                return Ok(Some(KernelUse::Expanded {
                    expr: expanded,
                    definition: None,
                }));
            }
        }

        // A macro of one of the required modules, in the order they're used
        let index = required
            .iter()
            .position(|x| x.provided.contains_key(&s))
            .filter(|_| !shadowed && !own_macro);

        if let (Some(index), Some(map)) = (index, self.map.as_mut()) {
            let required = &mut required[index];
            let name = required.provided[&s];

            if map.contains_syntax_object_macro(&name, Some(required.environment)) {
                let expanded = map.expand_syntax_object(
                    &name,
                    ExprKind::List(core::mem::replace(l, List::new(ThinVec::new()))),
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
            // One a body here defines, before its definition is expanded
            _ if pending_local => None,
            // Only the environment's own macros, not the kernel's
            _ if foreign_name
                && self.map.as_ref().is_some_and(|map| {
                    self.environment
                        .is_some_and(|env| map.has_own_macro(env, &s))
                }) =>
            {
                None
            }
            Some(allowed) => allowed.get(&s).copied(),
            None => Some(s),
        };

        // A macro a function defines is only that function's
        let local_macros = &self.local_macros;
        let out_of_scope = |map: &Kernel, name: &InternedString| {
            map.is_local_macro(name) && !local_macros.iter().any(|x| x.2 == *name)
        };

        if let (Some(map), Some(name)) = (&mut self.map, name) {
            if map
                .contains_syntax_object_macro(&name, self.environment.as_ref().map(|x| x.as_ref()))
                && !out_of_scope(map, &name)
            {
                let mut local_definition = None;

                // A macro defined in a function can refer to its locals
                if self.locals.depth() > 1 && name.resolve() == "define-syntax" {
                    let written = match l.args.get(1) {
                        Some(ExprKind::List(f)) => f.first(),
                        other => other,
                    }
                    .and_then(ExprKind::atom_syntax_object);
                    let defined_mark = written.map_or(ExpansionMark::NONE, |x| x.mark);
                    let defined = written.and_then(|x| match x.ty {
                        TokenType::Identifier(name) => Some(name),
                        _ => None,
                    });

                    if let Some(defined) = defined {
                        // A body defines a name once
                        let depth = self.locals.depth();
                        let key = written.and_then(kernel_local_key);
                        let here = written.map(|x| x.span).unwrap_or(l.location);
                        let duplicate = if self
                            .local_macros
                            .iter()
                            .any(|x| x.0 == defined && x.1 == depth && x.3 == defined_mark)
                        {
                            Some(here)
                        } else {
                            // Reported at the later of the two, as Racket does
                            key.and_then(|key| self.local_variables.get(&(key, depth)))
                                .map(|x| if x.start > here.start { *x } else { here })
                        };
                        if let Some(span) = duplicate {
                            stop!(BadSyntax => format!("define-syntax: duplicate binding name: {}", defined); span);
                        }

                        // Not the body's own variables, which it doesn't need to know of
                        let locals = self
                            .locals
                            .iter()
                            .filter(|x| self.locals.height_of(*x).is_some_and(|h| h > 0))
                            .filter(|x| !self.local_variables.contains_key(&(**x, depth)))
                            // Nor one a macro the body defines hides
                            .filter(|x| {
                                let height = self.locals.height_of(*x);
                                let hides =
                                    |macro_depth: usize| height.is_some_and(|h| h < macro_depth);
                                !self.local_macros.iter().any(|m| m.0 == **x && hides(m.1))
                                    && !self.local_hidden.iter().any(|m| m.0 == **x && hides(m.2))
                            })
                            .copied()
                            .collect();

                        // Under a name of its own, so it doesn't replace a macro
                        // of the module's of that name
                        let internal = fresh_local_macro_name(defined);
                        let written = match l.args.get_mut(1) {
                            Some(ExprKind::List(f)) => f.args.first_mut(),
                            other => other,
                        };
                        if let Some(ExprKind::Atom(a)) = written {
                            a.syn.ty = TokenType::Identifier(internal);
                        }

                        map.define_local_macro(internal, locals);
                        self.local_macros.push((
                            defined,
                            self.locals.depth(),
                            internal,
                            defined_mark,
                        ));
                        // The hygiene pass finds what it captures by this name,
                        // which is the macro's own
                        local_definition = Some((internal, l.location, defined_mark));
                        self.local_definitions.insert(internal, l.location);
                    }
                }

                let input = ExprKind::List(core::mem::replace(l, List::new(ThinVec::new())));
                let input_marks = local_index.map(|_| template_marks(&input));

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

                // What a function's macro wrote sees the macros where it's
                // defined: those before it, itself, and its body's
                if let (Some(index), Some(input_marks)) = (local_index, input_marks) {
                    let own = (index + 1, self.local_macros[index].1);
                    let sees = self
                        .local_expansions
                        .get(&head_mark)
                        .map_or(own, |outer| (own.0.min(outer.0), own.1.min(outer.1)));
                    // And the syntax-rules expanders, for local macros of theirs
                    let (written, _, internal, _) = self.local_macros[index];
                    let definition = self.local_definitions.get(&internal).copied();
                    for mark in template_marks(&expanded) {
                        if !input_marks.contains(&mark) {
                            self.local_expansions.insert(mark, sees);
                            if let Some(definition) = definition {
                                LOCAL_EXPANSIONS
                                    .with(|x| x.borrow_mut().insert(mark, (written, definition)));
                            }
                        }
                    }
                }

                return Ok(Some(KernelUse::Expanded {
                    expr: expanded,
                    definition: local_definition,
                }));
            }
        }

        if s == *REQUIRE_BUILTIN {
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

    // The scope of a function's or a let's body, with its binders and the procedural macros
    // the body defines, which hide others of their names in it. What `leave_scope` takes.
    fn enter_scope<'b>(
        &mut self,
        binders: impl Iterator<Item = &'b SyntaxObject>,
        body: &[ExprKind],
    ) -> usize {
        self.locals.push_layer();
        for binder in binders {
            if let Some(key) = kernel_local_key(binder) {
                self.locals.define(key);
            }
        }

        let hidden = self.local_hidden.len();
        let mut names = Vec::new();
        for form in body {
            defined_procedural_macros(form, &mut names);
        }
        let depth = self.locals.depth();
        self.local_hidden
            .extend(names.into_iter().map(|(name, mark)| (name, mark, depth)));
        hidden
    }

    fn leave_scope(&mut self, hidden: usize) {
        self.local_hidden.truncate(hidden);
        self.locals.pop_layer();
        self.leave_local_macros();
    }

    // Whether a name means a macro a function here defines, as a use of it would
    fn names_local_macro(&self, syn: &SyntaxObject) -> bool {
        let Some(s) = syn.ty.identifier().copied() else {
            return false;
        };
        let depth = self
            .local_macros
            .iter()
            .enumerate()
            .rev()
            .find(|(i, x)| x.0 == s && self.sees_local(x.3, syn.mark, Some(*i), x.1))
            .map(|(_, x)| x.1)
            .or_else(|| {
                self.local_hidden
                    .iter()
                    .rev()
                    .find(|x| x.0 == s && self.sees_local(x.1, syn.mark, None, x.2))
                    .map(|x| x.2)
            });
        let height = self
            .locals
            .height_of(&s)
            .max(kernel_local_key(syn).and_then(|key| self.locals.height_of(&key)));

        depth.is_some_and(|depth| height.is_none_or(|height| height < depth))
    }

    // A variable defined here, by the name of a define, `f` of `(define (f ...) ...)` too.
    // At the top level it replaces a procedural macro of its name.
    fn defined(&mut self, name: &ExprKind, body: bool) -> Result<()> {
        let name = match name {
            ExprKind::List(f) => match f.args.first() {
                Some(name) => name,
                None => return Ok(()),
            },
            name => name,
        };
        self.define_local(name, body)?;

        if self.locals.depth() <= 1 && self.environment.as_deref() == Some("top-level") {
            if let (Some(map), Some(name)) = (self.map.as_mut(), name.atom_identifier()) {
                map.forget_macro("top-level", name);
            }
        }

        Ok(())
    }

    // Where an earlier pass defined a function's macro, which is the body's again
    fn revive(&mut self, expr: &ExprKind) {
        if let Some((internal, mark, span)) = kernel_local_macro_entry(expr) {
            if !self.local_macros.iter().any(|x| x.2 == internal) {
                let written = written_kernel_local_name(internal);
                self.local_macros
                    .push((written, self.locals.depth(), internal, mark));
                self.local_definitions.insert(internal, span);
            }
        }
    }

    // What the kernel's locals are where a function's syntax-rules macro is defined
    fn rules_defined(&mut self, l: &List) -> Result<()> {
        // A body defines a name once, a macro of it too
        if let Some(name) = l.get(1).and_then(ExprKind::atom_syntax_object) {
            let depth = self.locals.depth();
            if let Some(defined) = kernel_local_key(name)
                .filter(|_| depth > 1)
                .and_then(|key| self.local_variables.get(&(key, depth)))
            {
                let span = if defined.start > name.span.start {
                    *defined
                } else {
                    name.span
                };
                stop!(BadSyntax => format!("define-syntax: duplicate binding name: {}", name.ty); span);
            }
        }

        let span = l
            .first()
            .and_then(ExprKind::atom_syntax_object)
            .map(|x| x.span);
        if let (Some(name), Some(span)) = (l.get(1).and_then(ExprKind::atom_identifier), span) {
            self.rules_sees.insert(
                (*name, span),
                (self.local_macros.len(), self.locals.depth()),
            );
        }

        Ok(())
    }

    // A syntax-rules definition a procedural macro made, as the list it's written as
    fn macro_definition(&mut self, expr: &mut ExprKind) {
        let ExprKind::Macro(m) = core::mem::replace(expr, ExprKind::empty()) else {
            unreachable!()
        };
        if let Some(name) = m.name.atom_identifier() {
            self.rules_sees.insert(
                (*name, m.location.span),
                (self.local_macros.len(), self.locals.depth()),
            );
        }
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
// The written procedural macro definitions at the top level of the code, which are loaded before
// it's expanded, so they leave nothing
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

// Expands the keyword and default arguments of the functions in a lowered expression, each
// named by the define it's the value of
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

    // A call can give keywords before positional arguments too, so with keywords every
    // positional argument is found in the arguments given
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

    // Checked before the arity, so a keyword it doesn't take is what's reported
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
