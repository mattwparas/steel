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

use crate::compiler::modules::RequiredModuleMacros;
use crate::compiler::passes::VisitorMutRefUnit;
use crate::compiler::program::{LAMBDA, LAMBDA_SYMBOL, LET};
use crate::parser::ast::ExprKind;
use crate::parser::parser::{ExpansionMark, SyntaxObject};
use crate::parser::span_visitor::get_span;
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

// Top level begins are part of the top level, so the macros they define are lifted out
pub(crate) fn lift_macro_defs_from_begins(exprs: &mut Vec<ExprKind>) -> Result<()> {
    let mut found = Vec::new();
    let mut index = 0;

    while index < exprs.len() {
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

// A macro used at the top level can expand into another macro definition
pub(crate) fn define_generated_macros(
    expr: &mut ExprKind,
    macro_map: &mut FxHashMap<InternedString, SteelMacro>,
) -> Result<()> {
    rename_template_macros(core::slice::from_mut(expr));
    define_generated_macro_forms(expr, macro_map)
}

fn define_generated_macro_forms(
    expr: &mut ExprKind,
    macro_map: &mut FxHashMap<InternedString, SteelMacro>,
) -> Result<()> {
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
                macro_map.insert(*generated_macro.name(), generated_macro);
            }
        }
        ExprKind::Begin(b) => {
            for expr in b.exprs.iter_mut() {
                define_generated_macro_forms(expr, macro_map)?;
            }
        }
        ExprKind::List(l) if is_begin(l) => {
            for expr in l.args.iter_mut().skip(1) {
                define_generated_macro_forms(expr, macro_map)?;
            }
        }
        _ => {}
    }

    Ok(())
}

pub fn extract_macro_defs(
    exprs: &mut Vec<ExprKind>,
    macro_map: &mut FxHashMap<InternedString, SteelMacro>,
    _global_map: &FxHashMap<InternedString, usize>,
) -> Result<()> {
    lift_macro_defs_from_begins(exprs)?;

    let mut error = None;

    exprs.retain_mut(|expr| {
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
                    Ok(generated_macro) => {
                        let name = generated_macro.name();
                        macro_map.insert(*name, generated_macro);
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

    Ok(())
}

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
        map,
        changed: false,
        in_scope_values: ScopeSet::default(),
        source_id: SourceId::none(),
        exclusions: &exclusions,
        globals,
        local_macros: LocalMacros::default(),
        top_level: true,
    };
    expander.visit(expr)?;

    Ok(expander.in_scope_values)
}

pub fn expand_with_source_id(
    expr: &mut ExprKind,
    map: &FxHashMap<InternedString, SteelMacro>,
    exclusions: &HashSet<InternedString>,
    source_id: Option<SourceId>,
    globals: GlobalMap,
) -> Result<()> {
    let mut expander = Expander {
        depth: 0,
        map,
        changed: false,
        in_scope_values: ScopeSet::default(),
        source_id,
        exclusions,
        globals,
        local_macros: LocalMacros::default(),
        top_level: true,
    };

    expander.visit(expr)
}

fn is_let_head(head: &ExprKind) -> bool {
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

// Code passed to eval has `begin` as a plain identifier
fn is_begin(l: &List) -> bool {
    l.is_begin() || l.first_ident().is_some_and(|x| *x == *BEGIN)
}

fn defines_local_macro(l: &List) -> bool {
    l.is_define_syntax()
        && l.get(2)
            .and_then(ExprKind::list)
            .is_some_and(List::is_syntax_rules)
}

// The macros a template defines, written with the mark of its expansion
fn template_macro_names(forms: &[ExprKind], names: &mut Vec<(InternedString, ExpansionMark)>) {
    for form in forms {
        match form {
            ExprKind::List(l) if defines_local_macro(l) => {
                if let Some(syn) = l.get(1).and_then(ExprKind::atom_syntax_object) {
                    if let TokenType::Identifier(name) = syn.ty {
                        if syn.mark.is_template() && !name.resolve().starts_with("##") {
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
                *name = format!("##{}#{}", name.resolve(), mark.0).into();
            }
        }
    }

    fn visit_quote(&mut self, _quote: &mut Quote) {}
}

// The name a renamed template macro was written with
pub(crate) fn template_macro_written_name(name: &str) -> Option<&str> {
    let (written, mark) = name.strip_prefix("##")?.rsplit_once('#')?;
    (!written.is_empty() && !mark.is_empty() && mark.bytes().all(|x| x.is_ascii_digit()))
        .then_some(written)
}

// A macro named by a template is only visible to that expansion, so the name and the
// uses from the same expansion are renamed
fn rename_template_macros(forms: &mut [ExprKind]) {
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
    expander: SteelMacro,
    // Scopes open where the macro was defined
    depth: usize,
}

static LOCAL_MACRO_CAPTURE: &str = "##local-macro##";

static LOCAL_MACRO_CAPTURES: AtomicBool = AtomicBool::new(false);

// The name a template uses for a local of the body its macro is defined in.
// `ResolveExpansionMarks` puts the variable back.
fn local_macro_capture(macro_name: &InternedString, name: &InternedString) -> InternedString {
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

// The macro and the variable a captured name refers to
pub(crate) fn local_macro_captured(name: &str) -> Option<(&str, &str)> {
    let (len, rest) = name.strip_prefix(LOCAL_MACRO_CAPTURE)?.split_once('#')?;
    let len = len.parse::<usize>().ok()?;

    Some((rest.get(len..)?, rest.get(..len)?))
}

// Lets callers skip the search when nothing was captured
pub(crate) fn any_local_macro_captures() -> bool {
    LOCAL_MACRO_CAPTURES.load(Ordering::Relaxed)
}

pub(crate) fn is_local_macro_definition(expr: &ExprKind) -> bool {
    matches!(expr, ExprKind::List(l) if defines_local_macro(l))
}

struct CaptureLocals<'a> {
    scope: &'a ScopeSet<InternedString, FxBuildHasher>,
    body_defines: &'a [InternedString],
    macro_name: InternedString,
}

impl VisitorMutRefUnit for CaptureLocals<'_> {
    fn visit_atom(&mut self, a: &mut Atom) {
        if !a.syn.unresolved {
            return;
        }

        let TokenType::Identifier(name) = &mut a.syn.ty else {
            return;
        };

        // Height 0 is the top level
        let local = self.scope.height_of(name).is_some_and(|height| height > 0)
            || self.scope.depth() > 1 && self.body_defines.contains(name);

        if local {
            *name = local_macro_capture(&self.macro_name, name);
            a.syn.unresolved = false;
        }
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
                match l.get(1) {
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
    // A binding inside the defining body shadows the macro
    fn shadowed(&self, scope: &ScopeSet<InternedString, FxBuildHasher>) -> bool {
        scope
            .height_of(self.expander.name())
            .is_some_and(|height| height >= self.depth)
    }

    fn expand(
        &self,
        expr: List,
        span: steel_parser::span::Span,
        scope: &ScopeSet<InternedString, FxBuildHasher>,
        globals: GlobalMap,
    ) -> Result<ExprKind> {
        // Only bindings made after the macro was defined can collide with its template
        let mut bound_since = ScopeSet::default();

        for name in scope.iter() {
            if scope
                .height_of(name)
                .is_some_and(|height| height >= self.depth)
            {
                bound_since.define(*name);
            }
        }

        self.expander.expand(expr, span, &bound_since, globals)
    }
}

fn ends_with_definition(form: &ExprKind) -> bool {
    match form {
        ExprKind::List(l) if defines_local_macro(l) => true,
        ExprKind::Begin(b) => b.exprs.last().map_or(true, ends_with_definition),
        ExprKind::List(l) if is_begin(l) => l.args[1..].last().map_or(true, ends_with_definition),
        _ => false,
    }
}

// Macros defined with define-syntax in a body, visible only there. The definitions stay in
// the body so every round of expansion sees them, and `ResolveExpansionMarks` removes them.
#[derive(Default)]
struct LocalMacros {
    macros: Vec<LocalMacro>,
}

impl LocalMacros {
    fn find(&self, name: &InternedString) -> Option<&LocalMacro> {
        self.macros.iter().rev().find(|x| x.expander.name() == name)
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

                    // Already defined by the body this begin is part of
                    let defined = l
                        .get(1)
                        .and_then(ExprKind::atom_identifier)
                        .is_some_and(|x| {
                            self.find(x).is_some_and(|m| {
                                m.depth == depth && Some(m.expander.span()) == span
                            })
                        });

                    if defined {
                        continue;
                    }

                    if let ExprKind::Macro(m) =
                        lower_macro_and_require_definitions(ExprKind::List(l.clone()))?
                    {
                        self.macros.push(LocalMacro {
                            expander: SteelMacro::parse_from_ast_macro(m)?,
                            depth,
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

    // Returns what to pass to `leave` once the body is done
    fn enter<B: BodyForms>(
        &mut self,
        body: &mut B,
        start: usize,
        scope: &ScopeSet<InternedString, FxBuildHasher>,
    ) -> Result<usize> {
        let visible = self.macros.len();

        self.define_from(body, start, scope.depth())?;

        if self.macros.len() == visible {
            return Ok(visible);
        }

        if body
            .forms()
            .iter()
            .skip(start)
            .last()
            .map_or(true, ends_with_definition)
        {
            let span = self.macros[visible].expander.span();
            self.macros.truncate(visible);
            stop!(BadSyntax => "define-syntax in a body needs an expression after it"; span);
        }

        let mut names = Vec::new();
        body_defines(body.forms().get(start..).unwrap_or_default(), &mut names);

        for local in &mut self.macros[visible..] {
            let mut capture = CaptureLocals {
                scope,
                body_defines: &names,
                macro_name: *local.expander.name(),
            };

            for template in local.expander.exprs_mut() {
                VisitorMutRefUnit::visit(&mut capture, template);
            }
        }

        Ok(visible)
    }

    fn enter_expr(
        &mut self,
        body: &mut ExprKind,
        scope: &ScopeSet<InternedString, FxBuildHasher>,
    ) -> Result<usize> {
        match body {
            ExprKind::Begin(b) => self.enter(&mut b.exprs, 0, scope),
            ExprKind::List(l) if is_begin(l) => self.enter(&mut l.args, 1, scope),
            ExprKind::List(l) if defines_local_macro(l) => {
                stop!(BadSyntax => "define-syntax in a body needs an expression after it"; l.location)
            }
            _ => Ok(self.macros.len()),
        }
    }

    fn leave(&mut self, visible: usize) {
        self.macros.truncate(visible);
    }
}

fn visit_let_form<V: VisitorMutRef<Output = Result<()>>>(
    visitor: &mut V,
    scope: fn(&mut V) -> &mut ScopeSet<InternedString, FxBuildHasher>,
    local_macros: fn(
        &mut V,
    ) -> Option<(&mut LocalMacros, &ScopeSet<InternedString, FxBuildHasher>)>,
    l: &mut List,
) -> Result<()> {
    let name = l.args.get(1).and_then(|x| x.atom_identifier()).copied();
    let bindings_index = if name.is_some() { 2 } else { 1 };

    scope(visitor).push_layer();

    if let Some(name) = name {
        scope(visitor).define(name);
    }

    if let Some(ExprKind::List(bindings)) = l.args.get_mut(bindings_index) {
        for arg in &mut bindings.args {
            if let ExprKind::List(binding) = arg {
                if let Some(ident) = binding.first_ident() {
                    scope(visitor).define(*ident);
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
            Some(local_macros.enter(&mut l.args, bindings_index + 1, scope)?)
        }
        None => None,
    };

    if let Some(body_args) = l.args.get_mut(bindings_index + 1..) {
        for body_expr in body_args {
            visitor.visit(body_expr)?;
        }
    }

    if let Some(visible) = visible {
        if let Some((local_macros, _)) = local_macros(visitor) {
            local_macros.leave(visible);
        }
    }

    scope(visitor).pop_layer();

    Ok(())
}

// HACK: template identifiers that collide with a local get renamed to `##name`. If that
// identifier was a reference to a macro, put the original name back so that things
// behave
fn restore_hygienic_macro_head(
    l: &mut List,
    in_scope: &ScopeSet<InternedString, FxBuildHasher>,
    is_macro: impl Fn(&InternedString) -> bool,
) {
    if let Some(ExprKind::Atom(Atom {
        syn:
            SyntaxObject {
                ty: TokenType::Identifier(s),
                unresolved: true,
                ..
            },
    })) = l.args.first_mut()
    {
        if in_scope.contains(s) || is_macro(s) {
            return;
        }

        if let Some(original) = s.resolve().strip_prefix("##") {
            let original: InternedString = original.into();

            if is_macro(&original) {
                *s = original;
            }
        }
    }
}

pub struct Expander<'a> {
    map: &'a FxHashMap<InternedString, SteelMacro>,
    exclusions: &'a HashSet<InternedString>,
    globals: GlobalMap<'a>,
    pub(crate) changed: bool,
    // We're going to actually check if the macro is in scope
    in_scope_values: ScopeSet<InternedString, FxBuildHasher>,
    source_id: Option<SourceId>,
    depth: usize,
    local_macros: LocalMacros,
    top_level: bool,
}

#[derive(Debug)]
pub struct RequiredMacroMap<'a> {
    pub(crate) map: &'a mut RequiredModuleMacros,
    pub(crate) changed: bool,
}

pub struct ExpanderMany<'a> {
    // Base map, but then also we want to include _all_
    pub(crate) map: &'a mut FxHashMap<InternedString, SteelMacro>,

    pub(crate) overlays: Vec<RequiredMacroMap<'a>>,

    // We're going to actually check if the macro is in scope
    pub(crate) in_scope_values: ScopeSet<InternedString, FxBuildHasher>,

    pub(crate) globals: GlobalMap<'a>,
    source_id: Option<SourceId>,
    depth: usize,
    local_macros: LocalMacros,
    top_level: bool,
}

impl<'a> ExpanderMany<'a> {
    pub fn new(
        map: &'a mut FxHashMap<InternedString, SteelMacro>,
        overlays: Vec<RequiredMacroMap<'a>>,
        globals: GlobalMap<'a>,
    ) -> Self {
        Self {
            map,
            overlays,
            in_scope_values: ScopeSet::default(),
            source_id: SourceId::none(),
            globals,
            depth: 0,
            local_macros: LocalMacros::default(),
            top_level: true,
        }
    }

    pub fn expand(&mut self, expr: &mut ExprKind) -> Result<()> {
        self.top_level = true;
        self.visit(expr)?;

        // This isn't good! this is quadratic
        for i in self.in_scope_values.iter() {
            self.map.remove(i);
        }

        self.in_scope_values.clear_all();

        Ok(())
    }
}

impl<'a> Expander<'a> {
    pub fn new(
        map: &'a FxHashMap<InternedString, SteelMacro>,
        exclusions: &'a HashSet<InternedString>,
        globals: GlobalMap<'a>,
    ) -> Self {
        Self {
            map,
            changed: false,
            in_scope_values: ScopeSet::default(),
            source_id: SourceId::none(),
            depth: 0,
            exclusions,
            globals,
            local_macros: LocalMacros::default(),
            top_level: true,
        }
    }

    pub fn expand(&mut self, expr: &mut ExprKind) -> Result<()> {
        self.top_level = true;
        self.visit(expr)
    }
}

impl<'a> VisitorMutRef for Expander<'a> {
    type Output = Result<()>;

    fn visit(&mut self, expr: &mut ExprKind) -> Self::Output {
        if self.depth > 512 {
            stop!(Generic => "macro expansion depth reached!");
        }

        let top_level = core::mem::replace(&mut self.top_level, false);

        if top_level {
            // Its macros are defined for the rest of it, and for the top level after it
            match expr {
                ExprKind::Begin(b) => {
                    let visible = self.local_macros.macros.len();
                    let depth = self.in_scope_values.depth();
                    self.local_macros.define_from(&mut b.exprs, 0, depth)?;

                    for expr in b.exprs.iter_mut() {
                        self.top_level = true;
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
                        self.visit(expr)?;
                    }

                    self.local_macros.leave(visible);
                    return Ok(());
                }
                _ => {}
            }
        }

        match expr {
            ExprKind::If(f) => self.visit_if(f),
            ExprKind::Define(d) => self.visit_define(d),
            ExprKind::LambdaFunction(l) => self.visit_lambda_function(l),
            ExprKind::Begin(b) => self.visit_begin(b),
            ExprKind::Return(r) => self.visit_return(r),
            ExprKind::Let(l) => self.visit_let(l),
            ExprKind::Quote(q) => self.visit_quote(q),
            ExprKind::Macro(m) => self.visit_macro(m),
            ExprKind::Atom(a) => self.visit_atom(a),
            ExprKind::List(l) => {
                // Defined by the enclosing body, or at the top level
                if defines_local_macro(l) {
                    return Ok(());
                }

                restore_hygienic_macro_head(l, &self.in_scope_values, |name| {
                    self.local_macros.find(name).is_some()
                        || self.map.contains_key(name) && !self.exclusions.contains(name)
                });

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
                        return visit_let_form(
                            self,
                            |v| &mut v.in_scope_values,
                            |v| Some((&mut v.local_macros, &v.in_scope_values)),
                            l,
                        );
                    }

                    Some(ExprKind::Atom(Atom {
                        syn:
                            SyntaxObject {
                                ty: TokenType::Begin,
                                ..
                            },
                    })) => {
                        let visible =
                            self.local_macros
                                .enter(&mut l.args, 1, &self.in_scope_values)?;

                        for expr in l.args.iter_mut() {
                            self.visit(expr)?;
                        }

                        self.local_macros.leave(visible);

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
                        if let Some(arg) = l.args.get_mut(2) {
                            self.visit(arg)?;
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
                        if let Some(arg) = l.args.get_mut(2) {
                            self.visit(arg)?;
                        }

                        return Ok(());
                    }

                    // A list starting with `lambda` can be data before `case` is expanded,
                    // e.g. `((lambda) ...)`, so only a well formed one is parsed. Lowering
                    // reports malformed code later.
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
                        // `(define)` alone can be data too

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
                                    if let Some(ident) = arg.atom_identifier() {
                                        self.in_scope_values.define(*ident);
                                    }
                                }
                            }
                        }

                        let visible = if is_function {
                            Some(
                                self.local_macros
                                    .enter(&mut l.args, 2, &self.in_scope_values)?,
                            )
                        } else {
                            None
                        };

                        for expr in l.args[2..].iter_mut() {
                            self.visit(expr)?;
                        }

                        if let Some(visible) = visible {
                            self.local_macros.leave(visible);
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
                                ..
                            },
                    })) => {
                        if let Some(local) = self.local_macros.find(s) {
                            if !local.shadowed(&self.in_scope_values) {
                                let span = *sp;

                                let mut expanded = local.expand(
                                    List::new_maybe_improper(
                                        core::mem::take(&mut l.args),
                                        l.improper,
                                    ),
                                    span,
                                    &self.in_scope_values,
                                    self.globals,
                                )?;
                                self.changed = true;

                                self.depth += 1;

                                self.visit(&mut expanded)?;

                                self.depth -= 1;

                                *expr = expanded;

                                return Ok(());
                            }
                        } else if let Some(m) = self.map.get(s) {
                            // Interweave the macro expansion
                            if !self.exclusions.contains(s) {
                                // If this macro has been overwritten by any local value, respect
                                // the local binding and do not expand the macro, unless the
                                // identifier came from a macro template
                                let visible = self.source_id.is_none()
                                    || sp.source_id() == m.location.source_id();

                                if *unresolved || visible && !self.in_scope_values.contains(s) {
                                    let span = *sp;

                                    let mut expanded = m.expand(
                                        List::new_maybe_improper(
                                            core::mem::take(&mut l.args),
                                            l.improper,
                                        ),
                                        span,
                                        &self.in_scope_values,
                                        self.globals,
                                    )?;
                                    self.changed = true;

                                    self.depth += 1;
                                    self.top_level = top_level;

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

        self.visit(&mut define.body)?;
        Ok(())
    }

    fn visit_lambda_function(
        &mut self,
        lambda_function: &mut super::ast::LambdaFunction,
    ) -> Self::Output {
        self.in_scope_values.push_layer();

        for value in &lambda_function.args {
            if let Some(ident) = value.atom_identifier() {
                self.in_scope_values.define(*ident);
            }
        }

        for value in &mut lambda_function.args {
            self.visit(value)?;
        }

        let visible = self
            .local_macros
            .enter_expr(&mut lambda_function.body, &self.in_scope_values)?;

        self.visit(&mut lambda_function.body)?;

        self.local_macros.leave(visible);

        self.in_scope_values.pop_layer();

        Ok(())
    }

    fn visit_begin(&mut self, begin: &mut super::ast::Begin) -> Self::Output {
        let visible = self
            .local_macros
            .enter(&mut begin.exprs, 0, &self.in_scope_values)?;

        for expr in begin.exprs.iter_mut() {
            self.visit(expr)?;
        }

        self.local_macros.leave(visible);

        Ok(())
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

        self.visit(&mut l.body_expr)?;

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

impl<'a> VisitorMutRef for ExpanderMany<'a> {
    type Output = Result<()>;

    fn visit(&mut self, expr: &mut ExprKind) -> Self::Output {
        // println!("expanding: {:?}", expr);

        if self.depth > 512 {
            stop!(Generic => "macro expansion depth reached!");
        }

        let top_level = core::mem::replace(&mut self.top_level, false);

        if top_level {
            // Its macros are defined for the rest of it, and for the top level after it
            match expr {
                ExprKind::Begin(b) => {
                    let visible = self.local_macros.macros.len();
                    let depth = self.in_scope_values.depth();
                    self.local_macros.define_from(&mut b.exprs, 0, depth)?;

                    for expr in b.exprs.iter_mut() {
                        self.top_level = true;
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
                        self.visit(expr)?;
                    }

                    self.local_macros.leave(visible);
                    return Ok(());
                }
                _ => {}
            }
        }

        match expr {
            ExprKind::If(f) => self.visit_if(f),
            ExprKind::Define(d) => self.visit_define(d),
            ExprKind::LambdaFunction(l) => self.visit_lambda_function(l),
            ExprKind::Begin(b) => self.visit_begin(b),
            ExprKind::Return(r) => self.visit_return(r),
            ExprKind::Let(l) => self.visit_let(l),
            ExprKind::Quote(q) => self.visit_quote(q),
            ExprKind::Macro(m) => self.visit_macro(m),
            ExprKind::Atom(a) => self.visit_atom(a),
            ExprKind::List(l) => {
                // Defined by the enclosing body, or at the top level
                if defines_local_macro(l) {
                    return Ok(());
                }

                restore_hygienic_macro_head(l, &self.in_scope_values, |name| {
                    self.local_macros.find(name).is_some()
                        || self
                            .overlays
                            .iter()
                            .any(|x| x.map.in_scope_macros.contains_key(name))
                        || self.map.contains_key(name)
                });

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
                        return visit_let_form(
                            self,
                            |v| &mut v.in_scope_values,
                            |v| Some((&mut v.local_macros, &v.in_scope_values)),
                            l,
                        );
                    }

                    Some(ExprKind::Atom(Atom {
                        syn:
                            SyntaxObject {
                                ty: TokenType::Begin,
                                ..
                            },
                    })) => {
                        let visible =
                            self.local_macros
                                .enter(&mut l.args, 1, &self.in_scope_values)?;

                        for expr in l.args.iter_mut() {
                            self.visit(expr)?;
                        }

                        self.local_macros.leave(visible);

                        return Ok(());
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
                    // Same as the lambda arm in `Expander`.
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

                            // return self.visit_lambda_function(&mut lambda);
                        } else {
                            unreachable!()
                        }
                    }

                    Some(ExprKind::Atom(Atom {
                        syn:
                            SyntaxObject {
                                ty: TokenType::Identifier(i),
                                ..
                            },
                    })) if *i == *DEFINE_SYNTAX && !self.in_scope_values.contains(i) => {
                        if let Some(arg) = l.args.get_mut(2) {
                            self.visit(arg)?;
                        }
                    }

                    Some(ExprKind::Atom(Atom {
                        syn:
                            SyntaxObject {
                                ty: TokenType::DefineSyntax,
                                ..
                            },
                    })) if !self.in_scope_values.contains(&DEFINE_SYNTAX) => {
                        if let Some(arg) = l.args.get_mut(2) {
                            self.visit(arg)?;
                        }
                    }

                    Some(ExprKind::Atom(Atom {
                        syn:
                            SyntaxObject {
                                ty: TokenType::Define,
                                ..
                            },
                    })) if l.args.len() > 1 => {
                        // Same as the define arm in `Expander`.

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
                                    if let Some(ident) = arg.atom_identifier() {
                                        self.in_scope_values.define(*ident);
                                    }
                                }
                            }
                        }

                        let visible = if is_function {
                            Some(
                                self.local_macros
                                    .enter(&mut l.args, 2, &self.in_scope_values)?,
                            )
                        } else {
                            None
                        };

                        for expr in l.args[2..].iter_mut() {
                            self.visit(expr)?;
                        }

                        if let Some(visible) = visible {
                            self.local_macros.leave(visible);
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
                                ..
                            },
                    })) => {
                        let mut found_index = None;

                        if let Some(local) = self.local_macros.find(s) {
                            if !local.shadowed(&self.in_scope_values) {
                                let span = *sp;

                                let mut expanded = local.expand(
                                    List::new_maybe_improper(
                                        core::mem::take(&mut l.args),
                                        l.improper,
                                    ),
                                    span,
                                    &self.in_scope_values,
                                    self.globals,
                                )?;

                                self.depth += 1;

                                self.visit(&mut expanded)?;

                                self.depth -= 1;

                                *expr = expanded;

                                return Ok(());
                            }
                        } else if let Some(m) = self
                            .overlays
                            .iter()
                            .enumerate()
                            .find_map(|(index, x)| {
                                let res = x.map.in_scope_macros.get(s);
                                if res.is_some() {
                                    found_index = Some(index);
                                }

                                res
                            })
                            .or_else(|| self.map.get(s))
                        {
                            // If this macro has been overwritten by any local value, respect
                            // the local binding and do not expand the macro, unless the
                            // identifier came from a macro template
                            let visible = self.source_id.is_none()
                                || self.source_id == m.location.source_id();

                            if *unresolved || visible && !self.in_scope_values.contains(s) {
                                // if s.resolve().ends_with("skip-compile") {
                                //     println!("Expanding skip compile: {}", l);
                                // }

                                let span = *sp;

                                // println!("Expanding many: {}", l);

                                let mut expanded = m.expand(
                                    List::new_maybe_improper(
                                        core::mem::take(&mut l.args),
                                        l.improper,
                                    ),
                                    span,
                                    &self.in_scope_values,
                                    self.globals,
                                )?;

                                if let Some(index) = found_index {
                                    self.overlays[index].changed = true;
                                }

                                self.depth += 1;
                                self.top_level = top_level;

                                self.visit(&mut expanded)?;

                                self.depth -= 1;

                                *expr = expanded;

                                return Ok(());
                            }

                            // let expanded = m.expand(l.clone(), *sp)?;
                            // self.changed = true;
                            // return self.visit(expanded);
                        }
                    }
                    // Some(other) => {
                    //     println!("967 - DIDN'T MATCH: {}", other)
                    // }
                    _ => {}
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

        self.visit(&mut define.body)?;
        Ok(())
    }

    fn visit_lambda_function(
        &mut self,
        lambda_function: &mut super::ast::LambdaFunction,
    ) -> Self::Output {
        self.in_scope_values.push_layer();

        for value in &lambda_function.args {
            if let Some(ident) = value.atom_identifier() {
                self.in_scope_values.define(*ident);
            }
        }

        for value in &mut lambda_function.args {
            self.visit(value)?;
        }

        let visible = self
            .local_macros
            .enter_expr(&mut lambda_function.body, &self.in_scope_values)?;

        self.visit(&mut lambda_function.body)?;

        self.local_macros.leave(visible);

        self.in_scope_values.pop_layer();

        Ok(())
    }

    fn visit_begin(&mut self, begin: &mut super::ast::Begin) -> Self::Output {
        let visible = self
            .local_macros
            .enter(&mut begin.exprs, 0, &self.in_scope_values)?;

        for expr in begin.exprs.iter_mut() {
            self.visit(expr)?;
        }

        self.local_macros.leave(visible);

        Ok(())
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

        self.visit(&mut l.body_expr)?;

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

pub fn expand_kernel_in_env_with_allowed(
    mut expr: ExprKind,
    kernel: Option<&mut Kernel>,
    builtin_modules: ModuleContainer,
    env: &str,
    allowed: &FxHashSet<InternedString>,
    globals: GlobalMap,
) -> Result<(ExprKind, bool)> {
    let mut expander = KernelExpander {
        map: kernel,
        changed: false,
        builtin_modules,
        environment: Some(env),
        depth: 0,
        allowed_macros: Some(allowed),
        define_context: None,
        in_scope_values: ScopeSet::default(),
        globals,
    };

    expander.visit(&mut expr)?;

    Ok((expr, expander.changed))

    // expander.visit(expr).map(|x| (x, expander.changed))
}

pub fn expand_kernel_in_env_with_change(
    expr: &mut ExprKind,
    kernel: Option<&mut Kernel>,
    builtin_modules: ModuleContainer,
    env: &str,
    globals: GlobalMap,
) -> Result<bool> {
    let mut expander = KernelExpander {
        map: kernel,
        changed: false,
        builtin_modules,
        environment: Some(env),
        depth: 0,
        allowed_macros: None,
        define_context: None,
        in_scope_values: ScopeSet::default(),
        globals,
    };

    // TODO: Set the environment during loading here
    //

    expander.visit(expr)?;

    Ok(expander.changed)

    // Ok((expr, expander.changed))
}

pub fn expand_kernel_in_env(
    expr: &mut ExprKind,
    kernel: Option<&mut Kernel>,
    builtin_modules: ModuleContainer,
    env: &str,
    globals: GlobalMap,
) -> Result<()> {
    let mut expander = KernelExpander {
        map: kernel,
        changed: false,
        builtin_modules,
        environment: Some(env),
        depth: 0,
        allowed_macros: None,
        define_context: None,
        in_scope_values: ScopeSet::default(),
        globals,
    };

    expander.visit(expr)
}

pub fn expand_kernel(
    mut expr: ExprKind,
    kernel: Option<&mut Kernel>,
    builtin_modules: ModuleContainer,
    globals: GlobalMap,
) -> Result<ExprKind> {
    let mut expander = KernelExpander {
        map: kernel,
        changed: false,
        builtin_modules,
        environment: None,
        depth: 0,
        allowed_macros: None,
        define_context: None,
        in_scope_values: ScopeSet::default(),
        globals,
    };
    expander.visit(&mut expr)?;

    Ok(expr)
}

pub struct KernelExpander<'a> {
    map: Option<&'a mut Kernel>,
    pub(crate) changed: bool,
    builtin_modules: ModuleContainer,
    environment: Option<&'a str>,
    depth: usize,
    allowed_macros: Option<&'a FxHashSet<InternedString>>,
    define_context: Option<ExprKind>,
    in_scope_values: ScopeSet<InternedString, FxBuildHasher>,
    globals: GlobalMap<'a>,
}

impl<'a> KernelExpander<'a> {
    pub fn new(
        map: Option<&'a mut Kernel>,
        builtin_modules: ModuleContainer,
        globals: GlobalMap<'a>,
    ) -> Self {
        Self {
            map,
            changed: false,
            builtin_modules,
            environment: None,
            depth: 0,
            allowed_macros: None,
            define_context: None,
            in_scope_values: ScopeSet::default(),
            globals,
        }
    }

    pub fn expand(&mut self, mut expr: ExprKind) -> Result<ExprKind> {
        self.visit(&mut expr)?;

        Ok(expr)
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

// A literal, a quoted value or a variable, which costs nothing to evaluate
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

// The position of `name` among the parameters as written
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

    let mut non_keyword_or_default_args: SmallVec<[ExprKind; 6]> = lambda_function
        .args
        .iter()
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
    let mut seen_default_or_kwarg = false;
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
                    ExprKind::Quote(Box::new(Quote::new(
                        keyword.clone(),
                        lambda_function.location.clone(),
                    ))),
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
                    ExprKind::Quote(Box::new(Quote::new(
                        keyword.clone(),
                        lambda_function.location.clone(),
                    ))),
                    expr
                ]
            } else {
                // The default only runs if the keyword is missing
                let quoted = ExprKind::Quote(Box::new(Quote::new(
                    keyword.clone(),
                    lambda_function.location.clone(),
                )));
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

    // When a default uses an earlier argument, the arguments are bound one after another
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

    *lambda_function = LambdaFunction::new_with_rest_arg(
        non_keyword_or_default_args.into_iter().collect(),
        arity_check_condition,
        SyntaxObject::default(TokenType::Lambda),
    );

    Ok(())
}

// VisitorMutRef
impl<'a> VisitorMutRef for KernelExpander<'a> {
    type Output = Result<()>;

    fn visit_if(&mut self, f: &mut super::ast::If) -> Self::Output {
        self.visit(&mut f.test_expr)?;
        self.visit(&mut f.then_expr)?;
        self.visit(&mut f.else_expr)
    }

    fn visit_define(&mut self, define: &mut super::ast::Define) -> Self::Output {
        self.define_context = Some(define.name.clone());
        self.visit(&mut define.body)?;
        self.define_context = None;

        Ok(())
    }

    // TODO: Kernel expander should have the liberty to parse everything
    // As a normal expression in order to match behavior
    fn visit_lambda_function(
        &mut self,
        lambda_function: &mut super::ast::LambdaFunction,
    ) -> Self::Output {
        {
            self.in_scope_values.push_layer();

            for value in &lambda_function.args {
                if let Some(ident) = value.atom_identifier() {
                    self.in_scope_values.define(*ident);
                }
            }

            for value in &mut lambda_function.args {
                self.visit(value)?;
            }

            self.visit(&mut lambda_function.body)?;

            self.in_scope_values.pop_layer();
        }

        // TODO: Unfortunately this wipes out the span
        // There needs to be

        // self.visit(&mut lambda_function.body)?;

        // Expand keyword arguments if we can
        // TODO: If this isn't a lambda function, we're gonna have problems
        // if its a list, we should run the same thing, but on a list
        // that starts with lambda, and coerce it to be expanded that way
        // expand_keyword_arguments(lambda_function)?;

        expand_keyword_and_default_arguments(lambda_function, &self.define_context)?;

        Ok(())
    }

    fn visit_begin(&mut self, begin: &mut super::ast::Begin) -> Self::Output {
        for expr in begin.exprs.iter_mut() {
            self.visit(expr)?;
        }

        Ok(())
    }

    fn visit_return(&mut self, r: &mut super::ast::Return) -> Self::Output {
        self.visit(&mut r.expr)
    }

    fn visit_quote(&mut self, _quote: &mut super::ast::Quote) -> Self::Output {
        // TODO: We probably should not visit this here
        // self.visit(&mut quote.expr)
        Ok(())
    }

    fn visit_macro(&mut self, m: &mut super::ast::Macro) -> Self::Output {
        stop!(BadSyntax => format!("unexpected macro definition in kernel expander: {}", m); m.location.span)
    }

    fn visit_atom(&mut self, _a: &mut Atom) -> Self::Output {
        Ok(())
    }

    fn visit_list(&mut self, _l: &mut super::ast::List) -> Self::Output {
        Ok(())
    }

    fn visit_syntax_rules(&mut self, l: &mut super::ast::SyntaxRules) -> Self::Output {
        stop!(Generic => "unexpected syntax-rules definition"; l.location.span)
    }

    fn visit_set(&mut self, s: &mut super::ast::Set) -> Self::Output {
        self.visit(&mut s.variable)?;
        self.visit(&mut s.expr)?;

        Ok(())
    }

    fn visit_require(&mut self, _s: &mut super::ast::Require) -> Self::Output {
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

        self.visit(&mut l.body_expr)?;

        self.in_scope_values.pop_layer();

        Ok(())
    }

    fn visit(&mut self, expr: &mut ExprKind) -> Self::Output {
        if self.depth > 96 {
            stop!(BadSyntax => "Current expansion depth of defmacro style macros exceeded: depth capped at 96"; get_span(expr));
        }

        let res = match expr {
            ExprKind::If(f) => self.visit_if(f),
            ExprKind::Define(d) => self.visit_define(d),
            ExprKind::LambdaFunction(l) => self.visit_lambda_function(l),
            ExprKind::Begin(b) => self.visit_begin(b),
            ExprKind::Return(r) => self.visit_return(r),
            ExprKind::Let(l) => self.visit_let(l),
            ExprKind::Quote(q) => self.visit_quote(q),
            ExprKind::Macro(m) => self.visit_macro(m),
            ExprKind::Atom(a) => self.visit_atom(a),
            ExprKind::List(l) => {
                {
                    // Defined by the enclosing body, or at the top level
                    if defines_local_macro(l) {
                        return Ok(());
                    }

                    match l.first() {
                        Some(head) if is_let_head(head) => {
                            return visit_let_form(self, |v| &mut v.in_scope_values, |_| None, l);
                        }
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

                        _ => {}
                    }

                    if l.first()
                        .map(|x| {
                            if let ExprKind::Atom(Atom {
                                syn:
                                    SyntaxObject {
                                        ty: TokenType::DefineSyntax,
                                        ..
                                    },
                            }) = x
                            {
                                true
                            } else {
                                false
                            }
                        })
                        .unwrap_or_default()
                    {
                        if let Some(arg) = l.args.get_mut(2) {
                            self.visit(arg)?;
                            return Ok(());
                        }
                    }

                    if l.first()
                        .map(|x| {
                            if let ExprKind::Atom(Atom {
                                syn:
                                    SyntaxObject {
                                        ty: TokenType::SyntaxRules,
                                        ..
                                    },
                            }) = x
                            {
                                true
                            } else {
                                false
                            }
                        })
                        .unwrap_or_default()
                    {
                        if let Some(args) = l.args.get_mut(2..) {
                            for arg in args {
                                self.visit(arg)?;
                            }
                            return Ok(());
                        }
                    }

                    if let Some(s) = l.first().and_then(|x| {
                        if let ExprKind::Atom(Atom {
                            syn:
                                SyntaxObject {
                                    ty: TokenType::Identifier(s),
                                    ..
                                },
                        }) = x
                        {
                            Some(*s)
                        } else {
                            None
                        }
                    }) {
                        if s == *SYNTAX_CASE {
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

                                return Ok(());
                            }
                        }

                        if let Some(map) = &mut self.map {
                            if map.contains_syntax_object_macro(
                                &s,
                                self.environment.as_ref().map(|x| x.as_ref()),
                            ) && self
                                .allowed_macros
                                .as_ref()
                                .map(|x| x.contains(&s))
                                .unwrap_or(true)
                            {
                                let mut expanded = map.expand_syntax_object(
                                    &s,
                                    ExprKind::List(core::mem::replace(
                                        l,
                                        List::new(ThinVec::new()),
                                    )),
                                    self.environment
                                        .as_ref()
                                        .map(|x| x.as_ref())
                                        .unwrap_or("default"),
                                    &self.in_scope_values,
                                    self.globals,
                                )?;
                                self.changed = true;

                                self.depth += 1;

                                self.visit(&mut expanded)?;

                                self.depth -= 1;

                                *expr = expanded;

                                return Ok(());

                                // return result;
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
                                        *expr = module.to_syntax(None);
                                        return Ok(());

                                        // return Ok(module.to_syntax(None));
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
                                        *expr = module.to_syntax(None);
                                        return Ok(());
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
                                        *expr = module.to_syntax(Some(prefix.resolve()));

                                        return Ok(());
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
                                        *expr = module.to_syntax(Some(prefix.resolve()));

                                        return Ok(());
                                    } else {
                                        stop!(BadSyntax => format!("require-builtin: module not found: {}", s); *span);
                                    }
                                }

                                _ => {
                                    stop!(ArityMismatch => "require-builtin malformed - follows the pattern (require-builtin \"<module>\") or (require-builtin \"<module>\" as <prefix>")
                                }
                            }
                        }
                    }

                    for expr in l.args.iter_mut() {
                        self.visit(expr)?;
                    }

                    Ok(())
                }
            }
            ExprKind::SyntaxRules(s) => self.visit_syntax_rules(s),
            ExprKind::Set(s) => self.visit_set(s),
            ExprKind::Require(r) => self.visit_require(r),
            ExprKind::Vector(v) => self.visit_vector(v),
        };

        res
    }

    fn visit_vector(&mut self, _v: &mut super::ast::Vector) -> Self::Output {
        Ok(())
    }
}

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
