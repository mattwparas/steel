use rustc_hash::{FxHashMap, FxHashSet};

use crate::parser::{
    ast::{Atom, Begin, Define, ExprKind, LambdaFunction, Let, Quote},
    expand_visitor::{is_local_macro_definition, kernel_local_macro},
    interner::InternedString,
    parser::{SyntaxObject, SyntaxObjectId},
};

use super::{mangle::is_prelude_builtin, VisitorMutRefUnit};
use crate::parser::tokens::TokenType;
use crate::steel_vm::primitives::builtin_to_reserved;

// Everything after expansion resolves variables by name, so this renames local binders where
// resolving by name would pick a different binder than resolving by scopes.
//
// Has to run after expansion and lowering, and before constant evaluation.
//
// Its a bit odd - we now have a few various implementations of things to enforce hygience.
// There is the `unresolved` and `introduced_via_macro` flags. At some point, we'll want
// to do a full on rewrite of this part most likely. But for now, this pass is a decent
// attempt at fixing some bugs that have been reported.
struct Binder {
    name: InternedString,
    id: usize,
    syntax: SyntaxObjectId,
    scopes: steel_parser::scopes::ScopeSetId,
}

#[derive(Default)]
pub struct ResolveByScopes {
    scope: Vec<Binder>,
    // Positions of each name's binders in `scope`
    by_name: FxHashMap<InternedString, Vec<usize>>,
    next_id: usize,
    conflicts: FxHashSet<usize>,
    renamed: FxHashMap<usize, InternedString>,
    renaming: bool,
    // usize here is the binder count
    local_macros: Vec<(InternedString, usize)>,
    has_local_macros: bool,
    template_globals: bool,
    // Globals defined by templates in this and earlier compiles. A macro those templates
    // defined can still refer to them from a later input or another module
    earlier: TemplateGlobals,
    // Number of enclosing functions and lets
    bodies: usize,
}

// Kept by the compiler: for each name, the introduction scopes of the templates that defined it.
// Scopes are never reused.
pub type TemplateGlobals = FxHashMap<InternedString, FxHashSet<steel_parser::scopes::ScopeSetId>>;

impl ResolveByScopes {
    pub fn resolve(expr: &mut ExprKind, earlier: &mut TemplateGlobals) {
        let mut pass = Self {
            earlier: core::mem::take(earlier),
            ..Self::default()
        };

        pass.define_template_globals(expr);
        pass.visit(expr);

        if !pass.conflicts.is_empty() || pass.has_local_macros || pass.template_globals {
            pass.renaming = true;
            pass.next_id = 0;
            pass.visit(expr);
        }

        *earlier = pass.earlier;
    }

    fn template_global(
        name: InternedString,
        introductions: steel_parser::scopes::ScopeSetId,
    ) -> InternedString {
        format!("##{}#{}", name.resolve(), introductions.serial()).into()
    }

    // The template-defined global that a reference with these scopes refers to: the one whose
    // introduction scopes are the largest subset of the reference's
    fn template_global_of(
        &self,
        name: InternedString,
        scopes: &steel_parser::scopes::ScopeSetId,
    ) -> Option<InternedString> {
        // A module's mangler may have given it the module's name, which the template's
        // definition may not have
        let unmangled: InternedString =
            crate::parser::expand_visitor::unmangled(name.resolve()).into();
        let names = [name, unmangled];
        let introductions = scopes.introductions();
        // Usually the same expansion defined it
        for name in names {
            if self
                .earlier
                .get(&name)
                .is_some_and(|x| x.contains(&introductions))
            {
                return Some(Self::template_global(name, introductions));
            }
        }
        let (name, defined) = names
            .into_iter()
            .flat_map(|name| {
                self.earlier
                    .get(&name)
                    .into_iter()
                    .flatten()
                    .map(move |x| (name, x))
            })
            .filter(|(_, x)| x.is_subset(scopes))
            .max_by_key(|(_, x)| (x.len(), x.serial()))?;
        Some(Self::template_global(name, defined.clone()))
    }

    fn define_template_globals(&mut self, expr: &mut ExprKind) {
        match expr {
            ExprKind::Begin(b) => {
                for expr in b.exprs.iter_mut() {
                    self.define_template_globals(expr);
                }
            }
            ExprKind::Define(d) => {
                let Some(syn) = d.name.atom_syntax_object_mut() else {
                    return;
                };

                let introductions = syn.scopes.introductions();

                // Skip names the expander already renamed for its expansion, and transformer
                // names, which other compiles refer to without scopes
                if introductions.is_empty()
                    || syn.ty.identifier().is_some_and(|x| {
                        let name = x.resolve();
                        name.starts_with("#%phase1-var:")
                            || crate::parser::expand_visitor::template_macro_written_name(name)
                                .is_some()
                    })
                {
                    return;
                }

                if let Some(name) = syn.ty.identifier_mut() {
                    self.template_globals = true;
                    self.earlier
                        .entry(*name)
                        .or_default()
                        .insert(introductions.clone());
                    *name = Self::template_global(*name, introductions);
                }
            }
            _ => {}
        }
    }

    fn bind(&mut self, syn: &mut SyntaxObject) {
        let Some(name) = syn.ty.identifier_mut() else {
            return;
        };

        let scopes = &syn.scopes;

        let id = self.next_id;
        self.next_id += 1;

        self.by_name
            .entry(*name)
            .or_default()
            .push(self.scope.len());
        self.scope.push(Binder {
            name: *name,
            id,
            syntax: syn.syntax_object_id,
            scopes: scopes.clone(),
        });

        if self.renaming && self.conflicts.contains(&id) {
            let fresh: InternedString =
                format!("##{}#{}#{}", name.resolve(), scopes.serial(), id).into();
            *name = fresh;
            self.renamed.insert(id, fresh);
        }
    }

    fn exit_scope(&mut self, depth: usize) {
        for binder in self.scope.drain(depth..) {
            if let Some(positions) = self.by_name.get_mut(&binder.name) {
                positions.pop();
            }
        }
    }

    // A reference's binder is the local of the same name whose scopes are the largest subset of
    // the reference's. When the innermost local of that name is a different one, both are renamed.
    fn resolve_by_scopes(&mut self, a: &mut Atom) {
        let scopes = &a.syn.scopes;
        let Some(name) = a.syn.ty.identifier().copied() else {
            return;
        };

        let by_scopes = largest_subset(
            self.by_name
                .get(&name)
                .into_iter()
                .flatten()
                .map(|x| (&self.scope[*x].scopes, self.scope[*x].id)),
            scopes,
        );
        let innermost = self.innermost(name).map(|b| b.id);

        let Some(binder) = by_scopes else {
            // No local binder: a global a template defined at the top level, a prelude builtin,
            // or a global. Any local of the same name in the way gets renamed
            if let Some(global) = self.template_global_of(name, scopes) {
                a.syn.ty = TokenType::Identifier(global);
                a.syn.unresolved = false;
                return;
            }
            // A builtin written by a module's macro (not the top level's) refers to the builtin
            if scopes.has_module_except(steel_parser::scopes::Scope::module("top-level"))
                && is_prelude_builtin(&name)
            {
                a.syn.ty = TokenType::Identifier(builtin_to_reserved(name.resolve()));
                a.syn.unresolved = false;
                return;
            }
            // Rename every local of this name in the way, not only the innermost
            if !self.renaming {
                for position in self.by_name.get(&name).into_iter().flatten() {
                    self.conflicts.insert(self.scope[*position].id);
                }
            }
            return;
        };

        a.syn.unresolved = false;

        if !self.renaming {
            if innermost != Some(binder) {
                self.conflicts.insert(binder);
                if let Some(innermost) = innermost {
                    self.conflicts.insert(innermost);
                }
            }
            return;
        }

        if let Some(fresh) = self.renamed.get(&binder) {
            a.syn.ty = TokenType::Identifier(*fresh);
        }
    }

    fn innermost(&self, name: InternedString) -> Option<&Binder> {
        let position = *self.by_name.get(&name)?.last()?;
        self.scope.get(position)
    }

    // Internal defines are in scope for the entire body they appear in
    fn visit_body(&mut self, body: &mut ExprKind) {
        self.bind_internal_defines(body);
        self.visit(body);
    }

    fn define_local_macros(&mut self, exprs: &[ExprKind]) {
        for expr in exprs {
            match expr {
                ExprKind::Begin(b) => self.define_local_macros(&b.exprs),
                ExprKind::List(l) if is_local_macro_definition(expr) => {
                    if let Some(name) = l.get(1).and_then(ExprKind::atom_identifier) {
                        self.local_macros.push((*name, self.scope.len()));
                        self.has_local_macros = true;
                    }
                }
                _ if kernel_local_macro(expr).is_some() => {
                    self.local_macros
                        .push((kernel_local_macro(expr).unwrap(), self.scope.len()));
                    self.has_local_macros = true;
                }
                _ => {}
            }
        }
    }

    fn bind_internal_defines(&mut self, expr: &mut ExprKind) {
        match expr {
            ExprKind::Begin(b) => {
                for expr in b.exprs.iter_mut() {
                    self.bind_internal_defines(expr);
                }
            }
            ExprKind::Define(d) => {
                if let Some(syn) = d.name.atom_syntax_object_mut() {
                    self.bind(syn);
                }
            }
            _ => {}
        }
    }
}

impl VisitorMutRefUnit for ResolveByScopes {
    fn visit_lambda_function(&mut self, lambda_function: &mut LambdaFunction) {
        let depth = self.scope.len();

        for syn in lambda_function.syntax_objects_arguments_mut() {
            self.bind(syn);
        }

        self.bodies += 1;
        self.visit_body(&mut lambda_function.body);
        self.bodies -= 1;

        self.exit_scope(depth);
    }

    fn visit_let(&mut self, l: &mut Let) {
        for (_, expr) in l.bindings.iter_mut() {
            self.visit(expr);
        }

        let depth = self.scope.len();

        for (name, _) in l.bindings.iter_mut() {
            if let Some(syn) = name.atom_syntax_object_mut() {
                self.bind(syn);
            }
        }

        self.bodies += 1;
        self.visit_body(&mut l.body_expr);
        self.bodies -= 1;

        self.exit_scope(depth);
    }

    fn visit_define(&mut self, define: &mut Define) {
        self.visit(&mut define.body);
    }

    fn visit_begin(&mut self, begin: &mut Begin) {
        let visible = self.local_macros.len();

        // In a nested body, defines written by a template are in scope after them
        if self.bodies > 0 {
            for expr in begin.exprs.iter_mut() {
                if let ExprKind::Define(d) = expr {
                    if let Some(syn) = d.name.atom_syntax_object_mut() {
                        let id = syn.syntax_object_id;

                        if !syn.scopes.introductions().is_empty()
                            && !self.scope.iter().any(|b| b.syntax == id)
                        {
                            self.bind(syn);
                        }
                    }
                }
            }
        }

        self.define_local_macros(&begin.exprs);

        if self.renaming && self.local_macros.len() > visible {
            begin.exprs.retain(|x| !is_local_macro_definition(x));
        }

        for expr in begin.exprs.iter_mut() {
            if !is_local_macro_definition(expr) {
                self.visit(expr);
            }
        }

        self.local_macros.truncate(visible);
    }

    fn visit_quote(&mut self, _quote: &mut Quote) {}

    fn visit_atom(&mut self, a: &mut Atom) {
        self.resolve_by_scopes(a)
    }
}

// The binder whose scopes are the largest subset of `scopes`, ties going to the last one.
// Between ones whose scopes don't contain each other, the one more expansions wrote wins:
// a template's identifiers carry the scopes of the macro's definition as well, which can
// make a binder there look like theirs. Scanned from the last one, so most binders are ruled
// out by size alone.
pub(crate) fn largest_subset<'a, T>(
    binders: impl DoubleEndedIterator<Item = (&'a steel_parser::scopes::ScopeSetId, T)>,
    scopes: &steel_parser::scopes::ScopeSetId,
) -> Option<T> {
    let mut best: Option<(&steel_parser::scopes::ScopeSetId, T)> = None;
    let mut best_size = (0, 0);
    for (binder, value) in binders.rev() {
        let len = binder.len();
        if len > scopes.len() {
            continue;
        }
        // One no larger and from no more expansions can't be better
        let introductions = binder.introductions().len();
        if best.is_some() && len <= best_size.1 && introductions <= best_size.0 {
            continue;
        }
        if !binder.is_subset(scopes) {
            continue;
        }
        let better = match &best {
            None => true,
            Some((current, _)) if current.is_subset(binder) => len > current.len(),
            Some((current, _)) if binder.is_subset(current) => false,
            Some((current, _)) => {
                (binder.introductions().len(), len) > (current.introductions().len(), current.len())
            }
        };
        if better {
            best = Some((binder, value));
            best_size = (introductions, len);
        }
    }
    best.map(|x| x.1)
}
