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

// Every identifier has the scopes of the expansions and binding forms it's in. Everything after
// expansion resolves variables by name, so this renames local binders where resolving by name
// would pick a different binder than the scopes.
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
    // Where in `scope` each name's binders are
    by_name: FxHashMap<InternedString, Vec<usize>>,
    next_id: usize,
    conflicts: FxHashSet<usize>,
    renamed: FxHashMap<usize, InternedString>,
    renaming: bool,
    // usize here is the binder count
    local_macros: Vec<(InternedString, usize)>,
    has_local_macros: bool,
    // Whether this input's templates define globals
    template_globals: bool,
    // The globals templates defined in this and earlier compiles, which a macro those
    // templates defined can still name, from a later input or another module
    earlier: TemplateGlobals,
    // How many functions and lets the visit is in
    bodies: usize,
}

// Kept by the compiler: each name's definitions, by the introduction scopes of what defined it.
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
        format!("##{}#{}", name.resolve(), introductions.0).into()
    }

    // A global a template defined that a reference with these scopes means: the one whose
    // introduction scopes are the largest subset of the reference's
    fn template_global_of(
        &self,
        name: InternedString,
        scopes: steel_parser::scopes::ScopeSetId,
    ) -> Option<InternedString> {
        let defined = self.earlier.get(&name)?;
        // Most often the one the same expansion wrote
        let introductions = scopes.introductions();
        if defined.contains(&introductions) {
            return Some(Self::template_global(name, introductions));
        }
        let defined = defined
            .iter()
            .filter(|x| x.is_subset(scopes))
            .max_by_key(|x| (x.len(), x.0))?;
        Some(Self::template_global(name, *defined))
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

                // Not one the expander already named for its expansion, nor a transformer's,
                // which other compiles name without scopes
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
                    self.earlier.entry(*name).or_default().insert(introductions);
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

        let scopes = syn.scopes;

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
            scopes,
        });

        if self.renaming && self.conflicts.contains(&id) {
            // One name per binder
            let fresh: InternedString = format!("##{}#{}#{}", name.resolve(), scopes.0, id).into();
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

    // A reference's binder is the local of its name whose scopes are the largest subset of its
    // own. Where the innermost local of the name isn't that one, both are renamed.
    fn resolve_by_scopes(&mut self, a: &mut Atom) {
        let scopes = a.syn.scopes;
        let Some(name) = a.syn.ty.identifier().copied() else {
            return;
        };

        // The last of the largest, as the expander's `meaning` picks
        let by_scopes = self
            .by_name
            .get(&name)
            .into_iter()
            .flatten()
            .map(|x| &self.scope[*x])
            .filter(|b| b.scopes.is_subset(scopes))
            .max_by_key(|b| b.scopes.len())
            .map(|b| b.id);
        let innermost = self.innermost(name).map(|b| b.id);

        let Some(binder) = by_scopes else {
            // No local: one a template defines at the top level, a prelude builtin, or a global,
            // which a local of its name in the way is renamed for
            if let Some(global) = self.template_global_of(name, scopes) {
                a.syn.ty = TokenType::Identifier(global);
                a.syn.unresolved = false;
                return;
            }
            // From a module's macro, not the top level's own, a builtin means the builtin
            if scopes.has_module_except(steel_parser::scopes::Scope::module("top-level"))
                && is_prelude_builtin(&name)
            {
                a.syn.ty = TokenType::Identifier(builtin_to_reserved(name.resolve()));
                a.syn.unresolved = false;
                return;
            }
            // Every local of its name in the way, not only the innermost
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

        // A template's defines in a body that's nested in a branch are in scope after them
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
