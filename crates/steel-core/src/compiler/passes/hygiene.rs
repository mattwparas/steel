use rustc_hash::{FxHashMap, FxHashSet};

use crate::parser::{
    ast::{Atom, Begin, Define, ExprKind, LambdaFunction, Let, Quote},
    expand_visitor::{is_local_macro_definition, local_macro_captured},
    interner::InternedString,
    parser::{ExpansionMark, SyntaxObject},
};

use super::{mangle::is_prelude_builtin, VisitorMutRefUnit};
use crate::steel_vm::primitives::builtin_to_reserved;

// Identifiers from a syntax-rules template are marked with the expansion that introduced
// them. Everything after expansion resolves variables by name, so this renames local binders
// where resolving by name would pick a different binder than the marks.
//
// Has to run after expansion and lowering, and before constant evaluation.
//
// Its a bit odd - we now have a few various implementations of things to enforce hygience.
// There is the `unresolved` and `introduced_via_macro` flags. At some point, we'll want
// to do a full on rewrite of this part most likely. But for now, this pass is a decent
// attempt at fixing some bugs that have been reported.
struct Binder {
    name: InternedString,
    mark: ExpansionMark,
    id: usize,
    local_macro: Option<InternedString>,
}

#[derive(Default)]
pub struct ResolveExpansionMarks {
    scope: Vec<Binder>,
    // How many of the binders in scope were introduced by a template
    template_binders: usize,
    next_id: usize,
    conflicts: FxHashSet<usize>,
    renamed: FxHashMap<usize, InternedString>,
    renaming: bool,
    // usize here is the binder count
    local_macros: Vec<(InternedString, usize)>,
    has_local_macros: bool,
    template_globals: FxHashSet<(InternedString, ExpansionMark)>,
}

struct RestoreCapturedNames;

impl VisitorMutRefUnit for RestoreCapturedNames {
    fn visit_atom(&mut self, a: &mut Atom) {
        if let Some(name) = a.syn.ty.identifier_mut() {
            if let Some((_, variable)) = local_macro_captured(name.resolve()) {
                *name = variable.into();
            }
        }
    }
}

impl ResolveExpansionMarks {
    pub fn resolve(expr: &mut ExprKind) {
        let mut pass = Self::default();

        pass.define_template_globals(expr);
        pass.visit(expr);

        if pass.conflicts.is_empty() && !pass.has_local_macros && pass.template_globals.is_empty() {
            return;
        }

        pass.renaming = true;
        pass.next_id = 0;
        pass.visit(expr);
    }

    fn template_global(name: InternedString, mark: ExpansionMark) -> InternedString {
        format!("##{}#{}", name.resolve(), mark.0).into()
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

                let mark = syn.mark;

                if !mark.is_template() {
                    return;
                }

                if let Some(name) = syn.ty.identifier_mut() {
                    self.template_globals.insert((*name, mark));
                    *name = Self::template_global(*name, mark);
                }
            }
            _ => {}
        }
    }

    fn bind(&mut self, syn: &mut SyntaxObject) {
        let mark = syn.mark;

        let Some(name) = syn.ty.identifier_mut() else {
            return;
        };

        let mut local_macro = None;

        if let Some((macro_name, variable)) = local_macro_captured(name.resolve()) {
            local_macro = Some(macro_name.into());
            *name = variable.into();
        }

        let id = self.next_id;
        self.next_id += 1;

        if mark.is_template() {
            self.template_binders += 1;
        }

        self.scope.push(Binder {
            name: *name,
            mark,
            id,
            local_macro,
        });

        if self.renaming && self.conflicts.contains(&id) {
            let fresh: InternedString = if mark == ExpansionMark::NONE {
                format!("##{}#{}#{}", name.resolve(), mark.0, id).into()
            } else {
                format!("##{}#{}", name.resolve(), mark.0).into()
            };
            *name = fresh;
            self.renamed.insert(id, fresh);
        }
    }

    fn exit_scope(&mut self, depth: usize) {
        for binder in self.scope.drain(depth..) {
            if binder.mark.is_template() {
                self.template_binders -= 1;
            }
        }
    }

    fn innermost(&self, name: InternedString) -> Option<&Binder> {
        self.scope.iter().rev().find(|b| b.name == name)
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
                _ => {}
            }
        }
    }

    fn captured_variable(&self, macro_name: &str, variable: &str) -> (InternedString, usize) {
        let depth = self
            .local_macros
            .iter()
            .rev()
            .find(|x| x.0.resolve() == macro_name)
            .map_or(self.scope.len(), |x| x.1);

        (variable.into(), depth)
    }

    fn resolve_captured(&mut self, a: &mut Atom, variable: InternedString, depth: usize) {
        let depth = depth.min(self.scope.len());

        if !self.renaming {
            self.has_local_macros = true;

            for binder in self.scope[depth..].iter().filter(|b| b.name == variable) {
                self.conflicts.insert(binder.id);
            }

            return;
        }

        let binder = self.scope[..depth]
            .iter()
            .rev()
            .find(|b| b.name == variable);
        let name = binder
            .and_then(|b| self.renamed.get(&b.id))
            .copied()
            .unwrap_or(variable);

        if let Some(captured) = a.syn.ty.identifier_mut() {
            *captured = name;
        }

        a.syn.unresolved = false;
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

impl VisitorMutRefUnit for ResolveExpansionMarks {
    fn visit_lambda_function(&mut self, lambda_function: &mut LambdaFunction) {
        let depth = self.scope.len();

        for syn in lambda_function.syntax_objects_arguments_mut() {
            self.bind(syn);
        }

        self.visit_body(&mut lambda_function.body);

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

        self.visit_body(&mut l.body_expr);

        self.exit_scope(depth);
    }

    fn visit_define(&mut self, define: &mut Define) {
        self.visit(&mut define.body);
    }

    fn visit_begin(&mut self, begin: &mut Begin) {
        let visible = self.local_macros.len();

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

    fn visit_quote(&mut self, quote: &mut Quote) {
        if self.renaming && self.has_local_macros {
            RestoreCapturedNames.visit(&mut quote.expr);
        }
    }

    fn visit_atom(&mut self, a: &mut Atom) {
        let mark = a.syn.mark;

        let Some(name) = a.syn.ty.identifier_mut() else {
            return;
        };

        if let Some((macro_name, variable)) = local_macro_captured(name.resolve()) {
            let (variable, depth) = self.captured_variable(macro_name, variable);

            // We want to keep the mark, or the captured name if a macro built
            // this from quoted values
            let macro_name: InternedString = macro_name.into();
            let bound_by_template = self.scope.iter().any(|b| {
                b.name == variable
                    && (mark.is_template() && b.mark == mark || b.local_macro == Some(macro_name))
            });

            if !bound_by_template {
                self.resolve_captured(a, variable, depth);
                return;
            }

            *name = variable;
        }

        if mark == ExpansionMark::NONE {
            // The template binders between this reference and its binder have to be renamed
            if !self.renaming && self.template_binders > 0 {
                for binder in self.scope.iter().rev().filter(|b| b.name == *name) {
                    if !binder.mark.is_template() {
                        break;
                    }

                    self.conflicts.insert(binder.id);
                }
            }

            if self.renaming && self.has_local_macros {
                let binder = self
                    .scope
                    .iter()
                    .rev()
                    .find(|b| b.name == *name && !b.mark.is_template());

                if let Some(fresh) = binder.and_then(|b| self.renamed.get(&b.id)) {
                    *name = *fresh;
                }
            }

            return;
        }

        let by_mark = if mark == ExpansionMark::UNKNOWN {
            None
        } else {
            self.scope
                .iter()
                .rev()
                .find(|b| b.name == *name && b.mark == mark)
        };

        if by_mark.is_none() && self.template_globals.contains(&(*name, mark)) {
            *name = Self::template_global(*name, mark);
            a.syn.unresolved = false;
            return;
        }

        if by_mark.is_none() && mark.is_definition_resolved() && is_prelude_builtin(name) {
            *name = builtin_to_reserved(name.resolve());
            a.syn.unresolved = false;
            return;
        }

        if !self.renaming {
            if let Some(binder) = by_mark {
                a.syn.unresolved = false;

                if self.innermost(*name).map(|b| b.id) != Some(binder.id) {
                    self.conflicts.insert(binder.id);
                }
            }

            return;
        }

        let binder = by_mark.or_else(|| self.innermost(*name));

        if let Some(fresh) = binder.and_then(|b| self.renamed.get(&b.id)) {
            *name = *fresh;
            a.syn.unresolved = false;
        }
    }
}
