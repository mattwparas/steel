use rustc_hash::{FxHashMap, FxHashSet};

use crate::parser::{
    ast::{Atom, Define, ExprKind, LambdaFunction, Let, Quote},
    interner::InternedString,
    parser::{ExpansionMark, SyntaxObject},
};

use super::VisitorMutRefUnit;

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
}

impl ResolveExpansionMarks {
    pub fn resolve(expr: &mut ExprKind) {
        let mut pass = Self::default();

        pass.visit(expr);

        if pass.conflicts.is_empty() {
            return;
        }

        pass.renaming = true;
        pass.next_id = 0;
        pass.visit(expr);
    }

    fn bind(&mut self, syn: &mut SyntaxObject) {
        let mark = syn.mark;

        let Some(name) = syn.ty.identifier_mut() else {
            return;
        };

        let id = self.next_id;
        self.next_id += 1;

        if mark.is_template() {
            self.template_binders += 1;
        }

        self.scope.push(Binder {
            name: *name,
            mark,
            id,
        });

        if self.renaming && self.conflicts.contains(&id) {
            let fresh: InternedString = format!("##{}#{}", name.resolve(), mark.0).into();
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

    // Internal define names are bound in `visit_body`. Top level defines are
    // globals, which this pass doesn't touch.
    fn visit_define(&mut self, define: &mut Define) {
        self.visit(&mut define.body);
    }

    fn visit_quote(&mut self, _quote: &mut Quote) {}

    fn visit_atom(&mut self, a: &mut Atom) {
        let mark = a.syn.mark;

        let Some(name) = a.syn.ty.identifier_mut() else {
            return;
        };

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

        if !self.renaming {
            if let Some(binder) = by_mark {
                if self.innermost(*name).map(|b| b.id) != Some(binder.id) {
                    self.conflicts.insert(binder.id);
                }
            }

            return;
        }

        // Without a binder with the same mark, resolve by name
        let binder = by_mark.or_else(|| self.innermost(*name));

        if let Some(fresh) = binder.and_then(|b| self.renamed.get(&b.id)) {
            *name = *fresh;
            a.syn.unresolved = false;
        }
    }
}
