// Checks that resolving each local reference by scopes, as Racket does, finds the binder the
// hygiene pass's renaming leads the compiler to. `STEEL_SCOPES_CHECK=1` reports each reference
// where they differ.

use rustc_hash::FxHashMap;
use steel_parser::scopes::ScopeSetId;

use crate::compiler::passes::VisitorMutRefUnit;
use crate::parser::{
    ast::{Atom, Begin, Define, ExprKind, LambdaFunction, Let, Quote},
    interner::InternedString,
    parser::{SyntaxObject, SyntaxObjectId},
    span::Span,
};

// Whether to check the hygiene pass's renaming against resolving by scopes
// (`STEEL_SCOPES_CHECK`)
pub fn comparing() -> bool {
    static COMPARING: once_cell::sync::Lazy<bool> =
        once_cell::sync::Lazy::new(|| std::env::var_os("STEEL_SCOPES_CHECK").is_some());
    *COMPARING
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Resolution {
    Local(SyntaxObjectId),
    // No local binder: a global, a builtin, or free
    Global,
    // Two binders, neither's scopes a subset of the other's
    Ambiguous,
}

struct Binder {
    name: InternedString,
    scopes: ScopeSetId,
    id: SyntaxObjectId,
}

#[derive(Default)]
struct Resolver {
    by_scopes: bool,
    scope: Vec<Binder>,
    resolved: FxHashMap<SyntaxObjectId, (InternedString, Span, Resolution)>,
    binders: FxHashMap<SyntaxObjectId, (InternedString, Span)>,
}

impl Resolver {
    fn bind(&mut self, syn: &SyntaxObject) {
        if let Some(name) = syn.ty.identifier() {
            self.scope.push(Binder {
                name: *name,
                scopes: syn.scopes.clone(),
                id: syn.syntax_object_id,
            });
            self.binders.insert(syn.syntax_object_id, (*name, syn.span));
        }
    }

    fn resolve(&self, syn: &SyntaxObject, name: InternedString) -> Resolution {
        if !self.by_scopes {
            return self
                .scope
                .iter()
                .rev()
                .find(|b| b.name == name)
                .map_or(Resolution::Global, |b| Resolution::Local(b.id));
        }

        // The binder whose scopes are the largest subset of the reference's
        let candidates: Vec<&Binder> = self
            .scope
            .iter()
            .filter(|b| b.name == name && b.scopes.is_subset(&syn.scopes))
            .collect();
        let Some(largest) = candidates.iter().max_by_key(|b| b.scopes.len()) else {
            if std::env::var_os("STEEL_SCOPES_CHECK").is_some_and(|x| x == "debug") {
                let binders: Vec<_> = self
                    .scope
                    .iter()
                    .filter(|b| b.name == name)
                    .map(|b| b.scopes.scopes())
                    .collect();
                if !binders.is_empty() {
                    eprintln!(
                        "SCOPES-DEBUG {} {:?} ref {:?} binders {:?}",
                        name.resolve(),
                        syn.span,
                        syn.scopes.scopes(),
                        binders
                    );
                }
            }
            return Resolution::Global;
        };
        if candidates
            .iter()
            .all(|b| b.scopes.is_subset(&largest.scopes))
        {
            Resolution::Local(largest.id)
        } else {
            Resolution::Ambiguous
        }
    }

    // A body's internal defines are in scope for all of it
    fn bind_internal_defines(&mut self, expr: &ExprKind) {
        match expr {
            ExprKind::Begin(b) => {
                for expr in b.exprs.iter() {
                    self.bind_internal_defines(expr);
                }
            }
            ExprKind::Define(d) => {
                if let Some(syn) = d.name.atom_syntax_object() {
                    self.bind(syn);
                }
            }
            _ => {}
        }
    }

    fn visit_body(&mut self, body: &mut ExprKind) {
        self.bind_internal_defines(body);
        self.visit(body);
    }
}

impl VisitorMutRefUnit for Resolver {
    fn visit_lambda_function(&mut self, lambda_function: &mut LambdaFunction) {
        let depth = self.scope.len();
        for arg in lambda_function.args.iter() {
            if let Some(syn) = arg.atom_syntax_object() {
                self.bind(syn);
            }
        }
        self.visit_body(&mut lambda_function.body);
        self.scope.truncate(depth);
    }

    fn visit_let(&mut self, l: &mut Let) {
        for (_, expr) in l.bindings.iter_mut() {
            self.visit(expr);
        }
        let depth = self.scope.len();
        for (binder, _) in l.bindings.iter() {
            if let Some(syn) = binder.atom_syntax_object() {
                self.bind(syn);
            }
        }
        self.visit_body(&mut l.body_expr);
        self.scope.truncate(depth);
    }

    fn visit_define(&mut self, define: &mut Define) {
        self.visit(&mut define.body);
    }

    fn visit_begin(&mut self, begin: &mut Begin) {
        for expr in begin.exprs.iter_mut() {
            self.visit(expr);
        }
    }

    fn visit_quote(&mut self, _quote: &mut Quote) {}

    fn visit_atom(&mut self, a: &mut Atom) {
        if let Some(name) = a.syn.ty.identifier().copied() {
            let resolution = self.resolve(&a.syn, name);
            self.resolved
                .insert(a.syn.syntax_object_id, (name, a.syn.span, resolution));
        }
    }
}

// Each local reference's resolution by scopes, before the hygiene pass renames
pub struct ByScopes {
    resolved: FxHashMap<SyntaxObjectId, (InternedString, Span, Resolution)>,
    binders: FxHashMap<SyntaxObjectId, (InternedString, Span)>,
}

pub fn by_scopes(expr: &mut ExprKind) -> ByScopes {
    let mut resolver = Resolver {
        by_scopes: true,
        ..Resolver::default()
    };
    resolver.visit(expr);
    ByScopes {
        resolved: resolver.resolved,
        binders: resolver.binders,
    }
}

// Reports where resolving by names after the hygiene pass differs
pub fn compare(expr: &mut ExprKind, before: &ByScopes) {
    let mut resolver = Resolver::default();
    resolver.visit(expr);

    let describe = |resolution: Resolution| match resolution {
        Resolution::Local(id) => match before.binders.get(&id) {
            Some((name, span)) => format!("local {} at {:?}", name.resolve(), span),
            None => format!("local {id:?}"),
        },
        Resolution::Global => "global".to_string(),
        Resolution::Ambiguous => "ambiguous".to_string(),
    };

    let mut differences: Vec<_> = before
        .resolved
        .iter()
        .filter_map(|(id, (name, span, by_scopes))| {
            let (_, _, by_names) = resolver.resolved.get(id)?;
            (by_scopes != by_names).then(|| (*span, *name, *by_scopes, *by_names))
        })
        .collect();
    differences.sort_by_key(|x| (x.0.source_id().map(|x| x.0), x.0.start));

    if std::env::var_os("STEEL_SCOPES_CHECK").is_some_and(|x| x == "count") {
        let locals = before
            .resolved
            .values()
            .filter(|x| matches!(x.2, Resolution::Local(_)))
            .count();
        eprintln!(
            "SCOPES-COUNT {} references, {} local, {} different",
            before.resolved.len(),
            locals,
            differences.len()
        );
    }

    for (span, name, by_scopes, by_names) in differences {
        eprintln!(
            "SCOPES {} at {:?}: scopes say {}, names say {}",
            name.resolve(),
            span,
            describe(by_scopes),
            describe(by_names)
        );
    }
}
