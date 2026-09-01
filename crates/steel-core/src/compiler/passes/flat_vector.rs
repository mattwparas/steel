//! Promotes vector construction sites to flat vectors.
//!
//! Proving a constructed vector never escapes is hopeless - it usually lands in a
//! cons cell immediately - so this walks backwards from the mutations instead. Every
//! mutating vector call has its target resolved to the binding it came from, and if
//! that binding was initialized by a construction site then only that site has to stay
//! mutable. A target that does not resolve means some unknown vector gets mutated, and
//! the whole unit is left alone.

use rustc_hash::{FxHashMap, FxHashSet};
use steel_parser::{
    ast::{Define, ExprKind, LambdaFunction, Let, List},
    parser::SyntaxObjectId,
};

use super::{analysis::Analysis, VisitorMutRefUnit, VisitorMutUnitRef};

const MUTATING: &[&str] = &[
    "vector-set!",
    "vector-fill!",
    "vector-copy!",
    "vector-swap!",
    "vector-append!",
    "vector-push!",
    "vector-push",
    "vector-push-front",
    "mutable-vector-pop!",
    "mutable-vector->clear",
    "push",
    "push-front",
    "pop-front",
];

// `make-vector` and `mutable-vector` have no flat counterpart yet, but they still have
// to be recognized so that a mutated binding resolves to one rather than looking like
// an unknown origin and bailing the whole unit.
const CONSTRUCTORS: &[&str] = &["vector", "make-vector", "mutable-vector"];

const REWRITABLE: &str = "vector";
const FLAT: &str = "flat-vector";
const PRIM: &str = "#%prim.";

pub fn enabled() -> bool {
    static ENABLED: std::sync::LazyLock<bool> = std::sync::LazyLock::new(|| {
        matches!(
            std::env::var("STEEL_FLAT_VECTORS").as_deref(),
            Ok("1") | Ok("true")
        )
    });

    *ENABLED
}

fn strip_prim(name: &str) -> &str {
    name.strip_prefix(PRIM).unwrap_or(name)
}

#[derive(Clone, Copy, PartialEq)]
enum Origin {
    Ctor(SyntaxObjectId),
    Unknown,
}

#[derive(Debug, Default)]
pub struct Report {
    pub bailed: bool,
    pub sites: usize,
    pub promoted: usize,
}

struct Collect<'a> {
    analysis: &'a Analysis,
    origins: FxHashMap<SyntaxObjectId, Origin>,
    targets: Vec<Option<SyntaxObjectId>>,
    sites: FxHashSet<SyntaxObjectId>,
}

impl<'a> Collect<'a> {
    fn classify(&self, init: &ExprKind) -> Origin {
        if let ExprKind::List(l) = init {
            if let Some(name) = l.first_ident() {
                if CONSTRUCTORS.contains(&strip_prim(name.resolve())) {
                    if let Some(syn) = l.args.first().and_then(|x| x.atom_syntax_object()) {
                        return Origin::Ctor(syn.syntax_object_id);
                    }
                }
            }
        }

        Origin::Unknown
    }

    fn record_binding(&mut self, name: &ExprKind, init: &ExprKind) {
        if let Some(syn) = name.atom_syntax_object() {
            let origin = self.classify(init);
            self.origins.insert(syn.syntax_object_id, origin);
        }
    }

    // The binding a mutation is aimed at, or None when it cannot be pinned down.
    fn resolve(&self, target: &ExprKind) -> Option<SyntaxObjectId> {
        let syn = target.atom_syntax_object()?;
        self.analysis.get(syn)?.refers_to
    }
}

impl<'a> VisitorMutUnitRef<'a> for Collect<'a> {
    fn visit_let(&mut self, l: &'a Let) {
        for (name, init) in &l.bindings {
            self.record_binding(name, init);
            self.visit(init);
        }

        self.visit(&l.body_expr);
    }

    fn visit_define(&mut self, define: &'a Define) {
        self.record_binding(&define.name, &define.body);
        self.visit(&define.body);
    }

    fn visit_list(&mut self, l: &'a List) {
        // A let that has already been lowered to an applied lambda. The literal is in
        // call position so this is its only call site, and the arguments are its inits.
        if let Some(ExprKind::LambdaFunction(func)) = l.args.first() {
            for (param, arg) in func.args.iter().zip(l.args.iter().skip(1)) {
                self.record_binding(param, arg);
            }
        }

        if let Some(name) = l.first_ident() {
            match strip_prim(name.resolve()) {
                name if MUTATING.contains(&name) => {
                    let target = l.args.get(1).and_then(|target| self.resolve(target));
                    self.targets.push(target);
                }
                REWRITABLE => {
                    if let Some(syn) = l.args.first().and_then(|x| x.atom_syntax_object()) {
                        self.sites.insert(syn.syntax_object_id);
                    }
                }
                _ => {}
            }
        }

        for arg in &l.args {
            self.visit(arg);
        }
    }
}

struct Rewrite {
    tainted: FxHashSet<SyntaxObjectId>,
    promoted: usize,
}

impl VisitorMutRefUnit for Rewrite {
    fn visit_list(&mut self, l: &mut List) {
        if let Some(name) = l.first_ident() {
            let name = name.resolve();
            let promote = strip_prim(name) == REWRITABLE
                && l.args
                    .first()
                    .and_then(|x| x.atom_syntax_object())
                    .is_some_and(|syn| !self.tainted.contains(&syn.syntax_object_id));

            if promote {
                let flat = if name.starts_with(PRIM) {
                    format!("{}{}", PRIM, FLAT)
                } else {
                    FLAT.to_string()
                };

                if let Some(ident) = l.args.first_mut().and_then(|x| x.atom_identifier_mut()) {
                    *ident = flat.as_str().into();
                    self.promoted += 1;
                }
            }
        }

        for arg in &mut l.args {
            self.visit(arg);
        }
    }
}

pub fn run(exprs: &mut Vec<ExprKind>, analysis: &Analysis) -> Report {
    let mut collect = Collect {
        analysis,
        origins: FxHashMap::default(),
        targets: Vec::new(),
        sites: FxHashSet::default(),
    };

    for expr in exprs.iter() {
        collect.visit(expr);
    }

    let mut tainted = FxHashSet::default();
    let mut bailed = false;

    for target in &collect.targets {
        // A binding that is `set!` can hold a vector from anywhere, so it is no more
        // pinned down than an unresolvable target.
        let origin = target.filter(|id| {
            analysis
                .identifier_info()
                .get(id)
                .is_some_and(|info| !info.set_bang)
        });

        match origin.map(|id| collect.origins.get(&id).copied().unwrap_or(Origin::Unknown)) {
            Some(Origin::Ctor(site)) => {
                tainted.insert(site);
            }
            _ => {
                bailed = true;
                break;
            }
        }
    }

    let sites = collect.sites.len();

    if bailed {
        log::debug!(target: "flat-vector", "bailed: a mutation target did not resolve to a construction site");
        return Report {
            bailed,
            sites,
            promoted: 0,
        };
    }

    let mut rewrite = Rewrite {
        tainted,
        promoted: 0,
    };

    for expr in exprs.iter_mut() {
        rewrite.visit(expr);
    }

    log::debug!(target: "flat-vector", "promoted {}/{} construction sites", rewrite.promoted, sites);

    Report {
        bailed,
        sites,
        promoted: rewrite.promoted,
    }
}
