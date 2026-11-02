use core::cell::{Cell, RefCell};

use rustc_hash::FxHashMap;

use crate::rvals::{Result, SteelVal};
use steel_parser::scopes::{Scope, ScopeSetId};

pub(crate) enum SpecRequest {
    Import(SteelVal),
    Export(SteelVal),
    Exports(SteelVal),
    Eval(SteelVal),
}

thread_local! {
    static ANSWERS: RefCell<FxHashMap<String, (ScopeSetId, SteelVal)>> =
        RefCell::new(FxHashMap::default());
    static REQUESTS: RefCell<Vec<(String, SpecRequest)>> = const { RefCell::new(Vec::new()) };
    static ASKING: Cell<bool> = const { Cell::new(false) };
}

// While the compiler expands a module or a top level require. Requests made inside are its own
pub(crate) struct Asking(bool, Vec<(String, SpecRequest)>);

pub(crate) fn asking() -> Asking {
    Asking(ASKING.replace(true), take_requests())
}

impl Drop for Asking {
    fn drop(&mut self) {
        ASKING.set(self.0);
        REQUESTS.with(|x| *x.borrow_mut() = core::mem::take(&mut self.1));
    }
}

fn key(kind: &str, value: &SteelVal) -> String {
    let module = crate::parser::expand_visitor::current_module();
    let module = module.as_ref().map_or("top-level", |x| x.resolve());
    match value {
        SteelVal::SyntaxObject(s) => {
            let span = s.syntax_loc();
            format!(
                "{kind} {module} {:?} {}..{} {}",
                span.source_id(),
                span.start(),
                span.end(),
                s.syntax_datum()
            )
        }
        other => format!("{kind} {module} {other}"),
    }
}

pub(crate) fn spec_answer(kind: SteelVal, value: SteelVal) -> Result<SteelVal> {
    let kind = match &kind {
        SteelVal::SymbolV(s) => s.to_string(),
        _ => stop!(TypeMismatch => "#%spec-answer expects a symbol, found: {}", kind),
    };
    let key = key(&kind, &value);
    if let Some((asked, answer)) = ANSWERS.with(|x| x.borrow().get(&key).cloned()) {
        // Asked again from a new expansion, whose input has new use-site scopes
        let now = scopes_of(&value);
        let old: Vec<Scope> = asked.without(now.scopes()).scopes().to_vec();
        let new = now.without(asked.scopes());
        return Ok(if old.is_empty() {
            answer
        } else {
            rescoped(&answer, &old, &new)
        });
    }
    let (request, name) = match kind.as_str() {
        "import" => (SpecRequest::Import(value), "expand-import"),
        "export" => (SpecRequest::Export(value), "expand-export"),
        "exports" => (SpecRequest::Exports(value), "module->exports"),
        "eval" => (SpecRequest::Eval(value), "#%spec-answer"),
        _ => stop!(BadSyntax => "#%spec-answer: unknown request: {}", kind),
    };
    // The compiler can't answer while a transformer runs, so the transformer stops, the
    // compiler works out the answer, and the spec is expanded again
    if ASKING.get() {
        REQUESTS.with(|x| x.borrow_mut().push((key, request)));
    }
    stop!(Generic => "{}: only available while a require or provide spec expands", name)
}

pub(crate) fn take_requests() -> Vec<(String, SpecRequest)> {
    REQUESTS.with(|x| core::mem::take(&mut *x.borrow_mut()))
}

pub(crate) fn has_requests() -> bool {
    REQUESTS.with(|x| !x.borrow().is_empty())
}

fn scopes_of(value: &SteelVal) -> ScopeSetId {
    match value {
        SteelVal::SyntaxObject(s) => s.scopes(),
        _ => ScopeSetId::EMPTY,
    }
}

fn rescoped(value: &SteelVal, old: &[Scope], new: &ScopeSetId) -> SteelVal {
    match value {
        SteelVal::SyntaxObject(s) => {
            let mut syntax = crate::rvals::Syntax::clone(s);
            syntax.syntax = rescoped(&s.syntax, old, new);
            if old.iter().all(|x| s.scopes().contains(*x)) {
                syntax = syntax.with_scopes(s.scopes().without(old).union(new));
            }
            SteelVal::SyntaxObject(crate::gc::Gc::new(syntax))
        }
        SteelVal::ListV(items) => {
            SteelVal::ListV(items.iter().map(|x| rescoped(x, old, new)).collect())
        }
        other => other.clone(),
    }
}

pub(crate) fn answers() -> usize {
    ANSWERS.with(|x| x.borrow().len())
}

pub(crate) fn answer(key: String, asked: &SteelVal, value: SteelVal) {
    ANSWERS.with(|x| x.borrow_mut().insert(key, (scopes_of(asked), value)));
}

pub(crate) fn clear() {
    ANSWERS.with(|x| x.borrow_mut().clear());
    REQUESTS.with(|x| x.borrow_mut().clear());
}
