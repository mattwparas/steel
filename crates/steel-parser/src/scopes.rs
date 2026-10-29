// Sets of scopes, as Racket's expander has them. A scope is a fresh token; every identifier has
// a set of them. Sets are interned, so a syntax object holds one `ScopeSetId`, and the set
// operations are memoised.

use core::sync::atomic::{AtomicU32, Ordering};
use std::sync::{Arc, Mutex};

use once_cell::sync::Lazy;
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};

#[derive(
    Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Debug, Ord, PartialOrd, Default,
)]
pub struct Scope(pub u32);

static SCOPE_COUNTER: AtomicU32 = AtomicU32::new(1);

// Each module's scope, both ways
#[allow(clippy::type_complexity)]
static MODULES: once_cell::sync::Lazy<Mutex<(FxHashMap<String, Scope>, FxHashMap<Scope, String>)>> =
    once_cell::sync::Lazy::new(Default::default);

// The next scope's number, below the flag bits
fn next_scope() -> u32 {
    let n = SCOPE_COUNTER.fetch_add(1, Ordering::Relaxed);
    assert!(
        n < 1 << 30,
        "out of scopes: more than 2^30 made in this process"
    );
    n
}

impl Scope {
    // A macro expansion's introduction scope, as against a binding form's
    const INTRODUCTION: u32 = 1 << 31;
    // A macro use's use-site scope, which its input gets and a definition's name drops
    const USE_SITE: u32 = 1 << 30;
    // A module's, which what its macros' templates write gets: both bits
    const MODULE: u32 = Self::INTRODUCTION | Self::USE_SITE;

    // A binding form's
    pub fn fresh() -> Self {
        Scope(next_scope())
    }

    pub fn fresh_introduction() -> Self {
        Scope(next_scope() | Self::INTRODUCTION)
    }

    pub fn is_introduction(self) -> bool {
        self.0 & Self::MODULE == Self::INTRODUCTION
    }

    pub fn fresh_use_site() -> Self {
        Scope(next_scope() | Self::USE_SITE)
    }

    pub fn is_use_site(self) -> bool {
        self.0 & Self::MODULE == Self::USE_SITE
    }

    // The scope of a module, by its name: the same one each time
    pub fn module(name: &str) -> Self {
        let mut modules = MODULES.lock().unwrap();
        if let Some(scope) = modules.0.get(name) {
            return *scope;
        }
        let scope = Scope(next_scope() | Self::MODULE);
        modules.0.insert(name.to_string(), scope);
        modules.1.insert(scope, name.to_string());
        scope
    }

    // The module a module scope is of
    pub fn module_name(self) -> Option<String> {
        MODULES.lock().unwrap().1.get(&self).cloned()
    }

    // The prelude's, which its macros' templates have as well as their modules'
    pub fn prelude() -> Self {
        Self::module("#%prelude")
    }

    pub fn is_module(self) -> bool {
        self.0 & Self::MODULE == Self::MODULE
    }

    // When it was made: scopes made later have later times
    pub fn time(self) -> u32 {
        self.0 & !Self::MODULE
    }
}

#[derive(
    Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Debug, Ord, PartialOrd, Default,
)]
pub struct ScopeSetId(pub u32);

#[derive(Default)]
struct Interner {
    // Each set's scopes, sorted
    sets: Vec<Arc<[Scope]>>,
    ids: FxHashMap<Arc<[Scope]>, ScopeSetId>,
    added: FxHashMap<(ScopeSetId, Scope), ScopeSetId>,
    removed: FxHashMap<(ScopeSetId, Scope), ScopeSetId>,
    unions: FxHashMap<(ScopeSetId, ScopeSetId), ScopeSetId>,
}

impl Interner {
    fn new() -> Self {
        let mut interner = Interner::default();
        // The empty set is id 0
        interner.intern(Vec::new());
        interner
    }

    fn intern(&mut self, scopes: Vec<Scope>) -> ScopeSetId {
        if let Some(id) = self.ids.get(scopes.as_slice()) {
            return *id;
        }
        let id = ScopeSetId(self.sets.len() as u32);
        let scopes: Arc<[Scope]> = scopes.into();
        self.sets.push(Arc::clone(&scopes));
        self.ids.insert(scopes, id);
        id
    }

    fn get(&self, id: ScopeSetId) -> &[Scope] {
        &self.sets[id.0 as usize]
    }
}

static INTERNER: Lazy<Mutex<Interner>> = Lazy::new(|| Mutex::new(Interner::new()));

thread_local! {
    // This thread's copy of the interned sets, which never change once made, and of the
    // operations' results, so most of them don't take the lock
    static LOCAL: core::cell::RefCell<Local> = core::cell::RefCell::new(Local::default());
}

#[derive(Default)]
struct Local {
    sets: Vec<Arc<[Scope]>>,
    unions: FxHashMap<(ScopeSetId, ScopeSetId), ScopeSetId>,
    added: FxHashMap<(ScopeSetId, Scope), ScopeSetId>,
}

impl Local {
    fn get(&mut self, id: ScopeSetId) -> Arc<[Scope]> {
        if id.0 as usize >= self.sets.len() {
            let interner = INTERNER.lock().unwrap();
            self.sets
                .extend(interner.sets[self.sets.len()..].iter().cloned());
        }
        Arc::clone(&self.sets[id.0 as usize])
    }
}

fn local_set(id: ScopeSetId) -> Arc<[Scope]> {
    LOCAL.with(|x| x.borrow_mut().get(id))
}

// Reads a set without counting a reference to it
fn with_sets<R>(a: ScopeSetId, b: ScopeSetId, f: impl FnOnce(&[Scope], &[Scope]) -> R) -> R {
    LOCAL.with(|x| {
        let mut local = x.borrow_mut();
        let needed = a.0.max(b.0) as usize;
        if needed >= local.sets.len() {
            let interner = INTERNER.lock().unwrap();
            let have = local.sets.len();
            local.sets.extend(interner.sets[have..].iter().cloned());
        }
        f(&local.sets[a.0 as usize], &local.sets[b.0 as usize])
    })
}

impl ScopeSetId {
    pub const EMPTY: Self = ScopeSetId(0);

    pub fn of(scopes: &[Scope]) -> Self {
        let mut scopes = scopes.to_vec();
        scopes.sort_unstable();
        scopes.dedup();
        INTERNER.lock().unwrap().intern(scopes)
    }

    // The set of scopes that are already sorted and distinct
    pub fn of_sorted(scopes: &[Scope]) -> Self {
        debug_assert!(scopes.windows(2).all(|x| x[0] < x[1]));
        INTERNER.lock().unwrap().intern(scopes.to_vec())
    }

    // The set's scopes, sorted
    pub fn scopes(self) -> Arc<[Scope]> {
        local_set(self)
    }

    pub fn len(self) -> usize {
        if self.is_empty() {
            return 0;
        }
        with_sets(self, self, |x, _| x.len())
    }

    pub fn is_empty(self) -> bool {
        self == Self::EMPTY
    }

    pub fn contains(self, scope: Scope) -> bool {
        !self.is_empty() && with_sets(self, self, |x, _| x.binary_search(&scope).is_ok())
    }

    pub fn add(self, scope: Scope) -> Self {
        if let Some(id) = LOCAL.with(|x| x.borrow().added.get(&(self, scope)).copied()) {
            return id;
        }
        let id = self.add_shared(scope);
        LOCAL.with(|x| x.borrow_mut().added.insert((self, scope), id));
        id
    }

    fn add_shared(self, scope: Scope) -> Self {
        let mut interner = INTERNER.lock().unwrap();
        if let Some(id) = interner.added.get(&(self, scope)) {
            return *id;
        }
        let mut scopes = interner.get(self).to_vec();
        let id = match scopes.binary_search(&scope) {
            Ok(_) => self,
            Err(at) => {
                scopes.insert(at, scope);
                interner.intern(scopes)
            }
        };
        interner.added.insert((self, scope), id);
        id
    }

    pub fn remove(self, scope: Scope) -> Self {
        let mut interner = INTERNER.lock().unwrap();
        if let Some(id) = interner.removed.get(&(self, scope)) {
            return *id;
        }
        let mut scopes = interner.get(self).to_vec();
        let id = match scopes.binary_search(&scope) {
            Ok(at) => {
                scopes.remove(at);
                interner.intern(scopes)
            }
            Err(_) => self,
        };
        interner.removed.insert((self, scope), id);
        id
    }

    // Adds the scope if it isn't there, removes it if it is
    pub fn flip(self, scope: Scope) -> Self {
        if self.contains(scope) {
            self.remove(scope)
        } else {
            self.add(scope)
        }
    }

    pub fn union(self, other: ScopeSetId) -> Self {
        if self == other || other.is_empty() {
            return self;
        }
        if self.is_empty() {
            return other;
        }
        let key = if self < other {
            (self, other)
        } else {
            (other, self)
        };
        if let Some(id) = LOCAL.with(|x| x.borrow().unions.get(&key).copied()) {
            return id;
        }
        let id = Self::union_shared(key);
        LOCAL.with(|x| x.borrow_mut().unions.insert(key, id));
        id
    }

    fn union_shared(key: (ScopeSetId, ScopeSetId)) -> Self {
        let (this, other) = key;
        let mut interner = INTERNER.lock().unwrap();
        if let Some(id) = interner.unions.get(&key) {
            return *id;
        }
        // Merged, as both are sorted
        let (a, b) = (interner.get(this), interner.get(other));
        let mut scopes = Vec::with_capacity(a.len() + b.len());
        let (mut i, mut j) = (0, 0);
        while i < a.len() && j < b.len() {
            match a[i].cmp(&b[j]) {
                core::cmp::Ordering::Less => {
                    scopes.push(a[i]);
                    i += 1;
                }
                core::cmp::Ordering::Greater => {
                    scopes.push(b[j]);
                    j += 1;
                }
                core::cmp::Ordering::Equal => {
                    scopes.push(a[i]);
                    i += 1;
                    j += 1;
                }
            }
        }
        scopes.extend_from_slice(&a[i..]);
        scopes.extend_from_slice(&b[j..]);
        let id = interner.intern(scopes);
        interner.unions.insert(key, id);
        id
    }

    // When an identifier with these scopes was made: the time of the expansion that wrote it,
    // or 0 for code as written. Introduction scopes sort after binding ones.
    pub fn birth(self) -> u32 {
        if self.is_empty() {
            return 0;
        }
        // Module scopes sort after the rest
        with_sets(self, self, |x, _| {
            x.iter()
                .rev()
                .find(|x| !x.is_module())
                .filter(|x| x.is_introduction())
                .map_or(0, |x| x.time())
        })
    }

    // The set without these scopes
    pub fn without(self, removed: &[Scope]) -> Self {
        if self.is_empty() || removed.is_empty() {
            return self;
        }
        let kept = with_sets(self, self, |x, _| {
            if x.iter().any(|x| removed.contains(x)) {
                Some(
                    x.iter()
                        .copied()
                        .filter(|x| !removed.contains(x))
                        .collect::<Vec<_>>(),
                )
            } else {
                None
            }
        });
        match kept {
            Some(kept) => INTERNER.lock().unwrap().intern(kept),
            None => self,
        }
    }

    // Just its introduction scopes, which tell apart what different expansions wrote
    pub fn introductions(self) -> Self {
        if self.is_empty() {
            return self;
        }
        let kept = with_sets(self, self, |x, _| {
            if x.iter().all(|x| x.is_introduction()) {
                None
            } else {
                Some(
                    x.iter()
                        .copied()
                        .filter(|x| x.is_introduction())
                        .collect::<Vec<_>>(),
                )
            }
        });
        match kept {
            Some(kept) => INTERNER.lock().unwrap().intern(kept),
            None => self,
        }
    }

    // The set without its use-site scopes
    pub fn without_use_sites(self) -> Self {
        if self.is_empty() {
            return self;
        }
        let kept = with_sets(self, self, |x, _| {
            if x.iter().any(|x| x.is_use_site()) {
                Some(
                    x.iter()
                        .copied()
                        .filter(|x| !x.is_use_site())
                        .collect::<Vec<_>>(),
                )
            } else {
                None
            }
        });
        match kept {
            Some(kept) => INTERNER.lock().unwrap().intern(kept),
            None => self,
        }
    }

    // Whether the last expansion it went through wrote it, rather than took it as input: its
    // newest expansion scope is an introduction scope, not a use-site one
    pub fn from_template(self) -> bool {
        !self.is_empty()
            && with_sets(self, self, |x, _| {
                x.iter()
                    .filter(|x| x.is_introduction() || x.is_use_site())
                    .max_by_key(|x| x.time())
                    .is_some_and(|x| x.is_introduction())
            })
    }

    // Whether a template wrote it, rather than the code as written
    pub fn introduced(self) -> bool {
        !self.is_empty() && with_sets(self, self, |x, _| x.iter().any(|x| x.is_introduction()))
    }

    // Whether another module's macro wrote it, than the one with this scope
    pub fn has_module_except(self, own: Scope) -> bool {
        !self.is_empty()
            && with_sets(self, self, |x, _| {
                x.iter()
                    .rev()
                    .take_while(|x| x.is_module())
                    .any(|x| *x != own)
            })
    }

    // The modules whose macros wrote it
    pub fn modules(self) -> Vec<Scope> {
        if self.is_empty() {
            return Vec::new();
        }
        with_sets(self, self, |x, _| {
            x.iter().copied().filter(|x| x.is_module()).collect()
        })
    }

    // Whether a module's macro wrote it
    pub fn has_module(self) -> bool {
        !self.is_empty() && with_sets(self, self, |x, _| x.last().is_some_and(|x| x.is_module()))
    }

    pub fn is_subset(self, other: ScopeSetId) -> bool {
        if self == other || self.is_empty() {
            return true;
        }
        with_sets(self, other, |small, large| {
            small.len() <= large.len() && small.iter().all(|x| large.binary_search(x).is_ok())
        })
    }
}

// TEMP: interner size
// Clears the memoised set operations once they're large; the sets stay, as syntax holds them
pub fn trim_caches() {
    const LIMIT: usize = 1 << 18;
    LOCAL.with(|x| {
        let mut local = x.borrow_mut();
        if local.unions.len() + local.added.len() > LIMIT {
            local.unions.clear();
            local.added.clear();
        }
    });
    let mut interner = INTERNER.lock().unwrap();
    if interner.added.len() + interner.removed.len() + interner.unions.len() > LIMIT {
        interner.added.clear();
        interner.removed.clear();
        interner.unions.clear();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn operations() {
        let (a, b, c) = (Scope::fresh(), Scope::fresh(), Scope::fresh());
        let ab = ScopeSetId::EMPTY.add(a).add(b);
        assert_eq!(ab, ScopeSetId::of(&[b, a]));
        assert_eq!(ab.add(a), ab);
        assert_eq!(ab.remove(b), ScopeSetId::of(&[a]));
        assert_eq!(ab.flip(c), ScopeSetId::of(&[a, b, c]));
        assert_eq!(ab.flip(b), ScopeSetId::of(&[a]));
        assert!(ScopeSetId::of(&[a]).is_subset(ab));
        assert!(!ScopeSetId::of(&[c]).is_subset(ab));
        assert_eq!(ab.union(ScopeSetId::of(&[c])), ScopeSetId::of(&[a, b, c]));
        assert_eq!(ab.len(), 2);
    }
}
