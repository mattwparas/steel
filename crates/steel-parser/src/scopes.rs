// Sets of scopes, as Racket's expander has them. A scope is a fresh token; every identifier has
// a set of them. Sets are interned, so a syntax object holds one `ScopeSetId`, and the set
// operations are memoised.

use core::sync::atomic::{AtomicU32, Ordering};
use std::sync::{Arc, Mutex, Weak};

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

// A set's scopes, sorted, and a number of its own, which names made from it use, as its
// address can be another set's once it's freed
struct SetData {
    serial: u64,
    scopes: Box<[Scope]>,
}

// A set of scopes. Sets are hash-consed, so two of the same scopes are the same set, and
// reference counted, so a set no syntax holds any more is freed. The empty set is `None`.
#[derive(Clone, Default)]
pub struct ScopeSetId(Option<Arc<SetData>>);

impl PartialEq for ScopeSetId {
    fn eq(&self, other: &Self) -> bool {
        match (&self.0, &other.0) {
            (None, None) => true,
            (Some(a), Some(b)) => Arc::ptr_eq(a, b),
            _ => false,
        }
    }
}

impl Eq for ScopeSetId {}

impl core::hash::Hash for ScopeSetId {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        self.serial().hash(state)
    }
}

impl PartialOrd for ScopeSetId {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ScopeSetId {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        self.serial().cmp(&other.serial())
    }
}

impl core::fmt::Debug for ScopeSetId {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "ScopeSetId({}: {:?})", self.serial(), self.scopes())
    }
}

static SERIAL: core::sync::atomic::AtomicU64 = core::sync::atomic::AtomicU64::new(1);

// The sets there are, by a hash of their scopes. Weak, so they don't keep a set alive.
#[derive(Default)]
struct Interner {
    sets: FxHashMap<u64, Vec<Weak<SetData>>>,
    // Interned since the dead entries were last dropped
    since_prune: usize,
}

fn hash_scopes(scopes: &[Scope]) -> u64 {
    use core::hash::{Hash, Hasher};
    let mut hasher = rustc_hash::FxHasher::default();
    scopes.hash(&mut hasher);
    hasher.finish()
}

impl Interner {
    fn intern(&mut self, scopes: Vec<Scope>) -> ScopeSetId {
        if scopes.is_empty() {
            return ScopeSetId::EMPTY;
        }
        let hash = hash_scopes(&scopes);
        let bucket = self.sets.entry(hash).or_default();
        let mut found = None;
        bucket.retain(|x| match x.upgrade() {
            Some(set) => {
                if found.is_none() && *set.scopes == *scopes {
                    found = Some(set);
                }
                true
            }
            None => false,
        });
        if let Some(set) = found {
            return ScopeSetId(Some(set));
        }
        let set = Arc::new(SetData {
            serial: SERIAL.fetch_add(1, Ordering::Relaxed),
            scopes: scopes.into_boxed_slice(),
        });
        bucket.push(Arc::downgrade(&set));

        self.since_prune += 1;
        if self.since_prune > 1 << 12 && self.since_prune > self.sets.len() {
            self.since_prune = 0;
            self.sets.retain(|_, bucket| {
                bucket.retain(|x| x.strong_count() > 0);
                !bucket.is_empty()
            });
        }

        ScopeSetId(Some(set))
    }
}

static INTERNER: Lazy<Mutex<Interner>> = Lazy::new(Default::default);

fn intern(scopes: Vec<Scope>) -> ScopeSetId {
    if scopes.is_empty() {
        return ScopeSetId::EMPTY;
    }
    INTERNER.lock().unwrap().intern(scopes)
}

// A memoised operation's operand or result, which is only what it was while it's alive
#[derive(Clone)]
struct Held(Option<Weak<SetData>>);

impl Held {
    fn of(set: &ScopeSetId) -> Self {
        Held(set.0.as_ref().map(Arc::downgrade))
    }

    fn is(&self, set: &ScopeSetId) -> bool {
        match (&self.0, &set.0) {
            (None, None) => true,
            (Some(held), Some(set)) => {
                held.strong_count() > 0 && Weak::ptr_eq(held, &Arc::downgrade(set))
            }
            _ => false,
        }
    }

    fn get(&self) -> Option<ScopeSetId> {
        match &self.0 {
            None => Some(ScopeSetId::EMPTY),
            Some(held) => held.upgrade().map(|x| ScopeSetId(Some(x))),
        }
    }
}

thread_local! {
    // This thread's memoised operations, by their operands' serials
    static MEMO: core::cell::RefCell<Memo> = core::cell::RefCell::new(Memo::default());
}

#[derive(Default)]
struct Memo {
    added: FxHashMap<(u64, Scope), (Held, Held)>,
    unions: FxHashMap<(u64, u64), (Held, Held, Held)>,
}

impl ScopeSetId {
    pub const EMPTY: Self = ScopeSetId(None);

    pub fn of(scopes: &[Scope]) -> Self {
        let mut scopes = scopes.to_vec();
        scopes.sort_unstable();
        scopes.dedup();
        intern(scopes)
    }

    // The set of scopes that are already sorted and distinct
    pub fn of_sorted(scopes: &[Scope]) -> Self {
        debug_assert!(scopes.windows(2).all(|x| x[0] < x[1]));
        intern(scopes.to_vec())
    }

    // A number of the set's own, never another live or freed set's; 0 for the empty set
    pub fn serial(&self) -> u64 {
        self.0.as_ref().map_or(0, |x| x.serial)
    }

    // The set's scopes, sorted
    pub fn scopes(&self) -> &[Scope] {
        self.0.as_ref().map_or(&[], |x| &x.scopes)
    }

    pub fn len(&self) -> usize {
        self.scopes().len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_none()
    }

    pub fn contains(&self, scope: Scope) -> bool {
        self.scopes().binary_search(&scope).is_ok()
    }

    pub fn add(&self, scope: Scope) -> Self {
        let key = (self.serial(), scope);
        let memoised = MEMO.with(|x| {
            let memo = x.borrow();
            let (operand, result) = memo.added.get(&key)?;
            operand.is(self).then(|| result.get()).flatten()
        });
        if let Some(set) = memoised {
            return set;
        }
        let set = match self.scopes().binary_search(&scope) {
            Ok(_) => self.clone(),
            Err(at) => {
                let mut scopes = self.scopes().to_vec();
                scopes.insert(at, scope);
                intern(scopes)
            }
        };
        MEMO.with(|x| {
            x.borrow_mut()
                .added
                .insert(key, (Held::of(self), Held::of(&set)))
        });
        set
    }

    pub fn remove(&self, scope: Scope) -> Self {
        match self.scopes().binary_search(&scope) {
            Ok(at) => {
                let mut scopes = self.scopes().to_vec();
                scopes.remove(at);
                intern(scopes)
            }
            Err(_) => self.clone(),
        }
    }

    // Adds the scope if it isn't there, removes it if it is
    pub fn flip(&self, scope: Scope) -> Self {
        if self.contains(scope) {
            self.remove(scope)
        } else {
            self.add(scope)
        }
    }

    pub fn union(&self, other: &ScopeSetId) -> Self {
        if self == other || other.is_empty() {
            return self.clone();
        }
        if self.is_empty() {
            return other.clone();
        }
        let (a, b) = if self < other {
            (self, other)
        } else {
            (other, self)
        };
        let key = (a.serial(), b.serial());
        let memoised = MEMO.with(|x| {
            let memo = x.borrow();
            let (first, second, result) = memo.unions.get(&key)?;
            (first.is(a) && second.is(b))
                .then(|| result.get())
                .flatten()
        });
        if let Some(set) = memoised {
            return set;
        }

        // Merged, as both are sorted
        let (x, y) = (a.scopes(), b.scopes());
        let mut scopes = Vec::with_capacity(x.len() + y.len());
        let (mut i, mut j) = (0, 0);
        while i < x.len() && j < y.len() {
            match x[i].cmp(&y[j]) {
                core::cmp::Ordering::Less => {
                    scopes.push(x[i]);
                    i += 1;
                }
                core::cmp::Ordering::Greater => {
                    scopes.push(y[j]);
                    j += 1;
                }
                core::cmp::Ordering::Equal => {
                    scopes.push(x[i]);
                    i += 1;
                    j += 1;
                }
            }
        }
        scopes.extend_from_slice(&x[i..]);
        scopes.extend_from_slice(&y[j..]);
        let set = intern(scopes);
        MEMO.with(|m| {
            m.borrow_mut()
                .unions
                .insert(key, (Held::of(a), Held::of(b), Held::of(&set)))
        });
        set
    }

    // When an identifier with these scopes was made: the time of the expansion that wrote it,
    // or 0 for code as written. Introduction scopes sort after binding ones, module scopes
    // after the rest.
    pub fn birth(&self) -> u32 {
        self.scopes()
            .iter()
            .rev()
            .find(|x| !x.is_module())
            .filter(|x| x.is_introduction())
            .map_or(0, |x| x.time())
    }

    fn filtered(&self, keep: impl Fn(&Scope) -> bool) -> Self {
        if self.scopes().iter().all(&keep) {
            return self.clone();
        }
        intern(self.scopes().iter().copied().filter(keep).collect())
    }

    // The set without these scopes
    pub fn without(&self, removed: &[Scope]) -> Self {
        if removed.is_empty() {
            return self.clone();
        }
        self.filtered(|x| !removed.contains(x))
    }

    // Just its introduction scopes, which tell apart what different expansions wrote
    pub fn introductions(&self) -> Self {
        self.filtered(|x| x.is_introduction())
    }

    // The set without its use-site scopes
    pub fn without_use_sites(&self) -> Self {
        self.filtered(|x| !x.is_use_site())
    }

    // Whether the last expansion it went through wrote it, rather than took it as input: its
    // newest expansion scope is an introduction scope, not a use-site one
    pub fn from_template(&self) -> bool {
        self.scopes()
            .iter()
            .filter(|x| x.is_introduction() || x.is_use_site())
            .max_by_key(|x| x.time())
            .is_some_and(|x| x.is_introduction())
    }

    // Whether a template wrote it, rather than the code as written
    pub fn introduced(&self) -> bool {
        self.scopes().iter().any(|x| x.is_introduction())
    }

    // Whether another module's macro wrote it, than the one with this scope
    pub fn has_module_except(&self, own: Scope) -> bool {
        self.scopes()
            .iter()
            .rev()
            .take_while(|x| x.is_module())
            .any(|x| *x != own)
    }

    // The modules whose macros wrote it
    pub fn modules(&self) -> Vec<Scope> {
        self.scopes()
            .iter()
            .copied()
            .filter(|x| x.is_module())
            .collect()
    }

    // Whether a module's macro wrote it
    pub fn has_module(&self) -> bool {
        self.scopes().last().is_some_and(|x| x.is_module())
    }

    pub fn is_subset(&self, other: &ScopeSetId) -> bool {
        if self == other || self.is_empty() {
            return true;
        }
        let (small, large) = (self.scopes(), other.scopes());
        small.len() <= large.len() && small.iter().all(|x| large.binary_search(x).is_ok())
    }
}

// Drops this thread's memoised operations once there are many, and those of sets that are gone
pub fn trim_caches() {
    const LIMIT: usize = 1 << 13;
    MEMO.with(|x| {
        let mut memo = x.borrow_mut();
        if memo.unions.len() + memo.added.len() > LIMIT {
            memo.unions.clear();
            memo.added.clear();
        }
    });
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn operations() {
        let (a, b, c) = (Scope::fresh(), Scope::fresh(), Scope::fresh());
        let ab = ScopeSetId::EMPTY.add(a).add(b);
        let c_set = ScopeSetId::of(&[c]);
        assert_eq!(ab, ScopeSetId::of(&[b, a]));
        assert_eq!(ab.add(a), ab);
        assert_eq!(ab.remove(b), ScopeSetId::of(&[a]));
        assert_eq!(ab.flip(c), ScopeSetId::of(&[a, b, c]));
        assert_eq!(ab.flip(b), ScopeSetId::of(&[a]));
        assert!(ScopeSetId::of(&[a]).is_subset(&ab));
        assert!(!c_set.is_subset(&ab));
        assert_eq!(ab.union(&c_set), ScopeSetId::of(&[a, b, c]));
        assert_eq!(ab.len(), 2);
    }

    #[test]
    fn freed_sets() {
        let (a, b) = (Scope::fresh(), Scope::fresh());
        let ab = ScopeSetId::of(&[a, b]);
        let serial = ab.serial();
        assert_eq!(ScopeSetId::of(&[b, a]).serial(), serial);
        drop(ab);
        // Made again, it's another set
        assert_ne!(ScopeSetId::of(&[a, b]).serial(), serial);
    }
}
