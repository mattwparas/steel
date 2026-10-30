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

// Maps module names to scopes and back
#[allow(clippy::type_complexity)]
static MODULES: once_cell::sync::Lazy<Mutex<(FxHashMap<String, Scope>, FxHashMap<Scope, String>)>> =
    once_cell::sync::Lazy::new(Default::default);

// The next scope number, which has to stay below the flag bits
fn next_scope() -> u32 {
    let n = SCOPE_COUNTER.fetch_add(1, Ordering::Relaxed);
    assert!(
        n < 1 << 30,
        "out of scopes: more than 2^30 made in this process"
    );
    n
}

impl Scope {
    // Marks a macro expansion's introduction scope, as opposed to a binding form's scope
    const INTRODUCTION: u32 = 1 << 31;
    // Marks a use-site scope. A macro use's input gets it, and a definition's name drops it
    const USE_SITE: u32 = 1 << 30;
    // Marks a module scope with both bits. Identifiers the module's macro templates write get it
    const MODULE: u32 = Self::INTRODUCTION | Self::USE_SITE;

    // A fresh scope for a binding form
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

    // A module's scope, looked up by name so it's the same each time
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

    // The name of the module this scope belongs to
    pub fn module_name(self) -> Option<String> {
        MODULES.lock().unwrap().1.get(&self).cloned()
    }

    // The prelude's scope. Prelude macro templates get it in addition to their module's
    pub fn prelude() -> Self {
        Self::module("#%prelude")
    }

    pub fn is_module(self) -> bool {
        self.0 & Self::MODULE == Self::MODULE
    }

    // Scopes made later have later times
    pub fn time(self) -> u32 {
        self.0 & !Self::MODULE
    }
}

// A set's sorted scopes and a unique serial number. Names made from the set use the serial,
// since another set can reuse its address once it's freed
struct SetData {
    serial: u64,
    scopes: Box<[Scope]>,
    introduced: bool,
}

// A set of scopes. Sets are hash-consed, so equal sets are the same set, and reference counted,
// so a set no syntax holds anymore is freed. The empty set is `None`.
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

// All sets, keyed by a hash of their scopes. The references are weak so they don't keep sets
// alive.
#[derive(Default)]
struct Interner {
    sets: FxHashMap<u64, Vec<Weak<SetData>>>,
    // Sets interned since dead entries were last pruned, and how many entries were left then
    since_prune: usize,
    after_prune: usize,
}

fn hash_scopes(scopes: &[Scope]) -> u64 {
    use core::hash::{Hash, Hasher};
    let mut hasher = rustc_hash::FxHasher::default();
    scopes.hash(&mut hasher);
    hasher.finish()
}

impl Interner {
    // Looks the set up before allocating anything
    fn intern(&mut self, scopes: &[Scope]) -> ScopeSetId {
        if scopes.is_empty() {
            return ScopeSetId::EMPTY;
        }
        let hash = hash_scopes(scopes);
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
            scopes: scopes.into(),
            introduced: scopes.iter().any(|x| x.is_introduction()),
        });
        bucket.push(Arc::downgrade(&set));

        // Prune dead entries once as many sets have been made as there were entries left after
        // the last prune, so the table stays within twice the live sets
        self.since_prune += 1;
        if self.since_prune > self.after_prune.max(1 << 12) {
            self.since_prune = 0;
            self.sets.retain(|_, bucket| {
                bucket.retain(|x| x.strong_count() > 0);
                !bucket.is_empty()
            });
            self.after_prune = self.sets.len();
            if self.sets.capacity() > 4 * self.sets.len().max(1 << 10) {
                self.sets.shrink_to_fit();
            }
        }

        ScopeSetId(Some(set))
    }
}

static INTERNER: Lazy<Mutex<Interner>> = Lazy::new(Default::default);

fn intern(scopes: &[Scope]) -> ScopeSetId {
    if scopes.is_empty() {
        return ScopeSetId::EMPTY;
    }
    INTERNER.lock().unwrap().intern(scopes)
}

// The result of a memoised operation, only valid while it's alive. Operands are keyed by their
// serials, which are never reused.
#[derive(Clone)]
struct Held(Option<Weak<SetData>>);

impl Held {
    fn of(set: &ScopeSetId) -> Self {
        Held(set.0.as_ref().map(Arc::downgrade))
    }

    fn get(&self) -> Option<ScopeSetId> {
        match &self.0 {
            None => Some(ScopeSetId::EMPTY),
            Some(held) => held.upgrade().map(|x| ScopeSetId(Some(x))),
        }
    }
}

thread_local! {
    // This thread's memoised set operations, keyed by the operands' serials
    static MEMO: core::cell::RefCell<Memo> = core::cell::RefCell::new(Memo::default());
}

#[derive(Default)]
struct Memo {
    added: FxHashMap<(u64, Scope), Held>,
    unions: FxHashMap<(u64, u64), Held>,
    // `introductions` and `without_use_sites`, keyed by which one it is
    filtered: FxHashMap<(u64, bool), Held>,
}

// Maximum number of memoised operations a thread keeps
const MEMO_LIMIT: usize = 1 << 13;

impl Memo {
    fn lookup<K: core::hash::Hash + Eq>(
        map: impl FnOnce(&Memo) -> &FxHashMap<K, Held>,
        key: &K,
    ) -> Option<ScopeSetId> {
        MEMO.with(|x| map(&x.borrow()).get(key)?.get())
    }

    fn insert<K: core::hash::Hash + Eq>(
        map: impl FnOnce(&mut Memo) -> &mut FxHashMap<K, Held>,
        key: K,
        set: &ScopeSetId,
    ) {
        MEMO.with(|x| {
            let mut memo = x.borrow_mut();
            if memo.added.len() + memo.unions.len() + memo.filtered.len() > MEMO_LIMIT {
                memo.added.clear();
                memo.unions.clear();
                memo.filtered.clear();
            }
            map(&mut memo).insert(key, Held::of(set));
        })
    }
}

impl ScopeSetId {
    pub const EMPTY: Self = ScopeSetId(None);

    pub fn of(scopes: &[Scope]) -> Self {
        let mut scopes = scopes.to_vec();
        scopes.sort_unstable();
        scopes.dedup();
        intern(&scopes)
    }

    // Like `of`, for scopes that are already sorted and distinct
    pub fn of_sorted(scopes: &[Scope]) -> Self {
        if !scopes.windows(2).all(|x| x[0] < x[1]) {
            return Self::of(scopes);
        }
        intern(scopes)
    }

    // A number unique to this set, never reused by another set. 0 for the empty set
    pub fn serial(&self) -> u64 {
        self.0.as_ref().map_or(0, |x| x.serial)
    }

    // The scopes, in sorted order
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
        if let Some(set) = Memo::lookup(|x| &x.added, &key) {
            return set;
        }
        let set = match self.scopes().binary_search(&scope) {
            Ok(_) => self.clone(),
            Err(at) => {
                let mut scopes = Vec::with_capacity(self.len() + 1);
                scopes.extend_from_slice(&self.scopes()[..at]);
                scopes.push(scope);
                scopes.extend_from_slice(&self.scopes()[at..]);
                intern(&scopes)
            }
        };
        Memo::insert(|x| &mut x.added, key, &set);
        set
    }

    pub fn remove(&self, scope: Scope) -> Self {
        match self.scopes().binary_search(&scope) {
            Ok(at) => {
                let mut scopes = self.scopes().to_vec();
                scopes.remove(at);
                intern(&scopes)
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
        if let Some(set) = Memo::lookup(|x| &x.unions, &key) {
            return set;
        }

        // Both are sorted, so merge them
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
        let set = intern(&scopes);
        Memo::insert(|x| &mut x.unions, key, &set);
        set
    }

    // When an identifier with these scopes was made: the time of the expansion that wrote it,
    // or 0 for code as written. Introduction scopes sort after binding scopes, and module
    // scopes sort last.
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
        let kept: Vec<Scope> = self.scopes().iter().copied().filter(keep).collect();
        intern(&kept)
    }

    fn filtered_memoised(&self, introductions: bool) -> Self {
        let key = (self.serial(), introductions);
        if let Some(set) = Memo::lookup(|x| &x.filtered, &key) {
            return set;
        }
        let set = if introductions {
            self.filtered(|x| x.is_introduction())
        } else {
            self.filtered(|x| !x.is_use_site())
        };
        Memo::insert(|x| &mut x.filtered, key, &set);
        set
    }

    pub fn without(&self, removed: &[Scope]) -> Self {
        if removed.is_empty() {
            return self.clone();
        }
        self.filtered(|x| !removed.contains(x))
    }

    // Only the introduction scopes, which tell apart what different expansions wrote
    pub fn introductions(&self) -> Self {
        self.filtered_memoised(true)
    }

    pub fn without_use_sites(&self) -> Self {
        self.filtered_memoised(false)
    }

    // Whether the most recent expansion wrote this identifier instead of taking it as input,
    // meaning its newest expansion scope is an introduction scope rather than a use-site one
    pub fn from_template(&self) -> bool {
        self.scopes()
            .iter()
            .filter(|x| x.is_introduction() || x.is_use_site())
            .max_by_key(|x| x.time())
            .is_some_and(|x| x.is_introduction())
    }

    // Whether a template wrote this identifier, rather than the original code
    pub fn introduced(&self) -> bool {
        self.0.as_ref().is_some_and(|x| x.introduced)
    }

    // Whether a macro from a module other than `own` wrote this identifier
    pub fn has_module_except(&self, own: Scope) -> bool {
        self.scopes()
            .iter()
            .rev()
            .take_while(|x| x.is_module())
            .any(|x| *x != own)
    }

    // Scopes of the modules whose macros wrote this identifier
    pub fn modules(&self) -> Vec<Scope> {
        self.scopes()
            .iter()
            .copied()
            .filter(|x| x.is_module())
            .collect()
    }

    // Whether a module's macro wrote this identifier
    pub fn has_module(&self) -> bool {
        self.scopes().last().is_some_and(|x| x.is_module())
    }

    pub fn is_subset(&self, other: &ScopeSetId) -> bool {
        if self == other || self.is_empty() {
            return true;
        }
        let (small, large) = (self.scopes(), other.scopes());
        if small.len() > large.len() {
            return false;
        }
        // Both are sorted
        let mut rest = large.iter();
        small.iter().all(|x| rest.any(|y| y == x))
    }
}

// Clears this thread's memoised operations once there are too many
pub fn trim_caches() {
    MEMO.with(|x| {
        let mut memo = x.borrow_mut();
        if memo.added.len() + memo.unions.len() + memo.filtered.len() > MEMO_LIMIT / 2 {
            memo.added.clear();
            memo.unions.clear();
            memo.filtered.clear();
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
        // Interning the same scopes again makes a new set
        assert_ne!(ScopeSetId::of(&[a, b]).serial(), serial);
    }
}
