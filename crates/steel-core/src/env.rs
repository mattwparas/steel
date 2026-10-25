use crate::gc::shared::ShareableMut;
use crate::rvals::{IntoSteelVal, Result, SteelVal};

// What a global's slot holds until its define runs, when a later global was defined first
#[derive(Clone)]
struct UndefinedGlobal;

impl crate::rvals::Custom for UndefinedGlobal {
    fn fmt(&self) -> Option<core::result::Result<String, core::fmt::Error>> {
        Some(Ok("#<undefined>".to_string()))
    }
}

#[cfg(feature = "sync")]
static UNDEFINED_GLOBAL: Lazy<SteelVal> = Lazy::new(|| UndefinedGlobal.into_steelval().unwrap());

// One value for every such slot, so compiled code can tell one by comparing with it
#[cfg(feature = "sync")]
pub(crate) fn undefined_global() -> SteelVal {
    UNDEFINED_GLOBAL.clone()
}

#[cfg(not(feature = "sync"))]
pub(crate) fn undefined_global() -> SteelVal {
    UndefinedGlobal.into_steelval().unwrap()
}

// The bits of that value as a slot holds it
#[cfg(feature = "sync")]
pub(crate) fn undefined_global_bits() -> u128 {
    unsafe { core::mem::transmute_copy::<SteelVal, u128>(&UNDEFINED_GLOBAL) }
}

// Read before its define ran
pub(crate) fn is_undefined_global(value: &SteelVal) -> bool {
    matches!(value, SteelVal::Custom(custom) if custom.read().as_any_ref().is::<UndefinedGlobal>())
}
use once_cell::sync::Lazy;
use shared_vector::AtomicSharedVector;

#[derive(Debug, Clone)]
#[repr(transparent)]
pub(crate) struct SharedVectorWrapper(pub AtomicSharedVector<SteelVal>);

impl SharedVectorWrapper {
    pub fn set_idx(&mut self, idx: usize, val: SteelVal) -> SteelVal {
        let guard = self.0.get_mut(idx).unwrap();
        let output = guard.clone();
        *guard = val;
        output
    }

    // None, and no write, when the define hasn't run yet
    pub fn set_defined_idx(&mut self, idx: usize, val: SteelVal) -> Option<SteelVal> {
        let guard = self.0.get_mut(idx).filter(|x| !is_undefined_global(x))?;
        Some(core::mem::replace(guard, val))
    }

    pub fn repl_define_idx(&mut self, idx: usize, val: SteelVal) {
        let guard = &mut self.0;
        if idx < guard.len() {
            guard[idx] = val.clone();
        } else {
            if idx > guard.len() {
                // if idx > self.thread_local_bindings.len() {
                // TODO: This seems suspect. Try to understand
                // what is happening here. This would be that values
                // are getting interned to be at a global offset in the
                // wrong order, which seems to be fine in general,
                // assuming that the values then get actually updated
                // to the correct values.
                for _ in 0..(idx - guard.len()) {
                    guard.push(undefined_global());
                }
            }

            guard.push(val.clone());
        }
    }
}

unsafe impl Sync for SharedVectorWrapper {}

#[allow(unused)]
#[derive(Debug)]
#[repr(C)]
pub struct Env {
    #[cfg(not(feature = "sync"))]
    pub(crate) bindings_vec: Vec<SteelVal>,
    #[cfg(feature = "sync")]
    pub(crate) bindings: SharedVectorWrapper,
}

#[cfg(feature = "sync")]
impl Clone for Env {
    fn clone(&self) -> Self {
        Self {
            bindings: self.bindings.clone(),
        }
    }
}

#[cfg(not(feature = "sync"))]
impl Clone for Env {
    fn clone(&self) -> Self {
        Self {
            bindings_vec: self.bindings_vec.clone(),
        }
    }
}

#[cfg(not(feature = "sync"))]
impl Env {
    pub fn extract(&self, idx: usize) -> Option<SteelVal> {
        self.bindings_vec
            .get(idx)
            .filter(|value| !is_undefined_global(value))
            .cloned()
    }

    pub fn len(&self) -> usize {
        self.bindings_vec.len()
    }

    /// top level global env has no parent
    pub fn root() -> Self {
        Env {
            bindings_vec: Vec::with_capacity(1024),
        }
    }

    #[cfg(feature = "dynamic")]
    pub(crate) fn _print_diagnostics(&self) {
        for (idx, value) in self.bindings_vec.iter().enumerate() {
            if let SteelVal::Closure(b) = value {
                let count = b.call_count();
                if count > 0 {
                    println!("Function: {} - Count: {}", idx, b.call_count());
                }
            }
        }
    }

    #[inline(always)]
    pub fn repl_lookup_idx(&self, idx: usize) -> SteelVal {
        self.bindings_vec[idx].clone()
    }

    #[inline(always)]
    pub(crate) fn repl_lookup_idx_ref(&self, idx: usize) -> &SteelVal {
        &self.bindings_vec[idx]
    }

    #[inline(always)]
    pub fn repl_maybe_lookup_idx(&self, idx: usize) -> Option<SteelVal> {
        // Look up the bindings using the local copy
        self.bindings_vec
            .get(idx)
            .filter(|value| !is_undefined_global(value))
            .cloned()
    }

    /// Get the value located at that index
    pub fn _repl_get_idx(&self, idx: usize) -> &SteelVal {
        &self.bindings_vec[idx]
    }

    #[inline]
    pub fn repl_define_idx(&mut self, idx: usize, val: SteelVal) {
        if idx < self.bindings_vec.len() {
            self.bindings_vec[idx] = val;
        } else {
            if idx > self.bindings_vec.len() {
                // TODO: This seems suspect. Try to understand
                // what is happening here. This would be that values
                // are getting interned to be at a global offset in the
                // wrong order, which seems to be fine in general,
                // assuming that the values then get actually updated
                // to the correct values.
                for _ in 0..(idx - self.bindings_vec.len()) {
                    self.bindings_vec.push(undefined_global());
                }
            }

            self.bindings_vec.push(val);
            assert_eq!(self.bindings_vec.len() - 1, idx);
        }
    }

    pub fn repl_set_idx(&mut self, idx: usize, val: SteelVal) -> Result<SteelVal> {
        let output = self.bindings_vec[idx].clone();
        self.bindings_vec[idx] = val;
        Ok(output)
    }

    // None, and no write, when the define hasn't run yet
    pub(crate) fn repl_set_defined_idx(&mut self, idx: usize, val: SteelVal) -> Option<SteelVal> {
        let guard = self
            .bindings_vec
            .get_mut(idx)
            .filter(|x| !is_undefined_global(x))?;
        Some(core::mem::replace(guard, val))
    }

    #[inline]
    pub fn add_root_value(&mut self, idx: usize, val: SteelVal) {
        // self.bindings_map.insert(idx, val);
        self.repl_define_idx(idx, val);
    }

    pub fn roots(&self) -> &Vec<SteelVal> {
        &self.bindings_vec
    }
}

#[cfg(feature = "sync")]
impl Env {
    pub fn len(&self) -> usize {
        self.bindings.0.len()
    }

    /// top level global env has no parent
    pub fn root() -> Self {
        Env {
            bindings: SharedVectorWrapper(AtomicSharedVector::with_capacity(1024)),
        }
    }

    pub fn deep_clone(&self) -> Self {
        Self {
            bindings: SharedVectorWrapper(
                self.bindings.clone().0.into_unique().into_shared_atomic(),
            ),
        }
    }

    #[cfg(feature = "dynamic")]
    pub(crate) fn _print_diagnostics(&self) {
        for (idx, value) in self.bindings_vec.iter().enumerate() {
            if let SteelVal::Closure(b) = value {
                let count = b.call_count();
                if count > 0 {
                    println!("Function: {} - Count: {}", idx, b.call_count());
                }
            }
        }
    }

    #[inline(always)]
    pub fn repl_lookup_idx(&self, idx: usize) -> SteelVal {
        // Look up the bindings using the local copy
        self.bindings.0[idx].clone()
    }

    #[inline(always)]
    pub(crate) fn repl_lookup_idx_ref(&self, idx: usize) -> &SteelVal {
        &self.bindings.0[idx]
    }

    #[inline(always)]
    pub fn repl_maybe_lookup_idx(&self, idx: usize) -> Option<SteelVal> {
        // Look up the bindings using the local copy
        self.bindings
            .0
            .get(idx)
            .filter(|value| !is_undefined_global(value))
            .cloned()
    }

    #[inline]
    pub fn update_env(&mut self, vec: SharedVectorWrapper) {
        self.bindings = vec;
    }

    #[inline]
    pub(crate) fn default_env(&mut self) {
        static DEFAULT_ENV: Lazy<SharedVectorWrapper> =
            Lazy::new(|| SharedVectorWrapper(shared_vector::arc_vector!()));

        self.bindings = DEFAULT_ENV.clone();
    }

    pub(crate) fn drain_env(&mut self) -> SharedVectorWrapper {
        let output = self.bindings.clone();
        self.default_env();
        output
    }

    #[inline(always)]
    pub fn repl_set_idx(&mut self, idx: usize, val: SteelVal) -> Result<SteelVal> {
        let guard = self.bindings.0.get_mut(idx).unwrap();
        let output = guard.clone();
        *guard = val;
        Ok(output)
    }

    pub(crate) fn repl_set_defined_idx(&mut self, idx: usize, val: SteelVal) -> Option<SteelVal> {
        self.bindings.set_defined_idx(idx, val)
    }

    #[cfg(feature = "sync")]
    pub fn roots(&self) -> &[SteelVal] {
        self.bindings.0.as_slice()
    }

    #[cfg(not(feature = "sync"))]
    pub fn roots(&self) -> &[SteelVal] {
        self.bindings_vec.as_slice()
    }
}
