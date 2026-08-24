#[allow(dead_code)]
pub(crate) mod closed;
pub(crate) mod contracts;
pub(crate) mod functions;
pub(crate) mod json_vals;
pub(crate) mod lazy_stream;
pub(crate) mod lists;
pub(crate) mod port;
pub(crate) mod recycler;
pub mod serde;
pub(crate) mod structs;
pub(crate) mod transducers;

pub use functions::LambdaMetadataTable;

pub use closed::RootToken;
pub use closed::RootedSteelVal;
pub use port::SteelPortRepr;

/// A `BuildHasher` with a fixed seed, so that hash iteration order is the same
/// from one run of the compiler to the next.
#[derive(Debug, Clone, Copy, Default)]
pub struct DeterministicHasher;

impl core::hash::BuildHasher for DeterministicHasher {
    type Hasher = ahash::AHasher;

    fn build_hasher(&self) -> Self::Hasher {
        const SEEDED: ahash::RandomState = ahash::RandomState::with_seeds(
            0x243f_6a88_85a3_08d3,
            0x1319_8a2e_0370_7344,
            0xa409_3822_299f_31d0,
            0x082e_fa98_ec4e_6c89,
        );

        SEEDED.build_hasher()
    }
}

pub use im_shims::{
    HashMap, HashMapConsumingIter, HashSet, HashSetConsumingIter, Vector, VectorConsumingIter,
};

#[cfg(not(feature = "sync"))]
mod im_shims {
    use super::DeterministicHasher;

    pub type Vector<T> = im_rc::Vector<T>;
    pub type HashMap<K, V, S = DeterministicHasher> = im_rc::HashMap<K, V, S>;
    pub type HashSet<K, S = DeterministicHasher> = im_rc::HashSet<K, S>;

    pub type VectorConsumingIter<T> = im_rc::vector::ConsumingIter<T>;
    pub type HashSetConsumingIter<T> = im_rc::hashset::ConsumingIter<T>;
    pub type HashMapConsumingIter<K, V> = im_rc::hashmap::ConsumingIter<(K, V)>;
}

#[cfg(all(feature = "sync", not(feature = "imbl")))]
mod im_shims {
    use super::DeterministicHasher;

    pub type Vector<T> = im::Vector<T>;
    pub type HashMap<K, V, S = DeterministicHasher> = im::HashMap<K, V, S>;
    pub type HashSet<K, S = DeterministicHasher> = im::HashSet<K, S>;

    pub type VectorConsumingIter<T> = im::vector::ConsumingIter<T>;
    pub type HashSetConsumingIter<T> = im::hashset::ConsumingIter<T>;
    pub type HashMapConsumingIter<K, V> = im::hashmap::ConsumingIter<(K, V)>;
}

#[cfg(all(feature = "sync", feature = "imbl"))]
mod im_shims {

    use super::DeterministicHasher;

    use crate::gc::Gc;
    use steel_imbl::shared_ptr::PointerFamily;

    const CHUNK_SIZE: usize = 64;

    pub struct GcPointerType;

    impl PointerFamily for GcPointerType {
        type Pointer<T: 'static> = Gc<T>;

        fn new<T: 'static>(value: T) -> Self::Pointer<T> {
            Gc::new(value)
        }

        fn strong_count<T: 'static>(this: &Self::Pointer<T>) -> usize {
            Gc::strong_count(this)
        }

        fn try_unwrap<T: 'static>(this: Self::Pointer<T>) -> Result<T, Self::Pointer<T>> {
            Gc::try_unwrap(this)
        }

        fn get_mut<T: 'static>(this: &mut Self::Pointer<T>) -> Option<&mut T> {
            Gc::get_mut(this)
        }

        fn ptr_eq<T: 'static>(this: &Self::Pointer<T>, other: &Self::Pointer<T>) -> bool {
            Gc::ptr_eq(this, other)
        }

        fn make_mut<T: Clone + 'static>(ptr: &mut Self::Pointer<T>) -> &mut T {
            Gc::make_mut(ptr)
        }

        fn clone<T: 'static>(ptr: &Self::Pointer<T>) -> Self::Pointer<T> {
            Gc::clone(ptr)
        }

        fn as_ptr<T: 'static>(this: &Self::Pointer<T>) -> *const T {
            Gc::as_ptr(this)
        }

        fn into_raw<T: 'static>(this: Self::Pointer<T>) -> *const T {
            Gc::into_raw(this)
        }

        unsafe fn from_raw<T: 'static>(this: *const T) -> Self::Pointer<T> {
            Gc::from_raw(this)
        }
    }

    pub type Vector<T> = steel_imbl::GenericVector<T, GcPointerType, CHUNK_SIZE>;
    pub type HashMap<K, V, S = DeterministicHasher> =
        steel_imbl::GenericHashMap<K, V, S, GcPointerType>;
    pub type HashSet<K, S = DeterministicHasher> = steel_imbl::GenericHashSet<K, S, GcPointerType>;

    pub type VectorConsumingIter<T> =
        steel_imbl::vector::ConsumingIter<T, GcPointerType, CHUNK_SIZE>;
    pub type HashSetConsumingIter<T> = steel_imbl::hashset::ConsumingIter<T, GcPointerType>;
    pub type HashMapConsumingIter<K, V> = steel_imbl::hashmap::ConsumingIter<(K, V), GcPointerType>;
}
