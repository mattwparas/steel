use std::alloc::Layout;
use std::any::Any;
use std::borrow::Borrow;
use std::convert::Infallible;
use std::marker::PhantomData;
use std::mem::{ManuallyDrop, MaybeUninit};
use std::ops::Deref;
use std::pin::Pin;
use std::ptr::{drop_in_place, NonNull};
use std::sync::atomic::AtomicU32;
use std::sync::LazyLock;
use std::thread::JoinHandle;
use std::{cell::Cell, sync::atomic::Ordering};

use core::convert::TryInto;
use core::num::{NonZeroU32, NonZeroUsize};

use std::{alloc, cmp, fmt, iter, mem, ptr};

use std::hash::{Hash, Hasher};

pub mod weak;

/// Width of the owner id in the biased half-word. BRC gives it 18 bits (PACT'18,
/// Fig. 5); one of those is handed to the allocation's owner as a spare flag,
/// leaving 17 - still far more concurrently live threads than is plausible,
/// since ids are recycled.
const TID_BITS: u32 = 18;
const MAX_TID: u32 = (1 << TID_BITS) - 1;

/// Handed out when no id is available. Never equal to anything, so an object
/// allocated by such a thread is simply never biased - it takes the atomic path.
const SENTINEL: u32 = MAX_TID;
const SENTINEL_ID: NonZeroU32 = NonZeroU32::new(SENTINEL).unwrap();

static NEXT_TID: AtomicU32 = AtomicU32::new(1);
static FREE_TIDS: std::sync::Mutex<Vec<u32>> = std::sync::Mutex::new(Vec::new());

thread_local! {
    /// No destructor, so this stays readable for the whole life of the thread,
    /// including while other thread locals are being torn down.
    static MY_TID: Cell<u32> = const { Cell::new(0) };

    /// Separate, and only for its `Drop`: hands the id back when the thread exits.
    static TID_RELEASER: TidReleaser = const { TidReleaser };
}

struct TidReleaser;

impl Drop for TidReleaser {
    fn drop(&mut self) {
        let id = MY_TID.with(|tid| tid.replace(0));
        if id != 0 && id != SENTINEL {
            if let Ok(mut free) = FREE_TIDS.lock() {
                free.push(id);
            }
        }
    }
}

/// Ids are recycled on thread exit, so the space bounds *concurrently live*
/// threads rather than total thread creations - which is what keeps 18 bits
/// workable for a long running process.
fn acquire_tid() -> u32 {
    if let Ok(mut free) = FREE_TIDS.lock() {
        if let Some(id) = free.pop() {
            return id;
        }
    }

    let mut current = NEXT_TID.load(Ordering::Relaxed);
    loop {
        if current >= SENTINEL {
            return SENTINEL;
        }
        match NEXT_TID.compare_exchange_weak(
            current,
            current + 1,
            Ordering::Relaxed,
            Ordering::Relaxed,
        ) {
            Ok(_) => return current,
            Err(actual) => current = actual,
        }
    }
}

/// A unique identifier for a running thread.
///
/// Uniqueness is guaranteed between running threads. The ids of dead threads
/// are reused, which is what keeps them small enough to pack into the biased
/// half-word; biased RC only needs "exactly one live thread believes it owns
/// this object", and a recycled id preserves that.
#[derive(Debug, Clone, Copy, Hash, Eq)]
#[repr(transparent)]
pub struct ThreadId(pub(crate) NonZeroU32);

impl ThreadId {
    #[inline(always)]
    pub(crate) const fn new(value: NonZeroU32) -> Self {
        Self(value)
    }

    /// The raw id, as generated code reads it out of the biased half-word.
    #[inline(always)]
    pub fn raw(&self) -> u32 {
        self.0.get()
    }

    /// Gets the id for the thread that invokes it.
    #[inline]
    pub fn current_thread() -> Self {
        let raw = MY_TID.with(|tid| {
            let existing = tid.get();
            if existing != 0 {
                return existing;
            }

            let id = acquire_tid();
            tid.set(id);
            // Registers the destructor that hands `id` back. During this
            // thread's own teardown there is nothing left to register.
            let _ = TID_RELEASER.try_with(|_| ());
            id
        });
        Self::new(NonZeroU32::new(raw).unwrap_or(SENTINEL_ID))
    }
}

impl PartialEq for ThreadId {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        match (self.0.get(), other.0.get()) {
            (SENTINEL, _) | (_, SENTINEL) => false,
            (a, b) => a == b,
        }
    }
}

// Okay, now that this appears to be working, we
// need to shrink this down as much as possible.
/// The owner id occupies the low bits and its count the high ones. That order
/// matters: adjusting the count is `word +/- COUNTER_ONE`, so a carry or borrow
/// leaves the word entirely rather than running into the owner id. With the
/// fields the other way round a stray decrement silently reassigns ownership.
const TID_MASK: u32 = (1 << TID_BITS) - 1;

const COUNTER_SHIFT: u32 = TID_BITS;
const COUNTER_ONE: u32 = 1 << COUNTER_SHIFT;
const BIASED_COUNTER_BITS: u32 = 32 - COUNTER_SHIFT;
const BIASED_COUNTER_MASK: u32 = (1 << BIASED_COUNTER_BITS) - 1;

/// The most references the owning thread can hold to one object. BRC calls 14
/// bits "more than enough for RC" - many Java programs need only 7.
pub const MAX_BIASED_COUNT: u32 = BIASED_COUNTER_MASK;

/// Where generated code finds the pieces of the biased half-word. It lives at
/// offset 0 of the `RcWord`, which is itself at offset 0 of the allocation.
/// Shift that moves the count down to the bottom of the word.
pub const fn biased_counter_shift() -> u32 {
    COUNTER_SHIFT
}

/// Mask selecting the owner id.
pub const fn biased_tid_mask() -> u32 {
    TID_MASK
}

/// What to add to the word to move the count by one.
pub const fn biased_counter_one() -> u32 {
    COUNTER_ONE
}

/// Largest count the field holds; past this the caller must spill to `shared`.
pub const fn biased_counter_max() -> u32 {
    BIASED_COUNTER_MASK
}

/// Offset of the biased half-word within the `RcWord`, which itself sits at the
/// start of the allocation.
pub const fn biased_offset() -> usize {
    core::mem::offset_of!(RcWord, biased)
}

/// Offset of the shared half-word, the counterpart to `biased_offset`.
///
/// Generated code reads this to decide whether anyone *other than the owner
/// thread* holds a reference: every non-owner acquire lands here, so a zero
/// word means every live reference belongs to the owner. A caller that holds
/// one is therefore the owner, and nothing else can be touching the payload.
pub const fn shared_offset() -> usize {
    core::mem::offset_of!(RcWord, shared)
}

/// BRC splits the word in two (PACT'18, Fig. 5): a biased half-word owned
/// outright by one thread, and a shared half-word every other thread updates
/// atomically. Packing the owner id in alongside its count is what keeps the
/// whole thing to 8 bytes.
///
/// Steel keeps weak references in a separate type, so the paper's 16 reserved
/// bits are spare here and `shared` keeps its full 30 bit counter rather than
/// the paper's 14.
#[repr(C)]
#[derive(Debug)]
pub struct RcWord {
    /// Owner id in the low `TID_BITS`, the owner's count above it. Only the
    /// owner thread touches it, hence a plain `Cell`.
    biased: Cell<u32>,
    shared: SharedPacked,
}

#[inline(always)]
const fn pack_biased(tid: u32, counter: u32) -> u32 {
    (counter << COUNTER_SHIFT) | tid
}

#[derive(Debug)]
#[repr(transparent)]
pub struct SharedPacked(AtomicU32);

impl SharedPacked {
    #[inline]
    pub fn load(&self, order: Ordering) -> Packed {
        Packed(self.0.load(order))
    }

    #[inline]
    pub fn compare_exchange(
        &self,
        current: Packed,
        new: Packed,
        success: Ordering,
        failure: Ordering,
    ) -> Result<u32, u32> {
        self.0.compare_exchange(current.0, new.0, success, failure)
    }
}

pub const FLAG_MERGED: u32 = 1 << 31;
pub const FLAG_QUEUED: u32 = 1 << 30;

const VALUE_BITS: u32 = 30;
const VALUE_MASK: u32 = (1 << VALUE_BITS) - 1;
const VALUE_SIGN_BIT: u32 = 1 << (VALUE_BITS - 1);

/// The shared half-word counts in a signed 30-bit field, so there is a ceiling
/// on how many references the non-owning threads can hold between them.
///
/// Three things can happen at that ceiling, and only one of them is safe.
/// Wrapping frees an object that is still referenced. Panicking unwinds
/// arbitrary code with the count already wrong, and an overflow is not
/// something the program can catch and repair. Saturating leaks the object -
/// it can never reach zero again - which costs memory and nothing else, so
/// that is what the counter does.
pub const MAX_SHARED_COUNT: i32 = (1 << (VALUE_BITS - 1)) - 1;
pub const MIN_SHARED_COUNT: i32 = -(1 << (VALUE_BITS - 1));

/// Room to move by one in either direction. One unsigned compare rather than
/// two signed ones, because this sits inside the shared counter's CAS loop and
/// `browse` runs through it millions of times.
#[inline(always)]
const fn shared_count_has_headroom(value: i32) -> bool {
    (value.wrapping_sub(MIN_SHARED_COUNT + 1) as u32) < ((MAX_SHARED_COUNT - MIN_SHARED_COUNT - 1) as u32)
}

#[derive(Copy, Clone, PartialEq, PartialOrd, Debug)]
pub struct Packed(u32);

impl Packed {
    pub fn set_queued(&mut self, queued: bool) {
        let mask = FLAG_QUEUED;
        if queued {
            self.0 |= mask;
        } else {
            self.0 &= !mask
        }
    }

    pub fn set_merged(&mut self, merged: bool) {
        let mask = FLAG_MERGED;
        if merged {
            self.0 |= mask
        } else {
            self.0 &= !mask
        }
    }

    pub fn get_merged(&self) -> bool {
        self.is_merged()
    }

    pub fn get_queued(&self) -> bool {
        self.is_queued()
    }

    pub fn is_merged(&self) -> bool {
        self.0 & FLAG_MERGED != 0
    }

    pub fn is_queued(&self) -> bool {
        self.0 & FLAG_QUEUED != 0
    }

    // pub fn value(&self) -> i32 {
    //     (self.0 & VALUE_MASK) as _
    // }

    pub fn get_counter(&self) -> i32 {
        self.value()
    }

    // fn set_value(&mut self, value: i32) {
    //     assert!(value >= -(1 << 29) && value < (1 << 29));
    //     let v = (value as u32) & VALUE_MASK;
    //     // self.0 = (self.0 & !VALUE_MASK) | v;

    //     self.0 = v;
    // }

    fn value(&self) -> i32 {
        let raw = self.0 & VALUE_MASK;

        if raw & VALUE_SIGN_BIT != 0 {
            // Sign-extend from bit 29
            (raw | !VALUE_MASK) as i32
        } else {
            raw as i32
        }
    }

    pub fn set_counter(&mut self, value: i32) {
        self.set_value(value);
    }

    fn set_value(&mut self, value: i32) {
        // Clamped, not asserted: see `MAX_SHARED_COUNT`. The counter operations
        // below already refuse to move a saturated count, so reaching the clamp
        // means something handed us an out-of-range value directly.
        self.store_value(value.clamp(MIN_SHARED_COUNT, MAX_SHARED_COUNT));
    }

    /// Stores a value already known to be in range. The mask keeps a wrong one
    /// inside the field rather than smearing it over the flags.
    #[inline(always)]
    fn store_value(&mut self, value: i32) {
        self.0 = (self.0 & !VALUE_MASK) | ((value as u32) & VALUE_MASK);
    }

    /// Adds one unless the count has saturated, in which case it stays put and
    /// the object leaks.
    #[inline(always)]
    fn saturating_inc(&mut self) {
        let v = self.value();
        if shared_count_has_headroom(v) {
            self.store_value(v + 1);
        }
    }

    /// Removes one unless the count has saturated. A saturated count must never
    /// come back down: it no longer reflects the real number of references, so
    /// letting it fall to zero would free a live object.
    #[inline(always)]
    fn saturating_dec(&mut self) {
        let v = self.value();
        if shared_count_has_headroom(v) {
            self.store_value(v - 1);
        }
    }

    fn update_counter(&mut self, f: impl FnOnce(i32) -> i32) {
        self.set_value((f)(self.value()))
    }

    fn new_with(value: i32, merged: bool, queued: bool) -> Self {
        // Ensure value fits in signed 30 bits
        assert!(value >= -(1 << 29) && value < (1 << 29));

        let mut bits = (value as u32) & VALUE_MASK;

        if merged {
            bits |= FLAG_MERGED;
        }
        if queued {
            bits |= FLAG_QUEUED;
        }

        Self(bits)
    }

    pub fn new() -> Self {
        Self::new_with(0, false, false)
    }
}

#[test]
fn packed_vs_unpacked() {
    let mut packed = Packed::new();

    packed.set_merged(true);

    assert_eq!(packed.get_merged(), true);
    assert_eq!(packed.get_queued(), false);
    assert_eq!(packed.get_counter(), 0);

    packed.set_merged(false);
    assert_eq!(packed.get_merged(), false);
    assert_eq!(packed.get_queued(), false);
    assert_eq!(packed.get_counter(), 0);

    packed.set_value(100);
    assert_eq!(packed.get_merged(), false);
    assert_eq!(packed.get_queued(), false);
    assert_eq!(packed.get_counter(), 100);

    packed.set_value(-1);
    assert_eq!(packed.get_merged(), false);
    assert_eq!(packed.get_queued(), false);
    assert_eq!(packed.get_counter(), -1);

    packed.set_value(-100);
    packed.set_queued(true);
    assert_eq!(packed.get_merged(), false);
    assert_eq!(packed.get_queued(), true);
    assert_eq!(packed.get_counter(), -100);
}

impl SharedPacked {
    pub fn set_flag_queued(&self, queued: bool) {
        let mask = FLAG_QUEUED;
        if queued {
            self.0.fetch_or(mask, Ordering::Relaxed);
        } else {
            self.0.fetch_and(!mask, Ordering::Relaxed);
        }
    }

    pub fn set_flag_merged(&self, merged: bool) {
        let mask = FLAG_MERGED;
        if merged {
            self.0.fetch_or(mask, Ordering::Relaxed);
        } else {
            self.0.fetch_and(!mask, Ordering::Relaxed);
        }
    }

    fn new() -> Self {
        SharedPacked(AtomicU32::new(Packed::new_with(0, false, false).0))
    }

    pub fn is_merged(&self) -> bool {
        self.0.fetch_and(FLAG_MERGED, Ordering::Relaxed) != 0
    }

    pub fn is_queued(&self) -> bool {
        self.0.fetch_and(FLAG_QUEUED, Ordering::Relaxed) != 0
    }

    pub fn value(&self) -> u32 {
        self.0.fetch_and(VALUE_MASK, Ordering::Relaxed)
    }
}

#[repr(C)]
pub struct RcBox<T: ?Sized> {
    pub rcword: RcWord,
    data: T,
}

impl RcWord {
    pub fn new() -> Self {
        let id = ThreadId::current_thread();

        // debug_assert!(QueueHandle::is_thread_registered(id));

        Self {
            biased: Cell::new(pack_biased(id.raw(), 1)),
            shared: SharedPacked::new(),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum DecrementAction {
    DoNothing,
    Queue,
    Deallocate,
}


// The biased reference counting protocol lives on the word itself, so any
// allocation that begins with one - RcBox, or a packed header - shares a
// single implementation rather than keeping its own copy in step.
impl RcWord {
    /// The thread local portion of the count. A cheap, non destructive hint -
    /// unlike `has_unique_ref`, which consumes the shared count when it wins.
    pub fn biased_count(&self) -> u32 {
        self.biased.get() >> COUNTER_SHIFT
    }

    /// The thread this object is biased to, if any.
    #[inline(always)]
    pub fn owner(&self) -> Option<ThreadId> {
        match self.biased.get() & TID_MASK {
            0 => None,
            tid => Some(ThreadId::new(unsafe { NonZeroU32::new_unchecked(tid) })),
        }
    }

    /// Unbias the object, leaving the count alone. BRC's `biased.tid := 0`.
    #[inline(always)]
    pub fn clear_owner(&self) {
        self.biased.set(self.biased.get() & !TID_MASK);
    }

    /// Masks the incoming value: the count shares a word with the owner id now,
    /// so a wrapping decrement must stay inside its own field rather than
    /// smearing ones across the id.
    #[inline(always)]
    fn set_biased_count(&self, counter: u32) {
        self.biased
            .set((self.biased.get() & TID_MASK) | ((counter & BIASED_COUNTER_MASK) << COUNTER_SHIFT));
    }

    pub fn fast_increment(&self) {
        let counter = self.biased_count();

        if counter == MAX_BIASED_COUNT {
            // The biased field is full. BRC's invariant is that biased + shared
            // is the true count, so carrying on in the shared half-word is
            // correct - just slower for this object. The paper calls 14 bits
            // "more than enough"; `browse` disagrees, and a panic is not an
            // acceptable answer to a program holding many references.
            self.slow_increment();
            return;
        }
        self.set_biased_count(counter + 1);
    }

    pub fn slow_increment(&self) {
        // loop {
        //     // TODO: Do some reading on the memory implications here
        //     // Do we have to read the whole thing together?
        //     let old = self.shared.load(Ordering::Relaxed);
        //     let mut new = old;
        //     new.update_counter(|x| x + 1);

        //     if self
        //         .rcword
        //         .shared
        //         .compare_exchange(old, new, Ordering::AcqRel, Ordering::Relaxed)
        //         .is_ok()
        //     {
        //         break;
        //     }
        // }

        // TODO: Use fetch update instead!
        // self.rcword
        //     .shared
        //     .0
        //     .fetch_update(Ordering::AcqRel, Ordering::Relaxed, |old| {
        //         let mut value = Packed(old);
        //         value.update_counter(|x| x + 1);
        //         Some(value.0)
        //     });

        let mut old = self.shared.load(Ordering::Relaxed);

        loop {
            // TODO: Do some reading on the memory implications here
            // Do we have to read the whole thing together?
            // let old = self.shared.load(Ordering::Relaxed);
            let mut new = old;
            new.saturating_inc();

            match self
                .shared
                .compare_exchange(old, new, Ordering::AcqRel, Ordering::Relaxed)
            {
                Ok(_) => break,
                Err(e) => {
                    old = Packed(e);
                }
            }
        }
    }

    pub fn increment(&self) {
        // let owner_tid = self.thread_id.load(Ordering::Relaxed);
        let owner_tid = self.owner();
        let my_tid = ThreadId::current_thread();

        if owner_tid == Some(my_tid) {
            self.fast_increment();
        } else {
            self.slow_increment();
        }
    }

    pub fn fast_decrement_drop_impl(&self) -> DecrementAction {
        let mut new;

        let mut old = self.shared.load(Ordering::Relaxed);

        loop {
            new = old;
            new.set_merged(true);
            match self
                .shared
                .compare_exchange(old, new, Ordering::AcqRel, Ordering::Relaxed)
            {
                Ok(_) => break,
                Err(e) => {
                    old = Packed(e);
                }
            }
        }

        std::sync::atomic::fence(Ordering::Acquire);

        if new.get_counter() == 0 {
            DecrementAction::Deallocate
        } else {
            self.clear_owner();
            DecrementAction::DoNothing
        }
    }

    pub fn fast_decrement(&self) -> DecrementAction {
        self.set_biased_count(self.biased_count() - 1);
        if self.biased_count() > 0 {
            return DecrementAction::DoNothing;
        }

        self.fast_decrement_drop_impl()
    }

    pub fn slow_decrement(&self) -> DecrementAction {
        // let mut old;
        // let mut new;
        // loop {
        //     old = self.shared.load(Ordering::Relaxed);
        //     new = old;

        //     new.update_counter(|x| x - 1);

        //     if new.get_counter() < 0 {
        //         new.set_queued(true);
        //     }

        //     if self
        //         .rcword
        //         .shared
        //         .compare_exchange(old, new, Ordering::AcqRel, Ordering::Relaxed)
        //         .is_ok()
        //     {
        //         break;
        //     }
        // }

        let mut old = self.shared.load(Ordering::Relaxed);
        let mut new;
        loop {
            new = old;
            new.saturating_dec();

            if new.get_counter() < 0 {
                new.set_queued(true);
            }

            match self
                .shared
                .compare_exchange(old, new, Ordering::AcqRel, Ordering::Relaxed)
            {
                Ok(_) => break,
                Err(e) => {
                    old = Packed(e);
                }
            }
        }

        std::sync::atomic::fence(Ordering::Acquire);

        if old.get_queued() != new.get_queued() {
            DecrementAction::Queue
        } else if new.get_merged() && new.get_counter() == 0 {
            DecrementAction::Deallocate
        } else {
            DecrementAction::DoNothing
        }
    }

    pub fn decrement(&self) -> DecrementAction {
        // let owner_tid = self.thread_id.load(Ordering::Relaxed);
        let owner_tid = self.owner();
        let my_tid = ThreadId::current_thread();

        if owner_tid == Some(my_tid) {
            self.fast_decrement()
        } else {
            self.slow_decrement()
        }
    }
}

impl RcWord {
    /// Destructive: when this wins it consumes the shared count, so the
    /// caller now holds the only reference.
    pub fn has_unique_ref(&self) -> bool {
        let owner = self.owner();
        match owner {
            None => {
                let meta = self;
                let mut new;
                let mut old;

                // loop {
                old = meta.shared.load(Ordering::Relaxed);
                new = old;

                old.set_counter(1);
                new.set_counter(0);

                std::sync::atomic::fence(Ordering::Acquire);

                if meta
                    .shared
                    .compare_exchange(old, new, Ordering::AcqRel, Ordering::Relaxed)
                    .is_err()
                {
                    false
                } else {
                    // let owner = self.owner();
                    // match owner {
                    //     None => false,
                    //     Some(tid) if tid == ThreadId::current_thread() => {
                    //         self.biased_count() as i32 == 1
                    //     }
                    //     Some(_) => false,
                    // }

                    true
                }
            }
            Some(tid) if tid == ThreadId::current_thread() => {
                let local_count = self.biased_count();
                if local_count == 1 {
                    let meta = self;
                    let old = meta.shared.load(Ordering::Relaxed);
                    std::sync::atomic::fence(Ordering::Acquire);
                    if old.get_counter() != 0 {
                        false
                    } else {
                        true
                    }
                } else {
                    false
                }
            }

            Some(_) => false,
        }
    }
}

impl<T: ?Sized> RcBox<T> {
    // TODO: Lift this to the Obj struct that eventually gets made
    pub fn increment(&self) {
        self.rcword.increment()
    }

    pub fn fast_increment(&self) {
        self.rcword.fast_increment()
    }

    pub fn slow_increment(&self) {
        self.rcword.slow_increment()
    }

    pub fn decrement(&self) -> DecrementAction {
        self.rcword.decrement()
    }

    // TODO: @Matt
    // Call this in the fast path for drop after the drop action is called.
    pub fn fast_decrement_drop_impl(&self) -> DecrementAction {
        self.rcword.fast_decrement_drop_impl()
    }

    pub fn fast_decrement(&self) -> DecrementAction {
        self.rcword.fast_decrement()
    }

    pub fn slow_decrement(&self) -> DecrementAction {
        self.rcword.slow_decrement()
    }

    fn has_unique_ref(&self) -> bool {
        self.rcword.has_unique_ref()
    }
}

pub struct Wrapper(Box<ManuallyDrop<dyn BiasedMerge>>);

unsafe impl Send for Wrapper {}
unsafe impl Sync for Wrapper {}

#[derive(Default)]
pub struct QueueHandle {
    map: dashmap::DashMap<Option<ThreadId>, Vec<Wrapper>>,
    unregistered: dashmap::DashMap<Option<ThreadId>, Vec<Wrapper>>,
}

/// The same as `std::thread::spawn`, however this will run
/// an explicit merge on that thread when the given function
/// exits.
pub fn with_explicit_merge<F, T>(f: F) -> JoinHandle<T>
where
    F: FnOnce() -> T,
    F: Send + 'static,
    T: Send + 'static,
{
    std::thread::spawn(|| {
        QueueHandle::register_thread();
        let res = f();
        QueueHandle::finish_thread_merge();
        res
    })
}

pub trait BiasedMerge {
    fn merge(self);
    fn meta_outer(&self) -> &RcWord;
    unsafe fn drop_contents_and_maybe_box_outer(&mut self);
}

impl<T: ?Sized> BiasedMerge for BiasedRc<T> {
    fn merge(mut self) {
        // let mut old;
        // let mut new;
        // loop {
        //     old = self.meta().shared.load(Ordering::Acquire);
        //     new = old;
        //     new.update_counter(|x| x + self.meta().biased_count() as i32);
        //     new.set_merged(true);

        //     if self
        //         .meta()
        //         .shared
        //         .compare_exchange(old, new, Ordering::AcqRel, Ordering::Relaxed)
        //         .is_ok()
        //     {
        //         break;
        //     }
        // }

        let mut old = self.meta().shared.load(Ordering::Acquire);
        let mut new;
        loop {
            new = old;
            new.update_counter(|x| x + self.meta().biased_count() as i32);
            new.set_merged(true);

            match self
                .meta()
                .shared
                .compare_exchange(old, new, Ordering::AcqRel, Ordering::Relaxed)
            {
                Ok(_) => break,
                Err(e) => {
                    old = Packed(e);
                }
            }
        }

        if new.get_counter() == 0 {
            unsafe { self.drop_contents_and_maybe_box() };
        } else {
            // self.meta().thread_id.store(None, Ordering::Relaxed);
            self.meta().clear_owner();
        }

        std::mem::forget(self);
    }

    fn meta_outer(&self) -> &RcWord {
        self.meta()
    }

    unsafe fn drop_contents_and_maybe_box_outer(&mut self) {
        unsafe { self.drop_contents_and_maybe_box() }
    }
}

pub static QUEUE: LazyLock<QueueHandle> = LazyLock::new(|| QueueHandle::default());

/// Registers the currently running thread with the queue collector.
/// This isn't explicitly required; however if you do not
pub fn register_thread() {
    QueueHandle::register_thread();
}

impl QueueHandle {
    pub fn register_thread() {
        let key = ThreadId::current_thread();

        if !QUEUE.map.contains_key(&Some(key)) {
            QUEUE.map.insert(Some(key), Vec::new());
        }
    }

    /// Hands an owned merge handle to its owning thread's queue.
    ///
    /// Split out from `enqueue` so that allocations other than `BiasedRc` - a
    /// `PackedRc`, say - can take the same path. The queue already stores these
    /// type erased, so only the entry point needed widening.
    pub fn enqueue_merge<M: BiasedMerge + 'static>(key: Option<ThreadId>, value: M) {
        let wrapper = || Wrapper(Box::new(ManuallyDrop::new(unsafe {
            core::ptr::read(&value as *const M)
        })));

        if let Some(mut q) = QUEUE.map.get_mut(&key) {
            q.push(wrapper());
        } else if let Some(mut q) = QUEUE.unregistered.get_mut(&key) {
            q.push(wrapper());
        } else {
            QUEUE.unregistered.insert(key, vec![wrapper()]);
        }

        core::mem::forget(value);
    }

    pub fn enqueue<T: ?Sized + 'static>(value: &BiasedRc<T>) {
        // let key = value.meta().thread_id.load(Ordering::Relaxed);
        let key = value.meta().owner();

        // TODO: The thread ID needs to be registered once its created. Otherwise,
        // this doesn't really work.
        if let Some(mut q) = QUEUE.map.get_mut(&key) {
            q.push(Wrapper(Box::new(ManuallyDrop::new(BiasedRc::from_inner(
                value.ptr,
            )))));
        } else {
            if let Some(mut q) = QUEUE.unregistered.get_mut(&key) {
                q.push(Wrapper(Box::new(ManuallyDrop::new(BiasedRc::from_inner(
                    value.ptr,
                )))));
            } else {
                QUEUE.unregistered.insert(
                    key,
                    vec![Wrapper(Box::new(ManuallyDrop::new(BiasedRc::from_inner(
                        value.ptr,
                    ))))],
                );
            }

            // Fallback thread, for something that is unclaimed
            // guard.insert_fallback(BiasedRc::from_inner(value.ptr));
        }
    }

    // TODO: @Matt
    // Audit where this is getting run! It doesn't need to run
    // nearly as much as we have it running during GC runs. We can probably
    // just run it after the gc collection rather than during every
    // allocation.
    pub fn run_explicit_merge() -> usize {
        let current = ThreadId::current_thread();

        // Attempt to coalesce the unregistered queue, if its now registered:

        let unregistered = QUEUE
            .unregistered
            .get_mut(&Some(current))
            .map(|mut x| Self::explicit_merge(&mut x))
            .unwrap_or_default();

        let registered = QUEUE
            .map
            .get_mut(&Some(ThreadId::current_thread()))
            .map(|mut x| Self::explicit_merge(&mut x))
            .unwrap_or_default();

        unregistered + registered

        // ret + Self::explicit_merge(&mut guard.inner)
    }

    pub fn finish_thread_merge() {
        let id = ThreadId::current_thread();
        let q = QUEUE.map.remove(&Some(id));
        q.map(|mut x| Self::explicit_merge(&mut x.1));
    }

    pub fn explicit_merge(values: &mut Vec<Wrapper>) -> usize {
        // println!(
        //     "Running explicit merge on thread: {:?} with count: {}",
        //     std::thread::current().id(),
        //     values.len()
        // );

        let ret = values.len();

        for value in values.drain(..) {
            let mut value = value.0;
            // let mut old;
            // let mut new;
            // loop {
            //     old = value.meta_outer().shared.load(Ordering::Acquire);
            //     new = old;
            //     new.update_counter(|x| x + value.meta_outer().biased_count() as i32);
            //     new.set_merged(true);

            //     if value
            //         .meta_outer()
            //         .shared
            //         .compare_exchange(old, new, Ordering::AcqRel, Ordering::Relaxed)
            //         .is_ok()
            //     {
            //         break;
            //     }
            // }

            let mut old = value.meta_outer().shared.load(Ordering::Acquire);
            let mut new;
            loop {
                new = old;
                new.update_counter(|x| x + value.meta_outer().biased_count() as i32);
                new.set_merged(true);

                match value.meta_outer().shared.compare_exchange(
                    old,
                    new,
                    Ordering::AcqRel,
                    Ordering::Relaxed,
                ) {
                    Ok(_) => break,
                    Err(e) => {
                        old = Packed(e);
                    }
                }
            }

            if new.get_counter() == 0 {
                // println!("invoking the destructor");
                unsafe { value.drop_contents_and_maybe_box_outer() };
            } else {
                // value.meta_outer().thread_id.store(None, Ordering::Relaxed);
                value.meta_outer().clear_owner();
            }

            drop(value);
        }

        ret
    }
}

impl BiasedRc<dyn Any> {
    #[inline]
    pub fn downcast<T: Any>(self) -> Result<BiasedRc<T>, Self> {
        if (*self).is::<T>() {
            let ptr = self.ptr.cast::<RcBox<T>>();
            mem::forget(self);
            Ok(BiasedRc::from_inner(ptr))
        } else {
            Err(self)
        }
    }
}

impl BiasedRc<dyn Any + Sync + Send> {
    #[inline]
    pub fn downcast<T: Any + Sync + Send>(self) -> Result<BiasedRc<T>, Self> {
        if (*self).is::<T>() {
            let ptr = self.ptr.cast::<RcBox<T>>();
            mem::forget(self);
            Ok(BiasedRc::from_inner(ptr))
        } else {
            Err(self)
        }
    }
}

impl<T: Any + 'static> From<BiasedRc<T>> for BiasedRc<dyn Any + 'static> {
    #[inline]
    fn from(src: BiasedRc<T>) -> Self {
        let ptr = src.ptr.as_ptr() as *mut RcBox<dyn Any>;
        mem::forget(src);
        Self::from_inner(unsafe { NonNull::new_unchecked(ptr) })
    }
}

impl<T: Any + Sync + Send + 'static> From<BiasedRc<T>>
    for BiasedRc<dyn Any + Sync + Send + 'static>
{
    #[inline]
    fn from(src: BiasedRc<T>) -> Self {
        let ptr = src.ptr.as_ptr() as *mut RcBox<dyn Any + Sync + Send>;
        mem::forget(src);
        Self::from_inner(unsafe { NonNull::new_unchecked(ptr) })
    }
}

impl<T: ?Sized> RcBox<T> {
    /// Deallocates an `RcBox`
    ///
    /// `meta` will be dropped, but `data` must have already been dropped in place.
    ///
    /// # Safety
    /// The allocation must have been previously allocated with `RcBox::allocate_*()`.
    #[inline]
    unsafe fn dealloc(ptr: NonNull<RcBox<T>>) {
        unsafe { ptr::addr_of_mut!((*ptr.as_ptr()).rcword).drop_in_place() };
        let layout = Layout::for_value(unsafe { ptr.as_ref() });
        unsafe { alloc::dealloc(ptr.as_ptr().cast(), layout) };
    }

    /// Get the pointer to a `RcBox<T>` from a pointer to the data
    ///
    /// # Safety
    ///
    /// The pointer must point to (and have valid metadata for) the data part of a previously
    /// valid instance of `RcBox<T>` and it must not be dangling.
    #[inline]
    unsafe fn ptr_from_data_ptr(ptr: *const T) -> *const RcBox<T> {
        // Calculate layout of RcBox<T> without `data` tail, but including padding
        let base_layout = Layout::new::<RcBox<()>>();
        // Safety: covered by the safety contract above
        let value_alignment = mem::align_of_val(unsafe { &*ptr });
        let value_offset_layout =
            Layout::from_size_align(0, value_alignment).expect("invalid memory layout");
        let layout = base_layout
            .extend(value_offset_layout)
            .expect("invalid memory layout")
            .0;

        // Move pointer to point to the start of the original RcBox<T>
        // Safety: covered by the safety contract above
        let rcbox = unsafe { ptr.cast::<u8>().offset(-(layout.size() as isize)) };
        set_ptr_value(ptr, rcbox as *mut u8) as *const RcBox<T>
    }
}

impl<T> RcBox<T> {
    /// Tries to allocate an `RcBox`
    ///
    /// Returns a mutable reference with arbitrary lifetime on success and the memory layout that
    /// could not be allocated if the allocation failed.
    #[inline]
    fn try_allocate(meta: RcWord) -> Result<NonNull<RcBox<mem::MaybeUninit<T>>>, Layout> {
        let layout = Layout::new::<RcBox<T>>();

        let ptr = unsafe { alloc::alloc(layout) }.cast::<RcBox<mem::MaybeUninit<T>>>();
        if ptr.is_null() {
            Err(layout)
        } else {
            unsafe { ptr::addr_of_mut!((*ptr).rcword).write(meta) };
            Ok(unsafe { NonNull::new_unchecked(ptr) })
        }
    }

    /// Allocates an `RcBox`
    ///
    /// Returns a mutable reference with arbitrary lifetime on success.
    ///
    /// # Panics
    /// Panics or aborts if the allocation failed.
    #[inline]
    fn allocate(meta: RcWord) -> NonNull<RcBox<mem::MaybeUninit<T>>> {
        match Self::try_allocate(meta) {
            Ok(result) => result,
            Err(layout) => alloc::handle_alloc_error(layout),
        }
    }

    /// Tries to allocate an `RcBox` for a slice.
    ///
    /// Returns a mutable reference with arbitrary lifetime on success and the memory layout that
    /// could not be allocated if the allocation failed or the layout calculation overflowed.
    #[inline]
    fn try_allocate_slice<'a>(
        meta: RcWord,
        len: usize,
        zeroed: bool,
    ) -> Result<&'a mut RcBox<[mem::MaybeUninit<T>]>, Layout> {
        // Calculate memory layout
        let layout = Layout::new::<RcBox<[T; 0]>>();
        let payload_layout = Layout::array::<T>(len).map_err(|_| layout)?;
        let layout = layout
            .extend(payload_layout)
            .map_err(|_| layout)?
            .0
            .pad_to_align();

        // Allocate memory
        let ptr = unsafe {
            if zeroed {
                alloc::alloc_zeroed(layout)
            } else {
                alloc::alloc(layout)
            }
        };

        // Build a fat pointer
        // The immediate slice reference [MaybeUninit<u8>] *should* be sound
        let ptr = ptr::slice_from_raw_parts_mut(ptr.cast::<mem::MaybeUninit<u8>>(), len)
            as *mut RcBox<[mem::MaybeUninit<T>]>;

        if ptr.is_null() {
            // Allocation failed
            Err(layout)
        } else {
            // Initialize metadata field and return result
            unsafe { ptr::addr_of_mut!((*ptr).rcword).write(meta) };
            Ok(unsafe { ptr.as_mut().unwrap() })
        }
    }

    /// Allocates an `RcBox` for a slice
    ///
    /// Returns a mutable reference with arbitrary lifetime on success.
    ///
    /// # Panics
    /// Panics or aborts if the allocation failed or the memory layout calculation overflowed.
    #[inline]
    fn allocate_slice<'a>(
        meta: RcWord,
        len: usize,
        zeroed: bool,
    ) -> &'a mut RcBox<[mem::MaybeUninit<T>]> {
        match Self::try_allocate_slice(meta, len, zeroed) {
            Ok(result) => result,
            Err(layout) => alloc::handle_alloc_error(layout),
        }
    }
}

impl<T> RcBox<mem::MaybeUninit<T>> {
    /// Converts to a mutable reference without the `MaybeUninit` wrapper.
    ///
    /// # Safety
    /// The payload must have been fully initialized or this causes immediate undefined behaviour.
    #[inline]
    unsafe fn assume_init(&mut self) -> &mut RcBox<T> {
        unsafe { (self as *mut Self).cast::<RcBox<T>>().as_mut() }.unwrap()
    }
}

impl<T> RcBox<[mem::MaybeUninit<T>]> {
    /// Converts to a mutable reference without the `MaybeUninit` wrapper.
    ///
    /// # Safety
    /// The payload slice must have been fully initialized or this causes immediate undefined
    /// behaviour.
    #[inline]
    unsafe fn assume_init(&mut self) -> &mut RcBox<[T]> {
        unsafe { (self as *mut _ as *mut RcBox<[T]>).as_mut() }.unwrap()
    }
}

/// Reimplementation of `ptr::set_ptr_value` as long as that one is unstable
///
/// Constructs a new pointer to `addr_ptr` with the metadata and type of `meta_ptr`.
#[inline]
fn set_ptr_value<T: ?Sized, U>(mut meta_ptr: *const T, addr_ptr: *mut U) -> *mut T {
    let thin = (&mut meta_ptr as *mut *const T).cast::<*const u8>();
    // Safety: In case of a thin pointer, this operations is identical
    // to a simple assignment. In case of a fat pointer, with the current
    // fat pointer layout implementation, the first field of such a
    // pointer is always the data pointer, which is likewise assigned.
    unsafe { *thin = addr_ptr.cast() };

    meta_ptr as *mut T
}

#[repr(C)]
pub struct BiasedRc<T: ?Sized + 'static> {
    ptr: NonNull<RcBox<T>>,
    phantom2: PhantomData<T>,
}

impl<T: ?Sized + 'static> BiasedRc<T> {
    pub fn raw_slow_decrement(&mut self) {
        match self.get_box().slow_decrement() {
            DecrementAction::DoNothing => {}
            DecrementAction::Queue => {
                // Enqueue the value
                QueueHandle::enqueue(self);
            }
            DecrementAction::Deallocate => {
                unsafe { self.drop_contents_and_maybe_box() };
            }
        }
    }

    pub fn raw_slow_increment(&mut self) {
        self.get_box().slow_increment();
    }

    pub fn fast_decrement_post_ref_count_dec(&mut self) {
        match self.get_box().fast_decrement_drop_impl() {
            DecrementAction::DoNothing => {}
            DecrementAction::Queue => {
                // Enqueue the value
                QueueHandle::enqueue(self);
            }
            DecrementAction::Deallocate => {
                unsafe { self.drop_contents_and_maybe_box() };
            }
        }
    }
}

impl<T: ?Sized + Clone> BiasedRc<T> {
    #[must_use]
    pub fn make_mut(this: &mut Self) -> &mut T {
        if !this.get_box().has_unique_ref() {
            // Another pointer exists; clone
            *this = Self::new(T::clone(this.data()));
        }

        unsafe {
            // This unsafety is ok because we're guaranteed that the pointer
            // returned is the *only* pointer that will ever be returned to T. Our
            // reference count is guaranteed to be 1 at this point, and we required
            // the Arc itself to be `mut`, so we're returning the only possible
            // reference to the inner data.
            Self::get_mut_unchecked(this)
        }
    }
}

impl<T: ?Sized> BiasedRc<T> {
    #[inline(always)]
    fn from_inner(ptr: NonNull<RcBox<T>>) -> Self {
        Self {
            ptr,
            phantom2: PhantomData,
        }
    }

    #[inline(always)]
    pub fn get_box(&self) -> &RcBox<T> {
        unsafe { &(*self.ptr.as_ptr()) }
    }

    /// Provides a reference to the inner value.
    #[inline(always)]
    fn data(&self) -> &T {
        unsafe { &(*self.ptr.as_ptr()).data }
    }

    /// Provides a reference to the shared metadata.
    #[inline(always)]
    fn meta(&self) -> &RcWord {
        unsafe { &(*self.ptr.as_ptr()).rcword }
    }

    #[inline(always)]
    unsafe fn pin_get_ref(this: &Pin<Self>) -> &Self {
        // SAFETY: Pin is repr(transparent) and by contract the caller doesn't use the reference
        // to move the value.
        unsafe { &*(this as *const Pin<Self>).cast::<Self>() }
    }

    #[must_use]
    #[inline]
    pub unsafe fn get_mut_unchecked(this: &mut Self) -> &mut T {
        unsafe { &mut (*this.ptr.as_ptr()).data }
    }

    #[must_use]
    #[inline]
    pub fn get_mut(this: &mut Self) -> Option<&mut T> {
        if this.get_box().has_unique_ref() {
            unsafe { Some(Self::get_mut_unchecked(this)) }
        } else {
            None
        }
    }

    #[must_use]
    #[inline]
    // The jit addresses fields by raw offset from the box pointer the value carries
    pub const fn data_offset() -> usize
    where
        T: Sized,
    {
        core::mem::offset_of!(RcBox<T>, data)
    }

    pub fn as_ptr(this: &Self) -> *const T {
        let ptr = this.ptr.as_ptr();

        // Safety: Neccessary for `from_raw()` (when implemented), retains provenance.
        // Besides that, does basically the same thing as `data()` or `get_mut_unchecked()`.
        unsafe { ptr::addr_of_mut!((*ptr).data) }
    }

    #[must_use = "Memory will leak if the result is not used"]
    pub fn into_raw(this: Self) -> *const T {
        let ptr = Self::as_ptr(&this);
        mem::forget(this);
        ptr
    }

    pub unsafe fn from_raw(ptr: *const T) -> Self {
        // Safety: covered by the safety contract for this function
        let box_ptr = unsafe { RcBox::<T>::ptr_from_data_ptr(ptr) };

        Self::from_inner(NonNull::new(box_ptr as *mut _).expect("invalid pointer"))
    }

    #[inline]
    pub fn ptr_eq(this: &Self, other: &BiasedRc<T>) -> bool {
        std::ptr::eq(this.ptr.as_ptr(), other.ptr.as_ptr())
    }

    #[inline]
    pub fn ptr_eq_pin(this: &Pin<Self>, other: &Pin<BiasedRc<T>>) -> bool {
        // SAFETY: we are not moving anything and we don't expose any pointers.
        let this = unsafe { Self::pin_get_ref(this) };
        let other = unsafe { BiasedRc::<T>::pin_get_ref(other) };
        std::ptr::eq(this.ptr.as_ptr(), other.ptr.as_ptr())
    }

    #[inline]
    pub fn strong_count(this: &Self) -> usize {
        let meta = this.meta();

        let word = meta.shared.load(Ordering::Acquire);
        let mut count = word.get_counter();

        // If the counter is 0, then we have to get the count from somewhere else
        if word.get_counter() == 0 {
            // let owner = self.rcword.thread_id.load(Ordering::Relaxed);
            let owner = meta.owner();
            match owner {
                None => {}
                Some(tid) if tid == ThreadId::current_thread() => {
                    count = meta.biased_count() as i32;
                }
                Some(_) => {
                    count = 2;
                }
            }
        }

        count as _
    }

    /// Gets the approximate number of strong pointers to the pinned inner value.
    ///
    #[inline]
    pub fn strong_count_pin(this: &Pin<Self>) -> usize {
        // SAFETY: We are not moving anything and we don't expose any pointers.
        let this = unsafe { Self::pin_get_ref(this) };
        Self::strong_count(this)
    }

    #[inline]
    fn build_new_meta() -> RcWord {
        RcWord::new()
    }

    unsafe fn drop_contents_and_maybe_box(&mut self) {
        // Safety: only called if this was the last strong reference
        unsafe {
            ptr::drop_in_place(Self::get_mut_unchecked(self));
        }

        // Safety: only called if this was the last (weak) reference
        unsafe {
            RcBox::dealloc(self.ptr);
        }
    }
}

impl<T> BiasedRc<T> {
    #[inline]
    pub fn new(data: T) -> Self {
        // register_thread();
        let mut inner = RcBox::allocate(Self::build_new_meta());
        let inner = unsafe { inner.as_mut() };
        inner.data.write(data);
        Self::from_inner(unsafe { inner.assume_init() }.into())
    }

    pub fn new_branded(data: T) -> Self {
        register_thread();
        Self::new(data)
    }

    #[inline]
    pub fn new_uninit() -> BiasedRc<mem::MaybeUninit<T>> {
        let inner = RcBox::allocate(Self::build_new_meta());
        BiasedRc::from_inner(inner)
    }

    #[inline]
    pub fn new_zeroed() -> BiasedRc<mem::MaybeUninit<T>> {
        let mut inner = RcBox::allocate(Self::build_new_meta());
        unsafe { inner.as_mut() }.data = mem::MaybeUninit::zeroed();
        BiasedRc::from_inner(inner)
    }

    #[inline]
    pub fn pin(data: T) -> Pin<Self> {
        unsafe { Pin::new_unchecked(Self::new(data)) }
    }

    pub fn try_new(data: T) -> Result<Self, AllocError> {
        let mut inner = RcBox::try_allocate(Self::build_new_meta()).map_err(|_| AllocError)?;
        let inner = unsafe { inner.as_mut() };
        inner.data.write(data);
        Ok(Self::from_inner(unsafe { inner.assume_init() }.into()))
    }

    pub fn try_new_uninit() -> Result<BiasedRc<mem::MaybeUninit<T>>, AllocError> {
        let inner = RcBox::try_allocate(Self::build_new_meta()).map_err(|_| AllocError)?;
        Ok(BiasedRc::from_inner(inner.into()))
    }

    pub fn try_new_zeroed() -> Result<BiasedRc<mem::MaybeUninit<T>>, AllocError> {
        let mut inner = RcBox::try_allocate(Self::build_new_meta()).map_err(|_| AllocError)?;
        unsafe { inner.as_mut() }.data = mem::MaybeUninit::zeroed();
        Ok(BiasedRc::from_inner(inner))
    }

    pub fn try_unwrap(this: Self) -> Result<T, Self> {
        // let owner = this.meta().thread_id.load(Ordering::Relaxed);
        let owner = this.meta().owner();
        match owner {
            None => Self::try_unwrap_internal(this),
            Some(tid) if tid == ThreadId::current_thread() => {
                let local_count = this.meta().biased_count();

                if local_count == 1 {
                    Self::try_unwrap_internal_same_thread(this)
                } else {
                    Err(this)
                }
            }
            // Has an owner on a different thread.
            Some(_) => Err(this),
        }
    }

    fn try_unwrap_internal_same_thread(this: Self) -> Result<T, Self> {
        let meta = this.meta();
        let old = meta.shared.load(Ordering::Relaxed);

        std::sync::atomic::fence(Ordering::Acquire);

        if old.get_counter() != 0 {
            Err(this)
        } else {
            // meta.thread_id.store(None, Ordering::Relaxed);
            let copy = unsafe { ptr::read(Self::as_ptr(&this)) };

            // Deallocate the box?
            unsafe { RcBox::dealloc(this.ptr) };

            mem::forget(this);

            Ok(copy)
        }
    }

    #[inline]
    fn try_unwrap_internal(this: Self) -> Result<T, Self> {
        let meta = this.meta();
        let mut new;
        let mut old;

        // loop {
        old = meta.shared.load(Ordering::Relaxed);
        new = old;

        old.set_counter(1);
        new.set_counter(0);

        std::sync::atomic::fence(Ordering::Acquire);

        if meta
            .shared
            .compare_exchange(old, new, Ordering::AcqRel, Ordering::Relaxed)
            .is_err()
        {
            Err(this)
        } else {
            // meta.thread_id.store(None, Ordering::Relaxed);
            let copy = unsafe { ptr::read(Self::as_ptr(&this)) };
            // Deallocate the box?
            unsafe { RcBox::dealloc(this.ptr) };

            mem::forget(this);

            Ok(copy)
        }
    }
}

/// The `AllocError` error indicates an allocation failure when using `try_new()` etc.
///
/// Will become a type alias for [`std::alloc::AllocError`] once that is stabilized.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct AllocError;

impl fmt::Display for AllocError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("memory allocation failed")
    }
}

impl std::error::Error for AllocError {}

impl From<Infallible> for AllocError {
    fn from(_: Infallible) -> AllocError {
        unreachable!();
    }
}

impl<T: ?Sized> Deref for BiasedRc<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &T {
        self.data()
    }
}

impl<T: ?Sized> Borrow<T> for BiasedRc<T> {
    #[inline]
    fn borrow(&self) -> &T {
        &**self
    }
}

impl<T: ?Sized> AsRef<T> for BiasedRc<T> {
    #[inline]
    fn as_ref(&self) -> &T {
        &**self
    }
}

impl<T: ?Sized> Clone for BiasedRc<T> {
    #[inline]
    fn clone(&self) -> Self {
        self.get_box().increment();
        Self::from_inner(self.ptr)
    }
}

impl<T: ?Sized + 'static> Drop for BiasedRc<T> {
    #[inline]
    fn drop(&mut self) {
        match self.get_box().decrement() {
            DecrementAction::DoNothing => {}
            DecrementAction::Queue => {
                // Enqueue the value
                QueueHandle::enqueue(self);
            }
            DecrementAction::Deallocate => {
                unsafe { self.drop_contents_and_maybe_box() };
            }
        }
    }
}

// Propagate some useful traits implemented by the inner type

impl<T: Default> Default for BiasedRc<T> {
    #[inline]
    fn default() -> Self {
        Self::new(Default::default())
    }
}

impl<T: ?Sized + PartialEq> PartialEq<BiasedRc<T>> for BiasedRc<T> {
    #[inline]
    fn eq(&self, other: &BiasedRc<T>) -> bool {
        **self == **other
    }
}

impl<T: ?Sized + Eq> Eq for BiasedRc<T> {}

impl<T: ?Sized + Hash> Hash for BiasedRc<T> {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        Self::data(self).hash(state);
    }
}

impl<T: ?Sized + PartialOrd> PartialOrd<BiasedRc<T>> for BiasedRc<T> {
    #[inline]
    fn partial_cmp(&self, other: &BiasedRc<T>) -> Option<cmp::Ordering> {
        (**self).partial_cmp(&**other)
    }
}

impl<T: ?Sized + Ord> Ord for BiasedRc<T> {
    #[inline]
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        (**self).cmp(&**other)
    }
}

impl<T: ?Sized + fmt::Display> fmt::Display for BiasedRc<T> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&Self::data(self), f)
    }
}

impl<T: ?Sized + fmt::Debug> fmt::Debug for BiasedRc<T> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&Self::data(self), f)
    }
}

impl<T: ?Sized> fmt::Pointer for BiasedRc<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Pointer::fmt(&Self::as_ptr(self), f)
    }
}

impl<T: ?Sized> Unpin for BiasedRc<T> {}

unsafe impl<T: ?Sized + Sync + Send> Send for BiasedRc<T> {}
unsafe impl<T: ?Sized + Sync + Send> Sync for BiasedRc<T> {}

impl<T> iter::FromIterator<T> for BiasedRc<[T]> {
    fn from_iter<I: iter::IntoIterator<Item = T>>(iter: I) -> Self {
        let vec: Vec<T> = iter.into_iter().collect();
        vec.into()
    }
}

impl<T> BiasedRc<[T]> {
    /// Creates a new reference-counted slice with uninitialized contents.
    #[inline]
    pub fn new_uninit_slice(len: usize) -> BiasedRc<[mem::MaybeUninit<T>]> {
        let inner = RcBox::allocate_slice(Self::build_new_meta(), len, false);
        BiasedRc::from_inner(inner.into())
    }

    /// Creates a new reference-counted slice with uninitialized contents, with the memory being
    /// filled with 0 bytes.
    #[inline]
    pub fn new_zeroed_slice(len: usize) -> BiasedRc<[mem::MaybeUninit<T>]> {
        let inner = RcBox::allocate_slice(Self::build_new_meta(), len, true);
        BiasedRc::from_inner(inner.into())
    }

    #[inline]
    unsafe fn copy_from_slice_unchecked(src: &[T]) -> Self {
        let len = src.len();
        let inner = RcBox::allocate_slice(Self::build_new_meta(), len, false);
        let dest = ptr::addr_of_mut!((*inner).data).cast();

        // Safety: The freshly allocated `RcBox` can't alias `src` and the payload can be fully
        // initialized by copying the slice memory. The copying is also safe as long as the safety
        // requirements for calling this are fulfilled.
        unsafe {
            src.as_ptr().copy_to_nonoverlapping(dest, src.len());
            BiasedRc::from_inner(inner.assume_init().into())
        }
    }
}

impl<T: Copy> BiasedRc<[T]> {
    #[inline]
    pub fn copy_from_slice(src: &[T]) -> Self {
        // Safety: `T` is `Copy`.
        unsafe { Self::copy_from_slice_unchecked(src) }
    }
}

#[must_use]
pub(crate) struct SliceBuilder<'a, T> {
    rcbox: &'a mut RcBox<[MaybeUninit<T>]>,
    n_elems: usize,
}

impl<'a, T> SliceBuilder<'a, T> {
    /// Constructs a new builder for a `RcBox<[T]>` with a slice length of `length`
    #[inline]
    pub fn new(meta: RcWord, length: usize) -> Self {
        let rcbox = RcBox::<T>::allocate_slice(meta, length, false);
        Self { rcbox, n_elems: 0 }
    }

    /// Fills the next free slot in the slice with `item`
    #[inline]
    pub fn append(&mut self, item: T) {
        self.rcbox.data[self.n_elems].write(item);
        self.n_elems += 1;
    }

    /// Consumes the builder and returns the initialized `RcBox<T>`
    ///
    /// The result is a mutable reference with arbitrary lifetime.
    ///
    /// # Panics
    /// Panics if the number of appended elements doesn't match the promised length.
    #[inline]
    pub fn finish(self) -> &'a mut RcBox<[T]> {
        assert_eq!(self.n_elems, self.rcbox.data.len());
        let rcbox: *mut _ = self.rcbox;
        std::mem::forget(self);
        unsafe { (*rcbox).assume_init() }
    }
}

impl<T> Drop for SliceBuilder<'_, T> {
    /// Drops the already cloned elements and deallocates the temporary `RcBox`
    ///
    /// Only reached if the builder wasn't consumed by `finish`, which should only happen in
    /// a panic unwind.
    #[cold]
    fn drop(&'_ mut self) {
        let slice = &mut self.rcbox.data[..self.n_elems];
        unsafe {
            let slice: &mut [T] = &mut *(slice as *mut [MaybeUninit<T>] as *mut [T]);
            drop_in_place(slice);
        }
        unsafe {
            RcBox::dealloc(self.rcbox.into());
        }
    }
}

impl<T> From<T> for BiasedRc<T> {
    #[inline]
    fn from(src: T) -> Self {
        Self::new(src)
    }
}

impl<T: Clone> From<&[T]> for BiasedRc<[T]> {
    #[inline]
    fn from(src: &[T]) -> Self {
        let mut builder = SliceBuilder::new(Self::build_new_meta(), src.len());
        for item in src {
            builder.append(Clone::clone(item));
        }
        Self::from_inner(builder.finish().into())
    }
}

impl<T> From<Vec<T>> for BiasedRc<[T]> {
    #[inline]
    fn from(mut src: Vec<T>) -> Self {
        unsafe {
            let result = BiasedRc::<_>::copy_from_slice_unchecked(&src[..]);

            // Set the length of `src`, so that the moved items are not dropped.
            src.set_len(0);

            result
        }
    }
}

impl From<&str> for BiasedRc<str> {
    #[inline]
    fn from(src: &str) -> Self {
        let bytes = BiasedRc::<_>::copy_from_slice(src.as_bytes());
        let inner = unsafe { (bytes.ptr.as_ptr() as *mut _ as *mut RcBox<str>).as_mut() }.unwrap();
        mem::forget(bytes);
        Self::from_inner(inner.into())
    }
}

impl From<String> for BiasedRc<str> {
    #[inline]
    fn from(src: String) -> Self {
        Self::from(&src[..])
    }
}

#[test]
fn does_this_work() {
    register_thread();
    let value = BiasedRc::new(10);
    println!("{}", value);
}

#[test]
fn test_drop_impl() {
    struct Foo {
        foo: usize,
    }
    impl Drop for Foo {
        fn drop(&mut self) {
            println!("Calling drop: {}", self.foo);
        }
    }
    let value = BiasedRc::new(Foo { foo: 10 });

    drop(value);
}

#[test]
fn test_clone_impl() {
    register_thread();
    struct Foo {
        foo: usize,
    }
    impl Drop for Foo {
        fn drop(&mut self) {
            println!("Calling drop: {}", self.foo);
        }
    }
    let value = BiasedRc::new(Foo { foo: 10 });
    let cloned = BiasedRc::clone(&value);

    drop(value);

    println!("Now we're done");

    drop(cloned);
}

#[test]
fn test_queue_impl() {
    register_thread();
    struct Foo {
        foo: String,
    }
    impl Drop for Foo {
        fn drop(&mut self) {
            println!("Calling drop: {}", self.foo);
        }
    }
    let value = BiasedRc::new(Foo {
        foo: "hello world".to_string(),
    });
    let cloned = BiasedRc::clone(&value);

    let thread = std::thread::spawn(move || {
        drop(cloned);

        // Run explicit merge:
        QueueHandle::run_explicit_merge();
    });

    thread.join().unwrap();

    QueueHandle::run_explicit_merge();
}

#[test]
fn test_try_unwrap_impl_same_thread() {
    #[derive(Debug)]
    struct Foo {
        foo: String,
    }
    impl Drop for Foo {
        fn drop(&mut self) {
            println!("Calling drop: {}", self.foo);
        }
    }
    let value = BiasedRc::new(Foo {
        foo: "hello world".to_string(),
    });
    let cloned = BiasedRc::clone(&value);

    let failed_unwrap = BiasedRc::try_unwrap(value).unwrap_err();

    drop(failed_unwrap);

    assert!(BiasedRc::try_unwrap(cloned).is_ok());

    // let thread = std::thread::spawn(move || {
    //     drop(cloned);

    //     // Run explicit merge:
    //     QueueHandle::run_explicit_merge();
    // });

    // thread.join().unwrap();

    // QueueHandle::run_explicit_merge();
}

#[test]
fn test_try_unwrap_impl_moved_thread() {
    #[derive(Debug)]
    struct Foo {
        foo: String,
    }
    impl Drop for Foo {
        fn drop(&mut self) {
            println!("Calling drop: {}", self.foo);
        }
    }
    let value = BiasedRc::new(Foo {
        foo: "hello world".to_string(),
    });
    let cloned = BiasedRc::clone(&value);

    let failed_unwrap = BiasedRc::try_unwrap(value).unwrap_err();

    drop(failed_unwrap);

    assert!(BiasedRc::try_unwrap(cloned).is_ok());

    // let thread = std::thread::spawn(move || {
    //     drop(cloned);

    //     // Run explicit merge:
    //     QueueHandle::run_explicit_merge();
    // });

    // thread.join().unwrap();

    // QueueHandle::run_explicit_merge();
}

#[test]
fn try_unwrap_data_race() {
    let a = BiasedRc::new(0);
    let b = a.clone();

    std::thread::spawn(move || {
        let _value = b;
    });

    std::thread::spawn(move || {
        BiasedRc::try_unwrap(a).unwrap();
    });
}

#[test]
fn try_unwrap_data_race_sleep() {
    // Exists to be exercised by Miri to check for data races.
    let a = BiasedRc::new(0);
    let b = a.clone();
    let t1 = std::thread::spawn(move || {
        let _value = *b;
    });
    let t2 = std::thread::spawn(move || {
        std::thread::sleep(std::time::Duration::from_millis(100));
        if let Ok(_) = BiasedRc::try_unwrap(a) {
            // u += 1;
        }
    });
    t1.join().unwrap();
    t2.join().unwrap();
}

#[test]
fn get_mut_data_race_sleep() {
    // Exists to be exercised by Miri to check for data races.
    let mut a = BiasedRc::new(0);
    let b = a.clone();
    let t1 = std::thread::spawn(move || {
        let _value = *b;
    });
    let t2 = std::thread::spawn(move || {
        std::thread::sleep(std::time::Duration::from_millis(100));
        if let Some(u) = BiasedRc::get_mut(&mut a) {
            *u += 1;
        }
    });
    t1.join().unwrap();
    t2.join().unwrap();
}

thread_local! {
    static DROP_COUNTER: Cell<usize> = Cell::new(0);
}

#[cfg(test)]
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Test(u8);

#[cfg(test)]
impl Drop for Test {
    fn drop(&mut self) {
        DROP_COUNTER.with(|x| x.set(x.get() + 1));
    }
}
#[cfg(test)]
impl Default for Test {
    fn default() -> Self {
        Test(1)
    }
}

#[cfg(test)]
// Panicing clone to test cloning from slice error cases
impl Clone for Test {
    fn clone(&self) -> Self {
        if self.0 == 0 {
            panic!();
        }
        Self(self.0)
    }
}

#[test]
fn test_any() {
    let a = BiasedRc::<Test>::new(Test(42));
    let b: BiasedRc<dyn Any> = From::from(a.clone());
    let c: BiasedRc<dyn Any + Send + Sync> = From::from(a);

    assert!(b.clone().downcast::<usize>().is_err());
    assert!(c.clone().downcast::<usize>().is_err());
    let a = c.downcast::<Test>().unwrap();
    assert_eq!(a.0, 42);
    let b = b.downcast::<Test>().unwrap();
    assert_eq!(b.0, 42);
}

/*
From the paper:

Lastly, when a thread terminates, it processes the objects re-
maining in its QueuedObjects list, and de-registers itself from
the QueuedObjects structure. Theoretically, an object can out-
live its owner thread if its biased counter is positive, and has not
been queued in the QueuedObjects list when the owner thread
terminates. We handle this case as follows. When a non-owner
thread makes the shared counter of an object negative, it first
checks whether the object’s owner thread is alive by looking-up
the QueuedObjects structure — which implicitly records the live
threads. If the owner thread is not alive, the non-owner thread
merges the counters instead of queuing the object, and either deal-
locates the object or unbiases it
*/
#[test]
fn test_moving_across_threads() {
    let (sender, receiver) = std::sync::mpsc::channel::<BiasedRc<usize>>();

    let (send_finish, receive_finish) = std::sync::mpsc::channel::<()>();

    let start = std::thread::spawn(move || {
        // register_thread();
        let value = BiasedRc::new(0);
        sender.send(value.clone()).unwrap();
        receive_finish.recv().unwrap();

        let _ = value.clone();
        let _ = value.clone();

        drop(value);
    });

    let value = receiver.recv().unwrap();
    dbg!(value.get_box().rcword.biased_count());
    let meta = value.get_box().rcword.shared.load(Ordering::Relaxed);
    dbg!(meta.get_merged());
    dbg!(meta.get_queued());
    dbg!(meta.get_counter());

    // Okay, so now we're in the realm of a value which has escaped its
    // owner thread, but the value has not been enqueued. At this point
    // now it should be clear that the thread is dead, because we've marked it
    // as such?
    dbg!(value.get_box().rcword.owner() == Some(ThreadId::current_thread()));

    drop(value);

    send_finish.send(()).unwrap();

    // Value should be on the thread:
    let merged = QueueHandle::run_explicit_merge();
    start.join().unwrap();

    dbg!(merged);
}

#[test]
fn test_dropping_across_multiple_threads() {
    register_thread();
    let original_value = BiasedRc::new("value".to_string());

    let value_one = original_value.clone();
    let other_thread_1 = std::thread::spawn(move || {
        register_thread();
        let value = value_one;

        for _ in 0..1000 {
            let _ = value.clone();
        }

        drop(value);
    });

    let value_one = original_value.clone();
    let other_thread_2 = std::thread::spawn(move || {
        let value = value_one;

        for _ in 0..1000 {
            let _ = value.clone();
        }

        drop(value);
    });

    other_thread_1.join().unwrap();
    other_thread_2.join().unwrap();

    let mut values = Vec::new();

    for _ in 0..100 {
        values.push(original_value.clone());
    }

    drop(original_value);

    values.clear();

    QueueHandle::run_explicit_merge();
}

#[test]
fn static_values() {
    static ROOT: LazyLock<BiasedRc<String>> = LazyLock::new(|| BiasedRc::new("Hello".to_string()));
    register_thread();

    with_explicit_merge(|| {
        let mut foo = ROOT.clone();

        let new = BiasedRc::make_mut(&mut foo);
        *new = "foo".to_string();

        assert_eq!(foo.data(), "foo");
        assert_eq!(ROOT.data(), "Hello");

        drop(foo);
    });

    with_explicit_merge(|| {
        let mut foo = ROOT.clone();

        let new = BiasedRc::make_mut(&mut foo);
        *new = "foo".to_string();

        drop(foo);
    });

    with_explicit_merge(|| {
        register_thread();
        let mut foo = ROOT.clone();

        let new = BiasedRc::make_mut(&mut foo);
        *new = "foo".to_string();

        drop(foo);
    })
    .join()
    .unwrap();

    with_explicit_merge(|| {
        register_thread();
        let foo = ROOT.clone();
        drop(foo);
    })
    .join()
    .unwrap();

    let foo = ROOT.clone();
    drop(foo);

    // dbg!(ROOT.get_box().rcword.biased_counter.get());
    let meta = ROOT.get_box().rcword.shared.load(Ordering::Relaxed);
    dbg!(meta.get_merged());
    dbg!(meta.get_queued());
    dbg!(meta.get_counter());
}

#[test]
fn make_mut_test() {
    let foo = BiasedRc::new("foo".to_string());
    let mut copy = foo.clone();
    let new_thing = BiasedRc::make_mut(&mut copy);

    *new_thing = "hello".to_string();
}

// #[test]
// fn test_merging() {
//     let word = BiasedRc::new(10);
//     drop(word);
// }



/// How a [`PackedRc`] tears its elements down when the last reference goes.
///
/// The default drops each element where it sits. An owner that cannot afford to
/// drop recursively - a deeply nested value that would blow the stack - supplies
/// its own handler and moves the elements into a work list instead. im-lists
/// takes the same approach with its list drop handler.
pub trait PackedDropHandler<T> {
    /// Must drop or move out every element exactly once. The block is released
    /// by the caller afterwards.
    ///
    /// # Safety
    ///
    /// The elements are live and owned on entry, and must not be read after.
    unsafe fn drop_elements(elements: *mut T, len: usize);
}

/// Drops each element in place.
pub struct DefaultPackedDrop;

impl<T> PackedDropHandler<T> for DefaultPackedDrop {
    #[inline]
    unsafe fn drop_elements(elements: *mut T, len: usize) {
        for i in 0..len {
            unsafe { core::ptr::drop_in_place(elements.add(i)) };
        }
    }
}

/// A thin, reference counted allocation holding a header and an inline slice.
///
/// `BiasedRc<[T]>` is a fat pointer, so it cannot be stored anywhere a value has
/// to stay pointer sized. This keeps the element count in the allocation instead
/// and hands back a one word handle. The elements sit directly after the header,
/// so reading one is a single load off the pointer rather than a hop through a
/// separately allocated buffer, and a value plus its elements is one allocation.
///
/// `H` is free space in the header for whatever the owner wants to keep beside
/// the count - a type descriptor, for instance.
pub struct PackedRc<H: 'static, T: 'static, D: PackedDropHandler<T> + 'static = DefaultPackedDrop> {
    ptr: NonNull<PackedHeader<H>>,
    phantom: PhantomData<(H, T, D)>,
}

/// The header of a [`PackedRc`] allocation.
///
/// `rcword` must stay first: the biased protocol addresses it at offset zero,
/// and code that inlines the counter decrement (the JIT does) relies on it.
#[repr(C)]
pub struct PackedHeader<H> {
    pub rcword: RcWord,
    pub len: u32,
    pub header: H,
}

impl<H: 'static, T: 'static, D: PackedDropHandler<T> + 'static> PackedRc<H, T, D> {
    /// Where the header payload sits, relative to the pointer.
    ///
    /// Public because code that addresses the allocation directly - the JIT
    /// reads the header and the elements without a call - needs both offsets.
    #[inline]
    pub const fn header_offset() -> usize {
        core::mem::offset_of!(PackedHeader<H>, header)
    }

    /// Where the elements start, relative to the header pointer.
    #[inline]
    pub const fn data_offset() -> usize {
        let align = core::mem::align_of::<T>();
        let size = core::mem::size_of::<PackedHeader<H>>();

        // Round the header up to the element alignment
        (size + align - 1) & !(align - 1)
    }

    fn layout(len: usize) -> Layout {
        let size = Self::data_offset() + len * core::mem::size_of::<T>();
        let align = if core::mem::align_of::<PackedHeader<H>>() > core::mem::align_of::<T>() {
            core::mem::align_of::<PackedHeader<H>>()
        } else {
            core::mem::align_of::<T>()
        };

        Layout::from_size_align(size, align).expect("packed allocation layout overflowed")
    }

    /// Allocates a header and the elements the iterator yields, in one block.
    pub fn new(header: H, values: impl ExactSizeIterator<Item = T>) -> Self {
        let len = values.len();
        assert!(len <= u32::MAX as usize, "packed allocation is too long");

        let layout = Self::layout(len);

        // Safety: the layout is non zero - the header alone occupies space - and
        // the allocation is fully initialized below before anything reads it.
        unsafe {
            let raw = std::alloc::alloc(layout) as *mut PackedHeader<H>;
            let Some(ptr) = NonNull::new(raw) else {
                std::alloc::handle_alloc_error(layout)
            };

            ptr.as_ptr().write(PackedHeader {
                rcword: RcWord::new(),
                len: len as u32,
                header,
            });

            let data = (ptr.as_ptr() as *mut u8).add(Self::data_offset()) as *mut T;

            let mut written = 0;
            for value in values {
                data.add(written).write(value);
                written += 1;
            }

            debug_assert_eq!(written, len, "iterator yielded a different count than len");

            Self {
                ptr,
                phantom: PhantomData,
            }
        }
    }

    #[inline]
    pub fn meta(&self) -> &RcWord {
        // Safety: the pointer is live for as long as this handle is
        unsafe { &self.ptr.as_ref().rcword }
    }

    #[inline]
    pub fn header(&self) -> &H {
        unsafe { &self.ptr.as_ref().header }
    }

    #[inline]
    pub fn len(&self) -> usize {
        unsafe { self.ptr.as_ref().len as usize }
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    #[inline]
    pub fn as_ptr(&self) -> *const T {
        unsafe { (self.ptr.as_ptr() as *const u8).add(Self::data_offset()) as *const T }
    }

    #[inline]
    pub fn as_slice(&self) -> &[T] {
        unsafe { core::slice::from_raw_parts(self.as_ptr(), self.len()) }
    }

    /// The raw pointer the handle carries. Thin, so it fits in a pointer sized slot.
    #[inline]
    pub fn as_raw(&self) -> *const PackedHeader<H> {
        self.ptr.as_ptr()
    }

    /// The elements, read back from a raw header pointer.
    ///
    /// # Safety
    ///
    /// `ptr` must have come from [`Self::as_raw`] on a handle that is still live.
    #[inline]
    pub unsafe fn slice_from_raw<'a>(ptr: *const PackedHeader<H>) -> &'a [T] {
        let len = unsafe { (*ptr).len as usize };
        let data = unsafe { (ptr as *const u8).add(Self::data_offset()) as *const T };

        unsafe { core::slice::from_raw_parts(data, len) }
    }

    #[inline]
    pub fn ptr_eq(&self, other: &Self) -> bool {
        core::ptr::eq(self.ptr.as_ptr(), other.ptr.as_ptr())
    }

    /// A mutable view of the elements, if this is the only live reference.
    ///
    /// Mirrors `Gc::get_mut`: callers that need to modify a shared value clone
    /// it instead. The uniqueness check is destructive - winning it consumes the
    /// shared count - so a `Some` result means this handle now owns the block
    /// outright.
    #[inline]
    pub fn get_mut(&mut self) -> Option<&mut [T]> {
        if self.meta().has_unique_ref() {
            let len = self.len();
            let data = unsafe { (self.ptr.as_ptr() as *mut u8).add(Self::data_offset()) as *mut T };

            Some(unsafe { core::slice::from_raw_parts_mut(data, len) })
        } else {
            None
        }
    }

    /// A fresh allocation with the same header and elements.
    ///
    /// This is a deep copy of the block itself - the elements are cloned - as
    /// opposed to `Clone`, which shares it.
    pub fn deep_clone(&self) -> Self
    where
        H: Clone,
        T: Clone,
    {
        Self::new(self.header().clone(), self.as_slice().iter().cloned())
    }

    /// Applies the outcome of a decrement: nothing, hand off to the owning
    /// thread, or tear down.
    #[inline]
    fn apply(&mut self, action: DecrementAction) {
        match action {
            DecrementAction::DoNothing => {}
            DecrementAction::Queue => {
                let key = self.meta().owner();
                let handoff = Self {
                    ptr: self.ptr,
                    phantom: PhantomData,
                };
                QueueHandle::enqueue_merge(key, handoff);
            }
            DecrementAction::Deallocate => unsafe { self.drop_contents_and_dealloc() },
        }
    }

    /// Finishes a decrement whose counter was already lowered elsewhere - the
    /// JIT lowers it inline and calls back in here.
    pub fn fast_decrement_post_ref_count_dec(&mut self) {
        let action = self.meta().fast_decrement_drop_impl();
        self.apply(action);
    }

    pub fn raw_slow_increment(&mut self) {
        self.meta().slow_increment();
    }

    pub fn raw_slow_decrement(&mut self) {
        let action = self.meta().slow_decrement();
        self.apply(action);
    }

    /// Drops the header and every element, then releases the block.
    ///
    /// # Safety
    ///
    /// The caller must hold the last reference.
    unsafe fn drop_contents_and_dealloc(&mut self) {
        let len = self.len();

        let data = (self.ptr.as_ptr() as *mut u8).add(Self::data_offset()) as *mut T;

        D::drop_elements(data, len);

        core::ptr::drop_in_place(core::ptr::addr_of_mut!((*self.ptr.as_ptr()).header));

        std::alloc::dealloc(self.ptr.as_ptr() as *mut u8, Self::layout(len));
    }
}

impl<H: 'static, T: 'static, D: PackedDropHandler<T> + 'static> Clone for PackedRc<H, T, D> {
    #[inline]
    fn clone(&self) -> Self {
        self.meta().increment();

        Self {
            ptr: self.ptr,
            phantom: PhantomData,
        }
    }
}

impl<H: 'static, T: 'static, D: PackedDropHandler<T> + 'static> Drop for PackedRc<H, T, D> {
    #[inline]
    fn drop(&mut self) {
        let action = self.meta().decrement();
        self.apply(action);
    }
}

impl<H: 'static, T: 'static, D: PackedDropHandler<T> + 'static> BiasedMerge for PackedRc<H, T, D> {
    fn merge(self) {
        let mut this = ManuallyDrop::new(self);

        match this.meta().fast_decrement_drop_impl() {
            DecrementAction::Deallocate => unsafe { this.drop_contents_and_dealloc() },
            _ => {}
        }
    }

    fn meta_outer(&self) -> &RcWord {
        self.meta()
    }

    unsafe fn drop_contents_and_maybe_box_outer(&mut self) {
        unsafe { self.drop_contents_and_dealloc() }
    }
}

unsafe impl<H: Send + Sync + 'static, T: Send + Sync + 'static, D: PackedDropHandler<T> + 'static> Send for PackedRc<H, T, D> {}
unsafe impl<H: Send + Sync + 'static, T: Send + Sync + 'static, D: PackedDropHandler<T> + 'static> Sync for PackedRc<H, T, D> {}

#[cfg(test)]
mod packed_rc_tests {
    use super::*;
    use std::cell::Cell;
    use std::rc::Rc;

    #[derive(Debug, PartialEq, Clone)]
    struct Desc(u32);

    fn packed(desc: u32, values: &[u64]) -> PackedRc<Desc, u64> {
        PackedRc::new(Desc(desc), values.iter().copied())
    }

    #[test]
    fn handle_is_pointer_sized() {
        // The whole point: a fat BiasedRc<[T]> would not fit where a value has
        // to stay pointer sized.
        assert_eq!(
            core::mem::size_of::<PackedRc<Desc, u64>>(),
            core::mem::size_of::<*const u8>()
        );
        assert_eq!(core::mem::size_of::<BiasedRc<[u64]>>(), 16);
    }

    #[test]
    fn rcword_sits_at_offset_zero() {
        // Code that inlines the counter decrement addresses the word directly
        assert_eq!(core::mem::offset_of!(PackedHeader<Desc>, rcword), 0);
    }

    #[test]
    fn header_and_elements_round_trip() {
        let p = packed(7, &[10, 20, 30, 40]);
        assert_eq!(p.header(), &Desc(7));
        assert_eq!(p.len(), 4);
        assert_eq!(p.as_slice(), &[10, 20, 30, 40]);
    }

    #[test]
    fn empty_is_allowed() {
        let p = packed(1, &[]);
        assert_eq!(p.len(), 0);
        assert!(p.is_empty());
        assert_eq!(p.as_slice(), &[] as &[u64]);
    }

    #[test]
    fn elements_are_one_load_after_the_header() {
        let p = packed(3, &[5, 6]);
        let base = p.as_raw() as usize;
        let first = p.as_ptr() as usize;
        assert_eq!(first - base, PackedRc::<Desc, u64>::data_offset());
        // and the elements are contiguous from there
        assert_eq!(unsafe { *p.as_ptr().add(1) }, 6);
    }

    #[test]
    fn clone_shares_the_allocation() {
        let a = packed(9, &[1, 2, 3]);
        let b = a.clone();
        assert!(a.ptr_eq(&b));
        assert_eq!(b.as_slice(), &[1, 2, 3]);
        drop(a);
        // still readable through the surviving handle
        assert_eq!(b.as_slice(), &[1, 2, 3]);
        assert_eq!(b.header(), &Desc(9));
    }

    // Counts its own drops so we can prove every element is dropped exactly once
    #[derive(Clone)]
    struct Tracked(Rc<Cell<usize>>);
    impl Drop for Tracked {
        fn drop(&mut self) {
            self.0.set(self.0.get() + 1);
        }
    }

    #[test]
    fn every_element_is_dropped_exactly_once() {
        let counter = Rc::new(Cell::new(0));

        {
            let values: Vec<Tracked> = (0..5).map(|_| Tracked(counter.clone())).collect();
            let p: PackedRc<Desc, Tracked> = PackedRc::new(Desc(0), values.into_iter());
            assert_eq!(p.len(), 5);
            assert_eq!(counter.get(), 0, "nothing dropped while live");
        }

        assert_eq!(counter.get(), 5, "each element dropped once");
    }

    #[test]
    fn clones_do_not_drop_early() {
        let counter = Rc::new(Cell::new(0));

        let values: Vec<Tracked> = (0..3).map(|_| Tracked(counter.clone())).collect();
        let a: PackedRc<Desc, Tracked> = PackedRc::new(Desc(0), values.into_iter());
        let b = a.clone();

        drop(a);
        assert_eq!(counter.get(), 0, "still held by the clone");

        drop(b);
        assert_eq!(counter.get(), 3);
    }

    // The header owns its contents too
    struct TrackedHeader(Rc<Cell<usize>>);
    impl Drop for TrackedHeader {
        fn drop(&mut self) {
            self.0.set(self.0.get() + 100);
        }
    }

    #[test]
    fn the_header_is_dropped_too() {
        let counter = Rc::new(Cell::new(0));
        {
            let _p: PackedRc<TrackedHeader, u64> =
                PackedRc::new(TrackedHeader(counter.clone()), [1u64, 2].into_iter());
        }
        assert_eq!(counter.get(), 100);
    }

    #[test]
    fn large_element_counts_are_contiguous() {
        let values: Vec<u64> = (0..1000).collect();
        let p: PackedRc<Desc, u64> = PackedRc::new(Desc(42), values.iter().copied());
        assert_eq!(p.len(), 1000);
        assert_eq!(p.as_slice(), values.as_slice());
        assert_eq!(p.header(), &Desc(42));
    }

    #[test]
    fn dropped_on_another_thread_is_handed_back() {
        // A handle released by a thread that does not own it takes the queue
        // path rather than freeing, which is what enqueue_merge exists for.
        let p = packed(11, &[1, 2, 3]);
        let kept = p.clone();

        std::thread::spawn(move || {
            assert_eq!(p.as_slice(), &[1, 2, 3]);
            drop(p);
        })
        .join()
        .unwrap();

        // The owning thread's copy is untouched
        assert_eq!(kept.as_slice(), &[1, 2, 3]);
        assert_eq!(kept.header(), &Desc(11));
    }

    // A handler that moves the elements out instead of dropping them in place,
    // which is what the iterative drop path needs.
    struct Collect;
    thread_local! {
        static COLLECTED: Cell<usize> = const { Cell::new(0) };
    }
    impl PackedDropHandler<Tracked> for Collect {
        unsafe fn drop_elements(elements: *mut Tracked, len: usize) {
            let mut taken = Vec::with_capacity(len);
            for i in 0..len {
                taken.push(unsafe { core::ptr::read(elements.add(i)) });
            }
            COLLECTED.with(|c| c.set(c.get() + taken.len()));
            // taken drops here, exactly once each
        }
    }

    #[test]
    fn a_custom_drop_handler_receives_the_elements() {
        let counter = Rc::new(Cell::new(0));
        COLLECTED.with(|c| c.set(0));

        {
            let values: Vec<Tracked> = (0..4).map(|_| Tracked(counter.clone())).collect();
            let _p: PackedRc<Desc, Tracked, Collect> = PackedRc::new(Desc(1), values.into_iter());
        }

        assert_eq!(COLLECTED.with(|c| c.get()), 4, "handler saw every element");
        assert_eq!(counter.get(), 4, "and each was dropped exactly once");
    }

    #[test]
    fn get_mut_when_unique_and_not_when_shared() {
        let mut a = packed(1, &[1, 2, 3]);

        {
            let slice = a.get_mut().expect("unique handle should be mutable");
            slice[1] = 99;
        }
        assert_eq!(a.as_slice(), &[1, 99, 3]);

        let b = a.clone();
        assert!(a.get_mut().is_none(), "shared handle must not hand out &mut");
        drop(b);
    }

    #[test]
    fn deep_clone_is_a_separate_block() {
        let a = packed(5, &[1, 2, 3]);
        let mut b = a.deep_clone();

        assert!(!a.ptr_eq(&b), "deep clone allocates its own block");
        assert_eq!(b.as_slice(), a.as_slice());
        assert_eq!(b.header(), a.header());

        b.get_mut().unwrap()[0] = 42;
        assert_eq!(b.as_slice(), &[42, 2, 3]);
        assert_eq!(a.as_slice(), &[1, 2, 3], "original is untouched");
    }

    #[test]
    fn deep_clone_drops_both_copies_exactly_once() {
        let counter = Rc::new(Cell::new(0));
        {
            let values: Vec<Tracked> = (0..3).map(|_| Tracked(counter.clone())).collect();
            let a: PackedRc<Desc, Tracked> = PackedRc::new(Desc(0), values.into_iter());
            let _b = a.deep_clone();
            assert_eq!(counter.get(), 0);
        }
        assert_eq!(counter.get(), 6, "three elements in each of two blocks");
    }
}

#[cfg(test)]
mod rcword_layout {
    use super::*;

    #[test]
    fn rcword_is_one_word() {

        assert_eq!(core::mem::size_of::<ThreadId>(), 4);
        assert_eq!(core::mem::size_of::<RcWord>(), 8, "BRC keeps RcWord to one word");
        assert_eq!(core::mem::size_of::<Option<ThreadId>>(), 4, "niche keeps it loadable as one u32");
        assert_eq!(BiasedRc::<[u8; 16]>::data_offset(), 8);


    }

    #[test]
    fn biased_packing_roundtrips() {
        let word = RcWord::new();
        let me = ThreadId::current_thread();
        assert_eq!(word.owner(), Some(me));
        assert_eq!(word.biased_count(), 1);

        for _ in 0..1000 {
            word.fast_increment();
        }
        // the count must not have disturbed the owner id
        assert_eq!(word.biased_count(), 1001);
        assert_eq!(word.owner(), Some(me));

        word.clear_owner();
        assert_eq!(word.owner(), None);
        assert_eq!(word.biased_count(), 1001, "unbias must not touch the count");
    }

    #[test]
    fn counter_saturation_spills_to_shared_instead_of_panicking() {
        let word = RcWord::new();
        let me = ThreadId::current_thread();

        // Fill the biased field, then keep going well past it.
        for _ in 0..MAX_BIASED_COUNT + 5_000 {
            word.fast_increment();
        }

        assert_eq!(word.biased_count(), MAX_BIASED_COUNT, "field saturated");
        assert_eq!(word.owner(), Some(me), "owner intact");
        assert!(
            word.shared.load(Ordering::Relaxed).get_counter() >= 5_000,
            "the overflow went to the shared counter"
        );
    }

    #[test]
    fn count_underflow_does_not_disturb_the_owner() {
        let word = RcWord::new();
        let me = ThreadId::current_thread();
        // Drive the count to zero and one past it, the way a stray decrement would.
        assert!(matches!(word.fast_decrement(), DecrementAction::Deallocate));
        word.set_biased_count(word.biased_count().wrapping_sub(1));
        assert!(
            word.biased_count() <= MAX_BIASED_COUNT,
            "count escaped its field"
        );
        let _ = me;
    }

    #[test]
    fn thread_ids_are_unique_among_live_threads() {
        use std::collections::HashSet;
        use std::sync::{Arc, Barrier, Mutex};

        let seen = Arc::new(Mutex::new(HashSet::new()));
        let barrier = Arc::new(Barrier::new(16));
        let handles: Vec<_> = (0..16)
            .map(|_| {
                let seen = seen.clone();
                let barrier = barrier.clone();
                std::thread::spawn(move || {
                    let id = ThreadId::current_thread();
                    assert!(seen.lock().unwrap().insert(id.0), "duplicate live id");
                    barrier.wait(); // hold every thread alive at once
                })
            })
            .collect();
        for h in handles {
            h.join().unwrap();
        }
        assert_eq!(seen.lock().unwrap().len(), 16);
    }

    #[test]
    fn ids_are_recycled_across_sequential_threads() {
        // Without recycling this would consume 4000 ids; with it, a handful.
        let before = NEXT_TID.load(Ordering::Relaxed);
        for _ in 0..4000 {
            std::thread::spawn(|| {
                let _ = ThreadId::current_thread();
            })
            .join()
            .unwrap();
        }
        let consumed = NEXT_TID.load(Ordering::Relaxed) - before;
        assert!(consumed < 64, "ids not being recycled: consumed {consumed}");
    }
}

#[cfg(test)]
mod unsized_roundtrip {
    use super::*;

    #[test]
    fn slice_rc_roundtrips() {
        println!("RcWord size={} align={}", size_of::<RcWord>(), align_of::<RcWord>());
        println!("RcBox<()> size={} align={}", size_of::<RcBox<()>>(), align_of::<RcBox<()>>());
        println!("data_offset u32={}", BiasedRc::<u32>::data_offset());
        println!("data_offset u64={}", BiasedRc::<u64>::data_offset());

        // DenseInstruction is 4 bytes / align 4; mirror that.
        let values: Vec<u32> = (0..21u32).collect();
        let rc: BiasedRc<[u32]> = values.clone().into();
        assert_eq!(rc.len(), 21, "length survived the round trip");
        assert_eq!(&rc[..], &values[..], "contents survived the round trip");

        let c1 = rc.clone();
        let c2 = rc.clone();
        assert_eq!(&c1[..], &values[..]);
        drop(c1);
        assert_eq!(&c2[..], &values[..], "still alive after one drop");
        drop(c2);
        assert_eq!(&rc[..], &values[..], "still alive after both drops");
    }
}

#[cfg(test)]
mod align1_slice {
    use super::*;

    // Mirrors DenseInstruction: 4 bytes, align 1.
    #[derive(Clone, Copy, PartialEq, Debug)]
    #[repr(C)]
    struct DI {
        op: u8,
        payload: [u8; 3],
    }

    #[test]
    fn align1_slice_roundtrips() {
        assert_eq!(size_of::<DI>(), 4);
        assert_eq!(align_of::<DI>(), 1);
        println!(
            "RcWord={} data_offset(DI)={} data_offset(u32)={}",
            size_of::<RcWord>(),
            BiasedRc::<DI>::data_offset(),
            BiasedRc::<u32>::data_offset()
        );

        for len in [0usize, 1, 2, 3, 5, 20, 21, 22, 23, 100, 257] {
            let values: Vec<DI> = (0..len)
                .map(|i| DI {
                    op: i as u8,
                    payload: [(i >> 8) as u8, 1, 2],
                })
                .collect();
            let rc: BiasedRc<[DI]> = values.clone().into();
            assert_eq!(rc.len(), len, "len wrong for {len}");
            assert_eq!(&rc[..], &values[..], "contents wrong for {len}");

            // and through the other construction path
            let rc2: BiasedRc<[DI]> = BiasedRc::from(&values[..]);
            assert_eq!(rc2.len(), len, "from_slice len wrong for {len}");
            assert_eq!(&rc2[..], &values[..], "from_slice contents wrong for {len}");

            // clone / drop cycle
            let c = rc.clone();
            drop(rc);
            assert_eq!(&c[..], &values[..], "survived a drop for {len}");
        }
    }
}

#[cfg(test)]
mod biased_word_layout {
    /// The owner id and the count share one 32-bit half-word, so widening
    /// either silently narrows the other. A 14-bit count is already reachable -
    /// `browse` overflowed it once - so pin both fields here.
    #[test]
    fn id_and_count_do_not_overlap() {
        assert_eq!(super::BIASED_COUNTER_BITS, 14, "counter width must not shrink");
        assert_eq!(super::MAX_BIASED_COUNT, 16383);
        assert_eq!(super::TID_MASK, (1 << super::COUNTER_SHIFT) - 1, "id fills everything below the count");
        assert_eq!(core::mem::size_of::<super::RcWord>(), 8);
    }
}

#[cfg(test)]
mod counter_overflow {
    use super::*;

    /// BRC's invariant I1: the true count is `biased + shared`. Once the biased
    /// field saturates, increments spill into the shared half-word - so the
    /// decrements that pair with them have to be able to come back out of it.
    #[test]
    fn owner_thread_can_drop_everything_it_cloned_past_saturation() {
        let word = RcWord::new();
        let n = MAX_BIASED_COUNT + 5_000;

        for _ in 0..n {
            word.fast_increment();
        }
        assert_eq!(word.biased_count(), MAX_BIASED_COUNT, "biased field saturated");
        // RcWord::new() starts at 1, so only MAX-1 of the increments fit in the
        // biased field and the remainder spill.
        assert_eq!(
            word.shared.load(Ordering::Relaxed).get_counter(),
            5_001,
            "the rest went to shared"
        );

        // RcWord::new() starts at 1, so `n` increments means n+1 references.
        // Drop them all; exactly one decrement may say Deallocate, and it must
        // be the last one.
        let mut deallocs = 0;
        for i in 0..=n {
            match word.decrement() {
                DecrementAction::Deallocate => {
                    deallocs += 1;
                    assert_eq!(i, n, "freed with {} references still live", n - i);
                }
                _ => {}
            }
        }
        assert_eq!(deallocs, 1, "exactly one deallocate for n+1 references");
    }

    /// The real scenario: one value cloned hard from many threads at once. The
    /// owning thread takes the biased path and saturates it; every other thread
    /// goes through the shared half-word.
    #[test]
    fn many_threads_cloning_one_value_do_not_panic() {
        let rc: BiasedRc<u64> = BiasedRc::new(99);
        let threads = 8;
        let per_thread = 20_000;

        std::thread::scope(|s| {
            for _ in 0..threads {
                let rc = rc.clone();
                s.spawn(move || {
                    let mut held = Vec::with_capacity(per_thread);
                    for _ in 0..per_thread {
                        held.push(rc.clone());
                    }
                    for c in &held {
                        assert_eq!(**c, 99);
                    }
                    drop(held);
                });
            }
        });

        assert_eq!(*rc, 99, "value survived the traffic");
    }

    /// The owning thread alone, holding far more live clones than the biased
    /// field can count.
    #[test]
    fn one_thread_holding_more_clones_than_the_biased_field_can_count() {
        let rc: BiasedRc<u64> = BiasedRc::new(7);
        let n = (MAX_BIASED_COUNT as usize) + 20_000;

        let held: Vec<_> = (0..n).map(|_| rc.clone()).collect();
        assert_eq!(**held.last().unwrap(), 7);
        drop(held);
        assert_eq!(*rc, 7, "still alive with one reference left");
    }

    /// Seeds the shared counter directly rather than performing 2^29 CAS ops.
    fn seed_shared(word: &RcWord, value: i32) {
        loop {
            let old = word.shared.load(Ordering::Relaxed);
            let mut new = old;
            new.set_counter(value);
            if word
                .shared
                .compare_exchange(old, new, Ordering::AcqRel, Ordering::Relaxed)
                .is_ok()
            {
                return;
            }
        }
    }

    /// At the ceiling the count pins instead of panicking or wrapping.
    #[test]
    fn shared_counter_saturates_rather_than_overflowing() {
        let word = RcWord::new();
        seed_shared(&word, MAX_SHARED_COUNT - 1);

        word.slow_increment();
        assert_eq!(
            word.shared.load(Ordering::Relaxed).get_counter(),
            MAX_SHARED_COUNT,
            "reaches the ceiling"
        );

        for _ in 0..1_000 {
            word.slow_increment();
        }
        assert_eq!(
            word.shared.load(Ordering::Relaxed).get_counter(),
            MAX_SHARED_COUNT,
            "and stays there instead of wrapping"
        );
    }

    /// The safety property that makes leaking the right answer: a saturated
    /// count must never fall back to zero, or the object is freed while it is
    /// still referenced.
    #[test]
    fn a_saturated_count_never_deallocates() {
        let word = RcWord::new();
        seed_shared(&word, MAX_SHARED_COUNT);
        word.shared.set_flag_merged(true);

        for _ in 0..100_000 {
            assert!(
                !matches!(word.slow_decrement(), DecrementAction::Deallocate),
                "a saturated object must never be freed"
            );
        }
        assert_eq!(
            word.shared.load(Ordering::Relaxed).get_counter(),
            MAX_SHARED_COUNT,
            "count stays pinned"
        );
    }

    /// The mirror case: BRC lets the shared count go negative when non-owning
    /// threads drop references the owner created, so the floor needs the same
    /// treatment as the ceiling.
    #[test]
    fn shared_counter_saturates_at_the_floor_too() {
        let word = RcWord::new();
        seed_shared(&word, MIN_SHARED_COUNT + 1);

        for _ in 0..1_000 {
            word.slow_decrement();
        }
        assert_eq!(
            word.shared.load(Ordering::Relaxed).get_counter(),
            MIN_SHARED_COUNT,
            "pins at the floor rather than wrapping positive"
        );
    }

    /// The third way the shared counter grows: ExplicitMerge folds the whole
    /// biased count in at once, so it can jump the ceiling rather than walk up
    /// to it. `set_value`'s clamp is what catches that one.
    #[test]
    fn a_merge_that_jumps_the_ceiling_clamps() {
        let mut p = Packed(0);
        p.set_counter(MAX_SHARED_COUNT - 10);
        p.update_counter(|x| x + 5_000);
        assert_eq!(
            p.get_counter(),
            MAX_SHARED_COUNT,
            "a merge past the ceiling pins instead of wrapping negative"
        );

        let mut q = Packed(0);
        q.set_counter(MIN_SHARED_COUNT + 10);
        q.update_counter(|x| x - 5_000);
        assert_eq!(q.get_counter(), MIN_SHARED_COUNT, "and the same at the floor");
    }

    /// Saturation must not disturb ordinary traffic just below the ceiling.
    #[test]
    fn counts_below_the_ceiling_are_exact() {
        let word = RcWord::new();
        seed_shared(&word, MAX_SHARED_COUNT - 10);

        for _ in 0..5 {
            word.slow_increment();
        }
        assert_eq!(
            word.shared.load(Ordering::Relaxed).get_counter(),
            MAX_SHARED_COUNT - 5
        );
        for _ in 0..5 {
            word.slow_decrement();
        }
        assert_eq!(
            word.shared.load(Ordering::Relaxed).get_counter(),
            MAX_SHARED_COUNT - 10,
            "exact both ways while there is headroom"
        );
    }
}
