use core::mem::offset_of;
use std::cell::UnsafeCell;
use std::marker::PhantomData;
use std::sync::atomic::{AtomicU32, Ordering};

// How many failed acquires before we let the scheduler run whoever is holding the
// lock instead of burning the core. Spinning only pays while the holder is running
// somewhere else - if its been descheduled, or parked at a gc safepoint, spinning
// gets us nowhere.
const SPINS_BEFORE_YIELD: u32 = 128;

// TODO: @Matt
// This needs to have a proper mutex backing.
//
// locked has to stay first: the jit emits its own inline acquire and release
// (emit_spinlock_inline) which does an atomic_cas at the base of this struct.
// lock_offset and data_offset are how it finds the two halves.
#[repr(C)]
#[derive(Debug)]
pub struct SpinLock<T> {
    locked: AtomicU32,
    data: UnsafeCell<T>,
}

unsafe impl<T: Send> Send for SpinLock<T> {}
unsafe impl<T: Send> Sync for SpinLock<T> {}

impl<T> SpinLock<T> {
    pub const fn new(val: T) -> Self {
        Self {
            locked: AtomicU32::new(0),
            data: UnsafeCell::new(val),
        }
    }

    pub const fn data_offset() -> usize {
        offset_of!(SpinLock<T>, data)
    }

    pub const fn lock_offset() -> usize {
        offset_of!(SpinLock<T>, locked)
    }

    pub fn write(&self) -> SpinGuard<'_, T> {
        self.lock()
    }

    // Reads the payload without taking the lock, so the caller has to know
    // nothing else can touch it - no other thread holding this, no gc pass
    // reaching the same slot
    pub unsafe fn get_value(&self) -> &T {
        unsafe { &*self.data.get() }
    }

    // Not reentrant - taking this twice on one thread spins forever with nothing
    // to say so, which matters if a guard is still alive across a call that might
    // reach the same lock
    pub fn lock(&self) -> SpinGuard<'_, T> {
        let mut spins: u32 = 0;

        while self
            .locked
            .compare_exchange_weak(0, 1, Ordering::Acquire, Ordering::Relaxed)
            .is_err()
        {
            while self.locked.load(Ordering::Relaxed) != 0 {
                if spins < SPINS_BEFORE_YIELD {
                    spins += 1;
                    std::hint::spin_loop();
                } else {
                    std::thread::yield_now();
                }
            }
        }

        SpinGuard::new(self)
    }

    pub fn try_lock(&self) -> Option<SpinGuard<'_, T>> {
        self.locked
            .compare_exchange(0, 1, Ordering::Acquire, Ordering::Relaxed)
            .ok()
            .map(|_| SpinGuard::new(self))
    }
}

#[must_use = "the lock is released as soon as the guard drops"]
pub struct SpinGuard<'a, T> {
    lock: &'a SpinLock<T>,
    // Opts out of the auto derived Send/Sync so the impls below are the only ones
    // that apply. Deriving from &SpinLock<T> makes the guard Sync whenever T is
    // Send, and since Deref hands out a &T that would let two threads share a
    // payload that isn't Sync at all.
    _marker: PhantomData<*const ()>,
}

impl<'a, T> SpinGuard<'a, T> {
    fn new(lock: &'a SpinLock<T>) -> Self {
        Self {
            lock,
            _marker: PhantomData,
        }
    }
}

// Sharing the guard shares the payload, so this wants Sync and not Send
unsafe impl<T: Sync> Sync for SpinGuard<'_, T> {}

// Nothing is tied to the acquiring thread - unlocking is a plain store - so the
// guard can move between threads as long as the payload can
unsafe impl<T: Send> Send for SpinGuard<'_, T> {}

impl<T> std::ops::Deref for SpinGuard<'_, T> {
    type Target = T;
    fn deref(&self) -> &T {
        unsafe { &*self.lock.data.get() }
    }
}

impl<T> std::ops::DerefMut for SpinGuard<'_, T> {
    fn deref_mut(&mut self) -> &mut T {
        unsafe { &mut *self.lock.data.get() }
    }
}

impl<T> Drop for SpinGuard<'_, T> {
    fn drop(&mut self) {
        self.lock.locked.store(0, Ordering::Release);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_send<T: Send>() {}
    fn assert_sync<T: Sync>() {}

    #[test]
    fn guard_auto_traits_follow_the_payload() {
        assert_sync::<SpinGuard<'static, i32>>();
        assert_send::<SpinGuard<'static, i32>>();

        // The other direction - SpinGuard<Cell<i32>> must not be Sync, since Cell
        // is Send but not Sync and &SpinGuard derefs to &Cell - would need a
        // compile fail harness, which we don't have here.
    }

    #[test]
    fn lock_excludes() {
        let lock = SpinLock::new(0usize);

        {
            let mut guard = lock.lock();
            *guard += 1;
            assert!(lock.try_lock().is_none(), "try_lock took a held lock");
        }

        assert_eq!(*lock.lock(), 1);
    }

    #[test]
    fn contended_across_threads() {
        use std::sync::Arc;

        let lock = Arc::new(SpinLock::new(0usize));
        let threads: Vec<_> = (0..8)
            .map(|_| {
                let lock = Arc::clone(&lock);
                std::thread::spawn(move || {
                    for _ in 0..1000 {
                        *lock.lock() += 1;
                    }
                })
            })
            .collect();

        for t in threads {
            t.join().unwrap();
        }

        assert_eq!(*lock.lock(), 8000);
    }
}
