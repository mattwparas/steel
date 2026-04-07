use core::mem::offset_of;
use std::cell::UnsafeCell;
use std::sync::atomic::{AtomicU32, Ordering};

// TODO: @Matt
// This needs to have a proper mutex backing.
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

    pub fn write(&self) -> SpinGuard<'_, T> {
        self.lock()
    }

    pub fn lock(&self) -> SpinGuard<'_, T> {
        while self
            .locked
            .compare_exchange_weak(0, 1, Ordering::Acquire, Ordering::Relaxed)
            .is_err()
        {
            while self.locked.load(Ordering::Relaxed) != 0 {
                std::hint::spin_loop();
            }
        }
        SpinGuard { lock: self }
    }

    pub fn try_lock(&self) -> Option<SpinGuard<'_, T>> {
        self.locked
            .compare_exchange(0, 1, Ordering::Acquire, Ordering::Relaxed)
            .ok()
            .map(|_| SpinGuard { lock: self })
    }
}

pub struct SpinGuard<'a, T> {
    lock: &'a SpinLock<T>,
}

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
