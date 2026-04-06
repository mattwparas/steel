use std::alloc::{self, Layout};
use std::fmt;
use std::mem;
use std::ops::Deref;
use std::ptr::{self, drop_in_place, NonNull};
use std::sync::atomic::{self, AtomicUsize, Ordering};

#[repr(C)]
struct ArcInner<T: ?Sized> {
    strong: AtomicUsize,
    weak: AtomicUsize,
    data: T,
}

#[repr(C)]
pub struct Arc<T: ?Sized> {
    ptr: NonNull<ArcInner<T>>,
}

unsafe impl<T: ?Sized + Send + Sync> Send for Arc<T> {}
unsafe impl<T: ?Sized + Send + Sync> Sync for Arc<T> {}

#[repr(C)]
pub struct Weak<T: ?Sized> {
    ptr: NonNull<ArcInner<T>>,
}

unsafe impl<T: ?Sized + Send + Sync> Send for Weak<T> {}
unsafe impl<T: ?Sized + Send + Sync> Sync for Weak<T> {}

impl<T> Arc<T> {
    pub fn new(data: T) -> Self {
        let layout = Layout::new::<ArcInner<T>>();
        let ptr = unsafe { alloc::alloc(layout) as *mut ArcInner<T> };
        if ptr.is_null() {
            alloc::handle_alloc_error(layout);
        }
        unsafe {
            ptr.write(ArcInner {
                strong: AtomicUsize::new(1),
                weak: AtomicUsize::new(1),
                data,
            });
            Arc {
                ptr: NonNull::new_unchecked(ptr),
            }
        }
    }

    pub fn as_ptr(&self) -> *const T {
        self.ptr.as_ptr() as _
    }

    pub fn try_unwrap(this: Self) -> Result<T, Self> {
        if this
            .inner()
            .strong
            .compare_exchange(1, 0, Ordering::Acquire, Ordering::Relaxed)
            .is_err()
        {
            return Err(this);
        }

        atomic::fence(Ordering::Acquire);

        unsafe {
            let val = ptr::read(&this.inner().data);
            let _weak = Weak { ptr: this.ptr };
            mem::forget(this);
            Ok(val)
        }
    }
}

impl<T: ?Sized> Arc<T> {
    #[inline]
    fn inner(&self) -> &ArcInner<T> {
        unsafe { self.ptr.as_ref() }
    }

    pub fn strong_count(this: &Self) -> usize {
        this.inner().strong.load(Ordering::Relaxed)
    }

    pub fn weak_count(this: &Self) -> usize {
        let weak = this.inner().weak.load(Ordering::Relaxed);
        // Subtract the implicit weak ref
        if weak == usize::MAX {
            0
        } else {
            weak - 1
        }
    }

    pub fn downgrade(this: &Self) -> Weak<T> {
        let mut cur = this.inner().weak.load(Ordering::Relaxed);
        loop {
            if cur == usize::MAX {
                std::hint::spin_loop();
                cur = this.inner().weak.load(Ordering::Relaxed);
                continue;
            }
            match this.inner().weak.compare_exchange(
                cur,
                cur + 1,
                Ordering::Acquire,
                Ordering::Relaxed,
            ) {
                Ok(_) => {
                    return Weak { ptr: this.ptr };
                }
                Err(old) => cur = old,
            }
        }
    }

    pub fn ptr_eq(a: &Self, b: &Self) -> bool {
        ptr::eq(a.ptr.as_ptr(), b.ptr.as_ptr())
    }
}

impl<T: ?Sized> Deref for Arc<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &T {
        &self.inner().data
    }
}

impl<T: ?Sized> Clone for Arc<T> {
    #[inline]
    fn clone(&self) -> Self {
        let old = self.inner().strong.fetch_add(1, Ordering::Relaxed);
        if old > isize::MAX as usize {
            std::process::abort();
        }
        Arc { ptr: self.ptr }
    }
}

impl<T: ?Sized> Drop for Arc<T> {
    fn drop(&mut self) {
        if self.inner().strong.fetch_sub(1, Ordering::Release) != 1 {
            return;
        }
        atomic::fence(Ordering::Acquire);

        unsafe {
            drop_in_place(&mut (*self.ptr.as_ptr()).data);
        }

        drop(Weak { ptr: self.ptr });
    }
}

impl<T: ?Sized> Weak<T> {
    #[inline]
    fn inner(&self) -> &ArcInner<T> {
        unsafe { self.ptr.as_ref() }
    }

    pub fn as_ptr(&self) -> *const T {
        self.ptr.as_ptr() as _
    }

    pub fn upgrade(&self) -> Option<Arc<T>> {
        let mut cur = self.inner().strong.load(Ordering::Relaxed);
        loop {
            if cur == 0 {
                return None;
            }
            match self.inner().strong.compare_exchange(
                cur,
                cur + 1,
                Ordering::Acquire,
                Ordering::Relaxed,
            ) {
                Ok(_) => {
                    return Some(Arc { ptr: self.ptr });
                }
                Err(old) => cur = old,
            }
        }
    }

    pub fn strong_count(&self) -> usize {
        self.inner().strong.load(Ordering::Relaxed)
    }

    pub fn weak_count(&self) -> usize {
        let inner = self.inner();
        let weak = inner.weak.load(Ordering::Acquire);
        let strong = inner.strong.load(Ordering::Relaxed);
        if strong == 0 {
            0
        } else {
            // Since we observed that there was at least one strong pointer
            // after reading the weak count, we know that the implicit weak
            // reference (present whenever any strong references are alive)
            // was still around when we observed the weak count, and can
            // therefore safely subtract it.
            weak - 1
        }
    }

    pub fn ptr_eq(a: &Self, b: &Self) -> bool {
        ptr::eq(a.ptr.as_ptr(), b.ptr.as_ptr())
    }
}

impl<T: ?Sized> Clone for Weak<T> {
    #[inline]
    fn clone(&self) -> Self {
        let old = self.inner().weak.fetch_add(1, Ordering::Relaxed);
        if old > isize::MAX as usize {
            panic!("Weak count overflowed");
        }
        Weak { ptr: self.ptr }
    }
}

impl<T: ?Sized> Drop for Weak<T> {
    fn drop(&mut self) {
        if self.inner().weak.fetch_sub(1, Ordering::Release) != 1 {
            return;
        }
        atomic::fence(Ordering::Acquire);

        unsafe {
            alloc::dealloc(
                self.ptr.as_ptr() as *mut u8,
                Layout::for_value(self.ptr.as_ref()),
            );
        }
    }
}

impl<T: ?Sized + fmt::Debug> fmt::Debug for Arc<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&**self, f)
    }
}

impl<T: ?Sized + fmt::Display> fmt::Display for Arc<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&**self, f)
    }
}

impl<T: ?Sized> fmt::Debug for Weak<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(Weak)")
    }
}

impl<T: Default> Default for Arc<T> {
    fn default() -> Self {
        Arc::new(T::default())
    }
}

impl<T: ?Sized + PartialEq> PartialEq for Arc<T> {
    fn eq(&self, other: &Self) -> bool {
        **self == **other
    }
}

impl<T: ?Sized + Eq> Eq for Arc<T> {}

impl<T: ?Sized + std::hash::Hash> std::hash::Hash for Arc<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (**self).hash(state);
    }
}

impl<T: ?Sized + PartialOrd> PartialOrd for Arc<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        (**self).partial_cmp(&**other)
    }
}

impl<T: ?Sized + Ord> Ord for Arc<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (**self).cmp(&**other)
    }
}

impl<T> From<T> for Arc<T> {
    fn from(val: T) -> Self {
        Arc::new(val)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};

    #[test]
    fn basic_usage() {
        let a = Arc::new(42);
        assert_eq!(*a, 42);
        assert_eq!(Arc::strong_count(&a), 1);
        assert_eq!(Arc::weak_count(&a), 0);
    }

    #[test]
    fn clone_increments_strong() {
        let a = Arc::new(1);
        let b = a.clone();
        assert_eq!(Arc::strong_count(&a), 2);
        drop(b);
        assert_eq!(Arc::strong_count(&a), 1);
    }

    #[test]
    fn weak_does_not_prevent_drop() {
        static DROPS: AtomicUsize = AtomicUsize::new(0);

        struct D;
        impl Drop for D {
            fn drop(&mut self) {
                DROPS.fetch_add(1, Ordering::SeqCst);
            }
        }

        let a = Arc::new(D);
        let w = Arc::downgrade(&a);
        assert!(w.upgrade().is_some());
        drop(a);
        assert_eq!(DROPS.load(Ordering::SeqCst), 1);
        assert!(w.upgrade().is_none());
    }

    #[test]
    fn weak_count_tracks() {
        let a = Arc::new(0);
        let w1 = Arc::downgrade(&a);
        let w2 = Arc::downgrade(&a);
        assert_eq!(Arc::weak_count(&a), 2);
        drop(w1);
        assert_eq!(Arc::weak_count(&a), 1);
        drop(w2);
        assert_eq!(Arc::weak_count(&a), 0);
    }

    #[test]
    fn try_unwrap_success() {
        let a = Arc::new(String::from("hello"));
        let s = Arc::try_unwrap(a).unwrap();
        assert_eq!(s, "hello");
    }

    #[test]
    fn try_unwrap_fail() {
        let a = Arc::new(5);
        let _b = a.clone();
        assert!(Arc::try_unwrap(a).is_err());
    }

    #[test]
    fn send_across_threads() {
        let a = Arc::new(100);
        let b = a.clone();
        let handle = std::thread::spawn(move || {
            assert_eq!(*b, 100);
        });
        handle.join().unwrap();
        assert_eq!(Arc::strong_count(&a), 1);
    }

    #[test]
    fn weak_upgrade_across_threads() {
        let a = Arc::new(42);
        let w = Arc::downgrade(&a);
        let handle = std::thread::spawn(move || {
            let upgraded = w.upgrade().unwrap();
            assert_eq!(*upgraded, 42);
        });
        handle.join().unwrap();
    }
}
