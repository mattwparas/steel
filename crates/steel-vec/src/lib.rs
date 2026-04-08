use core::{fmt, slice};
use std::alloc::{self, Layout};
use std::ops::{Deref, DerefMut, Range, RangeBounds};
use std::ptr::{self, NonNull};
use std::{mem, ops};

use crate::drain::Drain;

mod drain;

#[repr(C)]
struct RawVec<T> {
    ptr: NonNull<T>,
    cap: usize,
}

unsafe impl<T: Send> Send for RawVec<T> {}
unsafe impl<T: Sync> Sync for RawVec<T> {}

impl<T> RawVec<T> {
    fn new() -> Self {
        // !0 is usize::MAX. This branch should be stripped at compile time.
        let cap = if mem::size_of::<T>() == 0 { !0 } else { 0 };

        // `NonNull::dangling()` doubles as "unallocated" and "zero-sized allocation"
        RawVec {
            ptr: NonNull::dangling(),
            cap,
        }
    }

    fn with_capacity(capacity: usize) -> Self {
        let new_cap = capacity;
        let new_layout = Layout::array::<T>(capacity);
        let new_layout = new_layout.expect("Allocation too large");

        let new_ptr = unsafe { alloc::alloc(new_layout) };

        // If allocation fails, `new_ptr` will be null, in which case we abort.
        let ptr = match NonNull::new(new_ptr as *mut T) {
            Some(p) => p,
            None => alloc::handle_alloc_error(new_layout),
        };

        Self { ptr, cap: new_cap }
    }

    fn reserve_exact(&mut self, len: usize, additional: usize) {
        assert!(mem::size_of::<T>() != 0, "capacity overflow");

        let required = len.checked_add(additional).expect("capacity overflow");
        if required <= self.cap {
            return;
        }

        let new_cap = required;
        let new_layout = Layout::array::<T>(new_cap).expect("Allocation too large");

        let new_ptr = if self.cap == 0 {
            unsafe { alloc::alloc(new_layout) }
        } else {
            let old_layout = Layout::array::<T>(self.cap).unwrap();
            let old_ptr = self.ptr.as_ptr() as *mut u8;
            unsafe { alloc::realloc(old_ptr, old_layout, new_layout.size()) }
        };

        self.ptr = match NonNull::new(new_ptr as *mut T) {
            Some(p) => p,
            None => alloc::handle_alloc_error(new_layout),
        };
        self.cap = new_cap;
    }

    fn grow(&mut self) {
        // since we set the capacity to usize::MAX when T has size 0,
        // getting to here necessarily means the Vec is overfull.
        assert!(mem::size_of::<T>() != 0, "capacity overflow");

        let (new_cap, new_layout) = if self.cap == 0 {
            (1, Layout::array::<T>(1))
        } else {
            // This can't overflow since self.cap <= isize::MAX.
            let new_cap = 2 * self.cap;
            (new_cap, Layout::array::<T>(new_cap))
        };

        // `Layout::array` checks that the number of bytes allocated is
        // in 1..=isize::MAX and will error otherwise.  An allocation of
        // 0 bytes isn't possible thanks to the above condition.
        let new_layout = new_layout.expect("Allocation too large");

        let new_ptr = if self.cap == 0 {
            unsafe { alloc::alloc(new_layout) }
        } else {
            let old_layout = Layout::array::<T>(self.cap).unwrap();
            let old_ptr = self.ptr.as_ptr() as *mut u8;
            unsafe { alloc::realloc(old_ptr, old_layout, new_layout.size()) }
        };

        // If allocation fails, `new_ptr` will be null, in which case we abort.
        self.ptr = match NonNull::new(new_ptr as *mut T) {
            Some(p) => p,
            None => alloc::handle_alloc_error(new_layout),
        };
        self.cap = new_cap;
    }
}

impl<T> Drop for RawVec<T> {
    fn drop(&mut self) {
        let elem_size = mem::size_of::<T>();

        if self.cap != 0 && elem_size != 0 {
            unsafe {
                alloc::dealloc(
                    self.ptr.as_ptr() as *mut u8,
                    Layout::array::<T>(self.cap).unwrap(),
                );
            }
        }
    }
}

#[repr(C)]
pub struct Vec<T> {
    buf: RawVec<T>,
    len: usize,
}

impl<T: Clone> Clone for Vec<T> {
    fn clone(&self) -> Self {
        let len = self.len();
        // TODO: Consider not cloning the capacity too?
        // Just start with the length instead?
        let mut new_vec = Vec::<T>::with_capacity(self.cap());
        let mut data_raw = new_vec.ptr();
        for x in self.iter() {
            unsafe {
                ptr::write(data_raw, x.clone());
                data_raw = data_raw.add(1);
            }
        }
        new_vec.len = len;
        new_vec
    }
}

impl<T> Vec<T> {
    fn ptr(&self) -> *mut T {
        self.buf.ptr.as_ptr()
    }

    pub const fn buf_offset() -> usize {
        std::mem::offset_of!(Vec<T>, buf)
    }

    pub const fn len_offset() -> usize {
        std::mem::offset_of!(Vec<T>, len)
    }

    pub const fn capacity_offset() -> usize {
        std::mem::offset_of!(RawVec<T>, cap) + Self::buf_offset()
    }

    pub fn cap(&self) -> usize {
        self.buf.cap
    }

    pub fn new() -> Self {
        Vec {
            buf: RawVec::new(),
            len: 0,
        }
    }

    pub fn grow_capacity(&mut self) {
        self.buf.grow();
    }

    pub fn reserve_exact(&mut self, additional: usize) {
        self.buf.reserve_exact(self.len, additional);
    }

    pub fn with_capacity(cap: usize) -> Self {
        Self {
            buf: RawVec::with_capacity(cap),
            len: 0,
        }
    }

    pub fn split_off(&mut self, at: usize) -> Self
    where
        T: Clone,
    {
        #[cold]
        #[track_caller]
        fn assert_failed(at: usize, len: usize) -> ! {
            panic!("`at` split index (is {at}) should be <= len (is {len})");
        }

        if at > self.len() {
            assert_failed(at, self.len());
        }

        let other_len = self.len - at;
        let mut other = Vec::with_capacity(other_len);

        // Unsafely `set_len` and copy items to `other`.
        unsafe {
            self.set_len(at);
            other.set_len(other_len);

            ptr::copy_nonoverlapping(self.as_ptr().add(at), other.as_mut_ptr(), other.len());
        }
        other
    }

    pub unsafe fn set_len(&mut self, new_len: usize) {
        self.len = new_len;
    }

    pub fn push(&mut self, elem: T) {
        if self.len == self.cap() {
            self.buf.grow();
        }

        unsafe {
            ptr::write(self.ptr().add(self.len), elem);
        }

        // Can't overflow, we'll OOM first.
        self.len += 1;
    }

    pub fn pop(&mut self) -> Option<T> {
        if self.len == 0 {
            None
        } else {
            self.len -= 1;
            unsafe { Some(ptr::read(self.ptr().add(self.len))) }
        }
    }

    pub fn insert(&mut self, index: usize, elem: T) {
        assert!(index <= self.len, "index out of bounds");
        if self.len == self.cap() {
            self.buf.grow();
        }

        unsafe {
            ptr::copy(
                self.ptr().add(index),
                self.ptr().add(index + 1),
                self.len - index,
            );
            ptr::write(self.ptr().add(index), elem);
        }

        self.len += 1;
    }

    pub fn remove(&mut self, index: usize) -> T {
        assert!(index < self.len, "index out of bounds");

        self.len -= 1;

        unsafe {
            let result = ptr::read(self.ptr().add(index));
            ptr::copy(
                self.ptr().add(index + 1),
                self.ptr().add(index),
                self.len - index,
            );
            result
        }
    }

    pub fn drain<R>(&mut self, range: R) -> Drain<'_, T>
    where
        R: RangeBounds<usize>,
    {
        // Memory safety
        //
        // When the Drain is first created, it shortens the length of
        // the source vector to make sure no uninitialized or moved-from elements
        // are accessible at all if the Drain's destructor never gets to run.
        //
        // Drain will ptr::read out the values to remove.
        // When finished, remaining tail of the vec is copied back to cover
        // the hole, and the vector length is restored to the new length.
        //
        let len = self.len();

        fn slice_index_fail(start: usize, end: usize, len: usize) -> ! {
            if start > len {
                panic!(
                    "slice start index is out of range for slice: range start index {start} out of range for slice of length {len}"
                )
            }

            if end > len {
                panic!(
                    "slice end index is out of range for slice: range end index {end} out of range for slice of length {len}",
                )
            }

            if start > end {
                panic!(
                    "slice index start is larger than end: slice index starts at {start} but ends at {end}",
                )
            }

            // Only reachable if the range was a `RangeInclusive` or a
            // `RangeToInclusive`, with `end == len`.
            panic!(
                "slice end index is out of range for slice:  range end index {end} out of range for slice of length {len}",
            )
        }

        pub fn slice_range<R>(range: R, bounds: ops::RangeTo<usize>) -> ops::Range<usize>
        where
            R: ops::RangeBounds<usize>,
        {
            let len = bounds.end;

            let end = match range.end_bound() {
                ops::Bound::Included(&end) if end >= len => slice_index_fail(0, end, len),
                // Cannot overflow because `end < len` implies `end < usize::MAX`.
                ops::Bound::Included(&end) => end + 1,

                ops::Bound::Excluded(&end) if end > len => slice_index_fail(0, end, len),
                ops::Bound::Excluded(&end) => end,
                ops::Bound::Unbounded => len,
            };

            let start = match range.start_bound() {
                ops::Bound::Excluded(&start) if start >= end => slice_index_fail(start, end, len),
                // Cannot overflow because `start < end` implies `start < usize::MAX`.
                ops::Bound::Excluded(&start) => start + 1,

                ops::Bound::Included(&start) if start > end => slice_index_fail(start, end, len),
                ops::Bound::Included(&start) => start,

                ops::Bound::Unbounded => 0,
            };

            ops::Range { start, end }
        }

        let Range { start, end } = slice_range(range, ..len);

        unsafe {
            // set self.vec length's to start, to be safe in case Drain is leaked
            self.set_len(start);
            let range_slice = slice::from_raw_parts(self.as_ptr().add(start), end - start);
            Drain {
                tail_start: end,
                tail_len: len - end,
                iter: range_slice.iter(),
                vec: NonNull::from(self),
            }
        }
    }

    pub fn truncate(&mut self, len: usize) {
        // This is safe because:
        //
        // * the slice passed to `drop_in_place` is valid; the `len > self.len`
        //   case avoids creating an invalid slice, and
        // * the `len` of the vector is shrunk before calling `drop_in_place`,
        //   such that no value will be dropped twice in case `drop_in_place`
        //   were to panic once (if it panics twice, the program aborts).
        unsafe {
            // Note: It's intentional that this is `>` and not `>=`.
            //       Changing it to `>=` has negative performance
            //       implications in some cases. See #78884 for more.
            if len > self.len {
                return;
            }
            let remaining_len = self.len - len;
            let s = ptr::slice_from_raw_parts_mut(self.as_mut_ptr().add(len), remaining_len);
            self.len = len;
            ptr::drop_in_place(s);
        }
    }

    pub fn clear(&mut self) {
        let elems: *mut [T] = &mut self[..];

        // SAFETY:
        // - `elems` comes directly from `as_mut_slice` and is therefore valid.
        // - Setting `self.len` before calling `drop_in_place` means that,
        //   if an element's `Drop` impl panics, the vector's `Drop` impl will
        //   do nothing (leaking the rest of the elements) instead of dropping
        //   some twice.
        unsafe {
            self.len = 0;
            ptr::drop_in_place(elems);
        }
    }

    pub fn append(&mut self, other: &mut Vec<T>) {
        self.extend(other.drain(..))
    }
}

impl<T: fmt::Debug> fmt::Debug for Vec<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&**self, f)
    }
}

impl<A, B> PartialEq<Vec<B>> for Vec<A>
where
    A: PartialEq<B>,
{
    #[inline]
    fn eq(&self, other: &Vec<B>) -> bool {
        self[..] == other[..]
    }
}

impl<T> Eq for Vec<T> where T: Eq {}

impl<T> Drop for Vec<T> {
    fn drop(&mut self) {
        while let Some(_) = self.pop() {}
        // deallocation is handled by RawVec
    }
}

impl<T> Deref for Vec<T> {
    type Target = [T];
    fn deref(&self) -> &[T] {
        unsafe { std::slice::from_raw_parts(self.ptr(), self.len) }
    }
}

impl<T> DerefMut for Vec<T> {
    fn deref_mut(&mut self) -> &mut [T] {
        unsafe { std::slice::from_raw_parts_mut(self.ptr(), self.len) }
    }
}

impl<T> IntoIterator for Vec<T> {
    type Item = T;
    type IntoIter = IntoIter<T>;
    fn into_iter(self) -> IntoIter<T> {
        let (iter, buf) = unsafe { (RawValIter::new(&self), ptr::read(&self.buf)) };

        mem::forget(self);

        IntoIter { iter, _buf: buf }
    }
}

impl<'a, T> IntoIterator for &'a Vec<T> {
    type Item = &'a T;
    type IntoIter = std::slice::Iter<'a, T>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

struct RawValIter<T> {
    start: *const T,
    end: *const T,
}

impl<T> RawValIter<T> {
    unsafe fn new(slice: &[T]) -> Self {
        RawValIter {
            start: slice.as_ptr(),
            end: if mem::size_of::<T>() == 0 {
                ((slice.as_ptr() as usize) + slice.len()) as *const _
            } else if slice.len() == 0 {
                slice.as_ptr()
            } else {
                slice.as_ptr().add(slice.len())
            },
        }
    }
}

impl<T> Iterator for RawValIter<T> {
    type Item = T;
    fn next(&mut self) -> Option<T> {
        if self.start == self.end {
            None
        } else {
            unsafe {
                if mem::size_of::<T>() == 0 {
                    self.start = (self.start as usize + 1) as *const _;
                    Some(ptr::read(NonNull::<T>::dangling().as_ptr()))
                } else {
                    let old_ptr = self.start;
                    self.start = self.start.offset(1);
                    Some(ptr::read(old_ptr))
                }
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let elem_size = mem::size_of::<T>();
        let len =
            (self.end as usize - self.start as usize) / if elem_size == 0 { 1 } else { elem_size };
        (len, Some(len))
    }
}

impl<T> DoubleEndedIterator for RawValIter<T> {
    fn next_back(&mut self) -> Option<T> {
        if self.start == self.end {
            None
        } else {
            unsafe {
                if mem::size_of::<T>() == 0 {
                    self.end = (self.end as usize - 1) as *const _;
                    Some(ptr::read(NonNull::<T>::dangling().as_ptr()))
                } else {
                    self.end = self.end.offset(-1);
                    Some(ptr::read(self.end))
                }
            }
        }
    }
}

pub struct IntoIter<T> {
    _buf: RawVec<T>, // we don't actually care about this. Just need it to live.
    iter: RawValIter<T>,
}

impl<T> Iterator for IntoIter<T> {
    type Item = T;
    fn next(&mut self) -> Option<T> {
        self.iter.next()
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}

impl<T> DoubleEndedIterator for IntoIter<T> {
    fn next_back(&mut self) -> Option<T> {
        self.iter.next_back()
    }
}

impl<T> Drop for IntoIter<T> {
    fn drop(&mut self) {
        for _ in &mut *self {}
    }
}

impl<T> Extend<T> for Vec<T> {
    #[inline]
    fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = T>,
    {
        let iter = iter.into_iter();
        let hint = iter.size_hint().0;
        if hint > 0 {
            self.reserve_exact(hint);
        }
        for x in iter {
            self.push(x);
        }
    }
}

impl<T> FromIterator<T> for Vec<T> {
    #[inline]
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Vec<T> {
        let mut vec = Vec::new();
        vec.extend(iter);
        vec
    }
}

impl<T> Default for Vec<T> {
    fn default() -> Self {
        Self::new()
    }
}
