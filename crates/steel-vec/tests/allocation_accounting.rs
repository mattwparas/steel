// Its own test binary because a global allocator can only be installed once per
// binary. Counters are thread local so parallel tests can't throw off a
// measurement, and const initialized so the allocator can't recurse into them.

use std::alloc::{GlobalAlloc, Layout, System};
use std::cell::Cell;

thread_local! {
    // Net live allocations made by this thread
    static LIVE: Cell<isize> = const { Cell::new(0) };
    // Requests for a zero sized layout, which alloc::alloc forbids
    static ZERO_SIZED_REQUESTS: Cell<usize> = const { Cell::new(0) };
}

struct Counting;

unsafe impl GlobalAlloc for Counting {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        if layout.size() == 0 {
            let _ = ZERO_SIZED_REQUESTS.try_with(|c| c.set(c.get() + 1));
        }
        let _ = LIVE.try_with(|c| c.set(c.get() + 1));
        unsafe { System.alloc(layout) }
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        let _ = LIVE.try_with(|c| c.set(c.get() - 1));
        unsafe { System.dealloc(ptr, layout) }
    }

    unsafe fn realloc(&self, ptr: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
        unsafe { System.realloc(ptr, layout, new_size) }
    }
}

#[global_allocator]
static ALLOCATOR: Counting = Counting;

// Run f, then check it neither leaked nor asked for a zero sized layout
#[track_caller]
fn assert_balanced(f: impl FnOnce()) {
    let live_before = LIVE.with(|c| c.get());
    let zero_before = ZERO_SIZED_REQUESTS.with(|c| c.get());

    f();

    let leaked = LIVE.with(|c| c.get()) - live_before;
    let zero_sized = ZERO_SIZED_REQUESTS.with(|c| c.get()) - zero_before;

    assert_eq!(zero_sized, 0, "requested {zero_sized} zero-sized layouts");
    assert_eq!(leaked, 0, "leaked {leaked} allocations");
}

#[test]
fn cloning_an_empty_vec_neither_allocates_nor_leaks() {
    let empty: steel_vec::Vec<String> = steel_vec::Vec::new();
    assert_eq!(empty.cap(), 0);

    assert_balanced(|| {
        for _ in 0..1000 {
            drop(empty.clone());
        }
    });
}

#[test]
fn with_capacity_zero_does_not_allocate() {
    assert_balanced(|| {
        for _ in 0..1000 {
            drop(steel_vec::Vec::<String>::with_capacity(0));
        }
    });
}

#[test]
fn splitting_off_the_tail_does_not_leak() {
    assert_balanced(|| {
        for _ in 0..1000 {
            let mut v: steel_vec::Vec<String> = steel_vec::Vec::new();
            v.push(String::from("a"));
            // at == len, so the new vector is empty
            let tail = v.split_off(1);
            assert_eq!(tail.len(), 0);
        }
    });
}

#[test]
fn a_vec_that_grew_from_empty_still_frees_its_buffer() {
    assert_balanced(|| {
        for _ in 0..100 {
            let mut v = steel_vec::Vec::<String>::with_capacity(0);
            for i in 0..8 {
                v.push(i.to_string());
            }
            assert_eq!(v.len(), 8);
        }
    });
}

#[test]
fn round_trip_through_into_iter_is_balanced() {
    assert_balanced(|| {
        for n in 0..32 {
            let v: steel_vec::Vec<String> = (0..n).map(|i| i.to_string()).collect();
            let collected: std::vec::Vec<String> = v.into_iter().collect();
            assert_eq!(collected.len(), n);
        }
    });
}

#[test]
fn cloning_a_populated_vec_is_balanced() {
    let source: steel_vec::Vec<String> = (0..16).map(|i| i.to_string()).collect();

    assert_balanced(|| {
        for _ in 0..100 {
            let cloned = source.clone();
            assert_eq!(cloned.len(), 16);
        }
    });
}

#[test]
fn zero_sized_elements_never_allocate() {
    assert_balanced(|| {
        let mut v: steel_vec::Vec<()> = steel_vec::Vec::with_capacity(16);
        for _ in 0..1000 {
            v.push(());
        }
        assert_eq!(v.len(), 1000);
        assert_eq!(v.iter().count(), 1000);
        let drained: usize = v.drain(..).count();
        assert_eq!(drained, 1000);
        assert_eq!(v.len(), 0);
    });
}
