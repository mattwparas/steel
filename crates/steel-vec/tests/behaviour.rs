use std::cell::Cell;
use std::panic::{catch_unwind, AssertUnwindSafe};

fn collect<T: Clone>(v: &steel_vec::Vec<T>) -> std::vec::Vec<T> {
    v.iter().cloned().collect()
}

#[test]
fn push_pop_and_index() {
    let mut v = steel_vec::Vec::new();
    assert_eq!(v.pop(), None);

    for i in 0..10 {
        v.push(i);
    }

    assert_eq!(v.len(), 10);
    assert_eq!(v[0], 0);
    assert_eq!(v[9], 9);
    assert_eq!(v.pop(), Some(9));
    assert_eq!(v.len(), 9);
}

#[test]
fn clone_is_independent_and_equal() {
    let mut v = steel_vec::Vec::new();
    for i in 0..5 {
        v.push(i.to_string());
    }

    let mut cloned = v.clone();
    assert_eq!(v, cloned);

    cloned.push(String::from("extra"));
    assert_ne!(v, cloned);
    assert_eq!(v.len(), 5);
}

#[test]
fn clone_of_empty_vec_is_empty() {
    let empty: steel_vec::Vec<String> = steel_vec::Vec::new();
    let cloned = empty.clone();
    assert_eq!(cloned.len(), 0);
    assert!(cloned.is_empty());
}

// A clone that unwinds part way through shouldn't leak what it already cloned
#[test]
fn clone_that_panics_drops_what_it_already_cloned() {
    thread_local! {
        static CLONES: Cell<usize> = const { Cell::new(0) };
        static DROPS: Cell<usize> = const { Cell::new(0) };
    }

    struct Boom;

    impl Clone for Boom {
        fn clone(&self) -> Self {
            let n = CLONES.with(|c| c.get());
            CLONES.with(|c| c.set(n + 1));
            if n == 3 {
                panic!("boom");
            }
            Boom
        }
    }

    impl Drop for Boom {
        fn drop(&mut self) {
            DROPS.with(|c| c.set(c.get() + 1));
        }
    }

    let mut source = steel_vec::Vec::new();
    for _ in 0..6 {
        source.push(Boom);
    }

    DROPS.with(|c| c.set(0));

    let result = catch_unwind(AssertUnwindSafe(|| {
        let _ = source.clone();
    }));
    assert!(result.is_err(), "clone was expected to unwind");

    // Three cloned fine before the fourth panicked - all three belong to the half
    // built vector, so they should have been dropped
    assert_eq!(
        DROPS.with(|c| c.get()),
        3,
        "partially cloned elements were leaked"
    );
}

#[test]
fn zero_sized_types_round_trip() {
    let mut v: steel_vec::Vec<()> = steel_vec::Vec::new();
    for _ in 0..100 {
        v.push(());
    }
    assert_eq!(v.len(), 100);
    assert_eq!(v.iter().count(), 100);
    assert_eq!(v.pop(), Some(()));
    assert_eq!(v.len(), 99);

    let cloned = v.clone();
    assert_eq!(cloned.len(), 99);

    assert_eq!(v.into_iter().count(), 99);
}

#[test]
fn zero_sized_types_via_with_capacity_and_extend() {
    // with_capacity used to hand a zero sized type a finite capacity, which then
    // tripped grow's overflow assert on the first push past it.
    let mut v: steel_vec::Vec<()> = steel_vec::Vec::with_capacity(4);
    v.extend((0..10).map(|_| ()));
    assert_eq!(v.len(), 10);

    let collected: steel_vec::Vec<()> = (0..10).map(|_| ()).collect();
    assert_eq!(collected.len(), 10);
}

#[test]
fn split_off_moves_the_tail() {
    let mut v: steel_vec::Vec<String> = (0..6).map(|i| i.to_string()).collect();

    let tail = v.split_off(4);
    assert_eq!(collect(&v), vec!["0", "1", "2", "3"]);
    assert_eq!(collect(&tail), vec!["4", "5"]);
}

#[test]
fn split_off_at_len_yields_an_empty_vec() {
    let mut v: steel_vec::Vec<String> = (0..3).map(|i| i.to_string()).collect();

    let tail = v.split_off(3);
    assert_eq!(v.len(), 3);
    assert_eq!(tail.len(), 0);
}

#[test]
#[should_panic(expected = "`at` split index")]
fn split_off_past_the_end_panics() {
    let mut v: steel_vec::Vec<String> = (0..3).map(|i| i.to_string()).collect();
    let _ = v.split_off(4);
}

#[test]
fn insert_remove_and_truncate() {
    let mut v: steel_vec::Vec<i32> = (0..5).collect();

    v.insert(0, -1);
    assert_eq!(collect(&v), vec![-1, 0, 1, 2, 3, 4]);

    assert_eq!(v.remove(1), 0);
    assert_eq!(collect(&v), vec![-1, 1, 2, 3, 4]);

    v.truncate(2);
    assert_eq!(collect(&v), vec![-1, 1]);

    // Truncating to a larger length does nothing
    v.truncate(10);
    assert_eq!(v.len(), 2);

    v.clear();
    assert!(v.is_empty());
}

#[test]
fn drain_ranges() {
    let mut v: steel_vec::Vec<i32> = (0..6).collect();
    let drained: std::vec::Vec<i32> = v.drain(1..3).collect();
    assert_eq!(drained, vec![1, 2]);
    assert_eq!(collect(&v), vec![0, 3, 4, 5]);

    let all: std::vec::Vec<i32> = v.drain(..).collect();
    assert_eq!(all, vec![0, 3, 4, 5]);
    assert!(v.is_empty());
}

#[test]
fn append_moves_everything() {
    let mut a: steel_vec::Vec<i32> = (0..3).collect();
    let mut b: steel_vec::Vec<i32> = (3..6).collect();

    a.append(&mut b);
    assert_eq!(collect(&a), vec![0, 1, 2, 3, 4, 5]);
    assert!(b.is_empty());
}

#[test]
fn reserve_exact_does_not_shrink() {
    let mut v: steel_vec::Vec<i32> = (0..4).collect();
    let cap = v.cap();
    v.reserve_exact(0);
    assert!(v.cap() >= cap);
    assert_eq!(collect(&v), vec![0, 1, 2, 3]);
}
