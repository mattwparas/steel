//! Checks that cranelift keeps the jit's re-reads of a sequence number. If
//! the second load is folded into the first, a shared read is never checked
//! and nothing that runs can tell.

use std::time::{Duration, Instant};

use crate::jit2::cgen::atomic_load_watch::SEEN;
use crate::steel_vm::engine::Engine;

/// Loops that read boxes, vectors and struct fields. A module, since that
/// is where the jit reads struct fields inline.
const SHAPES: &str = r#"
(provide shape-run)

(struct shape-node (left right) #:mutable)

(define shape-first (box (list 1 1)))
(define shape-second (box (list 2 2)))
(define shape-slots (mutable-vector (list 3 3) (list 4 4) (list 5 5)))
(define shape-fields (shape-node (list 6 6) (list 7 7)))

(define (shape-box-copy n)
  (when (> n 0)
    (set-box! shape-first (list n n))
    (set-box! shape-second (unbox shape-first))
    (shape-box-copy (- n 1))))

(define (shape-box-read n acc)
  (if (= n 0) acc (shape-box-read (- n 1) (+ acc (car (unbox shape-first))))))

(define (shape-slot-copy n)
  (when (> n 0)
    (vector-set! shape-slots (modulo n 3) (vector-ref shape-slots (modulo (+ n 1) 3)))
    (shape-slot-copy (- n 1))))

(define (shape-slot-read n acc)
  (if (= n 0) acc (shape-slot-read (- n 1) (+ acc (car (vector-ref shape-slots (modulo n 3)))))))

(define (shape-field-copy n)
  (when (> n 0)
    (set-shape-node-right! shape-fields (shape-node-left shape-fields))
    (shape-field-copy (- n 1))))

(define (shape-field-read n acc)
  (if (= n 0) acc (shape-field-read (- n 1) (+ acc (car (shape-node-left shape-fields))))))

(define (shape-run n)
  (shape-box-copy n)
  (shape-box-read n 0)
  (shape-slot-copy n)
  (shape-slot-read n 0)
  (shape-field-copy n)
  (shape-field-read n 0))
"#;

fn shapes_seen() -> Vec<(String, usize, usize)> {
    SEEN.lock()
        .map(|seen| {
            seen.iter()
                .filter(|(name, ..)| name.contains("shape-"))
                .cloned()
                .collect()
        })
        .unwrap_or_default()
}

#[test]
fn cranelift_keeps_every_reread_of_a_sequence_number() {
    if !crate::config::config().jit {
        return;
    }

    let mut engine = Engine::new();
    engine.register_steel_module("compiled-shapes".to_owned(), SHAPES.to_owned());
    engine
        .run(r#"(require "compiled-shapes") (shape-run 20000)"#)
        .expect("the shapes run");

    // Compiling may be on another thread
    let started = Instant::now();
    let wanted = [
        "box-copy",
        "box-read",
        "slot-copy",
        "slot-read",
        "field-copy",
        "field-read",
    ];
    let all_seen = |seen: &[(String, usize, usize)]| {
        wanted
            .iter()
            .all(|shape| seen.iter().any(|(name, ..)| name.contains(shape)))
    };
    let mut seen = shapes_seen();
    while !all_seen(&seen) && started.elapsed() < Duration::from_secs(30) {
        std::thread::sleep(Duration::from_millis(20));
        engine
            .run("(shape-run 2000)")
            .expect("the shapes run again");
        seen = shapes_seen();
    }

    // A busy background jit may not have reached every shape yet
    let waited_for_all = all_seen(&seen);
    if !crate::config::config().jit_background {
        assert!(
            waited_for_all,
            "not every shape was compiled with a checked read in it: {:?}",
            seen
        );
    }

    for (name, before, after) in &seen {
        assert_eq!(
            before, after,
            "{} went into cranelift with {} atomic loads and came out with {}",
            name, before, after
        );
    }

    // Each shape has a checked read in it
    for shape in wanted {
        if !waited_for_all {
            break;
        }
        assert!(
            seen.iter()
                .any(|(name, before, _)| name.contains(shape) && *before >= 2),
            "no compiled copy of {} has a checked read in it: {:?}",
            shape,
            seen
        );
    }
}

/// Every function the tests have compiled so far.
#[test]
fn cranelift_keeps_the_atomic_loads_of_everything_compiled_so_far() {
    let seen = SEEN.lock().map(|seen| seen.clone()).unwrap_or_default();

    for (name, before, after) in &seen {
        assert_eq!(
            before, after,
            "{} went into cranelift with {} atomic loads and came out with {}",
            name, before, after
        );
    }
}
