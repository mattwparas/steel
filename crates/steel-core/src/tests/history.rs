//! Checks what threads see of shared boxes, structs and vectors against a
//! model that does one operation at a time.
//!
//! Each round a few threads run short random lists of operations on the same
//! cells at once, and note for every operation when it started, when it
//! finished and what it returned. The round passes if the operations can be
//! put in some order, one after another, that gives every one of them the
//! result it got, without moving an operation ahead of one that had already
//! finished when it started. If there is no such order, the threads saw
//! something that no sequence of whole operations produces: a lost write, a
//! value handed out twice, a vector in between two states.
//!
//! The search is the one from Wing and Gong, with Lowe's cache of the states
//! already tried.
//!
//! `STEEL_HISTORY_ROUNDS` sets how many rounds each test runs.

use std::collections::HashSet;
use std::sync::atomic::{AtomicIsize, Ordering};

use crate::rvals::SteelVal;
use crate::steel_vm::engine::Engine;
use crate::steel_vm::register_fn::RegisterFn;

const BOX_REF: i64 = 0;
const BOX_SET: i64 = 1;
const FIELD_REF: i64 = 2;
const FIELD_SET: i64 = 3;
const VECTOR_REF: i64 = 4;
const VECTOR_SET: i64 = 5;
const VECTOR_PUSH: i64 = 6;
const VECTOR_POP: i64 = 7;
const VECTOR_LENGTH: i64 = 8;
const VECTOR_SWAP: i64 = 9;
const VECTOR_FILL: i64 = 10;
const VECTOR_CLEAR: i64 = 11;
const VECTOR_APPEND: i64 = 12;
// `set-box!` done as a read and then a write, which is not one operation.
// Only there to show that the check notices.
const BOX_SET_IN_TWO_STEPS: i64 = 13;

// Copies from one cell to another, checked as a read and then a write.
const BOX_TO_SLOT: i64 = 14;
const SLOT_TO_BOX: i64 = 15;
const FIELD_TO_BOX: i64 = 16;
const BOX_TO_FIELD: i64 = 17;
const SLOT_TO_SLOT: i64 = 18;
const FIELD_TO_FIELD: i64 = 19;

// What an operation returns when it has nothing else to say, and when it
// was refused: an index out of bounds, or a pop from an empty vector.
const DONE: i64 = 0;
const REFUSED: i64 = -1;
// A value that was not one of the ones the test stores.
const BROKEN: i64 = -2;
// The write half of a copy whose read found nothing to copy.
const SKIPPED: i64 = -3;

/// The program the threads run. Every value stored carries its id twice and
/// is one of six kinds, so a torn read does not look like a value. A module,
/// since that is where the jit reads struct fields inline.
const HARNESS: &str = r#"
(provide run-round)

(struct cell (a b) #:mutable)

(define box0 (box #f))
(define box1 (box #f))
(define fields (cell #f #f))
(define vector0 (mutable-vector))
(define vector1 (mutable-vector))
(define gate (box #f))

(struct labelled (n again) #:mutable)

(define (value id)
  (let ([kind (modulo id 6)])
    (cond
      [(= kind 0) (list id id)]
      [(= kind 1) (string-append (number->string id) ":" (number->string id))]
      [(= kind 2) (lambda () (list id id))]
      [(= kind 3) (mutable-vector id id)]
      [(= kind 4) (labelled id id)]
      [else (box (list id id))])))

(define (twice-id a b) (if (and (integer? a) (equal? a b)) a -2))
(define (pair-id v) (if (and (list? v) (= (length v) 2)) (twice-id (car v) (cadr v)) -2))
(define (string-id v)
  (let ([parts (split-many v ":")])
    (if (= (length parts) 2)
        (twice-id (string->number (car parts)) (string->number (cadr parts)))
        -2)))
(define (id-of v)
  (cond
    [(not v) -1]
    [(list? v) (pair-id v)]
    [(string? v) (string-id v)]
    [(function? v) (pair-id (v))]
    [(vector? v) (if (= (vector-length v) 2) (twice-id (vector-ref v 0) (vector-ref v 1)) -2)]
    [(labelled? v) (twice-id (labelled-n v) (labelled-again v))]
    [(symbol? v) -2]
    ;; Only a box is left.
    [else (pair-id (unbox v))]))

(define (reset!)
  (set-box! box0 (value 1))
  (set-box! box1 (value 2))
  (set-cell-a! fields (value 3))
  (set-cell-b! fields (value 4))
  (mutable-vector->clear vector0)
  (vector-push! vector0 (value 5))
  (vector-push! vector0 (value 6))
  (vector-push! vector0 (value 7))
  (mutable-vector->clear vector1)
  (vector-push! vector1 (value 8)))

(define (perform code which i j id)
  (let ([b (if (= which 0) box0 box1)]
        [v (if (= which 0) vector0 vector1)])
    (cond
      [(= code 0) (id-of (unbox b))]
      [(= code 1) (id-of (set-box! b (value id)))]
      [(= code 2) (id-of (if (= which 0) (cell-a fields) (cell-b fields)))]
      [(= code 3)
       (id-of (if (= which 0) (set-cell-a! fields (value id)) (set-cell-b! fields (value id))))]
      [(= code 4) (with-handler (lambda (e) -1) (id-of (vector-ref v i)))]
      [(= code 5)
       (with-handler (lambda (e) -1)
                     (begin
                       (vector-set! v i (value id))
                       0))]
      [(= code 6)
       (begin
         (vector-push! v (value id))
         0)]
      [(= code 7) (id-of (mutable-vector-pop! v))]
      [(= code 8) (vector-length v)]
      [(= code 9)
       (with-handler (lambda (e) -1)
                     (begin
                       (vector-swap! v i j)
                       0))]
      [(= code 10)
       (begin
         (vector-fill! v (value id))
         0)]
      [(= code 11)
       (begin
         (mutable-vector->clear v)
         0)]
      [(= code 12)
       (begin
         (vector-append! v (if (= which 0) vector1 vector0))
         0)]
      [else
       (let ([old (unbox b)])
         (set-box! b (value id))
         (id-of old))])))

(define (field which) (if (= which 0) (cell-a fields) (cell-b fields)))
(define (set-field! which x) (if (= which 0) (set-cell-a! fields x) (set-cell-b! fields x)))

;; Reads one cell and stores what it found in another. The entry has the
;; times of both halves, and the id that was read.
(define (copy code which i j)
  (let* ([b (if (= which 0) box0 box1)]
         [v (if (= which 0) vector0 vector1)]
         [start (history-tick)]
         [x (cond
              [(or (= code 14) (= code 17)) (unbox b)]
              [(or (= code 15) (= code 18)) (with-handler (lambda (e) 'none) (vector-ref v i))]
              [else (field which)])]
         [read-end (history-tick)]
         [write-start (history-tick)]
         [result (cond
                   [(symbol? x) -3]
                   [(= code 14)
                    (with-handler (lambda (e) -1)
                                  (begin
                                    (vector-set! v i x)
                                    0))]
                   [(or (= code 15) (= code 16)) (id-of (set-box! b x))]
                   [(= code 17) (id-of (set-field! which x))]
                   [(= code 18)
                    (with-handler (lambda (e) -1)
                                  (begin
                                    (vector-set! v j x)
                                    0))]
                   [else (id-of (set-field! (- 1 which) x))])]
         [end (history-tick)])
    (list start end result read-end write-start (if (symbol? x) -1 (id-of x)))))

(define (run ops log)
  (if (null? ops)
      (reverse log)
      (let* ([op (car ops)]
             [code (list-ref op 0)]
             [entry (if (>= code 14)
                        (copy code (list-ref op 1) (list-ref op 2) (list-ref op 3))
                        (let* ([start (history-tick)]
                               [result (perform code (list-ref op 1) (list-ref op 2) (list-ref op 3) (list-ref op 4))]
                               [end (history-tick)])
                          (list start end result)))])
        (run (cdr ops) (cons entry log)))))

(define (wait-for-gate)
  (unless (unbox gate)
    (wait-for-gate)))

(define (wait-for-all ready)
  (unless (null? ready)
    (if (unbox (car ready)) (wait-for-all (cdr ready)) (wait-for-all ready))))

;; The first list of operations is run by this thread, and each of the others
;; by a thread of its own. None starts until all of them are ready to.
(define (run-round all)
  (reset!)
  (set-box! gate #f)
  (let* ([ready (map (lambda (ops) (box #f)) (cdr all))]
         [threads (map (lambda (ops flag)
                         (spawn-native-thread (lambda ()
                                                (set-box! flag #t)
                                                (wait-for-gate)
                                                (run ops '()))))
                       (cdr all)
                       ready)])
    (wait-for-all ready)
    (set-box! gate #t)
    (let ([mine (run (car all) '())])
      (cons mine (map thread-join! threads)))))
"#;

static TICKS: AtomicIsize = AtomicIsize::new(0);

fn tick() -> isize {
    TICKS.fetch_add(1, Ordering::SeqCst)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Op {
    code: i64,
    which: usize,
    i: usize,
    j: usize,
    id: i64,
}

/// An operation as a thread saw it.
#[derive(Clone, Copy, Debug)]
struct Event {
    thread: usize,
    op: Op,
    start: i64,
    end: i64,
    result: i64,
}

/// The cells, as they are after `reset!`.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct Model {
    boxes: [i64; 2],
    fields: [i64; 2],
    vectors: [Vec<i64>; 2],
}

impl Model {
    fn new() -> Self {
        Model {
            boxes: [1, 2],
            fields: [3, 4],
            vectors: [vec![5, 6, 7], vec![8]],
        }
    }

    /// Does `op` by itself, and gives what it returns.
    fn apply(&mut self, op: Op) -> i64 {
        let Op {
            code,
            which,
            i,
            j,
            id,
        } = op;
        match code {
            BOX_REF => self.boxes[which],
            BOX_SET | BOX_SET_IN_TWO_STEPS => core::mem::replace(&mut self.boxes[which], id),
            FIELD_REF => self.fields[which],
            FIELD_SET => core::mem::replace(&mut self.fields[which], id),
            VECTOR_REF => self.vectors[which].get(i).copied().unwrap_or(REFUSED),
            VECTOR_SET => match self.vectors[which].get_mut(i) {
                Some(slot) => {
                    *slot = id;
                    DONE
                }
                None => REFUSED,
            },
            VECTOR_PUSH => {
                self.vectors[which].push(id);
                DONE
            }
            VECTOR_POP => self.vectors[which].pop().unwrap_or(REFUSED),
            VECTOR_LENGTH => self.vectors[which].len() as i64,
            VECTOR_SWAP => {
                let vector = &mut self.vectors[which];
                if i < vector.len() && j < vector.len() {
                    vector.swap(i, j);
                    DONE
                } else {
                    REFUSED
                }
            }
            VECTOR_FILL => {
                self.vectors[which].iter_mut().for_each(|slot| *slot = id);
                DONE
            }
            VECTOR_CLEAR => {
                self.vectors[which].clear();
                DONE
            }
            VECTOR_APPEND => {
                let moved = core::mem::take(&mut self.vectors[1 - which]);
                self.vectors[which].extend(moved);
                DONE
            }
            _ => unreachable!("not an operation: {}", code),
        }
    }
}

/// Whether the events can be put in an order, one at a time, that gives each
/// the result it got and keeps every event after the ones that had finished
/// before it started.
fn linearizable(events: &[Event]) -> bool {
    assert!(events.len() <= 64, "the search keeps its place in 64 bits");
    let all = if events.len() == 64 {
        u64::MAX
    } else {
        (1u64 << events.len()) - 1
    };
    let mut tried = HashSet::new();
    search(events, 0, all, Model::new(), &mut tried)
}

fn search(
    events: &[Event],
    done: u64,
    all: u64,
    model: Model,
    tried: &mut HashSet<(u64, Model)>,
) -> bool {
    if done == all {
        return true;
    }
    if !tried.insert((done, model.clone())) {
        return false;
    }

    // Whatever comes next started before the first of the rest finished.
    let first_end = events
        .iter()
        .enumerate()
        .filter(|(index, _)| done & (1 << index) == 0)
        .map(|(_, event)| event.end)
        .min()
        .unwrap();

    for (index, event) in events.iter().enumerate() {
        if done & (1 << index) != 0 || event.start > first_end {
            continue;
        }
        let mut next = model.clone();
        if next.apply(event.op) == event.result
            && search(events, done | (1 << index), all, next, tried)
        {
            return true;
        }
    }

    false
}

/// A small generator, so the lists of operations are the same from run to
/// run and a failure names the round that made it.
struct Random(u64);

impl Random {
    fn next(&mut self) -> u64 {
        self.0 ^= self.0 << 13;
        self.0 ^= self.0 >> 7;
        self.0 ^= self.0 << 17;
        self.0
    }

    fn below(&mut self, n: u64) -> u64 {
        self.next() % n
    }
}

/// Which operations a test draws from, each as often as it is listed.
#[derive(Clone, Copy)]
struct Mix(&'static [i64]);

const EVERYTHING: Mix = Mix(&[
    BOX_REF,
    BOX_SET,
    FIELD_REF,
    FIELD_SET,
    VECTOR_REF,
    VECTOR_REF,
    VECTOR_SET,
    VECTOR_SET,
    VECTOR_PUSH,
    VECTOR_PUSH,
    VECTOR_POP,
    VECTOR_POP,
    VECTOR_LENGTH,
    VECTOR_SWAP,
    VECTOR_FILL,
    VECTOR_CLEAR,
    VECTOR_APPEND,
]);

const BOXES_AND_FIELDS: Mix = Mix(&[BOX_REF, BOX_SET, BOX_SET, FIELD_REF, FIELD_SET, FIELD_SET]);

const VECTORS_GROWING: Mix = Mix(&[
    VECTOR_REF,
    VECTOR_REF,
    VECTOR_SET,
    VECTOR_PUSH,
    VECTOR_PUSH,
    VECTOR_PUSH,
    VECTOR_POP,
    VECTOR_POP,
    VECTOR_LENGTH,
    VECTOR_APPEND,
]);

const VECTORS_IN_PLACE: Mix = Mix(&[
    VECTOR_REF,
    VECTOR_REF,
    VECTOR_SET,
    VECTOR_SET,
    VECTOR_SWAP,
    VECTOR_SWAP,
    VECTOR_FILL,
    VECTOR_LENGTH,
]);

// Each copy is two events, so these lists are kept short.
const COPIES: Mix = Mix(&[
    BOX_TO_SLOT,
    SLOT_TO_BOX,
    FIELD_TO_BOX,
    BOX_TO_FIELD,
    SLOT_TO_SLOT,
    FIELD_TO_FIELD,
    BOX_SET,
    FIELD_SET,
    VECTOR_SET,
    VECTOR_PUSH,
    VECTOR_POP,
]);

const COPIES_IN_PLACE: Mix = Mix(&[
    BOX_TO_SLOT,
    SLOT_TO_BOX,
    SLOT_TO_SLOT,
    SLOT_TO_SLOT,
    FIELD_TO_FIELD,
    BOX_SET,
    VECTOR_SET,
    VECTOR_SWAP,
]);

fn random_ops(random: &mut Random, mix: Mix, thread: usize, count: usize) -> Vec<Op> {
    (0..count)
        .map(|n| Op {
            code: mix.0[random.below(mix.0.len() as u64) as usize],
            which: random.below(2) as usize,
            i: random.below(5) as usize,
            j: random.below(5) as usize,
            // Never one of the ids the cells start with, or one another
            // operation stores.
            id: (100 + thread * 1000 + n) as i64,
        })
        .collect()
}

fn as_int(value: &SteelVal) -> i64 {
    match value {
        SteelVal::IntV(n) => *n as i64,
        other => panic!("expected an integer in the log, found {}", other),
    }
}

fn as_list(value: &SteelVal) -> Vec<SteelVal> {
    match value {
        SteelVal::ListV(list) => list.iter().cloned().collect(),
        other => panic!("expected a list in the log, found {}", other),
    }
}

/// Runs one round on real threads and gives back what each of them saw.
fn run_round(engine: &mut Engine, threads: &[Vec<Op>]) -> Vec<Event> {
    let mut program = String::from("(run-round '(");
    for ops in threads {
        program.push('(');
        for op in ops {
            program.push_str(&format!(
                "({} {} {} {} {}) ",
                op.code, op.which, op.i, op.j, op.id
            ));
        }
        program.push_str(") ");
    }
    program.push_str("))");

    let results = engine.run(program).expect("the round ran");
    let logs = as_list(results.last().expect("the round returns its logs"));
    assert_eq!(logs.len(), threads.len());

    let mut events = Vec::new();
    for (thread, (log, ops)) in logs.iter().zip(threads).enumerate() {
        let entries = as_list(log);
        assert_eq!(entries.len(), ops.len());
        for (entry, op) in entries.iter().zip(ops) {
            let entry = as_list(entry);
            let (start, end, result) = (as_int(&entry[0]), as_int(&entry[1]), as_int(&entry[2]));

            let Some((read, write)) = copy_halves(*op) else {
                events.push(Event {
                    thread,
                    op: *op,
                    start,
                    end,
                    result,
                });
                continue;
            };

            let (read_end, write_start, read_id) =
                (as_int(&entry[3]), as_int(&entry[4]), as_int(&entry[5]));
            events.push(Event {
                thread,
                op: read,
                start,
                end: read_end,
                result: read_id,
            });
            if result != SKIPPED {
                events.push(Event {
                    thread,
                    op: Op {
                        id: read_id,
                        ..write
                    },
                    start: write_start,
                    end,
                    result,
                });
            }
        }
    }
    events
}

/// The read and the write a copy is made of.
fn copy_halves(op: Op) -> Option<(Op, Op)> {
    let Op { which, i, j, .. } = op;
    let half = |code, which, i| Op {
        code,
        which,
        i,
        j: 0,
        id: 0,
    };

    Some(match op.code {
        BOX_TO_SLOT => (half(BOX_REF, which, 0), half(VECTOR_SET, which, i)),
        SLOT_TO_BOX => (half(VECTOR_REF, which, i), half(BOX_SET, which, 0)),
        FIELD_TO_BOX => (half(FIELD_REF, which, 0), half(BOX_SET, which, 0)),
        BOX_TO_FIELD => (half(BOX_REF, which, 0), half(FIELD_SET, which, 0)),
        SLOT_TO_SLOT => (half(VECTOR_REF, which, i), half(VECTOR_SET, which, j)),
        FIELD_TO_FIELD => (half(FIELD_REF, which, 0), half(FIELD_SET, 1 - which, 0)),
        _ => return None,
    })
}

fn describe(events: &[Event]) -> String {
    let mut sorted = events.to_vec();
    sorted.sort_by_key(|event| event.start);
    let name = |code| match code {
        BOX_REF => "unbox",
        BOX_SET => "set-box!",
        FIELD_REF => "field",
        FIELD_SET => "set-field!",
        VECTOR_REF => "vector-ref",
        VECTOR_SET => "vector-set!",
        VECTOR_PUSH => "vector-push!",
        VECTOR_POP => "vector-pop!",
        VECTOR_LENGTH => "vector-length",
        VECTOR_SWAP => "vector-swap!",
        VECTOR_FILL => "vector-fill!",
        VECTOR_CLEAR => "vector-clear",
        VECTOR_APPEND => "vector-append!",
        _ => "set-box! in two steps",
    };
    sorted
        .iter()
        .map(|event| {
            format!(
                "  thread {} [{:>6} .. {:>6}] {:<14} cell {} i {} j {} id {:>5} => {}\n",
                event.thread,
                event.start,
                event.end,
                name(event.op.code),
                event.op.which,
                event.op.i,
                event.op.j,
                event.op.id,
                event.result
            )
        })
        .collect()
}

/// Whether any two threads were part way through their lists at once.
fn overlapped(events: &[Event], threads: usize) -> bool {
    let spans: Vec<(i64, i64)> = (0..threads)
        .map(|thread| {
            let mine = events.iter().filter(|event| event.thread == thread);
            (
                mine.clone().map(|event| event.start).min().unwrap_or(0),
                mine.map(|event| event.end).max().unwrap_or(0),
            )
        })
        .collect();
    spans.iter().enumerate().any(|(a, first)| {
        spans
            .iter()
            .skip(a + 1)
            .any(|second| first.0 < second.1 && second.0 < first.1)
    })
}

fn rounds() -> usize {
    std::env::var("STEEL_HISTORY_ROUNDS")
        .ok()
        .and_then(|rounds| rounds.parse().ok())
        .unwrap_or(150)
}

fn engine() -> Engine {
    let mut engine = Engine::new();
    engine.register_fn("history-tick", tick);
    engine.register_steel_module("history-harness".to_owned(), HARNESS.to_owned());
    engine
        .run(r#"(require "history-harness")"#)
        .expect("the harness loads");
    engine
}

fn check_histories(seed: u64, mix: Mix, threads: usize, ops_per_thread: usize) {
    let mut engine = engine();
    let mut random = Random(seed);
    let rounds = rounds();
    let mut concurrent = 0;

    for round in 0..rounds {
        let ops: Vec<Vec<Op>> = (0..threads)
            .map(|thread| random_ops(&mut random, mix, thread, ops_per_thread))
            .collect();
        let events = run_round(&mut engine, &ops);

        assert!(
            events.iter().all(|event| event.result != BROKEN),
            "seed {} round {}: a thread read something that was never stored\n{}",
            seed,
            round,
            describe(&events)
        );
        assert!(
            linearizable(&events),
            "seed {} round {}: no order of whole operations gives these results\n{}",
            seed,
            round,
            describe(&events)
        );

        if overlapped(&events, threads) {
            concurrent += 1;
        }
    }

    // A round whose threads ran one after another checks nothing about
    // threads. Most of them have to have been at it together.
    assert!(
        concurrent * 2 >= rounds,
        "seed {}: only {} of {} rounds had threads running at once",
        seed,
        concurrent,
        rounds
    );
}

/// The threads here swap a box's value in two steps, so two of them can
/// each take the same old value. If the rounds really run at once and the
/// check really looks, one of them is refused before long.
///
/// How soon depends on the machine, so this is not run with the rest:
/// `cargo test -- --ignored a_swap_in_two_steps_is_caught`.
#[test]
#[ignore]
fn a_swap_in_two_steps_is_caught() {
    let mut engine = engine();
    let mut random = Random(0x1234567887654321);
    let mix = Mix(&[BOX_SET_IN_TWO_STEPS, BOX_SET_IN_TWO_STEPS, BOX_REF]);

    for round in 0..5000 {
        let ops: Vec<Vec<Op>> = (0..3)
            .map(|thread| random_ops(&mut random, mix, thread, 14))
            .collect();
        let events = run_round(&mut engine, &ops);
        if !linearizable(&events) {
            eprintln!("refused in round {}\n{}", round, describe(&events));
            return;
        }
    }

    panic!("5000 rounds of a swap in two steps and none was refused");
}

#[test]
fn histories_of_every_operation() {
    check_histories(0x9E3779B97F4A7C15, EVERYTHING, 3, 14);
}

#[test]
fn histories_of_boxes_and_fields() {
    check_histories(0xD1B54A32D192ED03, BOXES_AND_FIELDS, 4, 12);
}

#[test]
fn histories_of_growing_vectors() {
    check_histories(0x94D049BB133111EB, VECTORS_GROWING, 3, 16);
}

#[test]
fn histories_of_vectors_changed_in_place() {
    check_histories(0xBF58476D1CE4E5B9, VECTORS_IN_PLACE, 4, 12);
}

#[test]
fn histories_of_two_threads_at_length() {
    check_histories(0x2545F4914F6CDD1D, EVERYTHING, 2, 30);
}

#[test]
fn histories_of_values_copied_between_cells() {
    check_histories(0xA0761D6478BD642F, COPIES, 3, 9);
}

#[test]
fn histories_of_values_copied_in_place() {
    check_histories(0xE7037ED1A0B428DB, COPIES_IN_PLACE, 4, 7);
}

#[test]
fn histories_of_copies_by_two_threads() {
    check_histories(0x8EBC6AF09C88C6E3, COPIES, 2, 14);
}

// The checker has to refuse what it is there to catch.
mod the_checker {
    use super::*;

    fn event(thread: usize, code: i64, which: usize, i: usize, id: i64) -> Op {
        let _ = thread;
        Op {
            code,
            which,
            i,
            j: 0,
            id,
        }
    }

    fn at(thread: usize, op: Op, start: i64, end: i64, result: i64) -> Event {
        Event {
            thread,
            op,
            start,
            end,
            result,
        }
    }

    #[test]
    fn accepts_operations_one_after_another() {
        let events = [
            at(0, event(0, BOX_SET, 0, 0, 100), 0, 1, 1),
            at(1, event(1, BOX_REF, 0, 0, 0), 2, 3, 100),
            at(0, event(0, VECTOR_POP, 0, 0, 0), 4, 5, 7),
            at(1, event(1, VECTOR_LENGTH, 0, 0, 0), 6, 7, 2),
        ];
        assert!(linearizable(&events));
    }

    #[test]
    fn accepts_either_order_of_overlapping_operations() {
        // The read overlaps the write, so it may see the old value or the new.
        for seen in [1, 100] {
            let events = [
                at(0, event(0, BOX_SET, 0, 0, 100), 0, 3, 1),
                at(1, event(1, BOX_REF, 0, 0, 0), 1, 2, seen),
            ];
            assert!(linearizable(&events));
        }
    }

    #[test]
    fn refuses_a_read_of_a_value_not_yet_written() {
        let events = [
            at(1, event(1, BOX_REF, 0, 0, 0), 0, 1, 100),
            at(0, event(0, BOX_SET, 0, 0, 100), 2, 3, 1),
        ];
        assert!(!linearizable(&events));
    }

    #[test]
    fn refuses_a_stale_read() {
        let events = [
            at(0, event(0, BOX_SET, 0, 0, 100), 0, 1, 1),
            at(1, event(1, BOX_REF, 0, 0, 0), 2, 3, 1),
        ];
        assert!(!linearizable(&events));
    }

    #[test]
    fn refuses_one_element_popped_twice() {
        let events = [
            at(0, event(0, VECTOR_POP, 0, 0, 0), 0, 3, 7),
            at(1, event(1, VECTOR_POP, 0, 0, 0), 1, 2, 7),
        ];
        assert!(!linearizable(&events));
    }

    #[test]
    fn refuses_a_lost_push() {
        let events = [
            at(0, event(0, VECTOR_PUSH, 1, 0, 100), 0, 3, 0),
            at(1, event(1, VECTOR_PUSH, 1, 0, 1100), 1, 2, 0),
            at(0, event(0, VECTOR_LENGTH, 1, 0, 0), 4, 5, 2),
        ];
        assert!(!linearizable(&events));
    }

    #[test]
    fn refuses_two_swaps_that_both_return_the_first_value() {
        let events = [
            at(0, event(0, BOX_SET, 0, 0, 100), 0, 3, 1),
            at(1, event(1, BOX_SET, 0, 0, 1100), 1, 2, 1),
        ];
        assert!(!linearizable(&events));
    }

    #[test]
    fn refuses_a_vector_seen_half_filled() {
        // After the fill both slots hold 100, and nothing else writes them.
        let events = [
            at(0, event(0, VECTOR_FILL, 0, 0, 100), 0, 5, 0),
            at(1, event(1, VECTOR_REF, 0, 0, 0), 1, 2, 100),
            at(1, event(1, VECTOR_REF, 0, 1, 0), 3, 4, 6),
        ];
        assert!(!linearizable(&events));
    }
}
