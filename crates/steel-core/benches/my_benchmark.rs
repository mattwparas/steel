#![allow(unused)]

use criterion::{black_box, criterion_group, criterion_main, Criterion};

use steel::stdlib::PRELUDE;
use steel::steel_vm::{engine::Engine, register_fn::RegisterFn};

fn benchmark_template(c: &mut Criterion, name: &str, script: &str, warmup: &str) {
    let mut vm = Engine::new();
    vm.compile_and_run_raw_program(PRELUDE).unwrap();
    vm.compile_and_run_raw_program(black_box(warmup)).unwrap();

    let program = vm.emit_raw_program_no_path(script).unwrap();
    let executable = vm.raw_program_to_executable(program).unwrap();

    c.bench_function(name, |b| b.iter(|| vm.run_executable(&executable)));
}

fn range(c: &mut Criterion) {
    let script = "(range 0 5000)";

    let mut vm = Engine::new();

    vm.compile_and_run_raw_program(PRELUDE).unwrap();

    let program = vm.emit_raw_program_no_path(script).unwrap();
    let executable = vm.raw_program_to_executable(program).unwrap();

    c.bench_function("range-big", |b| b.iter(|| vm.run_executable(&executable)));
}

fn map(c: &mut Criterion) {
    let script = "(map a lst)";
    let warmup = "(define lst (range 0 5000)) (define a (lambda (a) 0))";
    benchmark_template(c, "map-big", script, warmup);
}

fn transducer_map(c: &mut Criterion) {
    let script = "(transduce lst a (into-list))";
    let warmup = "(define lst (range 0 10000)) (define a (mapping (lambda (a) (* a 2))))";
    benchmark_template(c, "transducer-map", script, warmup);
}

fn filter(c: &mut Criterion) {
    let script = "(filter number? lst)";
    let warmup = "(define lst (range 0 5000))";
    benchmark_template(c, "filter-big", script, warmup);
}

fn multiple_transducers(c: &mut Criterion) {
    let warmup = "(define lst (range 0 50000))";
    let script = r#"
        (transduce lst (compose
                    (mapping (fn (x) (* x 2)))
                    (filtering even?)
                    (mapping (fn (x) (+ x 25)))
                    (taking 25000)
                    (taking 25))
                    (into-list))
    "#;
    benchmark_template(c, "multiple-transducers", script, warmup);
}

fn ackermann(c: &mut Criterion) {
    let warmup = r#"
    (define (ackermann m n)
        (cond [(equal? m 0) (+ n 1)]
            [(equal? n 0) (ackermann (- m 1) 1)]
            [else (ackermann (- m 1) (ackermann m (- n 1)))]))"#;
    let script = r#"(ackermann 3 3)"#;
    benchmark_template(c, "ackermann-3-3", script, warmup);
}

fn ten_thousand_iterations(c: &mut Criterion) {
    let script = "(test 0)";
    let warmup = "(define test (lambda (x) (if (= x 10000) x (test (+ x 1)))))";
    benchmark_template(c, "ten-thousand-iterations", script, warmup);
}

fn ten_thousand_iterations_letrec(c: &mut Criterion) {
    let script = "(test)";
    let warmup = r#"(define (test)
                            (let ((loop void))
                                (let ((loop-prime (lambda (x) 
                                                    (if (= x 10000)
                                                        x
                                                        (loop (+ x 1))))))
                                    (set! loop loop-prime))
                            (loop 0)))"#;

    benchmark_template(c, "ten-thousand-iterations-letrec", script, warmup);
}

fn trie_sort(c: &mut Criterion) {
    let mut vm = Engine::new();
    // vm.compile_and_run_raw_program(PRELUDE).unwrap();
    vm.compile_and_run_raw_program(steel::stdlib::TRIESORT)
        .unwrap();

    let warmup = "(define lst
        (list
         \"suppose\"
         \"believe\"
         \"changeable\"
         \"absent\"
         \"busy\"
         \"float\"
         \"debonair\"
         \"throat\"
         \"grey\"
         \"use\"
         \"measure\"
         \"van\"
         \"thirsty\"
         \"notify\"
         \"star\"))";

    vm.compile_and_run_raw_program(black_box(warmup)).unwrap();

    let script = "(trie-sort lst)";

    let program = vm.emit_raw_program_no_path(script).unwrap();
    let executable = vm.raw_program_to_executable(program).unwrap();

    c.bench_function("trie-sort-without-optimizations", |b| {
        b.iter(|| vm.run_executable(&executable))
    });
}

fn fib_28(c: &mut Criterion) {
    // std::env::set_var("CODE_GEN_V2", "true");

    let mut vm = Engine::new();
    // vm.compile_and_run_raw_program(PRELUDE).unwrap();
    vm.compile_and_run_raw_program(
        "(define (fib n) (if (<= n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))",
    )
    .unwrap();

    let script = "(fib 28)";
    let program = vm.emit_raw_program_no_path(script).unwrap();
    let executable = vm.raw_program_to_executable(program).unwrap();

    let mut group = c.benchmark_group("fib-28");
    group.sample_size(200);
    group.bench_function("fib-28", |b| b.iter(|| vm.run_executable(&executable)));
    group.finish();
}

fn thread_creation(c: &mut Criterion) {
    let mut vm = Engine::new();
    vm.compile_and_run_raw_program(
        r#"
(define (foo x)
  (vector 10 20 30 40 x))

(define (block)
    (thread-join! (spawn-thread! (lambda () (vector-ref (foo 100) 4))))) 
"#,
    )
    .unwrap();

    let script = "(block)";
    let program = vm.emit_raw_program_no_path(script).unwrap();
    let executable = vm.raw_program_to_executable(program).unwrap();

    let mut group = c.benchmark_group("thread-creation");
    // group.sample_size(200);
    group.bench_function("thread-creation", |b| {
        b.iter(|| vm.run_executable(&executable))
    });
    group.finish();
}

fn fib_28_contract(c: &mut Criterion) {
    let mut vm = Engine::new();
    vm.compile_and_run_raw_program(PRELUDE).unwrap();
    vm.compile_and_run_raw_program(
        r#"(define/contract (fib n) 
                (->/c integer? integer?)
                (if (<= n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))"#,
    )
    .unwrap();

    let script = "(fib 28)";
    let program = vm.emit_raw_program_no_path(script).unwrap();
    let executable = vm.raw_program_to_executable(program).unwrap();

    let mut group = c.benchmark_group("contract-fib-28");
    group.sample_size(200);
    group.bench_function("contract-fib-28", |b| {
        b.iter(|| vm.run_executable(&executable))
    });
    group.finish();
}

// This will include the definition inside the bench
// just to match against the Rhai benchmarks
// fn fib_20(c: &mut Criterion) {
//     let mut vm = Engine::new();
//     vm.compile_and_run_raw_program(PRELUDE).unwrap();

//     let script = "(define (fib n) (if (<= n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))) (fib 20)";

//     let program = vm.emit_raw_program_no_path(&script).unwrap();
//     let executable = vm.raw_program_to_executable(program).unwrap();

//     c.bench_function("fib-20", |b| {
//         b.iter(|| {
//             vm.execute(Rc::clone(&definition), &constant_map).unwrap();
//             vm.execute(Rc::clone(&bytecode), &constant_map)
//         })
//     });
// }

fn engine_creation(c: &mut Criterion) {
    c.bench_function("engine-creation", |b| b.iter(Engine::new));
}

fn register_function(c: &mut Criterion) {
    let mut vm = Engine::new();
    let f: fn(usize, usize) -> usize = |a: usize, b: usize| a + b;
    let name: &'static str = "addition";
    c.bench_function("register-fn", |b| {
        b.iter(|| {
            let _ = vm.register_fn(name, f);
        })
    });
}

fn binary_trees(c: &mut Criterion) {
    let mut vm = Engine::new();
    vm.compile_and_run_raw_program(
        r#"

; #lang racket/base

;;; The Computer Language Benchmarks Game
;;; https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

;;; Derived from the Chicken variant by Sven Hartrumpf
;;; contributed by Matthew Flatt
;;; *reset*

; (require racket/cmdline)

(struct node (left val right))

;; Instead of (define-struct leaf (val)):
(define (leaf val)
  (node #f val #f))
(define (leaf? l)
  (not (node-left l)))
(define (leaf-val l)
  node-val)

(define (make item d)
  (if (= d 0)
      (leaf item)
      (%plain-let ((item2 (* item 2)) (d2 (- d 1)))
                  (node (make (- item2 1) d2) item (make item2 d2)))))

(define (check t)
  (if (leaf? t) 1 (+ 1 (+ (check (node-left t)) (check (node-right t))))))

(define (iterate n m d sum)
  (if (equal? n m) sum (iterate (+ n 1) m d (+ sum (check (make n d))))))

(define (max x y)
  (if (> x y) x y))

(define (loop d end max-depth min-depth)
  (if (>= d end)
      void
      (begin
        (let ([iterations (arithmetic-shift 1 (+ (- max-depth d) min-depth))])
          (displayln iterations " trees of depth " d " check: " (iterate 0 iterations d 0)))
        (loop (+ 2 d) end max-depth min-depth))))

(define (main n)
  (let* ([min-depth 4] [max-depth (max (+ min-depth 2) n)])
    (let ([stretch-depth (+ max-depth 1)])
      (displayln "stretch tree of depth " stretch-depth " check: " (check (make 0 stretch-depth))))
    (let ([long-lived-tree (make 0 max-depth)])
      ; (begin
      ; (define end )

      (loop 4 (add1 max-depth) max-depth min-depth)

      ; )

      (displayln "long lived tree of depth " max-depth " check: " (check long-lived-tree)))))

            
        "#,
    )
    .unwrap();

    let script = "(main 12)";
    let program = vm.emit_raw_program_no_path(script).unwrap();
    let executable = vm.raw_program_to_executable(program).unwrap();

    let mut group = c.benchmark_group("binary-trees");
    group.bench_function("binary-trees", |b| {
        b.iter(|| vm.run_executable(&executable))
    });
    group.finish();
}

criterion_group!(
    benches,
    range,
    map,
    transducer_map,
    filter,
    ten_thousand_iterations,
    ten_thousand_iterations_letrec,
    trie_sort,
    fib_28,
    thread_creation,
    engine_creation,
    register_function,
    multiple_transducers,
    binary_trees,
    // fib_28_contract,
    ackermann // trie_sort,
              // merge_sort,
              // struct_construct,
              // struct_construct_bigger,
              // struct_get,
              // struct_set
);

criterion_main!(benches);
