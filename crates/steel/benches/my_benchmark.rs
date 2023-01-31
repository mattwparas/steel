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

fn trie_sort_without_optimizations(c: &mut Criterion) {
    let mut vm = Engine::new();
    vm.compile_and_run_raw_program(PRELUDE).unwrap();
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

fn trie_sort_with_optimizations(c: &mut Criterion) {
    let mut vm = Engine::new();
    vm.compile_and_run_raw_program(PRELUDE).unwrap();
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

    c.bench_function("trie-sort-with-optimizations", |b| {
        b.iter(|| vm.run_executable(&executable))
    });
}

fn fib_28(c: &mut Criterion) {
    // std::env::set_var("CODE_GEN_V2", "true");

    let mut vm = Engine::new();
    vm.compile_and_run_raw_program(PRELUDE).unwrap();
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

/*

fn trie_sort(c: &mut Criterion) {
    let mut interpreter = SteelInterpreter::new();
    // interpreter.require(PRELUDE).unwrap();
    // require the trie sort library
    interpreter.require(steel::stdlib::TRIESORT).unwrap();

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
    interpreter.evaluate(black_box(&warmup)).unwrap();
    let script = "(trie-sort lst)";
    c.bench_function("trie-sort", |b| {
        b.iter(|| interpreter.evaluate(black_box(&script)))
    });
}

fn merge_sort(c: &mut Criterion) {
    let mut interpreter = SteelInterpreter::new();
    // interpreter.require(PRELUDE).unwrap();
    let warmup = "
;;; -----------------------------------------------------------------
;;; Merge two lists of numbers which are already in increasing order

  (define merge-lists
    (lambda (l1 l2)
      (if (null? l1)
          l2
          (if (null? l2)
              l1
              (if (< (car l1) (car l2))
                  (cons (car l1) (merge-lists (cdr l1) l2))
                  (cons (car l2) (merge-lists (cdr l2) l1)))))))

;;; -------------------------------------------------------------------
;;; Given list l, output those tokens of l which are in even positions

  (define even-numbers
    (lambda (l)
      (if (null? l)
          '()
          (if (null? (cdr l))
              '()
              (cons (car (cdr l)) (even-numbers (cdr (cdr l))))))))

;;; -------------------------------------------------------------------
;;; Given list l, output those tokens of l which are in odd positions

  (define odd-numbers
    (lambda (l)
      (if (null? l)
          '()
          (if (null? (cdr l))
              (list (car l))
              (cons (car l) (odd-numbers (cdr (cdr l))))))))

;;; ---------------------------------------------------------------------
;;; Use the procedures above to create a simple and efficient merge-sort

  (define merge-sort
    (lambda (l)
      (if (null? l)
          l
          (if (null? (cdr l))
              l
              (merge-lists
                (merge-sort (odd-numbers l))
                (merge-sort (even-numbers l)))))))
    (define lst
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
            \"star\"))
    ";
    interpreter.evaluate(black_box(&warmup)).unwrap();
    let script = "(merge-sort lst)";
    c.bench_function("merge-sort", |b| {
        b.iter(|| interpreter.evaluate(black_box(&script)))
    });
}

fn struct_construct(c: &mut Criterion) {
    let mut interpreter = SteelInterpreter::new();
    // interpreter.require(PRELUDE).unwrap();
    let warmup = "(struct node (left right))";
    interpreter.evaluate(black_box(&warmup)).unwrap();
    let script = "(node (list 1 2 3 4) (list 1 2 3 4))";
    c.bench_function("struct-construct", |b| {
        b.iter(|| interpreter.evaluate(black_box(&script)))
    });
}

fn struct_construct_bigger(c: &mut Criterion) {
    let mut interpreter = SteelInterpreter::new();
    // interpreter.require(PRELUDE).unwrap();
    let warmup = "(struct node (left right middle back))";
    interpreter.evaluate(black_box(&warmup)).unwrap();
    let script = "(node (list 1 2 3 4) (list 1 2 3 4) (list 1 2 3 4) (list 1 2 3 4))";
    c.bench_function("struct-construct-big", |b| {
        b.iter(|| interpreter.evaluate(black_box(&script)))
    });
}

fn struct_get(c: &mut Criterion) {
    let mut interpreter = SteelInterpreter::new();
    // interpreter.require(PRELUDE).unwrap();
    let warmup = "(struct node (left right)) (define test (node (list 1 2 3) (list 1 2 3)))";
    interpreter.evaluate(black_box(&warmup)).unwrap();
    let script = "(node-left test)";
    c.bench_function("struct-get", |b| {
        b.iter(|| interpreter.evaluate(black_box(&script)))
    });
}

fn struct_set(c: &mut Criterion) {
    let mut interpreter = SteelInterpreter::new();
    // interpreter.require(PRELUDE).unwrap();
    let warmup = "(struct node (left right)) (define test (node (list 1 2 3) (list 1 2 3)))";
    interpreter.evaluate(black_box(&warmup)).unwrap();
    let script = "(set-node-left! test (list 1 2 3))";
    c.bench_function("struct-set", |b| {
        b.iter(|| interpreter.evaluate(black_box(&script)))
    });
}

// fn function_applications(c: &mut Criterion) {
//     let mut interpreter = SteelInterpreter::new();
//     interpreter.require(PRELUDE).unwrap();
// }

*/

criterion_group!(
    benches,
    range,
    map,
    transducer_map,
    filter,
    ten_thousand_iterations,
    ten_thousand_iterations_letrec,
    trie_sort_without_optimizations,
    trie_sort_with_optimizations,
    fib_28,
    engine_creation,
    register_function,
    multiple_transducers,
    // fib_28_contract,
    ackermann // trie_sort,
              // merge_sort,
              // struct_construct,
              // struct_construct_bigger,
              // struct_get,
              // struct_set
);

criterion_main!(benches);
