use criterion::{black_box, criterion_group, criterion_main, Criterion};
use steel::compiler::constants::ConstantMap;

use std::rc::Rc;
use steel::stdlib::PRELUDE;
use steel_vm::{engine::Engine, register_fn::RegisterFn};

fn range(c: &mut Criterion) {
    let script = "(range 0 5000)";

    let mut vm = Engine::new();
    // let mut ctx: Ctx<ConstantMap> = Ctx::new(
    //     Env::default_symbol_map(),
    //     ConstantMap::new(),
    //     ArityMap::new(),
    //     false,
    // );

    vm.parse_and_execute_without_optimizations(PRELUDE).unwrap();

    let program = vm.emit_program(&script).unwrap();
    let constant_map = ConstantMap::from_bytes(&program.constant_map).unwrap();
    let bytecode = Rc::from(program.instructions[0].clone().into_boxed_slice());

    // let bytecode = vm.emit_program(&script).unwrap();

    // Rc::new(x.into_boxed_slice()), &ctx.constant_map

    c.bench_function("range-big", |b| {
        b.iter(|| vm.execute(Rc::clone(&bytecode), &constant_map))
    });
}

fn map(c: &mut Criterion) {
    let script = "(map a lst)";

    let mut vm = Engine::new();
    // let mut ctx: Ctx<ConstantMap> = Ctx::new(
    //     Env::default_symbol_map(),
    //     ConstantMap::new(),
    //     ArityMap::new(),
    //     false,
    // );

    vm.parse_and_execute_without_optimizations(PRELUDE).unwrap();

    let warmup = "(define lst (range 0 5000)) (define a (lambda (a) 0))";
    vm.parse_and_execute_without_optimizations(black_box(warmup))
        .unwrap();

    let program = vm.emit_program(&script).unwrap();
    let constant_map = ConstantMap::from_bytes(&program.constant_map).unwrap();
    let bytecode = Rc::from(program.instructions[0].clone().into_boxed_slice());

    c.bench_function("map-big", |b| {
        b.iter(|| vm.execute(Rc::clone(&bytecode), &constant_map))
    });
}

fn transducer_map(c: &mut Criterion) {
    let script = "(execute a lst)";

    let mut vm = Engine::new();
    // let mut ctx: Ctx<ConstantMap> = Ctx::new(
    //     Env::default_symbol_map(),
    //     ConstantMap::new(),
    //     ArityMap::new(),
    //     false,
    // );

    vm.parse_and_execute_without_optimizations(PRELUDE).unwrap();

    let warmup = "(define lst (range 0 5000)) (define a (mapping (lambda (a) 0)))";
    vm.parse_and_execute_without_optimizations(black_box(&warmup))
        .unwrap();

    let program = vm.emit_program(&script).unwrap();
    let constant_map = ConstantMap::from_bytes(&program.constant_map).unwrap();
    let bytecode = Rc::from(program.instructions[0].clone().into_boxed_slice());

    c.bench_function("transducer-map", |b| {
        b.iter(|| vm.execute(Rc::clone(&bytecode), &constant_map))
    });
}

fn filter(c: &mut Criterion) {
    // let mut interpreter = SteelInterpreter::new();
    // interpreter.require(PRELUDE).unwrap();

    let script = "(filter number? lst)";

    let mut vm = Engine::new();
    // let mut ctx: Ctx<ConstantMap> = Ctx::new(
    //     Env::default_symbol_map(),
    //     ConstantMap::new(),
    //     ArityMap::new(),
    //     false,
    // );

    vm.parse_and_execute_without_optimizations(PRELUDE).unwrap();

    let warmup = "(define lst (range 0 5000))";
    vm.parse_and_execute_without_optimizations(black_box(&warmup))
        .unwrap();

    let program = vm.emit_program(&script).unwrap();
    let constant_map = ConstantMap::from_bytes(&program.constant_map).unwrap();
    let bytecode = Rc::from(program.instructions[0].clone().into_boxed_slice());

    c.bench_function("filter-big", |b| {
        b.iter(|| vm.execute(Rc::clone(&bytecode), &constant_map))
    });
}

fn ten_thousand_iterations(c: &mut Criterion) {
    let script = "(test 0)";
    let mut vm = Engine::new();

    let warmup = "(define test (lambda (x) (if (= x 10000) x (test (+ x 1)))))";
    vm.parse_and_execute_without_optimizations(black_box(&warmup))
        .unwrap();
    vm.parse_and_execute_without_optimizations(PRELUDE).unwrap();

    let program = vm.emit_program(&script).unwrap();
    let constant_map = ConstantMap::from_bytes(&program.constant_map).unwrap();
    let bytecode = Rc::from(program.instructions[0].clone().into_boxed_slice());

    c.bench_function("ten-thousand-iterations", |b| {
        b.iter(|| vm.execute(Rc::clone(&bytecode), &constant_map))
    });
}

fn trie_sort_without_optimizations(c: &mut Criterion) {
    let mut vm = Engine::new();
    // interpreter.require(PRELUDE).unwrap();
    // require the trie sort library
    vm.parse_and_execute_without_optimizations(PRELUDE).unwrap();
    vm.parse_and_execute_without_optimizations(steel::stdlib::TRIESORT)
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

    vm.parse_and_execute_without_optimizations(black_box(&warmup))
        .unwrap();

    let script = "(trie-sort lst)";
    let program = vm.emit_program(&script).unwrap();
    let constant_map = ConstantMap::from_bytes(&program.constant_map).unwrap();
    let bytecode = Rc::from(program.instructions[0].clone().into_boxed_slice());

    c.bench_function("trie-sort-without-optimizations", |b| {
        b.iter(|| vm.execute(Rc::clone(&bytecode), &constant_map))
    });
}

fn trie_sort_with_optimizations(c: &mut Criterion) {
    let mut vm = Engine::new();
    // interpreter.require(PRELUDE).unwrap();
    // require the trie sort library
    vm.parse_and_execute_without_optimizations(PRELUDE).unwrap();
    vm.parse_and_execute(steel::stdlib::TRIESORT).unwrap();

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

    vm.parse_and_execute_without_optimizations(black_box(&warmup))
        .unwrap();

    let script = "(trie-sort lst)";
    let program = vm.emit_program(&script).unwrap();
    let constant_map = ConstantMap::from_bytes(&program.constant_map).unwrap();
    let bytecode = Rc::from(program.instructions[0].clone().into_boxed_slice());

    c.bench_function("trie-sort-with-optimizations", |b| {
        b.iter(|| vm.execute(Rc::clone(&bytecode), &constant_map))
    });
}

fn fib_28(c: &mut Criterion) {
    let mut vm = Engine::new();
    // interpreter.require(PRELUDE).unwrap();
    // require the trie sort library
    vm.parse_and_execute_without_optimizations(PRELUDE).unwrap();
    vm.parse_and_execute_without_optimizations(
        "(define (fib n) (if (<= n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))",
    )
    .unwrap();

    let script = "(fib 28)";
    let program = vm.emit_program(&script).unwrap();
    let constant_map = ConstantMap::from_bytes(&program.constant_map).unwrap();
    let bytecode = Rc::from(program.instructions[0].clone().into_boxed_slice());

    c.bench_function("fib-28", |b| {
        b.iter(|| vm.execute(Rc::clone(&bytecode), &constant_map))
    });
}

// This will include the definition inside the bench
// just to match against the Rhai benchmarks
fn fib_20(c: &mut Criterion) {
    let mut vm = Engine::new();
    // interpreter.require(PRELUDE).unwrap();
    // require the trie sort library
    vm.parse_and_execute_without_optimizations(PRELUDE).unwrap();

    let script = "(define (fib n) (if (<= n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))) (fib 20)";
    let program = vm.emit_program(&script).unwrap();
    let constant_map = ConstantMap::from_bytes(&program.constant_map).unwrap();

    let definition = Rc::from(program.instructions[0].clone().into_boxed_slice());
    let bytecode = Rc::from(program.instructions[1].clone().into_boxed_slice());

    c.bench_function("fib-20", |b| {
        b.iter(|| {
            vm.execute(Rc::clone(&definition), &constant_map).unwrap();
            vm.execute(Rc::clone(&bytecode), &constant_map)
        })
    });
}

fn engine_creation(c: &mut Criterion) {
    c.bench_function("engine-creation", |b| b.iter(|| Engine::new()));
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
    trie_sort_without_optimizations,
    trie_sort_with_optimizations,
    fib_28,
    fib_20,
    engine_creation,
    register_function, // trie_sort,
                       // merge_sort,
                       // struct_construct,
                       // struct_construct_bigger,
                       // struct_get,
                       // struct_set
);

criterion_main!(benches);
