use criterion::{black_box, criterion_group, criterion_main, Criterion};

// use steel::env::Env;
// use steel::interpreter::SteelInterpreter;
use steel::stdlib::PRELUDE;
// use steel::vm::ArityMap;
// use steel::vm::ConstantMap;
// use steel::vm::ConstantTable;
use std::rc::Rc;
// use steel::vm::Ctx;
use steel::vm::VirtualMachine;
// use steel::PRELUDE;

fn range(c: &mut Criterion) {
    let script = "(range 0 50000)";

    let mut vm = VirtualMachine::new();
    // let mut ctx: Ctx<ConstantMap> = Ctx::new(
    //     Env::default_symbol_map(),
    //     ConstantMap::new(),
    //     ArityMap::new(),
    //     false,
    // );

    vm.parse_and_execute(PRELUDE).unwrap();

    let bytecode = Rc::new(
        vm.emit_instructions(&script).unwrap()[0]
            .clone()
            .into_boxed_slice(),
    );
    // Rc::new(x.into_boxed_slice()), &ctx.constant_map

    c.bench_function("range-big", |b| {
        b.iter(|| vm.execute(Rc::clone(&bytecode), false))
    });
}

fn map(c: &mut Criterion) {
    let script = "(map (lambda (a) 0) lst)";

    let mut vm = VirtualMachine::new();
    // let mut ctx: Ctx<ConstantMap> = Ctx::new(
    //     Env::default_symbol_map(),
    //     ConstantMap::new(),
    //     ArityMap::new(),
    //     false,
    // );

    vm.parse_and_execute(PRELUDE).unwrap();

    let warmup = "(define lst (range 0 50000))";
    vm.parse_and_execute(black_box(&warmup)).unwrap();

    let bytecode = Rc::new(
        vm.emit_instructions(&script).unwrap()[0]
            .clone()
            .into_boxed_slice(),
    );

    c.bench_function("map-big", |b| {
        b.iter(|| vm.execute(Rc::clone(&bytecode), false))
    });
}

fn filter(c: &mut Criterion) {
    // let mut interpreter = SteelInterpreter::new();
    // interpreter.require(PRELUDE).unwrap();

    let script = "(filter number? lst)";

    let mut vm = VirtualMachine::new();
    // let mut ctx: Ctx<ConstantMap> = Ctx::new(
    //     Env::default_symbol_map(),
    //     ConstantMap::new(),
    //     ArityMap::new(),
    //     false,
    // );

    vm.parse_and_execute(PRELUDE).unwrap();

    let warmup = "(define lst (range 0 50000))";
    vm.parse_and_execute(black_box(&warmup)).unwrap();

    let bytecode = Rc::new(
        vm.emit_instructions(&script).unwrap()[0]
            .clone()
            .into_boxed_slice(),
    );

    c.bench_function("filter-big", |b| {
        b.iter(|| vm.execute(Rc::clone(&bytecode), false))
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
    benches, range, map,
    filter,
    // trie_sort,
    // merge_sort,
    // struct_construct,
    // struct_construct_bigger,
    // struct_get,
    // struct_set
);

criterion_main!(benches);
