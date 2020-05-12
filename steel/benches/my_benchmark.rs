use criterion::{black_box, criterion_group, criterion_main, Criterion};

use steel::interpreter::SteelInterpreter;
use steel::PRELUDE;

fn range(c: &mut Criterion) {
    let mut interpreter = SteelInterpreter::new();
    interpreter.require(PRELUDE).unwrap();

    let script = "(range 0 50000)";

    c.bench_function("(range 0 50000)", |b| {
        b.iter(|| interpreter.evaluate(black_box(&script)))
    });
}

fn map(c: &mut Criterion) {
    let mut interpreter = SteelInterpreter::new();
    interpreter.require(PRELUDE).unwrap();

    let warmup = "(define lst (range 0 50000))";
    interpreter.evaluate(black_box(&warmup)).unwrap();

    let script = "(map (lambda (a) 0) lst)";

    c.bench_function("(map (lambda (a) 0) (range 0 50000))", |b| {
        b.iter(|| interpreter.evaluate(black_box(&script)))
    });
}

fn filter(c: &mut Criterion) {
    let mut interpreter = SteelInterpreter::new();
    interpreter.require(PRELUDE).unwrap();

    let warmup = "(define lst (range 0 50000))";
    interpreter.evaluate(black_box(&warmup)).unwrap();

    let script = "(filter number? lst)";

    c.bench_function("(filter number? (range 0 50000))", |b| {
        b.iter(|| interpreter.evaluate(black_box(&script)))
    });
}

fn trie_sort(c: &mut Criterion) {
    let mut interpreter = SteelInterpreter::new();
    interpreter.require(PRELUDE).unwrap();
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

criterion_group!(benches, range, map, filter, trie_sort);
criterion_main!(benches);
