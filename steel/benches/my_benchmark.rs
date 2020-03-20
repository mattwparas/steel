use criterion::{black_box, criterion_group, criterion_main, Criterion};

use steel::interpreter::SteelInterpreter;
use steel::PRELUDE;

fn range(c: &mut Criterion) {
    let mut interpreter = SteelInterpreter::new();
    interpreter.require(PRELUDE).unwrap();
    let script = "(range 0 100)";

    c.bench_function("(range 0 100)", |b| {
        b.iter(|| interpreter.evaluate(black_box(&script)))
    });

    let script = "(range 0 1000)";

    c.bench_function("(range 0 1000)", |b| {
        b.iter(|| interpreter.evaluate(black_box(&script)))
    });
}

fn map(c: &mut Criterion) {
    let mut interpreter = SteelInterpreter::new();
    interpreter.require(PRELUDE).unwrap();
    let script = "(map (lambda (a) 0) (range 0 100))";

    c.bench_function("(map (lambda (a) 0) (range 0 100))", |b| {
        b.iter(|| interpreter.evaluate(black_box(&script)))
    });

    let script = "(map (lambda (a) 0) (range 0 1000))";

    c.bench_function("(map (lambda (a) 0) (range 0 1000))", |b| {
        b.iter(|| interpreter.evaluate(black_box(&script)))
    });
}

fn filter(c: &mut Criterion) {
    let mut interpreter = SteelInterpreter::new();
    interpreter.require(PRELUDE).unwrap();
    let script = "(filter number? (range 0 100))";

    c.bench_function("(filter number? (range 0 100))", |b| {
        b.iter(|| interpreter.evaluate(black_box(&script)))
    });

    let script = "(filter number? (range 0 1000))";

    c.bench_function("(filter number? (range 0 100))", |b| {
        b.iter(|| interpreter.evaluate(black_box(&script)))
    });
}

criterion_group!(benches, range, map, filter);
criterion_main!(benches);
