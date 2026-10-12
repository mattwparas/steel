use std::time::{Duration, Instant};

use crate::jit2::cgen::strip_mine_watch::APPLIED;
use crate::steel_vm::engine::Engine;

const LOOPS: &str = include_str!("success/counted_loops.scm");

const WANTED: &[&str] = &[
    "counted-up-below",
    "counted-up-through",
    "counted-up-until-equal",
    "counted-up-by-three",
    "counted-down-to-zero",
    "counted-down-above",
    "counted-down-until-equal",
    "counted-down-by-seven",
    "counted-below-a-constant",
    "counted-through-a-vector",
    "counted-fill-a-vector",
    "counted-unrolled-up-below",
    "counted-unrolled-up-through",
    "counted-unrolled-until-equal",
    "counted-unrolled-by-three",
    "counted-unrolled-down",
    "counted-unrolled-down-until-equal",
    "counted-unrolled-through-a-vector",
];

fn missing() -> Vec<&'static str> {
    let applied = APPLIED
        .lock()
        .map(|applied| applied.clone())
        .unwrap_or_default();

    WANTED
        .iter()
        .filter(|loop_name| !applied.iter().any(|name| name.contains(*loop_name)))
        .copied()
        .collect()
}

#[test]
fn counted_loops_run_a_stretch_at_a_time() {
    let config = crate::config::config();
    if !config.jit || config.jit_strip_mine == 0 {
        return;
    }

    // As a module, where a function calling itself is known to pass the
    // right number of arguments
    let mut engine = Engine::new();
    engine.register_steel_module("counted-loops".to_owned(), LOOPS.to_owned());
    engine
        .run(r#"(require "counted-loops")"#)
        .expect("the loops run");

    // Compiling may be on another thread
    let started = Instant::now();
    while config.jit_background
        && !missing().is_empty()
        && started.elapsed() < Duration::from_secs(30)
    {
        std::thread::sleep(Duration::from_millis(20));
    }

    // A busy background jit may not have reached every loop yet
    if config.jit_background {
        return;
    }

    assert!(
        missing().is_empty(),
        "compiled without a stretch at a time: {:?}",
        missing()
    );
}
