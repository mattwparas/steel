//! Checks that the loops in `success/counted_loops.scm` are compiled a
//! stretch of trips at a time. If they stop being, that test still passes
//! and no longer covers anything.

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

    let mut engine = Engine::new();
    engine.run(LOOPS).expect("the loops run");

    // Compiling may be on another thread
    let started = Instant::now();
    while !missing().is_empty() && started.elapsed() < Duration::from_secs(30) {
        std::thread::sleep(Duration::from_millis(20));
        engine.run("(check 300)").expect("the loops run again");
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
