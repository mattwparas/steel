use alloc::vec::Vec;
use std::{collections::HashMap, sync::Mutex};

use steel_gen::OpCode;

use crate::core::instructions::DenseInstruction;

static PROFILER: std::sync::LazyLock<Mutex<Profiler>> =
    std::sync::LazyLock::new(|| Mutex::new(Profiler::default()));

#[derive(Default)]
struct Profiler {
    occurrences: HashMap<OpCode, usize>,
    pairs: HashMap<(OpCode, OpCode), usize>,
    cur: Option<OpCode>,
}

impl Profiler {
    pub fn record_starting_opcode(&mut self, opcode: OpCode) {
        self.cur = Some(opcode);

        self.occurrences
            .entry(opcode)
            .and_modify(|x| *x += 1)
            .or_insert(0);
    }

    pub fn record_next_opcode(&mut self, opcode: OpCode) {
        if let Some(cur) = self.cur {
            self.pairs
                .entry((cur, opcode))
                .and_modify(|x| *x += 1)
                .or_insert(0);
        }
    }
}

pub(crate) fn record_start_op(instruction: DenseInstruction) {
    PROFILER
        .lock()
        .unwrap()
        .record_starting_opcode(instruction.op_code);
}

pub(crate) fn record_next_op(instruction: DenseInstruction) {
    PROFILER
        .lock()
        .unwrap()
        .record_next_opcode(instruction.op_code);
}

pub(crate) fn profiling_report() {
    let guard = PROFILER.lock().unwrap();
    let mut single_counts = guard
        .occurrences
        .iter()
        .map(|x| x.clone())
        .collect::<Vec<_>>();
    let mut double_counts = guard.pairs.iter().map(|x| x.clone()).collect::<Vec<_>>();

    single_counts.sort_by_key(|x| x.1);
    single_counts.reverse();
    double_counts.sort_by_key(|x| x.1);
    double_counts.reverse();

    println!("Single op code counts");
    for pair in single_counts {
        println!("{:?}: {}", pair.0, pair.1);
    }

    println!("----------------------");

    println!("Double op code counts");
    for pair in double_counts {
        println!("{:?}: {}", pair.0, pair.1);
    }
}
