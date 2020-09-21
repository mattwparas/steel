extern crate steel;

use std::fs::File;
use std::io::{BufRead, BufReader};
// use steel::interpreter::evaluator::Evaluator;
// use steel::parser::*;
use steel::vm::VirtualMachine;
use steel::PRELUDE;

// use std::collections::HashMap;

/// test to make sure file used as input leads to output
pub fn test_from_files(input_path: &str, output_path: &str) {
    let inputfile = File::open(input_path).unwrap();
    let outputfile = File::open(output_path).unwrap();
    test_lines(BufReader::new(inputfile), BufReader::new(outputfile));
}

pub fn test_lines(input: impl BufRead, output: impl BufRead) {
    let mut evaluator = VirtualMachine::new();
    evaluator.parse_and_execute(PRELUDE).unwrap();

    let io_lines = input.lines().zip(output.lines());
    for (line_in, line_out) in io_lines {
        let line_in = line_in.unwrap();
        let line_out = line_out.unwrap();
        test_line(&line_in, &[&line_out], &mut evaluator);
    }
}

pub fn test_line(input: &str, output: &[&str], evaluator: &mut VirtualMachine) {
    let result = evaluator.parse_and_execute(input);
    match result {
        Ok(vals) => {
            assert_eq!(output.len(), vals.len());
            for (expr, &expected) in vals.iter().zip(output.iter()) {
                assert_eq!(expr.to_string(), expected);
                // match expr {
                //     Ok(x) => assert_eq!(x.to_string(), expected),
                //     Err(x) => assert_eq!(x.to_string(), expected),
                // }
            }
        }
        Err(e) => assert_eq!(e.to_string(), output[0]),
    }
}
