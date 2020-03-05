extern crate rucket;

use rucket::evaluator;
use rucket::parser::*;

use std::fs::File;
use std::io::{BufRead, BufReader};

/// test to make sure file used as input leads to output
pub fn test_from_files(input_path: &str, output_path: &str) {
    let inputfile = File::open(input_path).unwrap();
    let outputfile = File::open(output_path).unwrap();
    test_lines(BufReader::new(inputfile), BufReader::new(outputfile));
}

pub fn test_lines(input: impl BufRead, output: impl BufRead) {
    let mut evaluator = evaluator::Evaluator::new();
    let io_lines = input.lines().zip(output.lines());
    for (line_in, line_out) in io_lines {
        let line_in = line_in.unwrap();
        let line_out = line_out.unwrap();
        let p = Parser::new(&line_in);
        let exprs: Result<Vec<Expr>> = p.collect();
        match exprs {
            Ok(exprs) => {
                for expr in exprs {
                    let out = evaluator.eval(&expr);
                    assert_eq!(out.unwrap().to_string(), line_out);
                }
            }
            Err(e) => assert_eq!(e.to_string(), line_out),
        }
    }
}
