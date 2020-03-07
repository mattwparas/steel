extern crate steel;

use steel::evaluator::Evaluator;
use steel::parser::*;

use std::fs::File;
use std::io::{BufRead, BufReader};

/// test to make sure file used as input leads to output
pub fn test_from_files(input_path: &str, output_path: &str) {
    let inputfile = File::open(input_path).unwrap();
    let outputfile = File::open(output_path).unwrap();
    test_lines(BufReader::new(inputfile), BufReader::new(outputfile));
}

pub fn test_lines(input: impl BufRead, output: impl BufRead) {
    let mut evaluator = Evaluator::new();
    let io_lines = input.lines().zip(output.lines());
    for (line_in, line_out) in io_lines {
        let line_in = line_in.unwrap();
        let line_out = line_out.unwrap();
        test_line(&line_in, &[&line_out], &mut evaluator);
    }
}

pub fn test_line(input: &str, output: &[&str], evaluator: &mut Evaluator) {
    let p = Parser::new(input);
    let exprs: Result<Vec<Expr>> = p.collect();
    match exprs {
        Ok(exprs) => {
            assert_eq!(exprs.len(), output.len());
            for (expr, &expected) in exprs.iter().zip(output.iter()) {
                let out = evaluator.eval(expr.clone());
                match out {
                    Ok(x) => assert_eq!(x.to_string(), expected),
                    Err(x) => assert_eq!(x.to_string(), expected),
                }
            }
        }
        Err(e) => assert_eq!(e.to_string(), output[0]),
    }
}
