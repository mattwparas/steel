#![allow(unused_imports)]
#![allow(dead_code)]
use crate::evaluator::Evaluator;
use crate::parser::{Expr, ParseError, Parser};
use crate::rerrs::RucketErr;
use crate::rvals::RucketVal;
// use parser::{Expr, ParseError};
use std::io;
use std::io::Read;

pub struct RucketInterpreter {
    // parser: Result<Vec<Expr>, ParseError>,
    evaluator: Evaluator,
    // results: Vec<RucketVal>,
}

impl RucketInterpreter {
    // pub fn new(mut input: impl Read) -> io::Result<Self> {
    //     let mut exprs = String::new();
    //     input.read_to_string(&mut exprs)?;
    //     // let parsed: Result<Vec<Expr>, ParseError> = Parser::new(&exprs).collect();
    //     Ok(RucketInterpreter {
    //         // parser: parsed,
    //         evaluator: Evaluator::new(),
    //     })
    // }

    pub fn new() -> Self {
        RucketInterpreter {
            evaluator: Evaluator::new(),
        }
    }

    pub fn evaluate(&mut self, expr_str: &str) -> Result<Vec<RucketVal>, RucketErr> {
        self.evaluator.parse_and_eval(expr_str)
    }

    pub fn reset(&mut self) {
        self.evaluator.clear_bindings();
    }

    pub fn evaluate_from_reader(
        &mut self,
        mut input: impl Read,
    ) -> io::Result<Result<Vec<RucketVal>, RucketErr>> {
        let mut exprs = String::new();
        input.read_to_string(&mut exprs)?;
        Ok(self.evaluate(&exprs))
    }

    // pub fn parse_and_evaluate(&mut self, exprs: &str) -> Result<Vec<RucketVal>, RucketErr> {
    //     let parsed: Result<Vec<Expr>, ParseError> = Parser::new(exprs).collect();
    //     match parsed {
    //         Ok(pvec) => pvec.iter().map(|x| self.evaluator.eval(&x)).collect(),
    //         Err(e) => Err(RucketErr::BadSyntax(e.to_string())),
    //     }
    // }

    pub fn require(&mut self, _exprs: &str) -> Result<RucketVal, RucketErr> {
        unimplemented!();
    }

    pub fn require_paths(_paths: Vec<&str>) -> Result<RucketVal, RucketErr> {
        unimplemented!();
    }

    // pub fn parse(mut input: impl Read) -> io::result<Self> {

    // }
}
