use crate::evaluator::Evaluator;
use crate::parser::{Expr, ParseError, Parser};
// use parser::{Expr, ParseError};
use std::io;
use std::io::Read;

struct RucketInterpreter {
    contents: String,
    parser: Result<Vec<Expr>, ParseError>,
    evaluator: Evaluator,
}

impl RucketInterpreter {
    pub fn new(mut input: impl Read) -> io::Result<Self> {
        let mut exprs = String::new();
        input.read_to_string(&mut exprs)?;
        let parsed: Result<Vec<Expr>, ParseError> = Parser::new(&exprs).collect();
        Ok(RucketInterpreter {
            contents: exprs,
            parser: parsed,
            evaluator: Evaluator::new(),
        })
    }
}
