#![allow(dead_code)]
use crate::evaluator::Evaluator;
use crate::rerrs::RucketErr;
use crate::rvals::RucketVal;
use std::io::Read;
use std::path::Path;

pub struct RucketInterpreter {
    evaluator: Evaluator,
}

impl RucketInterpreter {
    pub fn new() -> Self {
        RucketInterpreter {
            evaluator: Evaluator::new(),
        }
    }

    pub fn evaluate(&mut self, expr_str: &str) -> Result<Vec<RucketVal>, RucketErr> {
        self.evaluator.parse_and_eval(expr_str)
    }

    pub fn reset(&mut self) {
        // make sure evaluator gets freed beforehand
        self.evaluator.clear_bindings();
        self.evaluator = Evaluator::new();
    }

    pub fn evaluate_from_reader(
        &mut self,
        mut input: impl Read,
    ) -> Result<Vec<RucketVal>, RucketErr> {
        let mut exprs = String::new();
        input.read_to_string(&mut exprs)?;
        self.evaluate(&exprs)
    }

    /// Evaluate statements for their side effects on the environment,
    /// ignoring the output
    pub fn require(&mut self, exprs: &str) -> Result<(), RucketErr> {
        self.evaluate(exprs).map(|_| ())
    }

    /// Evaluate statements from paths,ignoring output
    pub fn require_paths<P: AsRef<Path>>(
        &mut self,
        paths: impl Iterator<Item = P>,
    ) -> Result<(), RucketErr> {
        for path in paths {
            let file = std::fs::File::open(path)?;
            let _ = self.evaluate_from_reader(file)?;
        }
        Ok(())
    }

    pub fn insert_binding(&mut self, name: &str, value: RucketVal) {
        self.evaluator.insert_binding(name.to_string(), value);
    }

    pub fn insert_bindings(&mut self, vals: Vec<(&'static str, RucketVal)>) {
        self.evaluator.insert_bindings(vals);
    }

    pub fn extract_value(&mut self, name: &str) -> Result<RucketVal, RucketErr> {
        self.evaluator.lookup_binding(name)
    }
}
