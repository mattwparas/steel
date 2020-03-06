#![allow(dead_code)]
use crate::evaluator::Evaluator;
use crate::rerrs::SteelErr;
use crate::rvals::SteelVal;
use std::io::Read;
use std::path::Path;

#[macro_export]
macro_rules! build_interpreter {
    ($($type:ty),*) => {
        {
            let mut interpreter = SteelInterpreter::new();
            $ (
                interpreter.insert_bindings(<$type>::generate_bindings());
            ) *
            interpreter
        }
    };
}

pub struct SteelInterpreter {
    evaluator: Evaluator,
}

impl SteelInterpreter {
    pub fn new() -> Self {
        SteelInterpreter {
            evaluator: Evaluator::new(),
        }
    }

    pub fn evaluate(&mut self, expr_str: &str) -> Result<Vec<SteelVal>, SteelErr> {
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
    ) -> Result<Vec<SteelVal>, SteelErr> {
        let mut exprs = String::new();
        input.read_to_string(&mut exprs)?;
        self.evaluate(&exprs)
    }

    /// Evaluate statements for their side effects on the environment,
    /// ignoring the output
    pub fn require(&mut self, exprs: &str) -> Result<(), SteelErr> {
        self.evaluate(exprs).map(|_| ())
    }

    /// Evaluate statements from paths,ignoring output
    pub fn require_paths<P: AsRef<Path>>(
        &mut self,
        paths: impl Iterator<Item = P>,
    ) -> Result<(), SteelErr> {
        for path in paths {
            let file = std::fs::File::open(path)?;
            let _ = self.evaluate_from_reader(file)?;
        }
        Ok(())
    }

    pub fn insert_binding(&mut self, name: &str, value: SteelVal) {
        self.evaluator.insert_binding(name.to_string(), value);
    }

    pub fn insert_bindings(&mut self, vals: Vec<(&'static str, SteelVal)>) {
        self.evaluator.insert_bindings(vals);
    }

    pub fn extract_value(&mut self, name: &str) -> Result<SteelVal, SteelErr> {
        self.evaluator.lookup_binding(name)
    }
}
