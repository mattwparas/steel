#![allow(dead_code)]

pub mod evaluator;

use crate::interpreter::evaluator::Evaluator;
use crate::rerrs::SteelErr;
use crate::rvals::SteelVal;
use std::io::Read;
use std::path::Path;

#[macro_export]
macro_rules! build_interpreter {

    ($($type:ty),* $(,)?) => {
        {
            let mut interpreter = SteelInterpreter::new();
            $ (
                interpreter.insert_bindings(<$type>::generate_bindings());
            ) *
            interpreter
        }
    };

    (Structs => {$($type:ty),* $(,)?} Functions => {$($binding:expr => $func:ident),* $(,)?}) => {
        {
            let mut interpreter = SteelInterpreter::new();
            $ (
                interpreter.insert_bindings(<$type>::generate_bindings());
            ) *

            $ (
                interpreter.insert_binding($binding, SteelVal::FuncV($func));
            ) *

            interpreter
        }
    };
}

#[derive(Default)]
pub struct SteelInterpreter {
    evaluator: Evaluator,
}

impl SteelInterpreter {
    /// Constructs a fresh instance of the interpreter with a default environment
    /// # Examples
    /// ```rust
    /// use steel::interpreter::SteelInterpreter;
    ///
    /// let interpreter = SteelInterpreter::new();
    /// ```
    pub fn new() -> Self {
        SteelInterpreter {
            evaluator: Evaluator::new(),
        }
    }

    /// Parses and evaluates a steel statement, returns a vector of the results, or an error
    /// # Examples
    /// ```rust
    /// use steel::interpreter::SteelInterpreter;
    /// use steel::rvals::SteelVal;
    ///
    /// let mut interpreter = SteelInterpreter::new();
    /// let stmt = "(+ 1 2 3) (+ 4 5 6)";
    /// let results = interpreter.evaluate(stmt);
    /// let expected = vec![SteelVal::NumV(6.0), SteelVal::NumV(15.0)];
    /// assert_eq!(results.unwrap(), expected);
    /// ```
    pub fn evaluate(&mut self, expr_str: &str) -> Result<Vec<SteelVal>, SteelErr> {
        self.evaluator.parse_and_eval(expr_str)
    }

    /// Clears the environment for the interpreter
    /// # Examples
    /// ```rust
    /// use steel::interpreter::SteelInterpreter;
    /// use steel::rvals::SteelVal;
    ///
    /// let mut interpreter = SteelInterpreter::new();
    /// interpreter.insert_binding("test", SteelVal::BoolV(true));
    /// let stmt = "test";
    /// let results = interpreter.evaluate(stmt);
    /// let expected = vec![SteelVal::BoolV(true)];
    /// assert_eq!(results.unwrap(), expected);
    ///
    /// interpreter.reset();
    /// let stmt = "test";
    /// let results = interpreter.evaluate(stmt);
    /// assert!(results.is_err());
    /// ```
    pub fn reset(&mut self) {
        // make sure evaluator gets freed beforehand
        self.evaluator.clear_bindings();
        self.evaluator = Evaluator::new();
    }

    /// Evaluates from a reader
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

    /// Evaluate statements from paths, ignoring output
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

    /// Adds a global binding for a SteelVal
    /// # Examples
    /// ```rust
    /// use steel::interpreter::SteelInterpreter;
    /// use steel::rvals::SteelVal;
    ///
    /// let mut interpreter = SteelInterpreter::new();
    /// interpreter.insert_binding("test", SteelVal::BoolV(true));
    /// let stmt = "test";
    /// let results = interpreter.evaluate(stmt);
    /// let expected = vec![SteelVal::BoolV(true)];
    /// assert_eq!(results.unwrap(), expected);
    /// ```
    pub fn insert_binding(&mut self, name: &str, value: SteelVal) {
        self.evaluator.insert_binding(name.to_string(), value);
    }

    /// Adds global bindings for each `(&str, SteelVal)` in the vector
    pub fn insert_bindings(&mut self, vals: Vec<(String, SteelVal)>) {
        self.evaluator.insert_bindings(vals);
    }

    /// Extracts a value from the interpreter
    /// # Examples
    /// ```rust
    /// use steel::interpreter::SteelInterpreter;
    /// use steel::rvals::SteelVal;
    ///
    /// let mut interpreter = SteelInterpreter::new();
    /// interpreter.insert_binding("test", SteelVal::BoolV(true));
    /// assert_eq!(interpreter.extract_value("test").unwrap(), SteelVal::BoolV(true));
    /// ```
    pub fn extract_value(&mut self, name: &str) -> Result<SteelVal, SteelErr> {
        self.evaluator.lookup_binding(name)
    }
}

#[cfg(test)]
mod interpreter_tests {
    use super::*;
    use crate::rvals::SteelVal;

    #[test]
    fn evaluate_test() {
        let mut interpreter = SteelInterpreter::new();
        let stmt = "(+ 1 2 3) (+ 4 5 6)";
        let results = interpreter.evaluate(stmt);
        let expected = vec![SteelVal::NumV(6.0), SteelVal::NumV(15.0)];
        assert_eq!(results.unwrap(), expected);
    }

    #[test]
    fn reset_test() {
        let mut interpreter = SteelInterpreter::new();
        interpreter.insert_binding("test", SteelVal::BoolV(true));
        let stmt = "test";
        let results = interpreter.evaluate(stmt);
        let expected = vec![SteelVal::BoolV(true)];
        assert_eq!(results.unwrap(), expected);
        interpreter.reset();
        let stmt = "test";
        let results = interpreter.evaluate(stmt);
        assert!(results.is_err());
    }

    #[test]
    fn insert_binding_test() {
        let mut interpreter = SteelInterpreter::new();
        interpreter.insert_binding("test", SteelVal::BoolV(true));
        let stmt = "test";
        let results = interpreter.evaluate(stmt);
        let expected = vec![SteelVal::BoolV(true)];
        assert_eq!(results.unwrap(), expected);
    }

    #[test]
    fn extract_value_test() {
        let mut interpreter = SteelInterpreter::new();
        interpreter.insert_binding("test", SteelVal::BoolV(true));
        assert_eq!(
            interpreter.extract_value("test").unwrap(),
            SteelVal::BoolV(true)
        );
    }

    #[test]
    fn evaluate_from_reader_test() {
        let mut interpreter = SteelInterpreter::new();
        let b = "(+ 1 2 3)".as_bytes();
        assert_eq!(
            interpreter.evaluate_from_reader(b).unwrap(),
            vec![SteelVal::NumV(6.0)]
        );
    }
}
