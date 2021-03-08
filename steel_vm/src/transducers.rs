use crate::evaluation_progress::EvaluationProgress;
use steel::{
    parser::span::Span,
    primitives::{ListOperations, VectorOperations},
    rerrs::{ErrorKind, SteelErr},
    rvals::{CollectionType, Result, SteelVal, Transducer, Transducers},
    steel_compiler::constants::ConstantTable,
    stop,
};

use crate::inline_iter::*;
use crate::lazy_stream::LazyStreamIter;

pub trait TransducerExt {
    fn run<CT: ConstantTable>(
        &self,
        root: SteelVal,
        constants: &CT,
        cur_inst_span: &Span,
        repl: bool,
        callback: &EvaluationProgress,
        collection_type: Option<SteelVal>,
    ) -> Result<SteelVal>;

    fn transduce<CT: ConstantTable>(
        &self,
        root: SteelVal,
        initial_value: SteelVal,
        reducer: SteelVal,
        constants: &CT,
        cur_inst_span: &Span,
        repl: bool,
        callback: &EvaluationProgress,
    ) -> Result<SteelVal>;
}

pub trait TransducersExt {
    fn into_transducer<'global, I: Iterator<Item = Result<SteelVal>> + 'global, CT: ConstantTable>(
        &self,
        iter: I,
        // stack_func: SteelVal,
        constants: &'global CT,
        cur_inst_span: &'global Span,
        repl: bool,
        callback: &'global EvaluationProgress,
    ) -> Result<Box<dyn Iterator<Item = Result<SteelVal>> + 'global>>;
}

// This runs through the iterators  in sequence in the transducer
// we want to then finish with a reducer
// TODO see transduce vs educe
// TODO change the return type to match the given input type
// optionally add an argument to select the return type manually

impl TransducerExt for Transducer {
    fn run<CT: ConstantTable>(
        &self,
        root: SteelVal,
        constants: &CT,
        cur_inst_span: &Span,
        repl: bool,
        callback: &EvaluationProgress,
        collection_type: Option<SteelVal>,
    ) -> Result<SteelVal> {
        // By default, match the output type to the input type
        let output_type = match root {
            SteelVal::VectorV(_) => CollectionType::Vector,
            _ => CollectionType::List,
        };

        // Initialize the iterator to be the iterator over whatever is given, stop if its not iterable
        let mut my_iter: Box<dyn Iterator<Item = Result<SteelVal>>> = match &root {
            SteelVal::VectorV(v) => Box::new(v.iter().cloned().map(|x| Ok(x))),
            SteelVal::Pair(_) => Box::new(SteelVal::iter(root).into_iter().map(|x| Ok(x))),
            SteelVal::StreamV(lazy_stream) => Box::new(LazyStreamIter::new(
                lazy_stream.unwrap(),
                constants,
                cur_inst_span,
                repl,
                callback,
            )),
            SteelVal::StringV(s) => Box::new(s.chars().map(|x| Ok(SteelVal::CharV(x)))),
            _ => stop!(TypeMismatch => "Iterators not yet implemented for this type"),
        };

        // Chain the iterators together
        for t in &self.ops {
            my_iter = t.into_transducer(my_iter, constants, cur_inst_span, repl, callback)?;
        }

        // If an output type is given, use that one
        if let Some(collection_type) = collection_type {
            if let SteelVal::SymbolV(n) = collection_type {
                match n.as_ref() {
                    "list" => ListOperations::built_in_list_normal_iter(my_iter),
                    "vector" => VectorOperations::vec_construct_iter(my_iter),
                    _ => stop!(Generic => "Cannot collect into an undefined type"),
                }
            } else {
                stop!(Generic => "execute takes a symbol")
            }
        } else {
            match output_type {
                CollectionType::List => ListOperations::built_in_list_normal_iter(my_iter),
                CollectionType::Vector => VectorOperations::vec_construct_iter(my_iter),
            }
        }
    }

    fn transduce<CT: ConstantTable>(
        &self,
        root: SteelVal,
        initial_value: SteelVal,
        reducer: SteelVal,
        constants: &CT,
        cur_inst_span: &Span,
        repl: bool,
        callback: &EvaluationProgress,
    ) -> Result<SteelVal> {
        let mut my_iter: Box<dyn Iterator<Item = Result<SteelVal>>> = match &root {
            SteelVal::VectorV(v) => Box::new(v.iter().cloned().map(|x| Ok(x))),
            SteelVal::Pair(_) => Box::new(SteelVal::iter(root).into_iter().map(|x| Ok(x))),
            SteelVal::StreamV(lazy_stream) => Box::new(LazyStreamIter::new(
                lazy_stream.unwrap(),
                constants,
                cur_inst_span,
                repl,
                callback,
            )),
            SteelVal::StringV(s) => Box::new(s.chars().map(|x| Ok(SteelVal::CharV(x)))),
            _ => stop!(TypeMismatch => "Iterators not yet implemented for this type"),
        };

        for t in &self.ops {
            my_iter = t.into_transducer(my_iter, constants, cur_inst_span, repl, callback)?;
        }

        inline_reduce_iter(
            my_iter,
            initial_value,
            reducer,
            constants,
            cur_inst_span,
            repl,
            callback,
        )
    }
}

impl TransducersExt for Transducers {
    fn into_transducer<
        'global,
        I: Iterator<Item = Result<SteelVal>> + 'global,
        CT: ConstantTable,
    >(
        &self,
        iter: I,
        // stack_func: SteelVal,
        constants: &'global CT,
        cur_inst_span: &'global Span,
        repl: bool,
        callback: &'global EvaluationProgress,
    ) -> Result<Box<dyn Iterator<Item = Result<SteelVal>> + 'global>> {
        match self {
            Transducers::Map(func) => Ok(Box::new(inline_map_result_iter(
                iter,
                func.clone(),
                constants,
                cur_inst_span,
                repl,
                callback,
            ))),
            Transducers::Filter(func) => Ok(Box::new(inline_filter_result_iter(
                iter,
                func.clone(),
                constants,
                cur_inst_span,
                repl,
                callback,
            ))),
            Transducers::Take(num) => {
                if let SteelVal::IntV(num) = num {
                    if *num < 0 {
                        stop!(ContractViolation => "take transducer must have a position number"; *cur_inst_span)
                    }
                    Ok(Box::new(iter.take(*num as usize)))
                } else {
                    stop!(TypeMismatch => "take transducer takes an integer"; *cur_inst_span)
                }
            }
        }
    }
}

// }

// impl Transducers {

// }
