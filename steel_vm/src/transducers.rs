use crate::evaluation_progress::EvaluationProgress;
use steel::parser::span::Span;
use steel::rvals::{CollectionType, Result, SteelVal, Transducer, Transducers};

use steel::rerrs::SteelErr;
use steel::stop;

use crate::lazy_stream::LazyStreamIter;

use steel::gc::Gc;
use steel::steel_compiler::constants::ConstantTable;

use steel::primitives::{ListOperations, VectorOperations};

use crate::inline_iter::*;

// impl Transducer {

pub trait TransducerExt {
    fn run<CT: ConstantTable>(
        &self,
        root: Gc<SteelVal>,
        constants: &CT,
        cur_inst_span: &Span,
        repl: bool,
        callback: &EvaluationProgress,
        collection_type: Option<Gc<SteelVal>>,
    ) -> Result<Gc<SteelVal>>;

    fn transduce<CT: ConstantTable>(
        &self,
        root: Gc<SteelVal>,
        initial_value: Gc<SteelVal>,
        reducer: Gc<SteelVal>,
        constants: &CT,
        cur_inst_span: &Span,
        repl: bool,
        callback: &EvaluationProgress,
    ) -> Result<Gc<SteelVal>>;
}

pub trait TransducersExt {
    fn into_transducer<
        'global,
        I: Iterator<Item = Result<Gc<SteelVal>>> + 'global,
        CT: ConstantTable,
    >(
        &self,
        iter: I,
        // stack_func: Gc<SteelVal>,
        constants: &'global CT,
        cur_inst_span: &'global Span,
        repl: bool,
        callback: &'global EvaluationProgress,
    ) -> Result<Box<dyn Iterator<Item = Result<Gc<SteelVal>>> + 'global>>;
}

// This runs through the iterators  in sequence in the transducer
// we want to then finish with a reducer
// TODO see transduce vs educe
// TODO change the return type to match the given input type
// optionally add an argument to select the return type manually

impl TransducerExt for Transducer {
    fn run<CT: ConstantTable>(
        &self,
        root: Gc<SteelVal>,
        constants: &CT,
        cur_inst_span: &Span,
        repl: bool,
        callback: &EvaluationProgress,
        collection_type: Option<Gc<SteelVal>>,
    ) -> Result<Gc<SteelVal>> {
        // if let Some(collection_type) = collection_type {
        //     match collection_type.as_ref() {}
        // }

        let output_type = match root.as_ref() {
            SteelVal::VectorV(_) => CollectionType::Vector,
            _ => CollectionType::List,
        };

        let mut my_iter: Box<dyn Iterator<Item = Result<Gc<SteelVal>>>> = match root.as_ref() {
            SteelVal::VectorV(v) => Box::new(v.into_iter().map(|x| Ok(Gc::clone(x)))),
            SteelVal::Pair(_, _) => Box::new(SteelVal::iter(root).into_iter().map(|x| Ok(x))),
            SteelVal::StreamV(lazy_stream) => Box::new(LazyStreamIter::new(
                lazy_stream.clone(),
                constants,
                cur_inst_span,
                repl,
                callback,
            )),
            _ => stop!(TypeMismatch => "Iterators not yet implemented for this type"),
        };

        for t in &self.ops {
            my_iter = t.into_transducer(my_iter, constants, cur_inst_span, repl, callback)?;
        }

        if let Some(collection_type) = collection_type {
            if let SteelVal::SymbolV(n) = collection_type.as_ref() {
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
        root: Gc<SteelVal>,
        initial_value: Gc<SteelVal>,
        reducer: Gc<SteelVal>,
        constants: &CT,
        cur_inst_span: &Span,
        repl: bool,
        callback: &EvaluationProgress,
    ) -> Result<Gc<SteelVal>> {
        let mut my_iter: Box<dyn Iterator<Item = Result<Gc<SteelVal>>>> = match root.as_ref() {
            SteelVal::VectorV(v) => Box::new(v.into_iter().map(|x| Ok(Gc::clone(x)))),
            SteelVal::Pair(_, _) => Box::new(SteelVal::iter(root).into_iter().map(|x| Ok(x))),
            SteelVal::StreamV(lazy_stream) => Box::new(LazyStreamIter::new(
                lazy_stream.clone(),
                constants,
                cur_inst_span,
                repl,
                callback,
            )),
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
        I: Iterator<Item = Result<Gc<SteelVal>>> + 'global,
        CT: ConstantTable,
    >(
        &self,
        iter: I,
        // stack_func: Gc<SteelVal>,
        constants: &'global CT,
        cur_inst_span: &'global Span,
        repl: bool,
        callback: &'global EvaluationProgress,
    ) -> Result<Box<dyn Iterator<Item = Result<Gc<SteelVal>>> + 'global>> {
        match self {
            Transducers::Map(func) => Ok(Box::new(inline_map_result_iter(
                iter,
                Gc::clone(func),
                constants,
                cur_inst_span,
                repl,
                callback,
            ))),
            Transducers::Filter(func) => Ok(Box::new(inline_filter_result_iter(
                iter,
                Gc::clone(func),
                constants,
                cur_inst_span,
                repl,
                callback,
            ))),
            Transducers::Take(num) => {
                if let SteelVal::IntV(num) = num.as_ref() {
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
