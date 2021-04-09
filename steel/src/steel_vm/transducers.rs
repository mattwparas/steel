use super::{evaluation_progress::EvaluationProgress, stack::StackFrame, vm::VmCore};
use crate::{
    compiler::constants::ConstantTable,
    parser::span::Span,
    primitives::{ListOperations, VectorOperations},
    rerrs::{ErrorKind, SteelErr},
    rvals::{CollectionType, Result, SteelVal, Transducer, Transducers},
    stop,
};

use super::inline_iter::*;
use super::lazy_stream::LazyStreamIter;

impl<'a, CT: ConstantTable> VmCore<'a, CT> {
    pub(crate) fn run(
        &mut self,
        ops: &[Transducers],
        root: SteelVal,
        collection_type: Option<SteelVal>,
        cur_inst_span: &Span,
    ) -> Result<SteelVal> {
        // By default, match the output type to the input type
        let output_type = match root {
            SteelVal::VectorV(_) => CollectionType::Vector,
            _ => CollectionType::List,
        };

        // Initialize the iterator to be the iterator over whatever is given, stop if its not iterable
        let mut iter: Box<dyn Iterator<Item = Result<SteelVal>>> = match &root {
            SteelVal::VectorV(v) => Box::new(v.iter().cloned().map(|x| Ok(x))),
            SteelVal::Pair(_) => Box::new(SteelVal::iter(root).into_iter().map(|x| Ok(x))),
            SteelVal::StreamV(lazy_stream) => Box::new(LazyStreamIter::new(
                lazy_stream.unwrap(),
                self.constants,
                cur_inst_span,
                self.callback,
            )),
            SteelVal::StringV(s) => Box::new(s.chars().map(|x| Ok(SteelVal::CharV(x)))),
            _ => stop!(TypeMismatch => "Iterators not yet implemented for this type"),
        };

        for t in ops {
            iter = match t {
                Transducers::Map(func) => Box::new(inline_map_result_iter(
                    iter,
                    func.clone(),
                    self.constants,
                    &cur_inst_span,
                    self.callback,
                    &mut self.stack,
                )),
                Transducers::Filter(func) => Box::new(inline_filter_result_iter(
                    iter,
                    func.clone(),
                    self.constants,
                    &cur_inst_span,
                    self.callback,
                    &mut self.stack,
                )),
                Transducers::Take(num) => {
                    if let SteelVal::IntV(num) = num {
                        if *num < 0 {
                            stop!(ContractViolation => "take transducer must have a position number"; *cur_inst_span)
                        }
                        Box::new(iter.take(*num as usize))
                    } else {
                        stop!(TypeMismatch => "take transducer takes an integer"; *cur_inst_span)
                    }
                }
                Transducers::Drop(num) => {
                    if let SteelVal::IntV(num) = num {
                        if *num < 0 {
                            stop!(ContractViolation => "drop transducer must have a position number"; *cur_inst_span)
                        }
                        Box::new(iter.skip(*num as usize))
                    } else {
                        stop!(TypeMismatch => "drop transducer takes an integer"; *cur_inst_span)
                    }
                }
            }
        }

        // If an output type is given, use that one
        if let Some(collection_type) = collection_type {
            if let SteelVal::SymbolV(n) = collection_type {
                match n.as_ref() {
                    "list" => ListOperations::built_in_list_normal_iter(iter),
                    "vector" => VectorOperations::vec_construct_iter(iter),
                    _ => stop!(Generic => "Cannot collect into an undefined type"),
                }
            } else {
                stop!(Generic => "execute takes a symbol")
            }
        } else {
            match output_type {
                CollectionType::List => ListOperations::built_in_list_normal_iter(iter),
                CollectionType::Vector => VectorOperations::vec_construct_iter(iter),
            }
        }
    }

    pub(crate) fn transduce(
        &mut self,
        ops: &[Transducers],
        root: SteelVal,
        initial_value: SteelVal,
        reducer: SteelVal,
        cur_inst_span: &Span,
    ) -> Result<SteelVal> {
        let mut iter: Box<dyn Iterator<Item = Result<SteelVal>>> = match &root {
            SteelVal::VectorV(v) => Box::new(v.iter().cloned().map(|x| Ok(x))),
            SteelVal::Pair(_) => Box::new(SteelVal::iter(root).into_iter().map(|x| Ok(x))),
            SteelVal::StreamV(lazy_stream) => Box::new(LazyStreamIter::new(
                lazy_stream.unwrap(),
                self.constants,
                cur_inst_span,
                self.callback,
            )),
            SteelVal::StringV(s) => Box::new(s.chars().map(|x| Ok(SteelVal::CharV(x)))),
            _ => stop!(TypeMismatch => "Iterators not yet implemented for this type"),
        };

        for t in ops {
            // my_iter = t.into_transducer(my_iter, constants, cur_inst_span, repl, callback)?;

            iter = match t {
                Transducers::Map(func) => Box::new(inline_map_result_iter(
                    iter,
                    func.clone(),
                    self.constants,
                    &cur_inst_span,
                    self.callback,
                    &mut self.stack,
                )),
                Transducers::Filter(func) => Box::new(inline_filter_result_iter(
                    iter,
                    func.clone(),
                    self.constants,
                    &cur_inst_span,
                    self.callback,
                    &mut self.stack,
                )),
                Transducers::Take(num) => {
                    if let SteelVal::IntV(num) = num {
                        if *num < 0 {
                            stop!(ContractViolation => "take transducer must have a position number"; *cur_inst_span)
                        }
                        Box::new(iter.take(*num as usize))
                    } else {
                        stop!(TypeMismatch => "take transducer takes an integer"; *cur_inst_span)
                    }
                }
                Transducers::Drop(num) => {
                    if let SteelVal::IntV(num) = num {
                        if *num < 0 {
                            stop!(ContractViolation => "drop transducer must have a position number"; *cur_inst_span)
                        }
                        Box::new(iter.skip(*num as usize))
                    } else {
                        stop!(TypeMismatch => "drop transducer takes an integer"; *cur_inst_span)
                    }
                }
            }
        }

        inline_reduce_iter(
            iter,
            initial_value,
            reducer,
            self.constants,
            cur_inst_span,
            self.callback,
            &mut self.stack,
        )
    }
}
