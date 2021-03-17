use crate::evaluation_progress::EvaluationProgress;
use crate::heap::Heap;
use crate::vm::vm;
use std::cell::RefCell;
use std::rc::Rc;
use steel::env::Env;
use steel::parser::span::Span;
use steel::rerrs::{ErrorKind, SteelErr};
use steel::rvals::{Result, SteelVal};
use steel::steel_compiler::constants::ConstantTable;
use steel::stop;

use steel::values::lazy_stream::LazyStream;

// Used for inlining stream iterators
pub(crate) struct LazyStreamIter<'global, CT: ConstantTable> {
    stream: LazyStream,
    constants: &'global CT,
    cur_inst_span: &'global Span,
    repl: bool,
    local_heap: Heap,
    callback: &'global EvaluationProgress,
}

impl<'global, CT: ConstantTable> LazyStreamIter<'global, CT> {
    pub fn new(
        stream: LazyStream,
        constants: &'global CT,
        cur_inst_span: &'global Span,
        repl: bool,
        callback: &'global EvaluationProgress,
    ) -> Self {
        Self {
            stream,
            constants,
            cur_inst_span,
            repl,
            local_heap: Heap::new(),
            callback,
        }
    }
}

impl<'global, CT: ConstantTable> Iterator for LazyStreamIter<'global, CT> {
    type Item = Result<SteelVal>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.stream.empty_stream {
            return None;
        }

        let stream_first = self.stream.stream_first();
        let stream_thunk = self.stream.stream_thunk();

        let next_value = exec_func(
            stream_thunk,
            self.constants,
            self.cur_inst_span,
            self.repl,
            &mut self.local_heap,
            self.callback,
        );

        if let Ok(next_value) = next_value {
            if let SteelVal::StreamV(lazy_stream) = next_value {
                self.stream = lazy_stream.unwrap();
            } else {
                panic!("Lazy stream not implemented for the given type");
            }
        }

        Some(Ok(stream_first))
    }
}

// by definition, they need to provide a function that takes no arguments
// built in functions I do not think will work in this regard
// decide what a stream will look like first before continuing with this
// in order to make this work with transducers, just create an iterator that returns closures
// that then maps the application of that closure on each element to get our result
fn exec_func<CT: ConstantTable>(
    stack_func: SteelVal,
    constants: &CT,
    cur_inst_span: &Span,
    repl: bool,
    local_heap: &mut Heap,
    callback: &EvaluationProgress,
) -> Result<SteelVal> {
    match stack_func {
        SteelVal::FuncV(func) => {
            let arg_vec = vec![];
            func(&arg_vec).map_err(|x| x.set_span(*cur_inst_span))
        }
        SteelVal::BoxedFunction(func) => {
            let arg_vec = vec![];
            func(&arg_vec).map_err(|x| x.set_span(*cur_inst_span))
        }
        // SteelVal::StructClosureV(sc) => {
        //     let arg_vec = vec![];
        //     (sc.func)(&arg_vec, &sc.factory).map_err(|x| x.set_span(*cur_inst_span))
        // }
        SteelVal::Closure(closure) => {
            let args = vec![];

            let parent_env = closure.sub_expression_env();

            // TODO remove this unwrap
            let offset = closure.offset() + parent_env.upgrade().unwrap().borrow().local_offset();

            let inner_env = Rc::new(RefCell::new(Env::new_subexpression(
                parent_env.clone(),
                offset,
            )));

            inner_env
                .borrow_mut()
                .reserve_defs(if closure.ndef_body() > 0 {
                    closure.ndef_body() - 1
                } else {
                    0
                });

            // TODO make recursive call here with a very small stack
            // probably a bit overkill, but not much else I can do here I think
            vm(
                closure.body_exp(),
                args.into(),
                local_heap,
                inner_env,
                constants,
                repl,
                callback,
            )
        }
        _ => stop!(TypeMismatch => "stream expected a function"; *cur_inst_span),
    }
}

#[cfg(test)]
mod stream_tests {
    use super::*;
    use crate::test_util::assert_script;
    use steel::steel_compiler::constants::ConstantMap;

    #[test]
    fn test_empty_stream_creates_no_iter() {
        let constants = ConstantMap::new();
        let cur_inst_span = Span::new(0, 0);
        let repl = true;
        let callback = EvaluationProgress::new();
        let lazy_iter = LazyStreamIter::new(
            LazyStream::new_empty_stream(),
            &constants,
            &cur_inst_span,
            repl,
            &callback,
        );

        assert!(lazy_iter.into_iter().next().is_none());
    }

    #[test]
    fn simple_stream() {
        let script = r#"
        (define (stream-cdr stream)
            ((stream-cdr' stream)))

        (define (integers n)
            (stream-cons n (lambda () (integers (+ 1 n)))))

        (define (stream-section n stream)
            (cond ((= n 0) '())
                  (else
                   (cons
                    (stream-car stream)
                    (stream-section
                     (- n 1)
                     (stream-cdr stream))))))

        (assert! (equal? (list 0 1 2 3 4) (stream-section 5 (integers 0))))
        "#;
        assert_script(script);
    }

    #[test]
    fn simple_stream_with_map() {
        let script = r#"
        (define (stream-cdr stream)
            ((stream-cdr' stream)))

        (define (integers n)
            (stream-cons n (lambda () (integers (+ 1 n)))))

        (define (stream-section n stream)
            (cond ((= n 0) '())
                  (else
                   (cons
                    (stream-car stream)
                    (stream-section
                     (- n 1)
                     (stream-cdr stream))))))

        (define (map-stream func s)
            (cond
                [(stream-empty? s) s]
                [else
                    (stream-cons (func (stream-car s))
                                    (lambda ()
                                    (map-stream func (stream-cdr s))))]))

        (assert! 
            (equal? (list 10 10 10 10 10)
                    (stream-section 5 (map-stream (lambda (x) 10) (integers 0)))))
        "#;
        assert_script(script);
    }

    #[test]
    fn simple_stream_with_transducer() {
        let script = r#"
        (define (stream-cdr stream)
            ((stream-cdr' stream)))

        (define (integers n)
            (stream-cons n (lambda () (integers (+ 1 n)))))

        (assert! 
            (equal? (list 0 1 2 3 4)
                    (execute (taking 5) (integers 0))))
        "#;
        assert_script(script);
    }
}
