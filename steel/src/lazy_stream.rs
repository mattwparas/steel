use crate::env::Env;
use crate::gc::Gc;
use crate::parser::span::Span;
use crate::rerrs::SteelErr;
use crate::rvals::{Result, SteelVal};
use crate::vm::vm;
use crate::vm::ConstantTable;
use crate::vm::EvaluationProgress;
use crate::vm::Heap;
use std::cell::RefCell;
use std::rc::Rc;

use crate::env::VOID;

#[derive(Clone)]
pub struct LazyStream {
    initial_value: Gc<SteelVal>, // argument to stream
    stream_thunk: Gc<SteelVal>,  // function to get the next value
    empty_stream: bool,
}

impl LazyStream {
    // Perhaps do some error checking here in order to determine
    // if the arguments passed are actually valid
    pub fn new(initial_value: Gc<SteelVal>, stream_thunk: Gc<SteelVal>) -> Self {
        LazyStream {
            initial_value,
            stream_thunk,
            empty_stream: false,
        }
    }

    pub fn new_empty_stream() -> Self {
        LazyStream {
            initial_value: VOID.with(|x| Gc::clone(x)),
            stream_thunk: VOID.with(|x| Gc::clone(x)),
            empty_stream: true,
        }
    }

    // Should return the value in the `initial_value` field
    // is equivalent to calling (stream-first stream)
    pub fn stream_first(&self) -> Gc<SteelVal> {
        Gc::clone(&self.initial_value)
    }

    // `stream_thunk` should be a thunk that return the next `LazyStream`
    //  this should just return a new `LazyStream`
    pub fn stream_thunk(&self) -> Gc<SteelVal> {
        Gc::clone(&self.stream_thunk)
    }

    pub fn empty_stream(&self) -> Gc<SteelVal> {
        Gc::new(SteelVal::BoolV(self.empty_stream))
    }
}

pub struct LazyStreamIter<'global, CT: ConstantTable> {
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
    type Item = Result<Gc<SteelVal>>;
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
            if let SteelVal::StreamV(lazy_stream) = next_value.as_ref() {
                self.stream = lazy_stream.clone();
            } else {
                panic!("Lazy stream not implemented");
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
    stack_func: Gc<SteelVal>,
    constants: &CT,
    cur_inst_span: &Span,
    repl: bool,
    local_heap: &mut Heap,
    callback: &EvaluationProgress,
) -> Result<Gc<SteelVal>> {
    match stack_func.as_ref() {
        SteelVal::FuncV(func) => {
            let arg_vec = vec![];
            func(&arg_vec).map_err(|x| x.set_span(*cur_inst_span))
        }
        SteelVal::StructClosureV(factory, func) => {
            let arg_vec = vec![];
            func(arg_vec, factory).map_err(|x| x.set_span(*cur_inst_span))
        }
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
    use crate::vm::ConstantMap;

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
