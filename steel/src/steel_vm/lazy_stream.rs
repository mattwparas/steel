use super::options::ApplyContracts;
use super::options::UseCallbacks;
use super::vm::vm;
use super::vm::VmCore;
use super::{evaluation_progress::EvaluationProgress, heap::UpValueHeap};
use crate::compiler::constants::ConstantTable;
use crate::env::Env;
use crate::gc::Gc;
use crate::parser::span::Span;
use crate::rerrs::{ErrorKind, SteelErr};
use crate::rvals::{Result, SteelVal};
use crate::stop;
use std::cell::RefCell;
use std::rc::Rc;

use super::stack::Stack;

use crate::values::lazy_stream::LazyStream;

// Used for inlining stream iterators
pub(crate) struct LazyStreamIter<'global, 'a, CT: ConstantTable, U: UseCallbacks, A: ApplyContracts>
{
    stream: LazyStream,
    vm_ctx: Rc<RefCell<&'global mut VmCore<'a, CT, U, A>>>,
    cur_inst_span: &'global Span,
}

impl<'global, 'a, CT: ConstantTable, U: UseCallbacks, A: ApplyContracts>
    LazyStreamIter<'global, 'a, CT, U, A>
{
    pub fn new(
        stream: LazyStream,
        vm_ctx: Rc<RefCell<&'global mut VmCore<'a, CT, U, A>>>,
        cur_inst_span: &'global Span,
    ) -> Self {
        Self {
            stream,
            vm_ctx,
            cur_inst_span,
        }
    }
}

impl<'global, 'a, CT: ConstantTable, U: UseCallbacks, A: ApplyContracts> Iterator
    for LazyStreamIter<'global, 'a, CT, U, A>
{
    type Item = Result<SteelVal>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.stream.empty_stream {
            return None;
        }

        let stream_first = self.stream.stream_first();
        let stream_thunk = self.stream.stream_thunk();

        let next_value = self.vm_ctx.borrow_mut().call_func_or_else_many_args(
            &stream_thunk,
            Vec::new(),
            self.cur_inst_span,
            throw!(TypeMismatch => format!("stream expected a thunk, found: {}", stream_thunk)),
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

#[cfg(test)]
mod stream_tests {
    use super::*;
    use crate::compiler::constants::ConstantMap;
    use crate::steel_vm::options::ApplyContract;
    use crate::steel_vm::options::UseCallback;
    use crate::steel_vm::test_util::assert_script;

    // #[test]
    // fn test_empty_stream_creates_no_iter() {
    //     let constants = ConstantMap::new();
    //     let cur_inst_span = Span::new(0, 0);
    //     let callback = EvaluationProgress::new();
    //     let mut global_env = Env::root();
    //     let mut mut_ref = &mut global_env;

    //     let lazy_iter = LazyStreamIter::new(
    //         LazyStream::new_empty_stream(),
    //         &constants,
    //         &cur_inst_span,
    //         &callback,
    //         Rc::new(RefCell::new(&mut mut_ref)),
    //         UseCallback,
    //         ApplyContract,
    //     );

    //     assert!(lazy_iter.into_iter().next().is_none());
    // }

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
