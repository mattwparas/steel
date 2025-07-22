use crate::rvals::{poll_future, Result, SteelVal};
use crate::stop;
use crate::{
    gc::{get_object_count, Gc},
    rvals::FutureResult,
};

use futures_util::future::join_all;

// use async_compat::Compat;

use futures_util::FutureExt;

pub struct MetaOperations {}
impl MetaOperations {
    pub fn inspect_bytecode() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            // let mut error_message = String::new();

            if args.len() == 1 {
                match &args[0] {
                    SteelVal::Closure(bytecode_lambda) => {
                        crate::core::instructions::pretty_print_dense_instructions(
                            &bytecode_lambda.body_exp(),
                        );
                        Ok(SteelVal::Void)
                    }
                    // SteelVal::ContractedFunction(c) => {
                    //     if let SteelVal::Closure(bytecode_lambda) = &c.function {
                    //         crate::core::instructions::pretty_print_dense_instructions(
                    //             &bytecode_lambda.body_exp(),
                    //         );
                    //         Ok(SteelVal::Void)
                    //     } else {
                    //         stop!(TypeMismatch => "inspect-bytecode expects a closure object");
                    //     }
                    // }
                    _ => {
                        stop!(TypeMismatch => "inspect-bytecode expects a closure object");
                    }
                }
            } else {
                stop!(ArityMismatch => "inspect-bytecode takes only one argument");
            }
        })
    }

    pub fn active_objects() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if !args.is_empty() {
                stop!(ArityMismatch => "active-object-count expects only one argument");
            }
            Ok(SteelVal::IntV(get_object_count() as isize))
        })
    }

    pub fn memory_address() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 1 {
                stop!(ArityMismatch => "memory address takes one address")
            }

            match &args[0] {
                SteelVal::Closure(gc) => Ok(SteelVal::StringV(format!("{:p}", gc.clone()).into())),
                SteelVal::VectorV(steel_vector) => {
                    Ok(SteelVal::StringV(format!("{:p}", steel_vector.0).into()))
                }
                SteelVal::StringV(steel_string) => {
                    Ok(SteelVal::StringV(format!("{:p}", steel_string.0).into()))
                }
                SteelVal::FuncV(f) => Ok(SteelVal::StringV(format!("{:p}", *f).into())),
                SteelVal::SymbolV(steel_string) => {
                    Ok(SteelVal::StringV(format!("{:p}", steel_string.0).into()))
                }
                SteelVal::Custom(gc) => Ok(SteelVal::StringV(format!("{:p}", gc).into())),
                SteelVal::HashMapV(steel_hash_map) => {
                    Ok(SteelVal::StringV(format!("{:p}", steel_hash_map.0).into()))
                }
                SteelVal::HashSetV(steel_hash_set) => {
                    Ok(SteelVal::StringV(format!("{:p}", steel_hash_set.0).into()))
                }
                SteelVal::CustomStruct(gc) => {
                    Ok(SteelVal::StringV(format!("{:p}", gc.clone()).into()))
                }
                SteelVal::PortV(steel_port) => {
                    Ok(SteelVal::StringV(format!("{:p}", steel_port.port).into()))
                }
                // SteelVal::IterV(gc) => Ok(SteelVal::StringV(format!("{:p}", gc).into())),
                // SteelVal::ReducerV(gc) => Ok(SteelVal::StringV(format!("{:p}", gc).into())),
                // SteelVal::FutureFunc(_) => Ok(SteelVal::StringV(format!("{:p}", gc).into())),
                // SteelVal::FutureV(gc) => Ok(SteelVal::StringV(format!("{:p}", gc).into())),
                // SteelVal::StreamV(gc) => Ok(SteelVal::StringV(format!("{:p}", gc).into())),
                // SteelVal::BoxedFunction(gc) => Ok(SteelVal::StringV(format!("{:p}", gc).into())),
                // SteelVal::ContinuationFunction(continuation) => {
                //     Ok(SteelVal::StringV(format!("{:p}", gc).into()))
                // }
                SteelVal::ListV(generic_list) => Ok(SteelVal::StringV(
                    format!(
                        "{:?}:{:?}:{:?}",
                        generic_list.as_ptr_usize(),
                        generic_list.identity_tuple(),
                        generic_list.next_ptr_as_usize(),
                    )
                    .into(),
                )),
                SteelVal::Pair(gc) => Ok(SteelVal::StringV(format!("{:p}", gc).into())),
                // SteelVal::MutFunc(_) => Ok(SteelVal::StringV(format!("{:p}", gc).into())),
                // SteelVal::BuiltIn(_) => Ok(SteelVal::StringV(format!("{:p}", gc).into())),
                SteelVal::MutableVector(heap_ref) => Ok(SteelVal::StringV(
                    heap_ref.borrow(|x| format!("{:p}", x).into()),
                )),
                // SteelVal::BoxedIterator(gc) => Ok(SteelVal::StringV(format!("{:p}", gc).into())),
                // SteelVal::SyntaxObject(gc) => Ok(SteelVal::StringV(format!("{:p}", gc).into())),
                // SteelVal::Boxed(gc) => Ok(SteelVal::StringV(format!("{:p}", gc).into())),
                // SteelVal::HeapAllocated(heap_ref) => {
                //     Ok(SteelVal::StringV(format!("{:p}", gc).into()))
                // }
                // SteelVal::Reference(gc) => Ok(SteelVal::StringV(format!("{:p}", gc).into())),
                // SteelVal::BigNum(gc) => Ok(SteelVal::StringV(format!("{:p}", gc).into())),
                // SteelVal::BigRational(gc) => Ok(SteelVal::StringV(format!("{:p}", gc).into())),
                // SteelVal::Complex(gc) => Ok(SteelVal::StringV(format!("{:p}", gc).into())),
                // SteelVal::ByteVector(steel_byte_vector) => {
                //     Ok(SteelVal::StringV(format!("{:p}", gc).into()))
                // }
                _ => Ok(SteelVal::BoolV(false)),
            }
        })
    }

    pub fn assert_truthy() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 1 {
                stop!(ArityMismatch => "assert takes one argument")
            }
            if let SteelVal::BoolV(true) = &args[0] {
                Ok(SteelVal::Void)
            } else {
                panic!("Value given not true!")
            }
        })
    }

    // Uses a generic executor w/ the compat struct in order to allow tokio ecosystem functions inside
    // the interpreter
    // pub fn exec_async() -> SteelVal {
    //     SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
    //         let mut executor = LocalPool::new();

    //         let joined_futures: Vec<_> = args
    //             .into_iter()
    //             .map(|x| {
    //                 if let SteelVal::FutureV(f) = x {
    //                     Ok(f.unwrap().into_shared())
    //                 } else {
    //                     stop!(TypeMismatch => "exec-async given non future")
    //                 }
    //             })
    //             .collect::<Result<Vec<_>>>()?;

    //         let futures = join_all(joined_futures);

    //         // spawner.spawn_local_obj(joined_futures);

    //         // let future = LocalFutureObj::new(Box::pin(async {}));
    //         // spawner.spawn_local_obj(future);
    //         // executor.run_until(future);
    //         Ok(SteelVal::VectorV(Gc::new(
    //             executor
    //                 .run_until(Compat::new(futures))
    //                 .into_iter()
    //                 .collect::<Result<_>>()?,
    //         )))
    //     })
    // }

    pub fn poll_value() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 1 {
                stop!(Generic => "poll! only takes one argument");
            }

            if let SteelVal::FutureV(fut) = args[0].clone() {
                let fut = fut.unwrap();
                let ready = poll_future(fut.into_shared());
                match ready {
                    Some(v) => v,
                    None => Ok(SteelVal::BoolV(false)),
                }
            } else {
                stop!(Generic => "poll! accepts futures only");
            }
        })
    }

    pub fn block_on_with_local_executor() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 1 {
                stop!(Generic => "block-on! only takes one argument");
            }

            if let SteelVal::FutureV(fut) = args[0].clone() {
                futures_executor::block_on(fut.unwrap().into_shared())
            } else {
                stop!(Generic => "block-on! accepts futures only");
            }
        })
    }

    pub fn block_on() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 1 {
                stop!(Generic => "block-on! only takes one argument");
            }

            if let SteelVal::FutureV(fut) = args[0].clone() {
                loop {
                    let fut = fut.unwrap();
                    let ready = poll_future(fut.into_shared());
                    if let Some(v) = ready {
                        return v;
                    }
                }
            } else {
                stop!(Generic => "block-on! accepts futures only");
            }
        })
    }

    pub fn join_futures() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.is_empty() {
                stop!(Generic => "join! requires at least one argument");
            }

            let joined_futures: Vec<_> = args
                .iter()
                .map(|x| {
                    if let SteelVal::FutureV(f) = x {
                        Ok(f.unwrap().into_shared())
                    } else {
                        stop!(TypeMismatch => "join! given non future")
                    }
                })
                .collect::<Result<Vec<_>>>()?;

            let futures = join_all(joined_futures).map(|x| {
                x.into_iter()
                    .collect::<Result<crate::values::Vector<_>>>()
                    .map(|x| SteelVal::VectorV(Gc::new(x).into()))
            });

            Ok(SteelVal::FutureV(Gc::new(FutureResult::new(Box::pin(
                futures,
            )))))
        })
    }
}

// pub(crate) fn steel_box(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
//     if args.len() != 1 {
//         builtin_stop!(ArityMismatch => "box takes one argument, found: {}", args.len())
//     }

//     let arg = args[0].clone();

//     // Allocate the variable directly on the heap
//     let allocated_var = ctx.thread.heap.allocate(
//         arg,
//         ctx.thread.stack.iter(),
//         ctx.thread.stack_frames.iter().map(|x| x.function.as_ref()),
//         ctx.thread.global_env.roots(),
//     );

//     Some(Ok(SteelVal::Boxed(allocated_var)))
// }
