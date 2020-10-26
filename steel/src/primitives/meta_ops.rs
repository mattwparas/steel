// use crate::env::{FALSE, TRUE};
use crate::rerrs::SteelErr;
// use crate::rvals::SteelVal::*;
use crate::env::VOID;
use crate::gc::Gc;
// use crate::rvals::gc_get_size;
use crate::rvals::{Result, SteelVal};
use crate::stop;
// use crate::rvals::MemSize;

// use crate::primitives::lists::ListOperations;

use futures::executor::LocalPool;
use futures::future::join_all;
// use futures::future::LocalFutureObj;
// use futures::task::LocalSpawn;

// use tokio::runtime;

use async_compat::Compat;

use std::cell::RefCell;

pub struct MetaOperations {}
impl MetaOperations {
    pub fn inspect_bytecode() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            // let mut error_message = String::new();

            if args.len() == 1 {
                if let SteelVal::Closure(bytecode_lambda) = args[0].as_ref() {
                    crate::vm::pretty_print_dense_instructions(&bytecode_lambda.body_exp());
                    Ok(VOID.with(|f| Gc::clone(f)))
                } else {
                    stop!(TypeMismatch => "inspect-bytecode expects a closure object");
                }
            } else {
                stop!(ArityMismatch => "inspect-bytecode takes only one argument");
            }
        })
    }

    pub fn memory_address() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() != 1 {
                stop!(ArityMismatch => "memory address takes one address")
            }

            let memory_address = format!("{:p}", &args[0].as_ptr());

            Ok(Gc::new(SteelVal::StringV(memory_address)))
        })
    }

    pub fn assert_truthy() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() != 1 {
                stop!(ArityMismatch => "assert takes one argument")
            }
            // println!("Arg here: {}")
            if let SteelVal::BoolV(true) = &args[0].as_ref() {
                Ok(Gc::new(SteelVal::Void))
            } else {
                panic!("Value given not true!")
            }
            // assert!(&args[0].is_truthy());
            // Ok(Gc::new(SteelVal::Void))
        })
    }

    // TODO
    pub fn new_box() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() != 1 {
                stop!(ArityMismatch => "box takes one argument")
            }

            Ok(Gc::new(SteelVal::BoxV(RefCell::new(Gc::clone(&args[0])))))

            // println!("Arg here: {}")
            // if let SteelVal::BoolV(true) = &args[0].as_ref() {
            //     Ok(Gc::new(SteelVal::Void))
            // } else {
            //     panic!("Value given not true!")
            // }
        })
    }

    // TODO
    pub fn unbox() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() != 1 {
                stop!(ArityMismatch => "unbox takes one argument")
            }
            if let SteelVal::BoxV(inner) = &args[0].as_ref() {
                Ok(inner.clone().into_inner())
            } else {
                stop!(TypeMismatch => "unbox takes a box")
            }
        })
    }

    // TODO
    pub fn set_box() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() != 2 {
                stop!(ArityMismatch => "setbox! takes two arguments")
            }
            if let SteelVal::BoxV(inner) = &args[0].as_ref() {
                Ok(inner.replace(Gc::clone(&args[1])))
            } else {
                stop!(TypeMismatch => "setbox! takes a box")
            }
        })
    }

    // Uses a generic executor w/ the compat struct in order to allow tokio ecosystem functions inside
    // the interpreter
    pub fn exec_async() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            let mut executor = LocalPool::new();

            let joined_futures: Vec<_> = args
                .into_iter()
                .map(|x| {
                    if let SteelVal::FutureV(f) = x.as_ref() {
                        Ok(f.clone().into_shared())
                    } else {
                        stop!(TypeMismatch => "exec-async given non future")
                    }
                })
                .collect::<Result<Vec<_>>>()?;

            let futures = join_all(joined_futures);

            // spawner.spawn_local_obj(joined_futures);

            // let future = LocalFutureObj::new(Box::pin(async {}));
            // spawner.spawn_local_obj(future);
            // executor.run_until(future);
            Ok(Gc::new(SteelVal::VectorV(
                executor
                    .run_until(Compat::new(futures))
                    .into_iter()
                    .collect::<Result<_>>()?,
            )))

            // unimplemented!()
        })
    }

    // pub fn tokio_exec() -> SteelVal {
    //     SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
    //         // let mut executor = LocalPool::new();

    //         let mut basic_rt = runtime::Builder::new()
    //             .basic_scheduler()
    //             .enable_all()
    //             .build()
    //             .unwrap();

    //         // let ret = Builder::new().threaded_scheduler().enable_all().build();

    //         let joined_futures: Vec<_> = args
    //             .into_iter()
    //             .map(|x| {
    //                 if let SteelVal::FutureV(f) = x.as_ref() {
    //                     Ok(f.clone().into_shared())
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
    //         Ok(Gc::new(SteelVal::VectorV(
    //             basic_rt
    //                 .block_on(futures)
    //                 .into_iter()
    //                 .collect::<Result<_>>()?,
    //         )))

    //         // unimplemented!()
    //     })
    // }

    // pub fn size_of() -> SteelVal {
    //     SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
    //         // let mut error_message = String::new();
    //         if args.len() == 1 {
    //             Ok(Gc::new(SteelVal::IntV(
    //                 gc_get_size(Gc::clone(&args[0])) as isize
    //             )))

    //         // if let SteelVal::Closure(bytecode_lambda) = args[0].as_ref() {
    //         //     crate::vm::pretty_print_dense_instructions(&bytecode_lambda.body_exp());
    //         //     Ok(VOID.with(|f| Rc::clone(f)))
    //         // } else {
    //         //     stop!(TypeMismatch => "inspect-bytecode expects a closure object");
    //         // }
    //         } else {
    //             stop!(ArityMismatch => "sizeof takes only one argument");
    //         }
    //     })
    // }
}
