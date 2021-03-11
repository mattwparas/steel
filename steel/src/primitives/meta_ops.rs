use crate::gc::{get_object_count, Gc};
use crate::rerrs::{ErrorKind, SteelErr};
use crate::rvals::{Result, SteelVal};
use crate::stop;

use futures::{executor::LocalPool, future::join_all};

use async_compat::Compat;

use std::cell::RefCell;

pub struct MetaOperations {}
impl MetaOperations {
    pub fn inspect_bytecode() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            // let mut error_message = String::new();

            if args.len() == 1 {
                if let SteelVal::Closure(bytecode_lambda) = &args[0] {
                    crate::core::instructions::pretty_print_dense_instructions(
                        &bytecode_lambda.body_exp(),
                    );
                    Ok(SteelVal::Void)
                } else {
                    stop!(TypeMismatch => "inspect-bytecode expects a closure object");
                }
            } else {
                stop!(ArityMismatch => "inspect-bytecode takes only one argument");
            }
        })
    }

    pub fn active_objects() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 0 {
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

            // let memory_address = format!("{:p}", &args[0].as_ptr());

            Ok(SteelVal::StringV("TODO".into())) // TODO come back here
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

    // TODO
    pub fn new_box() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 1 {
                stop!(ArityMismatch => "box takes one argument")
            }

            Ok(SteelVal::BoxV(Gc::new(RefCell::new(args[0].clone()))))
        })
    }

    // TODO
    pub fn unbox() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 1 {
                stop!(ArityMismatch => "unbox takes one argument")
            }
            if let SteelVal::BoxV(inner) = &args[0] {
                Ok(inner.unwrap().into_inner())
            } else {
                stop!(TypeMismatch => "unbox takes a box")
            }
        })
    }

    // TODO
    pub fn set_box() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 2 {
                stop!(ArityMismatch => "setbox! takes two arguments")
            }
            if let SteelVal::BoxV(inner) = &args[0] {
                Ok(inner.replace(args[1].clone()))
            } else {
                stop!(TypeMismatch => "setbox! takes a box")
            }
        })
    }

    // Uses a generic executor w/ the compat struct in order to allow tokio ecosystem functions inside
    // the interpreter
    pub fn exec_async() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            let mut executor = LocalPool::new();

            let joined_futures: Vec<_> = args
                .into_iter()
                .map(|x| {
                    if let SteelVal::FutureV(f) = x {
                        Ok(f.unwrap().into_shared())
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
            Ok(SteelVal::VectorV(Gc::new(
                executor
                    .run_until(Compat::new(futures))
                    .into_iter()
                    .collect::<Result<_>>()?,
            )))
        })
    }
}
