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

use futures::executor::{LocalPool, LocalSpawner};
use futures::future::join_all;
use futures::future::LocalFutureObj;
use futures::task::LocalSpawn;

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

    pub fn exec_async() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            let mut executor = LocalPool::new();
            let spawner = executor.spawner();

            // let joined_futures: Vec<_> = args
            //     .into_iter()
            //     .map(|x| {
            //         if let SteelVal::FutureV(f) = x.as_ref() {
            //             Ok(f.into_shared())
            //         } else {
            //             stop!(TypeMismatch => "exec-async given non future")
            //         }
            //     })
            //     .collect::<Result<Vec<_>>>()?;

            // spawner.spawn_local_obj(joined_futures);

            // let future = LocalFutureObj::new(Box::pin(async {}));
            // spawner.spawn_local_obj(future);
            // executor.run_until(future);
            Ok(Gc::new(SteelVal::IntV(executor.run_until(async { 10 }))))

            // unimplemented!()
        })
    }

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
