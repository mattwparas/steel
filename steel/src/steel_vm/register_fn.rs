use std::{future::Future, marker::PhantomData, rc::Rc};

use super::engine::Engine;
use crate::rvals::{FromSteelVal, IntoSteelVal, Result, SteelVal};
use crate::stop;
use crate::{
    rerrs::{ErrorKind, SteelErr},
    rvals::FutureResult,
};
use futures::FutureExt;

/// Trait for allowing any function that satisfies the `Fn` trait to be embedded in the engine
/// This allows for clean embedding of function pointers as well as closures that capture immutable environments
pub trait RegisterFn<FN, ARGS, RET> {
    fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self;
}

pub trait RegisterAsyncFn<FN, ARGS, RET> {
    fn register_async_fn(&mut self, name: &'static str, func: FN) -> &mut Self;
}

// Exists only to provides some bounds for the impl_register_fn implementation
// Without this, upstream crates could provides alternative implementations for (_,_), (_,_,_), etc.
// This allows us to get away with some funny business in the arguments
pub struct Wrapper<ARGS>(PhantomData<ARGS>);

impl<
        FUT: Future<Output = RET> + 'static,
        RET: IntoSteelVal + 'static,
        FN: Fn() -> FUT + 'static,
    > RegisterAsyncFn<FN, Wrapper<()>, RET> for Engine
{
    fn register_async_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
        let f = move |args: &[SteelVal]| -> Result<FutureResult> {
            if args.len() != 0 {
                stop!(ArityMismatch => format!("{} expected 0 arguments, got {}", name, args.len()));
            }

            let res = func();

            Ok(FutureResult::new(Box::pin(res.map(|x| x.into_steelval()))))
        };

        self.register_value(name, SteelVal::FutureFunc(Rc::new(f)))
        // unimplemented!()
    }
}

impl<RET: IntoSteelVal, FN: Fn() -> RET + 'static> RegisterFn<FN, Wrapper<()>, RET> for Engine {
    fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 0 {
                stop!(ArityMismatch => format!("{} expected 0 arguments, got {}", name, args.len()));
            }

            let res = func();

            res.into_steelval()
        };

        self.register_value(name, SteelVal::BoxedFunction(Rc::new(f)))
    }
}

macro_rules! impl_register_fn {
    ($arg_count:expr => $($param:ident: $idx:expr),*) => {
        impl<
            $($param: FromSteelVal,)*
            FN: Fn($($param),*) -> RET + 'static,
            RET: IntoSteelVal
        > RegisterFn<FN, Wrapper<($($param,)*)>, RET> for Engine {
            fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
                let f = move |args: &[SteelVal]| -> Result<SteelVal> {
                    if args.len() != $arg_count {
                        stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, $arg_count, args.len()));
                    }

                    let res = func($(<$param>::from_steelval(args[$idx].clone())?,)*);

                    res.into_steelval()
                };

                self.register_value(name, SteelVal::BoxedFunction(Rc::new(f)))
            }
        }
    };
}

macro_rules! impl_register_async_fn {
    ($arg_count:expr => $($param:ident: $idx:expr),*) => {
        impl<
            FUT: Future<Output = RET> + 'static,
            $($param: FromSteelVal,)*
            FN: Fn($($param),*) -> FUT + 'static,
            RET: IntoSteelVal
        > RegisterAsyncFn<FN, Wrapper<($($param,)*)>, RET> for Engine {
            fn register_async_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
                let f = move |args: &[SteelVal]| -> Result<FutureResult> {
                    if args.len() != $arg_count {
                        stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, $arg_count, args.len()));
                    }

                    let res = func($(<$param>::from_steelval(args[$idx].clone())?,)*);

                    Ok(FutureResult::new(Box::pin(res.map(|x| x.into_steelval()))))
                };

                self.register_value(name, SteelVal::FutureFunc(Rc::new(f)))
            }
        }
    };
}

impl_register_fn!(1 => A:0);
impl_register_fn!(2 => A:0, B:1);
impl_register_fn!(3 => A:0, B:1, C:2);
impl_register_fn!(4 => A:0, B:1, C:2, D:3);
impl_register_fn!(5 => A:0, B:1, C:2, D:3, E:4);
impl_register_fn!(6 => A:0, B:1, C:2, D:3, E:4, F:5);
impl_register_fn!(7 => A:0, B:1, C:2, D:3, E:4, F:5, G:6);
impl_register_fn!(8 => A:0, B:1, C:2, D:3, E:4, F:5, G:6, H:7);
impl_register_fn!(9 => A:0, B:1, C:2, D:3, E:4, F:5, G:6, H:7, I:8);
impl_register_fn!(10 => A:0, B:1, C:2, D:3, E:4, F:5, G:6, H:7, I:8, J:9);
impl_register_fn!(11 => A:0, B:1, C:2, D:3, E:4, F:5, G:6, H:7, I:8, J:9, K:10);
impl_register_fn!(12 => A:0, B:1, C:2, D:3, E:4, F:5, G:6, H:7, I:8, J:9, K:10, L:11);
impl_register_fn!(13 => A:0, B:1, C:2, D:3, E:4, F:5, G:6, H:7, I:8, J:9, K:10, L:11, M: 12);
impl_register_fn!(14 => A:0, B:1, C:2, D:3, E:4, F:5, G:6, H:7, I:8, J:9, K:10, L:11, M: 12, N: 13);
impl_register_fn!(15 => A:0, B:1, C:2, D:3, E:4, F:5, G:6, H:7, I:8, J:9, K:10, L:11, M: 12, N: 13, O: 14);
impl_register_fn!(16 => A:0, B:1, C:2, D:3, E:4, F:5, G:6, H:7, I:8, J:9, K:10, L:11, M: 12, N: 14, O: 14, P: 15);

impl_register_async_fn!(1 => A:0);
impl_register_async_fn!(2 => A:0, B:1);
impl_register_async_fn!(3 => A:0, B:1, C:2);
impl_register_async_fn!(4 => A:0, B:1, C:2, D:3);
impl_register_async_fn!(5 => A:0, B:1, C:2, D:3, E:4);
impl_register_async_fn!(6 => A:0, B:1, C:2, D:3, E:4, F:5);
impl_register_async_fn!(7 => A:0, B:1, C:2, D:3, E:4, F:5, G:6);
impl_register_async_fn!(8 => A:0, B:1, C:2, D:3, E:4, F:5, G:6, H:7);
impl_register_async_fn!(9 => A:0, B:1, C:2, D:3, E:4, F:5, G:6, H:7, I:8);
impl_register_async_fn!(10 => A:0, B:1, C:2, D:3, E:4, F:5, G:6, H:7, I:8, J:9);
impl_register_async_fn!(11 => A:0, B:1, C:2, D:3, E:4, F:5, G:6, H:7, I:8, J:9, K:10);
impl_register_async_fn!(12 => A:0, B:1, C:2, D:3, E:4, F:5, G:6, H:7, I:8, J:9, K:10, L:11);
impl_register_async_fn!(13 => A:0, B:1, C:2, D:3, E:4, F:5, G:6, H:7, I:8, J:9, K:10, L:11, M: 12);
impl_register_async_fn!(14 => A:0, B:1, C:2, D:3, E:4, F:5, G:6, H:7, I:8, J:9, K:10, L:11, M: 12, N: 13);
impl_register_async_fn!(15 => A:0, B:1, C:2, D:3, E:4, F:5, G:6, H:7, I:8, J:9, K:10, L:11, M: 12, N: 13, O: 14);
impl_register_async_fn!(16 => A:0, B:1, C:2, D:3, E:4, F:5, G:6, H:7, I:8, J:9, K:10, L:11, M: 12, N: 14, O: 14, P: 15);
