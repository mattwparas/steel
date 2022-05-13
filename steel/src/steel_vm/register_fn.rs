use std::{future::Future, marker::PhantomData, rc::Rc};

use super::engine::Engine;
use crate::rvals::{AsRefMutSteelVal, FutureResult};
use crate::rvals::{AsRefSteelVal, FromSteelVal, IntoSteelVal, Result, SteelVal};
use crate::steel_vm::builtin::BuiltInModule;
use crate::stop;
use futures::FutureExt;

/// Trait for allowing any function that satisfies the `Fn` trait to be embedded in the engine
/// This allows for clean embedding of function pointers as well as closures that capture immutable environments
pub trait RegisterFn<FN, ARGS, RET> {
    fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self;
}

// Exists only to provides some bounds for the impl_register_fn implementation
// Without this, upstream crates could provides alternative implementations for (_,_), (_,_,_), etc.
// This allows us to get away with some funny business in the arguments
pub struct Wrapper<ARGS>(PhantomData<ARGS>);

pub struct AsyncWrapper<ARGS>(PhantomData<ARGS>);

impl<
        FUT: Future<Output = RET> + 'static,
        RET: IntoSteelVal + 'static,
        FN: Fn() -> FUT + 'static,
    > RegisterFn<FN, AsyncWrapper<()>, RET> for Engine
{
    fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
        let f = move |args: &[SteelVal]| -> Result<FutureResult> {
            if !args.is_empty() {
                stop!(ArityMismatch => format!("{} expected 0 arguments, got {}", name, args.len()));
            }

            let res = func();

            Ok(FutureResult::new(Box::pin(res.map(|x| x.into_steelval()))))
        };

        self.register_value(name, SteelVal::FutureFunc(Rc::new(f)))
    }
}

impl<RET: IntoSteelVal, FN: Fn() -> RET + 'static> RegisterFn<FN, Wrapper<()>, RET> for Engine {
    fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if !args.is_empty() {
                stop!(ArityMismatch => format!("{} expected 0 arguments, got {}", name, args.len()));
            }

            let res = func();

            res.into_steelval()
        };

        self.register_value(name, SteelVal::BoxedFunction(Rc::new(f)))
    }
}

impl<RET: IntoSteelVal, SELF: AsRefSteelVal, FN: Fn(&SELF) -> RET + 'static>
    RegisterFn<FN, MarkerWrapper1<SELF>, RET> for Engine
{
    fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
        // use std::Borrow();

        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 1 {
                stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, 0, args.len()));
            }

            let input = <SELF>::as_ref(&args[0])?;

            let res = func(&input);

            res.into_steelval()
        };

        self.register_value(name, SteelVal::BoxedFunction(Rc::new(f)))
    }
}

impl<RET: IntoSteelVal, SELF: AsRefMutSteelVal, FN: Fn(&mut SELF) -> RET + 'static>
    RegisterFn<FN, MarkerWrapper2<SELF>, RET> for Engine
{
    fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
        // use std::Borrow();

        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 1 {
                stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, 0, args.len()));
            }

            let mut input = <SELF>::as_mut_ref(&args[0])?;

            let res = func(&mut input);

            res.into_steelval()
        };

        self.register_value(name, SteelVal::BoxedFunction(Rc::new(f)))
    }
}

impl<
        FUT: Future<Output = RET> + 'static,
        RET: IntoSteelVal + 'static,
        FN: Fn() -> FUT + 'static,
    > RegisterFn<FN, AsyncWrapper<()>, RET> for BuiltInModule
{
    fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
        let f = move |args: &[SteelVal]| -> Result<FutureResult> {
            if !args.is_empty() {
                stop!(ArityMismatch => format!("{} expected 0 arguments, got {}", name, args.len()));
            }

            let res = func();

            Ok(FutureResult::new(Box::pin(res.map(|x| x.into_steelval()))))
        };

        self.register_value(name, SteelVal::FutureFunc(Rc::new(f)))
    }
}

impl<RET: IntoSteelVal, FN: Fn() -> RET + 'static> RegisterFn<FN, Wrapper<()>, RET>
    for BuiltInModule
{
    fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if !args.is_empty() {
                stop!(ArityMismatch => format!("{} expected 0 arguments, got {}", name, args.len()));
            }

            let res = func();

            res.into_steelval()
        };

        self.register_value(name, SteelVal::BoxedFunction(Rc::new(f)))
    }
}

impl<RET: IntoSteelVal, SELF: AsRefSteelVal, FN: Fn(&SELF) -> RET + 'static>
    RegisterFn<FN, MarkerWrapper1<SELF>, RET> for BuiltInModule
{
    fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 1 {
                stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, 0, args.len()));
            }

            let input = <SELF>::as_ref(&args[0])?;

            let res = func(&input);

            res.into_steelval()
        };

        self.register_value(name, SteelVal::BoxedFunction(Rc::new(f)))
    }
}

impl<RET: IntoSteelVal, SELF: AsRefMutSteelVal, FN: Fn(&mut SELF) -> RET + 'static>
    RegisterFn<FN, MarkerWrapper2<SELF>, RET> for BuiltInModule
{
    fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
        // use std::Borrow();

        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 1 {
                stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, 0, args.len()));
            }

            let mut input = <SELF>::as_mut_ref(&args[0])?;

            let res = func(&mut input);

            res.into_steelval()
        };

        self.register_value(name, SteelVal::BoxedFunction(Rc::new(f)))
    }
}

// TODO: Use this in the procedural macro to generate the combinations
// #[test]
// fn test_combinations() {
//     // use itertools::Itertools;

//     // let combos = std::iter::repeat(0)
//     //     .take(4)
//     //     .cartesian_product(std::iter::repeat(1).take(4))
//     //     .collect::<Vec<_>>();

//     let combos = (0..512)
//         .map(|x| format!("{:08b}", x))
//         .map(|x| {
//             x.chars()
//                 .map(|x| if x == '1' { 1 } else { 0 })
//                 .collect::<Vec<_>>()
//         })
//         .collect::<Vec<_>>();

//     println!("{:?}", combos);
// }

// pub struct Marker {}

pub struct MarkerWrapper1<ARGS>(PhantomData<ARGS>);
pub struct MarkerWrapper2<ARGS>(PhantomData<ARGS>);
pub struct MarkerWrapper3<ARGS>(PhantomData<ARGS>);

pub struct MarkerWrapper4<ARGS>(PhantomData<ARGS>);

impl<A: AsRefSteelVal, B: AsRefSteelVal, FN: Fn(&A, &B) -> RET + 'static, RET: IntoSteelVal>
    RegisterFn<FN, MarkerWrapper1<(A, B)>, RET> for Engine
{
    fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 2 {
                stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, 2, args.len()));
            }

            let one = A::as_ref(&args[1])?;
            let two = B::as_ref(&args[1])?;

            let res = func(&one, &two);

            res.into_steelval()
        };

        self.register_value(name, SteelVal::BoxedFunction(Rc::new(f)))
    }
}

impl<A: FromSteelVal, B: AsRefSteelVal, FN: Fn(A, &B) -> RET + 'static, RET: IntoSteelVal>
    RegisterFn<FN, MarkerWrapper2<(A, B)>, RET> for Engine
{
    fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 2 {
                stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, 2, args.len()));
            }

            let input = B::as_ref(&args[1])?;

            let res = func(A::from_steelval(&args[0])?, &input);

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

                    let res = func($(<$param>::from_steelval(&args[$idx])?,)*);

                    res.into_steelval()
                };

                self.register_value(name, SteelVal::BoxedFunction(Rc::new(f)))
            }
        }

        impl<
            $($param: FromSteelVal,)*
            FN: Fn($($param),*) -> RET + 'static,
            RET: IntoSteelVal
        > RegisterFn<FN, Wrapper<($($param,)*)>, RET> for BuiltInModule {
            fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
                let f = move |args: &[SteelVal]| -> Result<SteelVal> {
                    if args.len() != $arg_count {
                        stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, $arg_count, args.len()));
                    }

                    let res = func($(<$param>::from_steelval(&args[$idx])?,)*);

                    res.into_steelval()
                };

                self.register_value(name, SteelVal::BoxedFunction(Rc::new(f)))
            }
        }


        // Async functions

        impl<
            FUT: Future<Output = RET> + 'static,
            $($param: FromSteelVal,)*
            FN: Fn($($param),*) -> FUT + 'static,
            RET: IntoSteelVal
        > RegisterFn<FN, AsyncWrapper<($($param,)*)>, RET> for Engine {
            fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
                let f = move |args: &[SteelVal]| -> Result<FutureResult> {
                    if args.len() != $arg_count {
                        stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, $arg_count, args.len()));
                    }

                    let res = func($(<$param>::from_steelval(&args[$idx])?,)*);

                    Ok(FutureResult::new(Box::pin(res.map(|x| x.into_steelval()))))
                };

                self.register_value(name, SteelVal::FutureFunc(Rc::new(f)))
            }
        }

        impl<
            FUT: Future<Output = RET> + 'static,
            $($param: FromSteelVal,)*
            FN: Fn($($param),*) -> FUT + 'static,
            RET: IntoSteelVal
        > RegisterFn<FN, AsyncWrapper<($($param,)*)>, RET> for BuiltInModule {
            fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
                let f = move |args: &[SteelVal]| -> Result<FutureResult> {
                    if args.len() != $arg_count {
                        stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, $arg_count, args.len()));
                    }

                    let res = func($(<$param>::from_steelval(&args[$idx])?,)*);

                    Ok(FutureResult::new(Box::pin(res.map(|x| x.into_steelval()))))
                };

                self.register_value(name, SteelVal::FutureFunc(Rc::new(f)))
            }
        }
    };
}

macro_rules! impl_register_fn_self {
    ($arg_count:expr => $($param:ident: $idx:expr),*) => {
        impl<
            SELF: AsRefSteelVal,
            $($param: FromSteelVal,)*
            FN: Fn(&SELF, $($param),*) -> RET + 'static,
            RET: IntoSteelVal
        > RegisterFn<FN, MarkerWrapper3<(SELF, $($param,)*)>, RET> for Engine {
            fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
                let f = move |args: &[SteelVal]| -> Result<SteelVal> {
                    if args.len() != $arg_count {
                        stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, $arg_count, args.len()));
                    }

                    let input = <SELF>::as_ref(&args[0])?;

                    let res = func(&input, $(<$param>::from_steelval(&args[$idx])?,)*);

                    res.into_steelval()
                };

                self.register_value(name, SteelVal::BoxedFunction(Rc::new(f)))
            }
        }

        impl<
            SELF: AsRefMutSteelVal,
            $($param: FromSteelVal,)*
            FN: Fn(&mut SELF, $($param),*) -> RET + 'static,
            RET: IntoSteelVal
        > RegisterFn<FN, MarkerWrapper4<(SELF, $($param,)*)>, RET> for Engine {
            fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
                let f = move |args: &[SteelVal]| -> Result<SteelVal> {
                    if args.len() != $arg_count {
                        stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, $arg_count, args.len()));
                    }

                    let mut input = <SELF>::as_mut_ref(&args[0])?;

                    let res = func(&mut input, $(<$param>::from_steelval(&args[$idx])?,)*);

                    res.into_steelval()
                };

                self.register_value(name, SteelVal::BoxedFunction(Rc::new(f)))
            }
        }

        impl<
            SELF: AsRefSteelVal,
            $($param: FromSteelVal,)*
            FN: Fn(&SELF, $($param),*) -> RET + 'static,
            RET: IntoSteelVal
        > RegisterFn<FN, MarkerWrapper1<(SELF, $($param,)*)>, RET> for BuiltInModule {
            fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
                let f = move |args: &[SteelVal]| -> Result<SteelVal> {
                    if args.len() != $arg_count {
                        stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, $arg_count, args.len()));
                    }

                    let input = <SELF>::as_ref(&args[0])?;

                    let res = func(&input, $(<$param>::from_steelval(&args[$idx])?,)*);

                    res.into_steelval()
                };

                self.register_value(name, SteelVal::BoxedFunction(Rc::new(f)))
            }
        }

        impl<
            SELF: AsRefMutSteelVal,
            $($param: FromSteelVal,)*
            FN: Fn(&mut SELF, $($param),*) -> RET + 'static,
            RET: IntoSteelVal
        > RegisterFn<FN, MarkerWrapper3<(SELF, $($param,)*)>, RET> for BuiltInModule {
            fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
                let f = move |args: &[SteelVal]| -> Result<SteelVal> {
                    if args.len() != $arg_count {
                        stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, $arg_count, args.len()));
                    }

                    let mut input = <SELF>::as_mut_ref(&args[0])?;

                    let res = func(&mut input, $(<$param>::from_steelval(&args[$idx])?,)*);

                    res.into_steelval()
                };

                self.register_value(name, SteelVal::BoxedFunction(Rc::new(f)))
            }
        }
    };
}

// impl_register_fn_two!(1 => A:0);

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

impl_register_fn_self!(2 => B:1);
impl_register_fn_self!(3 => B:1, C:2);
impl_register_fn_self!(4 => B:1, C:2, D:3);
impl_register_fn_self!(5 => B:1, C:2, D:3, E:4);
impl_register_fn_self!(6 => B:1, C:2, D:3, E:4, F:5);
impl_register_fn_self!(7 => B:1, C:2, D:3, E:4, F:5, G:6);
impl_register_fn_self!(8 => B:1, C:2, D:3, E:4, F:5, G:6, H:7);
impl_register_fn_self!(9 => B:1, C:2, D:3, E:4, F:5, G:6, H:7, I:8);
impl_register_fn_self!(10 => B:1, C:2, D:3, E:4, F:5, G:6, H:7, I:8, J:9);
impl_register_fn_self!(11 => B:1, C:2, D:3, E:4, F:5, G:6, H:7, I:8, J:9, K:10);
impl_register_fn_self!(12 => B:1, C:2, D:3, E:4, F:5, G:6, H:7, I:8, J:9, K:10, L:11);
impl_register_fn_self!(13 => B:1, C:2, D:3, E:4, F:5, G:6, H:7, I:8, J:9, K:10, L:11, M: 12);
impl_register_fn_self!(14 => B:1, C:2, D:3, E:4, F:5, G:6, H:7, I:8, J:9, K:10, L:11, M: 12, N: 13);
impl_register_fn_self!(15 => B:1, C:2, D:3, E:4, F:5, G:6, H:7, I:8, J:9, K:10, L:11, M: 12, N: 13, O: 14);
impl_register_fn_self!(16 => B:1, C:2, D:3, E:4, F:5, G:6, H:7, I:8, J:9, K:10, L:11, M: 12, N: 14, O: 14, P: 15);
