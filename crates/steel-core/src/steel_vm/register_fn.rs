#![allow(unused)]

use alloc::{rc::Rc, sync::Arc};
use core::{cell::RefCell, future::Future, marker::PhantomData, ops::DerefMut};

use super::{
    builtin::{Arity, FunctionSignatureMetadata},
    engine::Engine,
};
use crate::{
    gc::{
        shared::MutContainer,
        unsafe_erased_pointers::{ReadOnlyTemporaryObject, TemporaryObject},
    },
    steel_vm::builtin::BuiltInModule,
};
use crate::{
    gc::{
        shared::StandardShared,
        unsafe_erased_pointers::{
            BorrowedObject, OpaqueReferenceNursery, ReadOnlyBorrowedObject, ReadOnlyTemporary,
            Temporary,
        },
        Shared,
    },
    rvals::{
        AsRefMutSteelValFromRef, AsRefSteelVal, AsRefSteelValFromUnsized, AsSlice, FromSteelVal,
        IntoSteelVal, Result, SteelVal,
    },
};
use crate::{
    gc::{unsafe_erased_pointers::increment_borrow_flag, Gc},
    rvals::MaybeSendSyncStatic,
    values::lists::List,
};
use crate::{rvals::AsRefSteelValFromRef, stop};
use crate::{
    rvals::{AsRefMutSteelVal, FutureResult},
    values::functions::BoxedDynFunction,
};
use futures_util::FutureExt;

use crate::containers::RegisterValue;

/// Trait for allowing any function that satisfies the `Fn` trait to be embedded in the engine
/// This allows for clean embedding of function pointers as well as closures that capture immutable environments
pub trait RegisterFn<FN, ARGS, RET> {
    fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self;
    fn register_owned_fn(&mut self, name: String, func: FN) -> &mut Self {
        self
    }
    fn register_fn_with_ctx(
        &mut self,
        ctx: &'static str,
        name: &'static str,
        func: FN,
    ) -> &mut Self;
}

pub trait RegisterFnBorrowed<FN, ARGS, RET> {
    fn register_fn_borrowed(&mut self, name: &'static str, func: FN) -> &mut Self;
    // fn register_owned_fn(&mut self, name: String, func: FN) -> &mut Self {
    //     self
    // }
}

pub trait SendSyncStatic: Send + Sync + 'static {}

impl<T: Send + Sync + 'static> SendSyncStatic for T {}

// Exists only to provides some bounds for the impl_register_fn implementation
// Without this, upstream crates could provides alternative implementations for (_,_), (_,_,_), etc.
// This allows us to get away with some funny business in the arguments
pub struct Wrapper<ARGS>(PhantomData<ARGS>);

pub struct NativeWrapper;

pub struct AsyncWrapper<ARGS>(PhantomData<ARGS>);

// The _rest_ of the arguments -> This has a specialized implementation for rest arguments getting
// collected into a list, so that register_fn / the macro can use this implementation itself when writing
// functions over many args
struct RestArgs<'a> {
    args: &'a [SteelVal],
}

impl<'a> core::ops::Deref for RestArgs<'a> {
    type Target = [SteelVal];

    fn deref(&self) -> &Self::Target {
        self.args
    }
}

impl<
        FUT: Future<Output = RET> + SendSyncStatic,
        RET: IntoSteelVal + 'static,
        FN: Fn() -> FUT + SendSyncStatic,
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

        self.register_value(name, SteelVal::FutureFunc(Shared::new(Box::new(f))))
    }

    fn register_fn_with_ctx(
        &mut self,
        ctx: &'static str,
        name: &'static str,
        func: FN,
    ) -> &mut Self {
        self.register_fn(name, func);
        self.supply_context_arg(ctx, name);
        self
    }
}

// impl RegisterFn<fn(&[SteelVal]) -> Result<SteelVal>, NativeWrapper, Result<SteelVal>> for Engine {
//     fn register_fn(
//         &mut self,
//         name: &'static str,
//         func: fn(&[SteelVal]) -> Result<SteelVal>,
//     ) -> &mut Self {
//         self.register_value(name, SteelVal::FuncV(func))
//     }
// }

// impl RegisterFn<fn(&[SteelVal]) -> Result<SteelVal>, NativeWrapper, Result<SteelVal>>
//     for BuiltInModule
// {
//     fn register_fn(
//         &mut self,
//         name: &'static str,
//         func: fn(&[SteelVal]) -> Result<SteelVal>,
//     ) -> &mut Self {
//         // Just automatically add it to the function pointer table to help out with searching
//         self.add_to_fn_ptr_table(
//             func,
//             FunctionSignatureMetadata::new(name, Arity::Exact(1234)),
//         );
//         self.register_value(name, SteelVal::FuncV(func))
//     }
// }

impl<RET: IntoSteelVal, FN: Fn() -> RET + SendSyncStatic> RegisterFn<FN, Wrapper<()>, RET>
    for Engine
{
    fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if !args.is_empty() {
                stop!(ArityMismatch => format!("{} expected 0 arguments, got {}", name, args.len()));
            }

            let res = func();

            res.into_steelval()
        };

        // self.register_value(name, SteelVal::FuncV(f))

        self.register_value(
            name,
            SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new(
                Arc::new(f),
                Some(name),
                Some(0),
            ))),
        )
    }

    fn register_fn_with_ctx(
        &mut self,
        ctx: &'static str,
        name: &'static str,
        func: FN,
    ) -> &mut Self {
        self.register_fn(name, func);
        self.supply_context_arg(ctx, name);
        self
    }
}

impl<RET: IntoSteelVal, SELF: AsRefSteelVal, FN: Fn(&SELF) -> RET + SendSyncStatic>
    RegisterFn<FN, MarkerWrapper1<SELF>, RET> for Engine
{
    fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
        // use std::Borrow();

        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 1 {
                stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, 1, args.len()));
            }

            let input = <SELF>::as_ref(&args[0]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;

            let res = func(&input);

            res.into_steelval()
        };

        self.register_value(
            name,
            SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new(
                Arc::new(f),
                Some(name),
                Some(1),
            ))),
        )
    }

    fn register_fn_with_ctx(
        &mut self,
        ctx: &'static str,
        name: &'static str,
        func: FN,
    ) -> &mut Self {
        self.register_fn(name, func);
        self.supply_context_arg(ctx, name);
        self
    }
}

impl<RET: IntoSteelVal, SELF: AsRefMutSteelVal, FN: Fn(&mut SELF) -> RET + SendSyncStatic>
    RegisterFn<FN, MarkerWrapper2<SELF>, RET> for Engine
{
    fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
        // use std::Borrow();

        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 1 {
                stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, 1, args.len()));
            }

            let mut input = <SELF>::as_mut_ref(&args[0]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;

            let res = func(&mut input);

            res.into_steelval()
        };

        self.register_value(
            name,
            SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new(
                Arc::new(f),
                Some(name),
                Some(1),
            ))),
        )
    }

    fn register_fn_with_ctx(
        &mut self,
        ctx: &'static str,
        name: &'static str,
        func: FN,
    ) -> &mut Self {
        self.register_fn(name, func);
        self.supply_context_arg(ctx, name);
        self
    }
}

impl<
        RET: IntoSteelVal,
        SELF: AsRefMutSteelValFromRef,
        FN: Fn(&mut SELF) -> RET + SendSyncStatic,
    > RegisterFn<FN, MarkerWrapper5<SELF>, RET> for Engine
{
    fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
        // use std::Borrow();

        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 1 {
                stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, 1, args.len()));
            }

            let mut input = <SELF>::as_mut_ref_from_ref(&args[0]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;

            let res = func(&mut input.as_mut());

            res.into_steelval()
        };

        self.register_value(
            name,
            SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new(
                Arc::new(f),
                Some(name),
                Some(1),
            ))),
        )
    }

    fn register_fn_with_ctx(
        &mut self,
        ctx: &'static str,
        name: &'static str,
        func: FN,
    ) -> &mut Self {
        self.register_fn(name, func);
        self.supply_context_arg(ctx, name);
        self
    }
}

impl<
        RET: IntoSteelVal,
        SELF: AsRefMutSteelValFromRef,
        FRAME: AsRefMutSteelVal,
        FN: Fn(&mut SELF, &mut FRAME) -> RET + SendSyncStatic,
    > RegisterFn<FN, MarkerWrapper6<(SELF, FRAME)>, RET> for Engine
{
    fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 2 {
                stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, 2, args.len()));
            }

            let mut input = <SELF>::as_mut_ref_from_ref(&args[0]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;
            let mut area = <FRAME>::as_mut_ref(&args[1]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;

            let res = func(&mut input.as_mut(), &mut area);

            res.into_steelval()
        };

        self.register_value(
            name,
            SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new(
                Arc::new(f),
                Some(name),
                Some(2),
            ))),
        )
    }

    fn register_fn_with_ctx(
        &mut self,
        ctx: &'static str,
        name: &'static str,
        func: FN,
    ) -> &mut Self {
        self.register_fn(name, func);
        self.supply_context_arg(ctx, name);
        self
    }
}

impl<
        RET: IntoSteelVal,
        SELF: AsRefMutSteelValFromRef,
        FRAME: AsRefMutSteelVal,
        FN: Fn(&mut SELF, &mut FRAME) -> RET + SendSyncStatic,
    > RegisterFn<FN, MarkerWrapper6<(SELF, FRAME)>, RET> for BuiltInModule
{
    fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 2 {
                stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, 2, args.len()));
            }

            let mut input = <SELF>::as_mut_ref_from_ref(&args[0]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;
            let mut area = <FRAME>::as_mut_ref(&args[1]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;

            let res = func(&mut input.as_mut(), &mut area);

            res.into_steelval()
        };

        self.register_value(
            name,
            SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new(
                Arc::new(f),
                Some(name),
                Some(2),
            ))),
        )
    }

    fn register_fn_with_ctx(
        &mut self,
        ctx: &'static str,
        name: &'static str,
        func: FN,
    ) -> &mut Self {
        self.register_fn(name, func);
        self.supply_context_arg(ctx, name);
        self
    }
}

// Handle the component interface in helix. This will be the following:
impl<
        RET: IntoSteelVal,
        SELF: AsRefMutSteelVal,
        AREA: FromSteelVal,
        FRAME: AsRefMutSteelValFromRef,
        CTX: AsRefMutSteelValFromRef,
        FN: Fn(&mut SELF, AREA, &mut FRAME, &mut CTX) -> RET + SendSyncStatic,
    > RegisterFn<FN, MarkerWrapper6<(SELF, AREA, FRAME, CTX)>, RET> for Engine
{
    fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 4 {
                stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, 4, args.len()));
            }

            let mut input = <SELF>::as_mut_ref(&args[0]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;
            let mut area = <AREA>::from_steelval(&args[1]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;
            let mut frame = <FRAME>::as_mut_ref_from_ref(&args[2]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;
            let mut ctx = <CTX>::as_mut_ref_from_ref(&args[3]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;

            let res = func(&mut input, area, &mut frame.as_mut(), &mut ctx.as_mut());

            res.into_steelval()
        };

        self.register_value(
            name,
            SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new(
                Arc::new(f),
                Some(name),
                Some(4),
            ))),
        )
    }

    fn register_fn_with_ctx(
        &mut self,
        ctx: &'static str,
        name: &'static str,
        func: FN,
    ) -> &mut Self {
        self.register_fn(name, func);
        self.supply_context_arg(ctx, name);
        self
    }
}

impl<
        RET: IntoSteelVal,
        SELF: AsRefMutSteelVal,
        AREA: FromSteelVal,
        FRAME: AsRefMutSteelValFromRef,
        CTX: AsRefMutSteelValFromRef,
        FN: Fn(&mut SELF, AREA, &mut FRAME, &mut CTX) -> RET + SendSyncStatic,
    > RegisterFn<FN, MarkerWrapper6<(SELF, AREA, FRAME, CTX)>, RET> for BuiltInModule
{
    fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 4 {
                stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, 4, args.len()));
            }

            let mut input = <SELF>::as_mut_ref(&args[0]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;
            let mut area = <AREA>::from_steelval(&args[1]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;
            let mut frame = <FRAME>::as_mut_ref_from_ref(&args[2]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;
            let mut ctx = <CTX>::as_mut_ref_from_ref(&args[3]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;

            let res = func(&mut input, area, &mut frame.as_mut(), &mut ctx.as_mut());

            res.into_steelval()
        };

        self.register_value(
            name,
            SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new(
                Arc::new(f),
                Some(name),
                Some(4),
            ))),
        )
    }

    fn register_fn_with_ctx(
        &mut self,
        ctx: &'static str,
        name: &'static str,
        func: FN,
    ) -> &mut Self {
        self.register_fn(name, func);
        self.supply_context_arg(ctx, name);
        self
    }
}

impl<
        RET: IntoSteelVal,
        SELF: AsRefMutSteelVal,
        AREA: AsRefSteelVal,
        CTX: AsRefMutSteelValFromRef,
        FN: Fn(&mut SELF, &AREA, &mut CTX) -> RET + SendSyncStatic,
    > RegisterFn<FN, MarkerWrapper7<(SELF, AREA, CTX)>, RET> for Engine
{
    fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 3 {
                stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, 3, args.len()));
            }

            let mut nursery = <AREA as AsRefSteelVal>::Nursery::default();

            let mut input = <SELF>::as_mut_ref(&args[0]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;
            let mut area = <AREA>::as_ref(&args[1]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;
            let mut ctx = <CTX>::as_mut_ref_from_ref(&args[2]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;

            let res = func(&mut input, &area, &mut ctx.as_mut());

            res.into_steelval()
        };

        self.register_value(
            name,
            SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new(
                Arc::new(f),
                Some(name),
                Some(3),
            ))),
        )
    }

    fn register_fn_with_ctx(
        &mut self,
        ctx: &'static str,
        name: &'static str,
        func: FN,
    ) -> &mut Self {
        self.register_fn(name, func);
        self.supply_context_arg(ctx, name);
        self
    }
}

impl<
        RET: IntoSteelVal,
        SELF: AsRefSteelVal,
        AREA: FromSteelVal,
        CTX: AsRefSteelValFromRef,
        FN: Fn(&SELF, AREA, &CTX) -> RET + SendSyncStatic,
    > RegisterFn<FN, MarkerWrapper8<(SELF, AREA, CTX)>, RET> for Engine
{
    fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 3 {
                stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, 3, args.len()));
            }

            let mut nursery = <SELF as AsRefSteelVal>::Nursery::default();

            let mut input = <SELF>::as_ref(&args[0]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;
            let mut area = <AREA>::from_steelval(&args[1]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;
            let mut ctx = <CTX>::as_ref_from_ref(&args[2]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;

            let res = func(&input, area, ctx.as_ro());

            res.into_steelval()
        };

        self.register_value(
            name,
            SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new(
                Arc::new(f),
                Some(name),
                Some(3),
            ))),
        )
    }

    fn register_fn_with_ctx(
        &mut self,
        ctx: &'static str,
        name: &'static str,
        func: FN,
    ) -> &mut Self {
        self.register_fn(name, func);
        self.supply_context_arg(ctx, name);
        self
    }
}

// TODO: @Matt - come back and fix this because its atrocious
impl<
        RET: IntoSteelVal,
        SELF: AsRefMutSteelValFromRef,
        INNER: FromSteelVal + Clone + AsRefSteelValFromUnsized<INNER>,
        F: FromSteelVal,
        FN: Fn(&mut SELF, &[INNER], F) -> RET + SendSyncStatic,
    > RegisterFn<FN, MarkerWrapper6<(SELF, F, INNER)>, RET> for Engine
{
    fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
        // use std::Borrow();

        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 3 {
                stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, 3, args.len()));
            }

            let mut input = <SELF>::as_mut_ref_from_ref(&args[0]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;

            let temp_res = INNER::as_ref_from_unsized(&args[1]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;

            let res = func(
                &mut input.as_mut(),
                temp_res.as_slice_repr(),
                F::from_steelval(&args[2])?,
            );

            res.into_steelval()
        };

        self.register_value(
            name,
            SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new(
                Arc::new(f),
                Some(name),
                Some(3),
            ))),
        )
    }

    fn register_owned_fn(&mut self, name: String, func: FN) -> &mut Self {
        // use std::Borrow();

        let cloned_name = name.clone();

        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 3 {
                stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, 3, args.len()));
            }

            let mut input = <SELF>::as_mut_ref_from_ref(&args[0]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;

            let temp_res = INNER::as_ref_from_unsized(&args[1]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;

            let res = func(
                &mut input.as_mut(),
                temp_res.as_slice_repr(),
                F::from_steelval(&args[2])?,
            );

            res.into_steelval()
        };

        self.register_value(
            &cloned_name.to_string(),
            SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new_owned(
                Arc::new(f),
                Some(cloned_name.into()),
                Some(3),
            ))),
        )
    }

    fn register_fn_with_ctx(
        &mut self,
        ctx: &'static str,
        name: &'static str,
        func: FN,
    ) -> &mut Self {
        self.register_fn(name, func);
        self.supply_context_arg(ctx, name);
        self
    }
}

impl<
        RET: IntoSteelVal,
        SELF: AsRefMutSteelValFromRef,
        INNER: FromSteelVal + Clone + AsRefSteelValFromUnsized<INNER>,
        FN: Fn(&mut SELF, &[INNER]) -> RET + SendSyncStatic,
    > RegisterFn<FN, MarkerWrapper7<(SELF, INNER)>, RET> for Engine
{
    fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
        // use std::Borrow();

        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 2 {
                stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, 2, args.len()));
            }

            let mut input = <SELF>::as_mut_ref_from_ref(&args[0]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;

            let temp_res = INNER::as_ref_from_unsized(&args[1]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;

            let res = func(&mut input.as_mut(), temp_res.as_slice_repr());

            res.into_steelval()
        };

        self.register_value(
            name,
            SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new(
                Arc::new(f),
                Some(name),
                Some(2),
            ))),
        )
    }

    fn register_owned_fn(&mut self, name: String, func: FN) -> &mut Self {
        // use std::Borrow();

        let cloned_name = name.clone();

        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 2 {
                stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, 2, args.len()));
            }

            let mut input = <SELF>::as_mut_ref_from_ref(&args[0]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;

            let temp_res = INNER::as_ref_from_unsized(&args[1]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;

            let res = func(&mut input.as_mut(), temp_res.as_slice_repr());

            res.into_steelval()
        };

        self.register_value(
            &cloned_name.to_string(),
            SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new_owned(
                Arc::new(f),
                Some(cloned_name.into()),
                Some(2),
            ))),
        )
    }

    fn register_fn_with_ctx(
        &mut self,
        ctx: &'static str,
        name: &'static str,
        func: FN,
    ) -> &mut Self {
        self.register_fn(name, func);
        self.supply_context_arg(ctx, name);
        self
    }
}

impl<
        RET: IntoSteelVal,
        SELF: AsRefMutSteelValFromRef,
        INNER: FromSteelVal + Clone + AsRefSteelValFromUnsized<INNER>,
        FN: Fn(&mut SELF, &[INNER]) -> RET + SendSyncStatic,
    > RegisterFn<FN, MarkerWrapper7<(SELF, INNER)>, RET> for BuiltInModule
{
    fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
        // use std::Borrow();

        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 2 {
                stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, 2, args.len()));
            }

            let mut input = <SELF>::as_mut_ref_from_ref(&args[0]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;

            let temp_res = INNER::as_ref_from_unsized(&args[1]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;

            let res = func(&mut input.as_mut(), temp_res.as_slice_repr());

            res.into_steelval()
        };

        self.register_value(
            name,
            SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new(
                Arc::new(f),
                Some(name),
                Some(2),
            ))),
        )
    }

    fn register_owned_fn(&mut self, name: String, func: FN) -> &mut Self {
        let cloned_name = name.clone();

        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 2 {
                stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, 2, args.len()));
            }

            let mut input = <SELF>::as_mut_ref_from_ref(&args[0]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;

            let temp_res = INNER::as_ref_from_unsized(&args[1]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;

            let res = func(&mut input.as_mut(), temp_res.as_slice_repr());

            res.into_steelval()
        };

        self.register_value(
            &cloned_name.to_string(),
            SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new_owned(
                Arc::new(f),
                Some(cloned_name.into()),
                Some(2),
            ))),
        )
    }

    fn register_fn_with_ctx(
        &mut self,
        ctx: &'static str,
        name: &'static str,
        func: FN,
    ) -> &mut Self {
        self.register_fn(name, func);
        self.supply_context_arg(ctx, name);
        self
    }
}

impl<
        // RET: IntoSteelVal,
        'a,
        SELF: AsRefMutSteelValFromRef + 'a,
        SELFSTAT: AsRefMutSteelValFromRef + 'static + MaybeSendSyncStatic,
        RET: AsRefMutSteelValFromRef + 'a,
        STATICRET: AsRefMutSteelValFromRef + 'static + MaybeSendSyncStatic,
        // INNER: FromSteelVal + Clone + AsRefSteelValFromUnsized<INNER>,
        // F: FromSteelVal,
        FN: (Fn(&'a mut SELF) -> &'a mut RET) + SendSyncStatic,
    > RegisterFn<FN, MarkerWrapper7<(SELF, RET, STATICRET, SELFSTAT)>, STATICRET> for Engine
{
    fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
        // use std::Borrow();

        // todo!()

        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            let args = unsafe { core::mem::transmute::<&[SteelVal], &'static [SteelVal]>(args) };

            if args.len() != 1 {
                stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, 1, args.len()));
            }

            // If this value is
            let mut input = <SELF>::as_mut_ref_from_ref(&args[0]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;

            let pointer = input.as_mut().deref_mut() as *mut _;

            let res = func(unsafe { &mut (*pointer) });

            let erased = res as *mut _;

            // Take the result - but we need to tie this lifetime to the existing lifetime of the parent one.
            // So here we should have a weak reference to the existing lifetime?
            let wrapped = StandardShared::new(MutContainer::new(erased));
            let weak_ptr = StandardShared::downgrade(&wrapped);

            let temporary_borrowed_object =
                crate::gc::unsafe_erased_pointers::TemporaryObject { ptr: wrapped };

            let temp_borrow = unsafe {
                core::mem::transmute::<TemporaryObject<RET>, TemporaryObject<STATICRET>>(
                    temporary_borrowed_object,
                )
            };

            // Allocate the rooted object here
            OpaqueReferenceNursery::allocate(temp_borrow.into_opaque_reference());

            let borrow_flag = args[0]
                .get_borrow_flag_if_borrowed_object::<SELFSTAT>()
                .unwrap();

            // Mark as borrowed now
            borrow_flag.store(true, std::sync::atomic::Ordering::SeqCst);

            let mut borrowed = BorrowedObject::new(weak_ptr).with_parent_flag(borrow_flag);

            let extended = unsafe {
                core::mem::transmute::<BorrowedObject<RET>, BorrowedObject<STATICRET>>(borrowed)
            };

            let return_value =
                SteelVal::Reference(Gc::new(extended.into_opaque_reference::<'static>()));

            Ok(return_value)

            // let weak_ptr = Rc::downgrade(&wrapped);

            // todo!()

            // res.into_steelval()
        };

        // todo!()

        self.register_value(
            name,
            SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new(
                Arc::new(f),
                Some(name),
                Some(1),
            ))),
        )
    }

    fn register_fn_with_ctx(
        &mut self,
        ctx: &'static str,
        name: &'static str,
        func: FN,
    ) -> &mut Self {
        RegisterFn::<FN, MarkerWrapper7<(SELF, RET, STATICRET, SELFSTAT)>, STATICRET>::register_fn(
            self, name, func,
        );
        self.supply_context_arg(ctx, name);
        self
    }
}

// TODO: Generalize this to multiple things, use more and more and more macros
impl<
        // RET: IntoSteelVal,
        'a,
        SELF: AsRefMutSteelValFromRef + 'a,
        SELFSTAT: AsRefMutSteelValFromRef + 'static + MaybeSendSyncStatic,
        RET: AsRefSteelValFromRef + 'a,
        STATICRET: AsRefSteelValFromRef + 'static + MaybeSendSyncStatic,
        FN: (Fn(&'a mut SELF) -> &'a RET) + SendSyncStatic,
    > RegisterFn<FN, MarkerWrapper8<(SELF, RET, STATICRET, SELFSTAT)>, STATICRET> for Engine
{
    fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            let args = unsafe { core::mem::transmute::<&[SteelVal], &'static [SteelVal]>(args) };

            if args.len() != 1 {
                stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, 1, args.len()));
            }

            // If this value is
            let mut input = <SELF>::as_mut_ref_from_ref(&args[0]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;

            let pointer = input.as_mut().deref_mut() as *mut _;

            let res = func(unsafe { &mut (*pointer) });

            let erased = res as *const _;

            // Take the result - but we need to tie this lifetime to the existing lifetime of the parent one.
            // So here we should have a weak reference to the existing lifetime?
            let wrapped = StandardShared::new(MutContainer::new(erased));
            let weak_ptr = StandardShared::downgrade(&wrapped);

            let temporary_borrowed_object =
                crate::gc::unsafe_erased_pointers::ReadOnlyTemporaryObject { ptr: wrapped };

            let temp_borrow = unsafe {
                core::mem::transmute::<
                    ReadOnlyTemporaryObject<RET>,
                    ReadOnlyTemporaryObject<STATICRET>,
                >(temporary_borrowed_object)
            };

            // Allocate the rooted object here
            OpaqueReferenceNursery::allocate(temp_borrow.into_opaque_reference());

            let borrow_flag = args[0]
                .get_borrow_count_if_borrowed_object::<SELFSTAT>()
                .unwrap();

            // borrow_flag.set(borrow_flag.get() + 1);

            increment_borrow_flag(&borrow_flag);

            let borrowed = ReadOnlyBorrowedObject::new(weak_ptr, borrow_flag);

            let extended = unsafe {
                core::mem::transmute::<ReadOnlyBorrowedObject<RET>, ReadOnlyBorrowedObject<STATICRET>>(
                    borrowed,
                )
            };

            let return_value =
                SteelVal::Reference(Gc::new(extended.into_opaque_reference::<'static>()));

            Ok(return_value)
        };

        self.register_value(
            name,
            SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new(
                Arc::new(f),
                Some(name),
                Some(1),
            ))),
        )
    }

    fn register_fn_with_ctx(
        &mut self,
        ctx: &'static str,
        name: &'static str,
        func: FN,
    ) -> &mut Self {
        RegisterFn::<FN, MarkerWrapper8<(SELF, RET, STATICRET, SELFSTAT)>, STATICRET>::register_fn(
            self, name, func,
        );
        self.supply_context_arg(ctx, name);
        self
    }
}

impl<
        // RET: IntoSteelVal,
        'a,
        SELF: AsRefMutSteelValFromRef + 'a,
        SELFSTAT: AsRefMutSteelValFromRef + 'static + MaybeSendSyncStatic,
        RET: AsRefSteelValFromRef + 'a,
        STATICRET: AsRefSteelValFromRef + 'static + MaybeSendSyncStatic,
        ARG: FromSteelVal,
        FN: (Fn(&'a mut SELF, ARG) -> &'a RET) + SendSyncStatic,
    > RegisterFn<FN, MarkerWrapper8<(SELF, ARG, RET, STATICRET, SELFSTAT)>, STATICRET> for Engine
{
    fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            let args = unsafe { core::mem::transmute::<&[SteelVal], &'static [SteelVal]>(args) };

            if args.len() != 2 {
                stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, 2, args.len()));
            }

            // If this value is
            let mut input = <SELF>::as_mut_ref_from_ref(&args[0]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;
            let arg = ARG::from_steelval(&args[1]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;

            let pointer = input.as_mut().deref_mut() as *mut _;

            let res = func(unsafe { &mut (*pointer) }, arg);

            let erased = res as *const _;

            // Take the result - but we need to tie this lifetime to the existing lifetime of the parent one.
            // So here we should have a weak reference to the existing lifetime?
            let wrapped = StandardShared::new(MutContainer::new(erased));
            let weak_ptr = StandardShared::downgrade(&wrapped);

            let temporary_borrowed_object =
                crate::gc::unsafe_erased_pointers::ReadOnlyTemporaryObject { ptr: wrapped };

            let temp_borrow = unsafe {
                core::mem::transmute::<
                    ReadOnlyTemporaryObject<RET>,
                    ReadOnlyTemporaryObject<STATICRET>,
                >(temporary_borrowed_object)
            };

            // Allocate the rooted object here
            OpaqueReferenceNursery::allocate(temp_borrow.into_opaque_reference());

            let borrow_flag = args[0]
                .get_borrow_count_if_borrowed_object::<SELFSTAT>()
                .unwrap();

            increment_borrow_flag(&borrow_flag);

            let borrowed = ReadOnlyBorrowedObject::new(weak_ptr, borrow_flag);

            let extended = unsafe {
                core::mem::transmute::<ReadOnlyBorrowedObject<RET>, ReadOnlyBorrowedObject<STATICRET>>(
                    borrowed,
                )
            };

            let return_value =
                SteelVal::Reference(Gc::new(extended.into_opaque_reference::<'static>()));

            Ok(return_value)
        };

        self.register_value(
            name,
            SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new(
                Arc::new(f),
                Some(name),
                Some(2),
            ))),
        )
    }

    fn register_fn_with_ctx(
        &mut self,
        ctx: &'static str,
        name: &'static str,
        func: FN,
    ) -> &mut Self {
        RegisterFn::<FN, MarkerWrapper8<(SELF, ARG, RET, STATICRET, SELFSTAT)>, STATICRET>::register_fn(self, name, func);
        self.supply_context_arg(ctx, name);
        self
    }
}

impl<
        'a,
        SELF: AsRefSteelValFromRef + 'a,
        SELFSTAT: AsRefSteelValFromRef + 'static + MaybeSendSyncStatic,
        RET: AsRefSteelValFromRef + 'a,
        RETSTAT: AsRefSteelValFromRef + 'static + MaybeSendSyncStatic,
        ARG: FromSteelVal,
        FN: (Fn(&'a SELF, ARG) -> RET) + SendSyncStatic,
    > RegisterFnBorrowed<FN, MarkerWrapper9<(SELF, ARG, SELFSTAT, RET, RETSTAT)>, RET>
    for BuiltInModule
{
    fn register_fn_borrowed(&mut self, name: &'static str, func: FN) -> &mut Self {
        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            let args = unsafe { core::mem::transmute::<&[SteelVal], &'static [SteelVal]>(args) };

            if args.len() != 2 {
                stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, 2, args.len()));
            }

            // If this value is
            let mut input = <SELF>::as_ref_from_ref(&args[0]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;

            let pointer = input.as_ro() as *const _;

            let arg = ARG::from_steelval(&args[1]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;

            let res = func(unsafe { &(*pointer) }, arg);

            let erased = res;

            // Take the result - but we need to tie this lifetime to the existing lifetime of the parent one.
            // So here we should have a weak reference to the existing lifetime?
            let wrapped = StandardShared::new(erased);
            let weak_ptr = StandardShared::downgrade(&wrapped);

            let temporary_borrowed_object =
                crate::gc::unsafe_erased_pointers::Temporary { ptr: wrapped };

            let temp_borrow = unsafe {
                core::mem::transmute::<Temporary<RET>, Temporary<RETSTAT>>(
                    temporary_borrowed_object,
                )
            };

            // Allocate the rooted object here
            OpaqueReferenceNursery::allocate(temp_borrow.into_opaque_reference());

            let borrowed = ReadOnlyTemporary { ptr: weak_ptr };

            let extended = unsafe {
                core::mem::transmute::<ReadOnlyTemporary<RET>, ReadOnlyTemporary<RETSTAT>>(borrowed)
            };

            let return_value =
                SteelVal::Reference(Gc::new(extended.into_opaque_reference::<'static>()));

            Ok(return_value)
        };

        self.register_value(
            name,
            SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new(
                Arc::new(f),
                Some(name),
                Some(2),
            ))),
        )
    }
}

impl<
        'a,
        SELF: AsRefSteelValFromRef + 'a,
        SELFSTAT: AsRefSteelValFromRef + 'static + MaybeSendSyncStatic,
        RET: AsRefSteelValFromRef + 'a,
        RETSTAT: AsRefSteelValFromRef + 'static + MaybeSendSyncStatic,
        FN: (Fn(&'a SELF) -> RET) + SendSyncStatic,
    > RegisterFnBorrowed<FN, MarkerWrapper9<(SELF, SELFSTAT, RET, RETSTAT)>, RET>
    for BuiltInModule
{
    fn register_fn_borrowed(&mut self, name: &'static str, func: FN) -> &mut Self {
        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            let args = unsafe { core::mem::transmute::<&[SteelVal], &'static [SteelVal]>(args) };

            if args.len() != 1 {
                stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, 1, args.len()));
            }

            // If this value is
            let mut input = <SELF>::as_ref_from_ref(&args[0]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;

            let pointer = input.as_ro() as *const _;

            let res = func(unsafe { &(*pointer) });

            // let lifted = unsafe { core::mem::transmute::<RET, RETSTAT>(res) };

            // lifted.into_steelval()

            let erased = res;

            // Take the result - but we need to tie this lifetime to the existing lifetime of the parent one.
            // So here we should have a weak reference to the existing lifetime?
            let wrapped = StandardShared::new(erased);
            let weak_ptr = StandardShared::downgrade(&wrapped);

            let temporary_borrowed_object =
                crate::gc::unsafe_erased_pointers::Temporary { ptr: wrapped };

            let temp_borrow = unsafe {
                core::mem::transmute::<Temporary<RET>, Temporary<RETSTAT>>(
                    temporary_borrowed_object,
                )
            };

            // Allocate the rooted object here
            OpaqueReferenceNursery::allocate(temp_borrow.into_opaque_reference());

            let borrowed = ReadOnlyTemporary { ptr: weak_ptr };

            let extended = unsafe {
                core::mem::transmute::<ReadOnlyTemporary<RET>, ReadOnlyTemporary<RETSTAT>>(borrowed)
            };

            let return_value =
                SteelVal::Reference(Gc::new(extended.into_opaque_reference::<'static>()));

            Ok(return_value)
        };

        self.register_value(
            name,
            SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new(
                Arc::new(f),
                Some(name),
                Some(1),
            ))),
        )
    }
}

impl<
        RET: IntoSteelVal,
        SELF: AsRefMutSteelVal,
        AREA: AsRefSteelVal,
        CTX: AsRefMutSteelValFromRef,
        FN: Fn(&mut SELF, &AREA, &mut CTX) -> RET + SendSyncStatic,
    > RegisterFn<FN, MarkerWrapper7<(SELF, AREA, CTX)>, RET> for BuiltInModule
{
    fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 3 {
                stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, 3, args.len()));
            }

            let mut nursery = <AREA as AsRefSteelVal>::Nursery::default();

            let mut input = <SELF>::as_mut_ref(&args[0]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;
            let mut area = <AREA>::as_ref(&args[1]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;
            let mut ctx = <CTX>::as_mut_ref_from_ref(&args[2]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;

            let res = func(&mut input, &area, &mut ctx.as_mut());

            res.into_steelval()
        };

        self.register_value(
            name,
            SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new(
                Arc::new(f),
                Some(name),
                Some(3),
            ))),
        )
    }

    fn register_fn_with_ctx(
        &mut self,
        ctx: &'static str,
        name: &'static str,
        func: FN,
    ) -> &mut Self {
        self.register_fn(name, func);
        self.supply_context_arg(ctx, name);
        self
    }
}

impl<
        RET: IntoSteelVal,
        SELF: AsRefSteelVal,
        AREA: FromSteelVal,
        CTX: AsRefSteelValFromRef,
        FN: Fn(&SELF, AREA, &CTX) -> RET + SendSyncStatic,
    > RegisterFn<FN, MarkerWrapper8<(SELF, AREA, CTX)>, RET> for BuiltInModule
{
    fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 3 {
                stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, 3, args.len()));
            }

            let mut nursery = <SELF as AsRefSteelVal>::Nursery::default();

            let mut input = <SELF>::as_ref(&args[0]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;
            let mut area = <AREA>::from_steelval(&args[1]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;
            let mut ctx = <CTX>::as_ref_from_ref(&args[2]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;

            let res = func(&input, area, ctx.as_ro());

            res.into_steelval()
        };

        self.register_value(
            name,
            SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new(
                Arc::new(f),
                Some(name),
                Some(3),
            ))),
        )
    }

    fn register_fn_with_ctx(
        &mut self,
        ctx: &'static str,
        name: &'static str,
        func: FN,
    ) -> &mut Self {
        self.register_fn(name, func);
        self.supply_context_arg(ctx, name);
        self
    }
}

// TODO: @Matt - come back and fix this because its atrocious
impl<
        RET: IntoSteelVal,
        SELF: AsRefMutSteelValFromRef,
        INNER: FromSteelVal + Clone + AsRefSteelValFromUnsized<INNER>,
        F: FromSteelVal,
        FN: Fn(&mut SELF, &[INNER], F) -> RET + SendSyncStatic,
    > RegisterFn<FN, MarkerWrapper6<(SELF, F, INNER)>, RET> for BuiltInModule
{
    fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
        // use std::Borrow();

        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 2 {
                stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, 2, args.len()));
            }

            let mut input = <SELF>::as_mut_ref_from_ref(&args[0]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;

            let temp_res = INNER::as_ref_from_unsized(&args[1]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;

            let res = func(
                &mut input.as_mut(),
                temp_res.as_slice_repr(),
                F::from_steelval(&args[2])?,
            );

            res.into_steelval()
        };

        self.register_value(
            name,
            SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new(
                Arc::new(f),
                Some(name),
                Some(2),
            ))),
        )
    }

    fn register_owned_fn(&mut self, name: String, func: FN) -> &mut Self {
        // use std::Borrow();

        let cloned_name = name.clone();

        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 2 {
                stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, 2, args.len()));
            }

            let mut input = <SELF>::as_mut_ref_from_ref(&args[0]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;

            let temp_res = INNER::as_ref_from_unsized(&args[1]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;

            let res = func(
                &mut input.as_mut(),
                temp_res.as_slice_repr(),
                F::from_steelval(&args[2]).map_err(|mut e| {
                    e.prepend_message(&format!("{}:", name));
                    e
                })?,
            );

            res.into_steelval()
        };

        self.register_value(
            &cloned_name.to_string(),
            SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new_owned(
                Arc::new(f),
                Some(cloned_name.into()),
                Some(2),
            ))),
        )
    }

    fn register_fn_with_ctx(
        &mut self,
        ctx: &'static str,
        name: &'static str,
        func: FN,
    ) -> &mut Self {
        self.register_fn(name, func);
        self.supply_context_arg(ctx, name);
        self
    }
}

impl<
        // RET: IntoSteelVal,
        'a,
        SELF: AsRefMutSteelValFromRef + 'a,
        SELFSTAT: AsRefMutSteelValFromRef + 'static + MaybeSendSyncStatic,
        RET: AsRefMutSteelValFromRef + 'a,
        STATICRET: AsRefMutSteelValFromRef + 'static + MaybeSendSyncStatic,
        // INNER: FromSteelVal + Clone + AsRefSteelValFromUnsized<INNER>,
        // F: FromSteelVal,
        FN: (Fn(&'a mut SELF) -> &'a mut RET) + SendSyncStatic,
    > RegisterFn<FN, MarkerWrapper7<(SELF, RET, STATICRET, SELFSTAT)>, STATICRET>
    for BuiltInModule
{
    fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
        // use std::Borrow();

        // todo!()

        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            let args = unsafe { core::mem::transmute::<&[SteelVal], &'static [SteelVal]>(args) };

            if args.len() != 1 {
                stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, 1, args.len()));
            }

            // If this value is
            let mut input = <SELF>::as_mut_ref_from_ref(&args[0]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;

            let pointer = input.as_mut().deref_mut() as *mut _;

            let res = func(unsafe { &mut (*pointer) });

            let erased = res as *mut _;

            // Take the result - but we need to tie this lifetime to the existing lifetime of the parent one.
            // So here we should have a weak reference to the existing lifetime?
            let wrapped = StandardShared::new(MutContainer::new(erased));
            let weak_ptr = StandardShared::downgrade(&wrapped);

            let temporary_borrowed_object =
                crate::gc::unsafe_erased_pointers::TemporaryObject { ptr: wrapped };

            let temp_borrow = unsafe {
                core::mem::transmute::<TemporaryObject<RET>, TemporaryObject<STATICRET>>(
                    temporary_borrowed_object,
                )
            };

            // Allocate the rooted object here
            OpaqueReferenceNursery::allocate(temp_borrow.into_opaque_reference());

            let borrow_flag = args[0]
                .get_borrow_flag_if_borrowed_object::<SELFSTAT>()
                .unwrap();

            borrow_flag.store(true, std::sync::atomic::Ordering::SeqCst);

            let borrowed = BorrowedObject::new(weak_ptr).with_parent_flag(borrow_flag);

            let extended = unsafe {
                core::mem::transmute::<BorrowedObject<RET>, BorrowedObject<STATICRET>>(borrowed)
            };

            let return_value =
                SteelVal::Reference(Gc::new(extended.into_opaque_reference::<'static>()));

            Ok(return_value)

            // let weak_ptr = Rc::downgrade(&wrapped);

            // todo!()

            // res.into_steelval()
        };

        // todo!()

        self.register_value(
            name,
            SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new(
                Arc::new(f),
                Some(name),
                Some(1),
            ))),
        )
    }

    fn register_fn_with_ctx(
        &mut self,
        ctx: &'static str,
        name: &'static str,
        func: FN,
    ) -> &mut Self {
        RegisterFn::<FN, MarkerWrapper7<(SELF, RET, STATICRET, SELFSTAT)>, STATICRET>::register_fn(
            self, name, func,
        );
        self.supply_context_arg(ctx, name);
        self
    }
}

// TODO: Generalize this to multiple things, use more and more and more macros
impl<
        // RET: IntoSteelVal,
        'a,
        SELF: AsRefMutSteelValFromRef + 'a,
        SELFSTAT: AsRefMutSteelValFromRef + 'static + MaybeSendSyncStatic,
        RET: AsRefSteelValFromRef + 'a,
        STATICRET: AsRefSteelValFromRef + 'static + MaybeSendSyncStatic,
        FN: (Fn(&'a mut SELF) -> &'a RET) + SendSyncStatic,
    > RegisterFn<FN, MarkerWrapper8<(SELF, RET, STATICRET, SELFSTAT)>, STATICRET>
    for BuiltInModule
{
    fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            let args = unsafe { core::mem::transmute::<&[SteelVal], &'static [SteelVal]>(args) };

            if args.len() != 1 {
                stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, 1, args.len()));
            }

            // If this value is
            let mut input = <SELF>::as_mut_ref_from_ref(&args[0]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;

            let pointer = input.as_mut().deref_mut() as *mut _;

            let res = func(unsafe { &mut (*pointer) });

            let erased = res as *const _;

            // Take the result - but we need to tie this lifetime to the existing lifetime of the parent one.
            // So here we should have a weak reference to the existing lifetime?
            let wrapped = StandardShared::new(MutContainer::new(erased));
            let weak_ptr = StandardShared::downgrade(&wrapped);

            let temporary_borrowed_object =
                crate::gc::unsafe_erased_pointers::ReadOnlyTemporaryObject { ptr: wrapped };

            let temp_borrow = unsafe {
                core::mem::transmute::<
                    ReadOnlyTemporaryObject<RET>,
                    ReadOnlyTemporaryObject<STATICRET>,
                >(temporary_borrowed_object)
            };

            // Allocate the rooted object here
            OpaqueReferenceNursery::allocate(temp_borrow.into_opaque_reference());

            let borrow_flag = args[0]
                .get_borrow_count_if_borrowed_object::<SELFSTAT>()
                .unwrap();

            increment_borrow_flag(&borrow_flag);

            // TODO: Mark the parent as borrowed -> Can't be access again until
            // the child value is out of scope. We shouldn't have a problem with RO values
            // from this point, since everything can be read
            let borrowed = ReadOnlyBorrowedObject::new(weak_ptr, borrow_flag);

            let extended = unsafe {
                core::mem::transmute::<ReadOnlyBorrowedObject<RET>, ReadOnlyBorrowedObject<STATICRET>>(
                    borrowed,
                )
            };

            let return_value =
                SteelVal::Reference(Gc::new(extended.into_opaque_reference::<'static>()));

            Ok(return_value)
        };

        self.register_value(
            name,
            SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new(
                Arc::new(f),
                Some(name),
                Some(1),
            ))),
        )
    }

    fn register_fn_with_ctx(
        &mut self,
        ctx: &'static str,
        name: &'static str,
        func: FN,
    ) -> &mut Self {
        RegisterFn::<FN, MarkerWrapper8<(SELF, RET, STATICRET, SELFSTAT)>, STATICRET>::register_fn(
            self, name, func,
        );
        self.supply_context_arg(ctx, name);
        self
    }
}

impl<
        // RET: IntoSteelVal,
        'a,
        SELF: AsRefMutSteelValFromRef + 'a,
        SELFSTAT: AsRefMutSteelValFromRef + 'static + MaybeSendSyncStatic,
        RET: AsRefSteelValFromRef + 'a,
        STATICRET: AsRefSteelValFromRef + 'static + MaybeSendSyncStatic,
        ARG: FromSteelVal,
        FN: (Fn(&'a mut SELF, ARG) -> &'a RET) + SendSyncStatic,
    > RegisterFn<FN, MarkerWrapper8<(SELF, ARG, RET, STATICRET, SELFSTAT)>, STATICRET>
    for BuiltInModule
{
    fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            let args = unsafe { core::mem::transmute::<&[SteelVal], &'static [SteelVal]>(args) };

            if args.len() != 2 {
                stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, 2, args.len()));
            }

            // If this value is
            let mut input = <SELF>::as_mut_ref_from_ref(&args[0]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;
            let arg = ARG::from_steelval(&args[1]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;

            let pointer = input.as_mut().deref_mut() as *mut _;

            let res = func(unsafe { &mut (*pointer) }, arg);

            let erased = res as *const _;

            // Take the result - but we need to tie this lifetime to the existing lifetime of the parent one.
            // So here we should have a weak reference to the existing lifetime?
            let wrapped = StandardShared::new(MutContainer::new(erased));
            let weak_ptr = StandardShared::downgrade(&wrapped);

            let temporary_borrowed_object =
                crate::gc::unsafe_erased_pointers::ReadOnlyTemporaryObject { ptr: wrapped };

            let temp_borrow = unsafe {
                core::mem::transmute::<
                    ReadOnlyTemporaryObject<RET>,
                    ReadOnlyTemporaryObject<STATICRET>,
                >(temporary_borrowed_object)
            };

            // Allocate the rooted object here
            OpaqueReferenceNursery::allocate(temp_borrow.into_opaque_reference());

            let borrow_flag = args[0]
                .get_borrow_count_if_borrowed_object::<SELFSTAT>()
                .unwrap();

            increment_borrow_flag(&borrow_flag);

            let borrowed = ReadOnlyBorrowedObject::new(weak_ptr, borrow_flag);

            let extended = unsafe {
                core::mem::transmute::<ReadOnlyBorrowedObject<RET>, ReadOnlyBorrowedObject<STATICRET>>(
                    borrowed,
                )
            };

            let return_value =
                SteelVal::Reference(Gc::new(extended.into_opaque_reference::<'static>()));

            Ok(return_value)
        };

        self.register_value(
            name,
            SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new(
                Arc::new(f),
                Some(name),
                Some(2),
            ))),
        )
    }

    fn register_fn_with_ctx(
        &mut self,
        ctx: &'static str,
        name: &'static str,
        func: FN,
    ) -> &mut Self {
        RegisterFn::<FN, MarkerWrapper8<(SELF, ARG, RET, STATICRET, SELFSTAT)>, STATICRET>::register_fn(self, name, func);
        self.supply_context_arg(ctx, name);
        self
    }
}

// impl<
//         RET: IntoSteelVal,
//         SELF: AsRefMutSteelValFromRef,
//         INNER: FromSteelVal,
//         F: FromSteelVal,
//         FN: Fn(&mut SELF, INNER, F) -> RET + 'static,
//     > RegisterFn<FN, MarkerWrapper7<(SELF, F, INNER)>, RET> for BuiltInModule
// {
//     fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
//         // use std::Borrow();

//         let f = move |args: &[SteelVal]| -> Result<SteelVal> {
//             if args.len() != 3 {
//                 stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, 0, args.len()));
//             }

//             let mut input = <SELF>::as_mut_ref_from_ref(&args[0])?;

//             let temp_res = INNER::from_steelval(&args[1])?;

//             let res = func(&mut input, temp_res, F::from_steelval(&args[2])?);

//             res.into_steelval()
//         };

//         self.register_value(
//             name,
//             SteelVal::BoxedFunction(Rc::new(BoxedDynFunction::new(
//                 Box::new(f),
//                 Some(name),
//                 Some(1),
//             ))),
//         )
//     }

//     // TODO: Make this use not a static string since we _could_ have dynamic names
//     fn register_owned_fn(&mut self, name: String, func: FN) -> &mut Self {
//         // use std::Borrow();

//         let cloned_name = name.clone();

//         let f = move |args: &[SteelVal]| -> Result<SteelVal> {
//             if args.len() != 3 {
//                 stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, 0, args.len()));
//             }

//             let mut input = <SELF>::as_mut_ref_from_ref(&args[0])?;

//             let temp_res = INNER::from_steelval(&args[1])?;

//             let res = func(&mut input, temp_res, F::from_steelval(&args[2])?);

//             res.into_steelval()
//         };

//         self.register_value(
//             &cloned_name.clone(),
//             SteelVal::BoxedFunction(Rc::new(BoxedDynFunction::new_owned(
//                 Box::new(f),
//                 Some(Rc::new(cloned_name)),
//                 Some(1),
//             ))),
//         )
//     }
// }

impl<
        FUT: Future<Output = RET> + SendSyncStatic,
        RET: IntoSteelVal + 'static + MaybeSendSyncStatic,
        FN: Fn() -> FUT + SendSyncStatic,
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

        self.register_value(name, SteelVal::FutureFunc(Shared::new(Box::new(f))))
    }

    fn register_fn_with_ctx(
        &mut self,
        ctx: &'static str,
        name: &'static str,
        func: FN,
    ) -> &mut Self {
        self.register_fn(name, func);
        self.supply_context_arg(ctx, name);
        self
    }
}

impl<RET: IntoSteelVal, FN: Fn() -> RET + SendSyncStatic> RegisterFn<FN, Wrapper<()>, RET>
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

        self.register_value(
            name,
            SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new(
                Arc::new(f),
                Some(name),
                Some(0),
            ))),
        )
    }

    fn register_fn_with_ctx(
        &mut self,
        ctx: &'static str,
        name: &'static str,
        func: FN,
    ) -> &mut Self {
        self.register_fn(name, func);
        self.supply_context_arg(ctx, name);
        self
    }
}

impl<RET: IntoSteelVal, SELF: AsRefSteelVal, FN: Fn(&SELF) -> RET + SendSyncStatic>
    RegisterFn<FN, MarkerWrapper1<SELF>, RET> for BuiltInModule
{
    fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 1 {
                stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, 1, args.len()));
            }

            let mut nursery = <SELF::Nursery>::default();

            let input = <SELF>::as_ref(&args[0]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;

            let res = func(&input);

            res.into_steelval()
        };

        self.register_value(
            name,
            SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new(
                Arc::new(f),
                Some(name),
                Some(1),
            ))),
        )
    }

    fn register_owned_fn(&mut self, name: String, func: FN) -> &mut Self {
        let name = Rc::new(name);
        let cloned_name = Rc::clone(&name);

        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 1 {
                stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, 1, args.len()));
            }

            let mut nursery = <SELF::Nursery>::default();

            let input = <SELF>::as_ref(&args[0]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;

            let res = func(&input);

            res.into_steelval()
        };

        todo!("Resolve using RC's here")

        // self.register_value(
        //     &Rc::clone(&cloned_name),
        //     SteelVal::BoxedFunction(Rc::new(BoxedDynFunction::new_owned(
        //         Arc::new(f),
        //         Some(cloned_name),
        //         Some(1),
        //     ))),
        // )
    }

    fn register_fn_with_ctx(
        &mut self,
        ctx: &'static str,
        name: &'static str,
        func: FN,
    ) -> &mut Self {
        self.register_fn(name, func);
        self.supply_context_arg(ctx, name);
        self
    }
}

impl<RET: IntoSteelVal, SELF: AsRefMutSteelVal, FN: Fn(&mut SELF) -> RET + SendSyncStatic>
    RegisterFn<FN, MarkerWrapper2<SELF>, RET> for BuiltInModule
{
    fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
        // use std::Borrow();

        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 1 {
                stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, 1, args.len()));
            }

            let mut input = <SELF>::as_mut_ref(&args[0]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;

            let res = func(&mut input);

            res.into_steelval()
        };

        self.register_value(
            name,
            SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new(
                Arc::new(f),
                Some(name),
                Some(1),
            ))),
        )
    }

    fn register_fn_with_ctx(
        &mut self,
        ctx: &'static str,
        name: &'static str,
        func: FN,
    ) -> &mut Self {
        self.register_fn(name, func);
        self.supply_context_arg(ctx, name);
        self
    }
}

impl<
        RET: IntoSteelVal,
        SELF: AsRefMutSteelValFromRef,
        FN: Fn(&mut SELF) -> RET + SendSyncStatic,
    > RegisterFn<FN, MarkerWrapper5<SELF>, RET> for BuiltInModule
{
    fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
        // use std::Borrow();

        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 1 {
                stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, 1, args.len()));
            }

            let mut input = <SELF>::as_mut_ref_from_ref(&args[0]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;

            let res = func(&mut input.as_mut());

            res.into_steelval()
        };

        self.register_value(
            name,
            SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new(
                Arc::new(f),
                Some(name),
                Some(1),
            ))),
        )
    }

    fn register_fn_with_ctx(
        &mut self,
        ctx: &'static str,
        name: &'static str,
        func: FN,
    ) -> &mut Self {
        self.register_fn(name, func);
        self.supply_context_arg(ctx, name);
        self
    }
}

impl<RET: IntoSteelVal, SELF: AsRefSteelValFromRef, FN: Fn(&SELF) -> RET + SendSyncStatic>
    RegisterFn<FN, MarkerWrapper8<SELF>, RET> for BuiltInModule
{
    fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 1 {
                stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, 1, args.len()));
            }

            let mut input = <SELF>::as_ref_from_ref(&args[0]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;

            let res = func(input.as_ro());

            res.into_steelval()
        };

        self.register_value(
            name,
            SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new(
                Arc::new(f),
                Some(name),
                Some(1),
            ))),
        )
    }

    fn register_fn_with_ctx(
        &mut self,
        ctx: &'static str,
        name: &'static str,
        func: FN,
    ) -> &mut Self {
        self.register_fn(name, func);
        self.supply_context_arg(ctx, name);
        self
    }
}

impl<
        RET: IntoSteelVal,
        ARG: FromSteelVal,
        SELF: AsRefSteelValFromRef,
        FN: Fn(&SELF, ARG) -> RET + SendSyncStatic,
    > RegisterFn<FN, MarkerWrapper8<(SELF, ARG)>, RET> for BuiltInModule
{
    fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 2 {
                stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, 2, args.len()));
            }

            let mut input = <SELF>::as_ref_from_ref(&args[0])?;
            let arg = <ARG>::from_steelval(&args[1]).map_err(|mut err| {
                err.prepend_message(":");
                err.prepend_message(name);
                err
            })?;

            let res = func(input.as_ro(), arg);

            res.into_steelval()
        };

        self.register_value(
            name,
            SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new(
                Arc::new(f),
                Some(name),
                Some(2),
            ))),
        )
    }

    fn register_fn_with_ctx(
        &mut self,
        ctx: &'static str,
        name: &'static str,
        func: FN,
    ) -> &mut Self {
        self.register_fn(name, func);
        self.supply_context_arg(ctx, name);
        self
    }
}

impl<RET: IntoSteelVal, SELF: AsRefSteelValFromRef, FN: Fn(&SELF) -> RET + SendSyncStatic>
    RegisterFn<FN, MarkerWrapper8<SELF>, RET> for Engine
{
    fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
        // use std::Borrow();

        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 1 {
                stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, 1, args.len()));
            }

            let mut input = <SELF>::as_ref_from_ref(&args[0]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;

            let res = func(input.as_ro());

            res.into_steelval()
        };

        self.register_value(
            name,
            SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new(
                Arc::new(f),
                Some(name),
                Some(1),
            ))),
        )
    }

    fn register_fn_with_ctx(
        &mut self,
        ctx: &'static str,
        name: &'static str,
        func: FN,
    ) -> &mut Self {
        self.register_fn(name, func);
        self
    }
}

// pub struct Marker {}

pub struct MarkerWrapper1<ARGS>(PhantomData<ARGS>);
pub struct MarkerWrapper2<ARGS>(PhantomData<ARGS>);
pub struct MarkerWrapper3<ARGS>(PhantomData<ARGS>);

pub struct MarkerWrapper4<ARGS>(PhantomData<ARGS>);

pub struct MarkerWrapper5<ARGS>(PhantomData<ARGS>);

pub struct MarkerWrapper6<ARGS>(PhantomData<ARGS>);
pub struct MarkerWrapper7<ARGS>(PhantomData<ARGS>);
pub struct MarkerWrapper8<ARGS>(PhantomData<ARGS>);

pub struct MarkerWrapper9<ARGS>(PhantomData<ARGS>);

impl<
        A: AsRefSteelVal,
        B: AsRefSteelVal,
        FN: Fn(&A, &B) -> RET + SendSyncStatic,
        RET: IntoSteelVal,
    > RegisterFn<FN, MarkerWrapper1<(A, B)>, RET> for Engine
{
    fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 2 {
                stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, 2, args.len()));
            }

            let mut nursery = <A::Nursery>::default();

            let one = A::as_ref(&args[0]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;

            let mut nursery = <B::Nursery>::default();

            let two = B::as_ref(&args[1]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;

            let res = func(&one, &two);

            res.into_steelval()
        };

        self.register_value(
            name,
            SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new(
                Arc::new(f),
                Some(name),
                Some(2),
            ))),
        )
    }

    fn register_fn_with_ctx(
        &mut self,
        ctx: &'static str,
        name: &'static str,
        func: FN,
    ) -> &mut Self {
        self.register_fn(name, func);
        self.supply_context_arg(ctx, name);
        self
    }
}

impl<
        A: FromSteelVal,
        B: AsRefSteelVal,
        FN: Fn(A, &B) -> RET + SendSyncStatic,
        RET: IntoSteelVal,
    > RegisterFn<FN, MarkerWrapper2<(A, B)>, RET> for Engine
{
    fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 2 {
                stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, 2, args.len()));
            }

            let mut nursery = <B::Nursery>::default();

            let input = B::as_ref(&args[1])?;

            let res = func(
                A::from_steelval(&args[0]).map_err(|mut err| {
                    err.prepend_message(":");
                    err.prepend_message(name);
                    err
                })?,
                &input,
            );

            res.into_steelval()
        };

        self.register_value(
            name,
            SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new(
                Arc::new(f),
                Some(name),
                Some(2),
            ))),
        )
    }

    fn register_fn_with_ctx(
        &mut self,
        ctx: &'static str,
        name: &'static str,
        func: FN,
    ) -> &mut Self {
        self.register_fn(name, func);
        self.supply_context_arg(ctx, name);
        self
    }
}

macro_rules! impl_register_fn {
    ($arg_count:expr => $($param:ident: $idx:expr),*) => {
        impl<
            $($param: FromSteelVal,)*
            FN: Fn($($param),*) -> RET + SendSyncStatic,
            RET: IntoSteelVal
        > RegisterFn<FN, Wrapper<($($param,)*)>, RET> for Engine {
            fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
                let f = move |args: &[SteelVal]| -> Result<SteelVal> {
                    if args.len() != $arg_count {
                        stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, $arg_count, args.len()));
                    }

                    let res = func($(<$param>::from_steelval(&args[$idx]).map_err(|mut err| {
                        err.prepend_message(":");
                        err.prepend_message(name);
                        err
                    })?,)*);

                    res.into_steelval()
                };

                self.register_value(
                    name,
                    SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new(
                        Arc::new(f),
                        Some(name),
                        Some($arg_count),
                    ))),
                )
            }

            fn register_fn_with_ctx(
                &mut self,
                ctx: &'static str,
                name: &'static str,
                func: FN,
            ) -> &mut Self {
                self.register_fn(name, func);
                self.supply_context_arg(ctx, name);
                self
            }

        }

        impl<
            $($param: FromSteelVal,)*
            FN: Fn($($param),*) -> RET + SendSyncStatic,
            RET: IntoSteelVal
        > RegisterFn<FN, Wrapper<($($param,)*)>, RET> for BuiltInModule {
            fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
                let f = move |args: &[SteelVal]| -> Result<SteelVal> {
                    if args.len() != $arg_count {
                        stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, $arg_count, args.len()));
                    }

                    let res = func($(<$param>::from_steelval(&args[$idx]).map_err(|mut err| {
                        err.prepend_message(":");
                        err.prepend_message(name);
                        err
                    })?,)*);

                    res.into_steelval()
                };

                self.register_value(
                    name,
                    SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new(
                        Arc::new(f),
                        Some(name),
                        Some($arg_count),
                    ))),
                )
            }


            fn register_fn_with_ctx(
                &mut self,
                ctx: &'static str,
                name: &'static str,
                func: FN,
            ) -> &mut Self {
                self.register_fn(name, func);
                self.supply_context_arg(ctx, name);
                self
            }
        }


        // Async functions

        impl<
            FUT: Future<Output = RET> + SendSyncStatic,
            $($param: FromSteelVal,)*
            FN: Fn($($param),*) -> FUT + SendSyncStatic,
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

                self.register_value(name, SteelVal::FutureFunc(Shared::new(Box::new(f))))
            }

            fn register_fn_with_ctx(
                &mut self,
                ctx: &'static str,
                name: &'static str,
                func: FN,
            ) -> &mut Self {
                self.register_fn(name, func);
                self.supply_context_arg(ctx, name);
                self
            }

        }

        impl<
            FUT: Future<Output = RET> + SendSyncStatic,
            $($param: FromSteelVal,)*
            FN: Fn($($param),*) -> FUT + SendSyncStatic,
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

                self.register_value(name, SteelVal::FutureFunc(Shared::new(Box::new(f))))
            }

            fn register_fn_with_ctx(
                &mut self,
                ctx: &'static str,
                name: &'static str,
                func: FN,
            ) -> &mut Self {
                self.register_fn(name, func);
                self.supply_context_arg(ctx, name);
                self
            }
        }
    };
}

macro_rules! impl_register_fn_self {
    ($arg_count:expr => $($param:ident: $idx:expr),*) => {
        impl<
            T: RegisterValue,
            SELF: AsRefSteelVal,
            $($param: FromSteelVal,)*
            FN: Fn(&SELF, $($param),*) -> RET + SendSyncStatic,
            RET: IntoSteelVal
        > RegisterFn<FN, MarkerWrapper3<(SELF, $($param,)*)>, RET> for T {

            fn register_fn_with_ctx(
                &mut self,
                ctx: &'static str,
                name: &'static str,
                func: FN,
            ) -> &mut Self {
                self.register_fn(name, func);
                self.supply_context_arg(ctx, name);
                self
            }


            fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
                let f = move |args: &[SteelVal]| -> Result<SteelVal> {
                    if args.len() != $arg_count {
                        stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, $arg_count, args.len()));
                    }

                    let mut nursery = <SELF::Nursery>::default();

                    let input = <SELF>::as_ref(&args[0]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;

                    let res = func(&input, $(<$param>::from_steelval(&args[$idx]).map_err(|mut err| {
                                    err.prepend_message(":");
                                    err.prepend_message(name);
                                    err
                              })?,)*);

                    res.into_steelval()
                };

                self.register_value_inner(
                    name,
                    SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new(
                        Arc::new(f),
                        Some(name),
                        Some($arg_count),
                    ))),
                )
            }
        }

        impl<
            T: RegisterValue,
            SELF: AsRefMutSteelVal,
            $($param: FromSteelVal,)*
            FN: Fn(&mut SELF, $($param),*) -> RET + SendSyncStatic,
            RET: IntoSteelVal
        > RegisterFn<FN, MarkerWrapper4<(SELF, $($param,)*)>, RET> for T {

            fn register_fn_with_ctx(
                &mut self,
                ctx: &'static str,
                name: &'static str,
                func: FN,
            ) -> &mut Self {
                self.register_fn(name, func);
                self.supply_context_arg(ctx, name);
                self
            }

            fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
                let f = move |args: &[SteelVal]| -> Result<SteelVal> {
                    if args.len() != $arg_count {
                        stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, $arg_count, args.len()));
                    }

                    let mut input = <SELF>::as_mut_ref(&args[0]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;

                    let res = func(&mut input, $(<$param>::from_steelval(&args[$idx]).map_err(|mut err| {
                            err.prepend_message(":");
                            err.prepend_message(name);
                            err
                        })?,)*);

                    res.into_steelval()
                };

                self.register_value_inner(
                    name,
                    SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new(
                        Arc::new(f),
                        Some(name),
                        Some($arg_count),
                    ))),
                )
            }
        }

        impl<
            T: RegisterValue,
            SELF: AsRefMutSteelValFromRef,
            $($param: FromSteelVal,)*
            FN: Fn(&mut SELF, $($param),*) -> RET + SendSyncStatic,
            RET: IntoSteelVal
        > RegisterFn<FN, MarkerWrapper5<(SELF, $($param,)*)>, RET> for T {

            fn register_fn_with_ctx(
                &mut self,
                ctx: &'static str,
                name: &'static str,
                func: FN,
            ) -> &mut Self {
                self.register_fn(name, func);
                self.supply_context_arg(ctx, name);
                self
            }


            fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
                let f = move |args: &[SteelVal]| -> Result<SteelVal> {
                    if args.len() != $arg_count {
                        stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, $arg_count, args.len()));
                    }

                    let mut input = <SELF>::as_mut_ref_from_ref(&args[0]).map_err(|mut e| {
                e.prepend_message(&format!("{}:", name));
                e
            })?;

                    let res = func(&mut input.as_mut(), $(<$param>::from_steelval(&args[$idx]).map_err(|mut err| {
                            err.prepend_message(":");
                            err.prepend_message(name);
                            err
                        })?,)*);

                    res.into_steelval()
                };

                self.register_value_inner(
                    name,
                    SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new(
                        Arc::new(f),
                        Some(name),
                        Some($arg_count),
                    ))),
                )
            }


        fn register_owned_fn(&mut self, name: String, func: FN) -> &mut Self {

            let cloned_name = name.clone();

             let f = move |args: &[SteelVal]| -> Result<SteelVal> {
                 if args.len() != $arg_count {
                     stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, $arg_count, args.len()));
                 }

                 let mut input = <SELF>::as_mut_ref_from_ref(&args[0]).map_err(|mut e| {
                    e.prepend_message(&format!("{}:", name));
                    e
                 })?;

                 let res = func(&mut input.as_mut(), $(<$param>::from_steelval(&args[$idx]).map_err(|mut e| {
                    e.prepend_message(&format!("{}:", name));
                    e
                 })?,)*);

                 res.into_steelval()
             };

             self.register_value_inner(
                 &cloned_name.to_string(),
                 SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new_owned(
                     Arc::new(f),
                     Some(Arc::new(cloned_name)),
                     Some($arg_count),
                 ))),
             )
         }

        }

        // impl<
        //     T: RegisterValue,
        //     SELF: AsRefSteelVal,
        //     $($param: FromSteelVal,)*
        //     FN: Fn(&SELF, $($param),*) -> RET + SendSyncStatic,
        //     RET: IntoSteelVal
        // > RegisterFn<FN, MarkerWrapper1<(SELF, $($param,)*)>, RET> for BuiltInModule {
        //     fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
        //         let f = move |args: &[SteelVal]| -> Result<SteelVal> {
        //             if args.len() != $arg_count {
        //                 stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, $arg_count, args.len()));
        //             }

        //             let mut nursery = <SELF::Nursery>::default();

        //             let input = <SELF>::as_ref(&args[0], &mut nursery)?;

        //             let res = func(&input, $(<$param>::from_steelval(&args[$idx])?,)*);

        //             res.into_steelval()
        //         };

        //         self.register_value(
        //             name,
        //             SteelVal::BoxedFunction(Rc::new(BoxedDynFunction::new(
        //                 Arc::new(f),
        //                 Some(name),
        //                 Some($arg_count),
        //             ))),
        //         )
        //     }
        // }

        // impl<
        //     SELF: AsRefMutSteelVal,
        //     $($param: FromSteelVal,)*
        //     FN: Fn(&mut SELF, $($param),*) -> RET + SendSyncStatic,
        //     RET: IntoSteelVal
        // > RegisterFn<FN, MarkerWrapper3<(SELF, $($param,)*)>, RET> for BuiltInModule {
        //     fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
        //         let f = move |args: &[SteelVal]| -> Result<SteelVal> {
        //             if args.len() != $arg_count {
        //                 stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, $arg_count, args.len()));
        //             }

        //             let mut input = <SELF>::as_mut_ref(&args[0])?;

        //             let res = func(&mut input, $(<$param>::from_steelval(&args[$idx])?,)*);

        //             res.into_steelval()
        //         };

        //         self.register_value(
        //             name,
        //             SteelVal::BoxedFunction(Rc::new(BoxedDynFunction::new(
        //                 Arc::new(f),
        //                 Some(name),
        //                 Some($arg_count),
        //             ))),
        //         )
        //     }
        // }

        // impl<
        //     SELF: AsRefMutSteelValFromRef,
        //     $($param: FromSteelVal,)*
        //     FN: Fn(&mut SELF, $($param),*) -> RET + SendSyncStatic,
        //     RET: IntoSteelVal
        // > RegisterFn<FN, MarkerWrapper5<(SELF, $($param,)*)>, RET> for BuiltInModule {
        //     fn register_fn(&mut self, name: &'static str, func: FN) -> &mut Self {
        //         let f = move |args: &[SteelVal]| -> Result<SteelVal> {
        //             if args.len() != $arg_count {
        //                 stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, $arg_count, args.len()));
        //             }

        //             let mut input = <SELF>::as_mut_ref_from_ref(&args[0])?;

        //             let res = func(&mut input, $(<$param>::from_steelval(&args[$idx])?,)*);

        //             res.into_steelval()
        //         };

        //         self.register_value(
        //             name,
        //             SteelVal::BoxedFunction(Rc::new(BoxedDynFunction::new(
        //                 Arc::new(f),
        //                 Some(name),
        //                 Some($arg_count),
        //             ))),
        //         )
        //     }

        //     fn register_owned_fn(&mut self, name: String, func: FN) -> &mut Self {

        //         let cloned_name = name.clone();

        //         let f = move |args: &[SteelVal]| -> Result<SteelVal> {
        //             if args.len() != $arg_count {
        //                 stop!(ArityMismatch => format!("{} expected {} argument, got {}", name, $arg_count, args.len()));
        //             }

        //             let mut input = <SELF>::as_mut_ref_from_ref(&args[0])?;

        //             let res = func(&mut input, $(<$param>::from_steelval(&args[$idx])?,)*);

        //             res.into_steelval()
        //         };

        //         self.register_value(
        //             &cloned_name.to_string(),
        //             SteelVal::BoxedFunction(Rc::new(BoxedDynFunction::new_owned(
        //                 Arc::new(f),
        //                 Some(Arc::new(cloned_name)),
        //                 Some($arg_count),
        //             ))),
        //         )
        //     }
        // }
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

// Generate the combinations for up to arity 5?
mod generated_impls {

    use super::*;

    pub struct FakeEngine {}

    #[derive(Clone)]
    struct FooBarBaz {}

    impl crate::rvals::Custom for FooBarBaz {}

    // Check the status of the code gen. Eventually we're going to pivot this to just be a blanket implementation
    // against anything that implements `RegisterValue`
    // #[test]
    // fn check_engine_light() {
    //     let mut engine = FakeEngine {};

    //     engine.register_fn(
    //         "applesauce",
    //         |foo: FooBarBaz,
    //          bar: &FooBarBaz,
    //          baz: &mut FooBarBaz,
    //          quux: FooBarBaz,
    //          qux: FooBarBaz| todo!(),
    //     );
    // }

    struct MarkerWrapper<ARGS>(PhantomData<ARGS>);

    impl FakeEngine {
        pub fn register_value(&mut self, name: &str, value: SteelVal) -> &mut Self {
            todo!()
        }
    }

    // TODO: Come up with better MarkerWrapper<ARGS>(PhantomData<ARGS>); -> This is gonna be nasty.
    // include!(concat!(env!("OUT_DIR"), "/generated.rs"));
}
