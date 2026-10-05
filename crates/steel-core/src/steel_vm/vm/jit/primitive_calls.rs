//! Calls from jitted code to a primitive it knows at compile time, passed in as
//! a function pointer.

use super::*;

/// Hands an error from a primitive to the interpreter, which reports it from
/// `fallback_ip`.
#[cold]
pub(super) fn raise_at(ctx: &mut VmCore, fallback_ip: usize, e: SteelErr) -> SteelVal {
    ctx.ip = fallback_ip;
    ctx.result = Some(Err(e.set_span_if_none(ctx.current_span())));
    ctx.is_native = false;
    SteelVal::Void
}

helper_family! {
    CallPrimitiveDefinitions, mod call_primitive_function_deopt,
    names "call_primitive_function_deopt_" "",
    add_func fn(
        ctx: *mut VmCore,
        func: fn(&[SteelVal]) -> Result<SteelVal>,
        fallback_ip: usize
    ) -> SteelVal,
    arities [0 1 2 3 4 5 6 7 8],
    |args| {
        let ctx = unsafe { &mut *ctx };
        match ctx.thread.enter_safepoint_holding(&args, |_: &SteelThread| (func)(&args)) {
            Ok(v) => v,
            Err(e) => raise_at(ctx, fallback_ip, e),
        }
    }
}

helper_family! {
    CallPrimitiveBorrowedDefinitions, mod call_primitive_function_borrowed,
    names "call_primitive_function_borrowed_" "",
    add_func fn(
        ctx: *mut VmCore,
        func: fn(&[SteelVal]) -> Result<SteelVal>,
        fallback_ip: usize,
        owned: u64
    ) -> SteelVal,
    arities [1 2 3 4 5 6 7 8],
    |args| {
        // Bit `i` of `owned` is set when argument `i` was handed over. The rest
        // are still held by the caller, so they are read here but not dropped.
        let ctx = unsafe { &mut *ctx };
        let args = core::mem::ManuallyDrop::new(args);
        let res = ctx.thread.enter_safepoint_holding(&*args, |_: &SteelThread| (func)(&*args));
        for (i, arg) in args.iter().enumerate() {
            if owned & (1 << i) != 0 {
                drop(unsafe { core::ptr::read(arg) });
            }
        }
        match res {
            Ok(v) => v,
            Err(e) => raise_at(ctx, fallback_ip, e),
        }
    }
}

helper_family! {
    CallPrimitiveMutDefinitions, mod call_primitive_mut_function_deopt,
    names "call_primitive_mut_function_deopt_" "",
    add_func fn(
        ctx: *mut VmCore,
        func: fn(&mut [SteelVal]) -> Result<SteelVal>,
        fallback_ip: usize
    ) -> SteelVal,
    arities [0 1 2 3 4 5 6 7 8],
    |args| match (func)(&mut args) {
        Ok(v) => v,
        Err(e) => raise_at(unsafe { &mut *ctx }, fallback_ip, e),
    }
}

/// `call_primitive_function_fixed_deopt_N`, for a primitive taking exactly `N`
/// arguments by value. The function pointer's type depends on the arity, which
/// `helper_family!` can't express.
macro_rules! make_primitive_function_fixed_arity_deopt {
    ($(($name:tt, $($typ:ident),*)),*) => {

        pub struct CallPrimitiveFixedDefinitions;

        impl CallPrimitiveFixedDefinitions {
            pub fn register(map: &mut crate::jit2::cgen::FunctionMap) {
                $(
                    #[cfg(target_os = "windows")]
                    map.add_func(
                        stringify!($name),
                        $name as extern "sysv64-unwind" fn(ctx: *mut VmCore, func: fn($($typ: SteelVal),*) -> Result<SteelVal>, fallback_ip: usize, $($typ: SteelVal),*) -> SteelVal
                    );


                    #[cfg(not(target_os = "windows"))]
                    map.add_func(
                        stringify!($name),
                        $name as extern "C-unwind" fn(ctx: *mut VmCore, func: fn($($typ: SteelVal),*) -> Result<SteelVal>, fallback_ip: usize, $($typ: SteelVal),*) -> SteelVal
                    );
                )*
            }

            pub fn arity_to_name(count: usize) -> Option<&'static str> {
                $(
                    {
                        $(
                            let $typ = 0usize;
                        )*

                        let arr: &[usize] = &[$($typ),*];

                        if count == arr.len() {
                            return Some(stringify!($name));
                        }
                    }
                )*

                None
            }
        }

        $(
            #[cross_platform_fn]
            fn $name(
                ctx: *mut VmCore,
                func: fn($($typ: SteelVal),*) -> Result<SteelVal>,
                fallback_ip: usize,
                $($typ: SteelVal),*
            ) -> SteelVal {
                match (func)($($typ),*) {
                    Ok(v) => v,
                    Err(e) => raise_at(unsafe { &mut *ctx }, fallback_ip, e),
                }
            }
        )*
    };
}

make_primitive_function_fixed_arity_deopt!(
    (call_primitive_function_fixed_deopt_0,),
    (call_primitive_function_fixed_deopt_1, a),
    (call_primitive_function_fixed_deopt_2, a, b),
    (call_primitive_function_fixed_deopt_3, a, b, c),
    (call_primitive_function_fixed_deopt_4, a, b, c, d),
    (call_primitive_function_fixed_deopt_5, a, b, c, d, e),
    (call_primitive_function_fixed_deopt_6, a, b, c, d, e, f),
    (call_primitive_function_fixed_deopt_7, a, b, c, d, e, f, g),
    (
        call_primitive_function_fixed_deopt_8,
        a,
        b,
        c,
        d,
        e,
        f,
        g,
        h
    )
);
