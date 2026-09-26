//! Arithmetic and comparison helpers for jitted code.

use super::*;

#[cross_platform_fn]
fn num_equal_value_bool(ctx: *mut VmCore, left: SteelVal, right: SteelVal) -> bool {
    let guard = unsafe { &mut *ctx };

    match number_equality(&left, &right) {
        Ok(SteelVal::BoolV(b)) => b,
        // Same as extern_c_lte_register. Comparing a number against a
        // non-number is a runtime error, so report it.
        Ok(other) => {
            guard.is_native = false;
            guard.result = Some(Err(SteelErr::new(
                ErrorKind::TypeMismatch,
                format!("=: expected a boolean result, found: {}", other),
            )));
            false
        }
        Err(e) => {
            guard.is_native = false;
            guard.result = Some(Err(e));
            false
        }
    }
}

#[cross_platform_fn]
fn num_equal_int(ctx: *mut VmCore, left: SteelVal, right: SteelVal) -> SteelVal {
    // The jit only picks this when it has inferred an integer on the right.
    // The left can still be any value.
    assert!(matches!(right, SteelVal::IntV(_) | SteelVal::BigNum(_)));

    match number_equality(&left, &right) {
        Ok(b) => b,
        Err(e) => {
            let guard = unsafe { &mut *ctx };
            guard.is_native = false;
            guard.result = Some(Err(e));
            SteelVal::Void
        }
    }
}

#[cross_platform_fn]
fn num_equal_int_register(ctx: *mut VmCore, i: usize, right: SteelVal) -> bool {
    assert!(matches!(right, SteelVal::IntV(_) | SteelVal::BigNum(_)));
    let ctx = unsafe { &mut *ctx };
    let offset = ctx.get_offset();
    let left = &ctx.thread.stack[i + offset];

    match number_equality(&left, &right) {
        Ok(SteelVal::BoolV(b)) => b,
        Err(e) => {
            ctx.result = Some(Err(e));
            ctx.is_native = false;
            false
        }
        _ => unreachable!(),
    }
}

#[cross_platform_fn]
fn equal_binop_bool(_ctx: *mut VmCore, left: SteelVal, right: SteelVal) -> bool {
    left == right
}

#[cross_platform_fn]
fn equal_binop_register_bool(ctx: *mut VmCore, i: usize, right: SteelVal) -> bool {
    let ctx = unsafe { &mut *ctx };
    let offset = ctx.get_offset();
    let left = &ctx.thread.stack[i + offset];
    left == &right
}

macro_rules! extern_binop {
    ($name:tt, $func:tt) => {
        #[cross_platform_fn]
        fn $name(ctx: *mut VmCore, a: SteelVal, b: SteelVal) -> SteelVal {
            match $func(&[a, b]) {
                Ok(v) => v,
                // Hand the error back the way extern_c_div_one does, so errors
                // like a divide by zero raise instead of panicking.
                Err(e) => {
                    let guard = unsafe { &mut *ctx };
                    guard.is_native = false;
                    guard.result = Some(Err(e));
                    SteelVal::Void
                }
            }
        }
    };
}

#[cross_platform_fn]
fn extern_c_add_two(ctx: *mut VmCore, a: SteelVal, b: SteelVal) -> SteelVal {
    match add_two(&a, &b) {
        Ok(v) => v,
        Err(e) => {
            unsafe {
                let guard = &mut *ctx;
                guard.result = Some(Err(e));
                guard.is_native = false;
            }

            SteelVal::Void
        }
    }
}

#[cross_platform_fn]
fn extern_c_add_three(ctx: *mut VmCore, a: SteelVal, b: SteelVal, c: SteelVal) -> SteelVal {
    match add_two(&a, &b).and_then(|x| add_two(&x, &c)) {
        Ok(v) => v,
        Err(e) => {
            unsafe {
                let guard = &mut *ctx;
                guard.result = Some(Err(e));
                guard.is_native = false;
            }

            SteelVal::Void
        }
    }
}

#[cross_platform_fn]
fn extern_c_mult_three(ctx: *mut VmCore, a: SteelVal, b: SteelVal, c: SteelVal) -> SteelVal {
    match multiply_primitive(&[a, b, c]) {
        Ok(v) => v,
        Err(e) => {
            unsafe {
                let guard = &mut *ctx;
                guard.result = Some(Err(e));
                guard.is_native = false;
            }

            SteelVal::Void
        }
    }
}

#[cross_platform_fn]
fn extern_c_sub_three(ctx: *mut VmCore, a: SteelVal, b: SteelVal, c: SteelVal) -> SteelVal {
    match subtract_primitive(&[a, b, c]) {
        Ok(v) => v,
        Err(e) => {
            unsafe {
                let guard = &mut *ctx;
                guard.result = Some(Err(e));
                guard.is_native = false;
            }

            SteelVal::Void
        }
    }
}

#[cross_platform_fn]
fn extern_c_add_four(
    ctx: *mut VmCore,
    a: SteelVal,
    b: SteelVal,
    c: SteelVal,
    d: SteelVal,
) -> SteelVal {
    match add_two(&a, &b)
        .and_then(|x| add_two(&x, &c))
        .and_then(|x| add_two(&x, &d))
    {
        Ok(v) => v,
        Err(e) => {
            unsafe {
                let guard = &mut *ctx;
                guard.result = Some(Err(e));
                guard.is_native = false;
            }

            SteelVal::Void
        }
    }
}

#[cross_platform_fn]
fn extern_c_add_two_binop_register(ctx: *mut VmCore, reg: usize, b: SteelVal) -> SteelVal {
    let ctx = unsafe { &mut *ctx };
    let offset = ctx.get_offset();
    let a = &ctx.thread.stack[reg + offset];

    match add_two(a, &b) {
        Ok(v) => v,
        Err(e) => {
            ctx.result = Some(Err(e));
            ctx.is_native = false;
            SteelVal::Void
        }
    }
}

#[cross_platform_fn]
fn extern_c_add_two_binop_register_both(ctx: *mut VmCore, reg1: usize, reg2: usize) -> SteelVal {
    let ctx = unsafe { &mut *ctx };
    let offset = ctx.get_offset();
    let a = &ctx.thread.stack[reg1 + offset];
    let b = &ctx.thread.stack[reg2 + offset];

    match add_two(a, b) {
        Ok(v) => v,
        Err(e) => {
            ctx.result = Some(Err(e));
            ctx.is_native = false;
            SteelVal::Void
        }
    }
}

#[cross_platform_fn]
fn extern_c_sub_two_int_reg(ctx: *mut VmCore, reg: usize, b: SteelVal) -> SteelVal {
    let ctx = unsafe { &mut *ctx };
    // Lets try to avoid the drop glue getting generated here?
    let b = std::mem::ManuallyDrop::new(b);
    let rhs = if let SteelVal::IntV(i) = &*b {
        *i
    } else {
        unsafe { unreachable_unchecked() }
    };

    let offset = ctx.get_offset();
    let a = &ctx.thread.stack[reg + offset];

    match a {
        SteelVal::IntV(l) => match (*l).checked_sub(rhs as _) {
            Some(x) => SteelVal::IntV(x),
            None => {
                let res = BigInt::from(*l) - rhs;
                res.into_steelval().unwrap()
            }
        },
        _ => match subtract_primitive(&[a.clone(), SteelVal::IntV(rhs as _)]) {
            Ok(v) => v,
            Err(e) => {
                ctx.result = Some(Err(e));
                ctx.is_native = false;
                SteelVal::Void
            }
        },
    }
}

#[cross_platform_fn]
fn extern_c_add_two_int_reg(ctx: *mut VmCore, reg: usize, b: SteelVal) -> SteelVal {
    let ctx = unsafe { &mut *ctx };
    // Lets try to avoid the drop glue getting generated here?
    let b = std::mem::ManuallyDrop::new(b);
    let rhs = if let SteelVal::IntV(i) = &*b {
        *i
    } else {
        unsafe { unreachable_unchecked() }
    };

    let offset = ctx.get_offset();
    let a = &ctx.thread.stack[reg + offset];

    match add_primitive(&[a.clone(), SteelVal::IntV(rhs as _)]) {
        Ok(v) => v,
        Err(e) => {
            ctx.result = Some(Err(e));
            ctx.is_native = false;
            SteelVal::Void
        }
    }
}

#[cross_platform_fn]
fn extern_c_sub_two_float_reg(ctx: *mut VmCore, reg: usize, b: SteelVal) -> SteelVal {
    let ctx = unsafe { &mut *ctx };
    // Lets try to avoid the drop glue getting generated here?
    let b = std::mem::ManuallyDrop::new(b);
    let rhs = if let SteelVal::NumV(i) = &*b {
        *i
    } else {
        unsafe { unreachable_unchecked() }
    };

    let offset = ctx.get_offset();
    let a = &ctx.thread.stack[reg + offset];

    match a {
        _ => match subtract_primitive(&[a.clone(), SteelVal::NumV(rhs as _)]) {
            Ok(v) => v,
            Err(e) => {
                ctx.result = Some(Err(e));
                ctx.is_native = false;
                SteelVal::Void
            }
        },
    }
}

#[cross_platform_fn]
fn extern_c_sub_two_reg(ctx: *mut VmCore, reg: usize, b: SteelVal) -> SteelVal {
    use crate::primitives::numbers::sub_two;

    let ctx = unsafe { &mut *ctx };

    let offset = ctx.get_offset();
    let a = &ctx.thread.stack[reg + offset];

    match sub_two(a, &b) {
        Ok(v) => v,
        Err(e) => {
            ctx.result = Some(Err(e));
            ctx.is_native = false;
            SteelVal::Void
        }
    }
}

#[cross_platform_fn]
fn extern_c_sub_two_int(ctx: *mut VmCore, a: SteelVal, b: SteelVal) -> SteelVal {
    let ctx = unsafe { &mut *ctx };
    let rhs = if let SteelVal::IntV(i) = b {
        i
    } else {
        panic!()
    };

    match a {
        SteelVal::IntV(l) => match l.checked_sub(rhs) {
            Some(x) => SteelVal::IntV(x),
            None => {
                let res = BigInt::from(l) - rhs;
                res.into_steelval().unwrap()
            }
        },
        _ => match subtract_primitive(&[a.clone(), SteelVal::IntV(rhs)]) {
            Ok(v) => v,
            Err(e) => {
                ctx.result = Some(Err(e));
                ctx.is_native = false;
                SteelVal::Void
            }
        },
    }
}

#[cross_platform_fn]
fn extern_c_sub_two_both_reg(ctx: *mut VmCore, l: usize, r: usize) -> SteelVal {
    let ctx = unsafe { &mut *ctx };

    let offset = ctx.get_offset();
    let a = &ctx.thread.stack[l + offset];
    let b = &ctx.thread.stack[r + offset];

    match crate::primitives::numbers::sub_two(a, b) {
        Ok(v) => v,
        Err(e) => {
            ctx.result = Some(Err(e));
            ctx.is_native = false;
            SteelVal::Void
        }
    }
}

#[cross_platform_fn]
fn extern_c_negate(ctx: *mut VmCore, arg: SteelVal) -> SteelVal {
    match crate::primitives::numbers::negate(&arg) {
        Ok(v) => v,
        Err(e) => {
            unsafe {
                let guard = &mut *ctx;
                guard.result = Some(Err(e));
                guard.is_native = false;
            }

            SteelVal::Void
        }
    }
}

extern_binop!(extern_c_sub_two, subtract_primitive);

// Go through the primitive so a pair that cannot be ordered raises, the same
// as `gt`/`gte`. `SteelVal::BoolV(a <= b)` would answer `#false` for it.
extern_binop!(extern_c_lt_two, lt_primitive);

extern_binop!(extern_c_lte_two, lte_primitive);

// Bool-returning comparisons on two arbitrary values.
//
// The `*-binop` helpers return a boxed `SteelVal`, which works when the JIT
// pushes the result. The fallback arm of an inlined comparison needs an unboxed
// `i1` to match the inlined arm.
//
// These must match `ordering_primitive` (the shared body of `<`, `<=`, `>` and
// `>=`), or an expression answers differently depending on whether the jit
// inlined the comparison. So they raise when `partial_cmp` returns `None`.
//
// The check is comparability, and does not require `realp` like
// `extern_c_lte_register` does. Strings order against strings; an incomparable
// pair, such as a number and a string, raises.
//
// These are plain comments because `cross_platform_fn` re-emits the item's
// attributes after the `extern "C-unwind"`, so a `///` here is a syntax error.
#[cross_platform_fn]
fn extern_c_lt_two_value_bool(ctx: *mut VmCore, a: SteelVal, b: SteelVal) -> bool {
    match a.partial_cmp(&b) {
        Some(ordering) => ordering == std::cmp::Ordering::Less,
        None => {
            let e = SteelErr::new(
                ErrorKind::TypeMismatch,
                format!("< expected comparable values, found: {} and {}", a, b),
            );

            unsafe {
                let guard = &mut *ctx;
                guard.result = Some(Err(e));
                guard.is_native = false;
            }

            false
        }
    }
}

#[cross_platform_fn]
fn extern_c_lte_two_value_bool(ctx: *mut VmCore, a: SteelVal, b: SteelVal) -> bool {
    match a.partial_cmp(&b) {
        Some(ordering) => ordering != std::cmp::Ordering::Greater,
        None => {
            let e = SteelErr::new(
                ErrorKind::TypeMismatch,
                format!("<= expected comparable values, found: {} and {}", a, b),
            );

            unsafe {
                let guard = &mut *ctx;
                guard.result = Some(Err(e));
                guard.is_native = false;
            }

            false
        }
    }
}

#[cross_platform_fn]
fn extern_c_gt_two_value_bool(ctx: *mut VmCore, a: SteelVal, b: SteelVal) -> bool {
    match a.partial_cmp(&b) {
        Some(ordering) => ordering == std::cmp::Ordering::Greater,
        None => {
            let e = SteelErr::new(
                ErrorKind::TypeMismatch,
                format!("> expected comparable values, found: {} and {}", a, b),
            );

            unsafe {
                let guard = &mut *ctx;
                guard.result = Some(Err(e));
                guard.is_native = false;
            }

            false
        }
    }
}

#[cross_platform_fn]
fn extern_c_gte_two_value_bool(ctx: *mut VmCore, a: SteelVal, b: SteelVal) -> bool {
    match a.partial_cmp(&b) {
        Some(ordering) => ordering != std::cmp::Ordering::Less,
        None => {
            let e = SteelErr::new(
                ErrorKind::TypeMismatch,
                format!(">= expected comparable values, found: {} and {}", a, b),
            );

            unsafe {
                let guard = &mut *ctx;
                guard.result = Some(Err(e));
                guard.is_native = false;
            }

            false
        }
    }
}

// The three argument orderings, `(< a b c)`. Like the two argument ones, these
// go through the primitive so an unorderable pair raises instead of answering
// `#false`.
#[cross_platform_fn]
fn extern_c_lt_three(ctx: *mut VmCore, a: SteelVal, b: SteelVal, c: SteelVal) -> SteelVal {
    match lt_primitive(&[a, b, c]) {
        Ok(v) => v,
        Err(e) => {
            let guard = unsafe { &mut *ctx };
            guard.is_native = false;
            guard.result = Some(Err(e));
            SteelVal::Void
        }
    }
}

#[cross_platform_fn]
fn extern_c_lte_three(ctx: *mut VmCore, a: SteelVal, b: SteelVal, c: SteelVal) -> SteelVal {
    match lte_primitive(&[a, b, c]) {
        Ok(v) => v,
        Err(e) => {
            let guard = unsafe { &mut *ctx };
            guard.is_native = false;
            guard.result = Some(Err(e));
            SteelVal::Void
        }
    }
}

#[cross_platform_fn]
fn extern_c_gt_three(ctx: *mut VmCore, a: SteelVal, b: SteelVal, c: SteelVal) -> SteelVal {
    match gt_primitive(&[a, b, c]) {
        Ok(v) => v,
        Err(e) => {
            let guard = unsafe { &mut *ctx };
            guard.is_native = false;
            guard.result = Some(Err(e));
            SteelVal::Void
        }
    }
}

#[cross_platform_fn]
fn extern_c_gte_three(ctx: *mut VmCore, a: SteelVal, b: SteelVal, c: SteelVal) -> SteelVal {
    match gte_primitive(&[a, b, c]) {
        Ok(v) => v,
        Err(e) => {
            let guard = unsafe { &mut *ctx };
            guard.is_native = false;
            guard.result = Some(Err(e));
            SteelVal::Void
        }
    }
}

// `b` is known to be an integer, but `a` is whatever was on the stack, so this
// still has to reject an `a` that will not order against it.
#[cross_platform_fn]
fn extern_c_lte_two_int(ctx: *mut VmCore, a: SteelVal, b: SteelVal) -> SteelVal {
    assert!(matches!(b, SteelVal::IntV(_)));
    match lte_primitive(&[a, b]) {
        Ok(v) => v,
        Err(e) => {
            let guard = unsafe { &mut *ctx };
            guard.is_native = false;
            guard.result = Some(Err(e));
            SteelVal::Void
        }
    }
}

#[cross_platform_fn]
fn extern_c_lte_register(ctx: *mut VmCore, reg: usize, b: SteelVal) -> bool {
    let ctx = unsafe { &mut *ctx };
    let offset = ctx.get_offset();
    let a = &ctx.thread.stack[reg + offset];

    match a.partial_cmp(&b) {
        Some(ordering) => ordering != std::cmp::Ordering::Greater,
        None => {
            let e = SteelErr::new(
                ErrorKind::TypeMismatch,
                format!("<= expected comparable values, found: {} and {}", a, b),
            );

            ctx.result = Some(Err(e));
            ctx.is_native = false;

            false
        }
    }
}

#[cross_platform_fn]
fn extern_c_lte_register_int(ctx: *mut VmCore, reg: usize, b: SteelVal) -> bool {
    assert!(matches!(b, SteelVal::IntV(_)));
    let ctx = unsafe { &mut *ctx };
    let offset = ctx.get_offset();
    let a = &ctx.thread.stack[reg + offset];

    // Avoid the drop glue. We've already asserted that this is an integer
    let b = ManuallyDrop::new(b);

    match a.partial_cmp(&*b) {
        Some(ordering) => ordering != std::cmp::Ordering::Greater,
        None => {
            let e = SteelErr::new(
                ErrorKind::TypeMismatch,
                format!("<= expected comparable values, found: {} and {}", a, *b),
            );

            ctx.result = Some(Err(e));
            ctx.is_native = false;

            false
        }
    }
}

#[cross_platform_fn]
fn extern_c_gte_register_int(ctx: *mut VmCore, reg: usize, b: SteelVal) -> bool {
    assert!(matches!(b, SteelVal::IntV(_)));
    let ctx = unsafe { &mut *ctx };
    let offset = ctx.get_offset();
    let a = &ctx.thread.stack[reg + offset];

    // Avoid the drop glue. We've already asserted that this is an integer
    let b = ManuallyDrop::new(b);

    match a.partial_cmp(&*b) {
        Some(ordering) => ordering != std::cmp::Ordering::Less,
        None => {
            let e = SteelErr::new(
                ErrorKind::TypeMismatch,
                format!(">= expected comparable values, found: {} and {}", a, *b),
            );

            ctx.result = Some(Err(e));
            ctx.is_native = false;

            false
        }
    }
}

#[cross_platform_fn]
fn extern_c_gte_register_unknown(ctx: *mut VmCore, reg: usize, b: SteelVal) -> bool {
    let ctx = unsafe { &mut *ctx };
    let offset = ctx.get_offset();
    let a = &ctx.thread.stack[reg + offset];

    match a.partial_cmp(&b) {
        Some(ordering) => ordering != std::cmp::Ordering::Less,
        None => {
            let e = SteelErr::new(
                ErrorKind::TypeMismatch,
                format!(">= expected comparable values, found: {} and {}", a, b),
            );

            ctx.result = Some(Err(e));
            ctx.is_native = false;

            false
        }
    }
}

#[cross_platform_fn]
fn extern_c_lt_register_int(ctx: *mut VmCore, reg: usize, b: SteelVal) -> bool {
    assert!(matches!(b, SteelVal::IntV(_)));
    let ctx = unsafe { &mut *ctx };
    let offset = ctx.get_offset();
    let a = &ctx.thread.stack[reg + offset];

    // Avoid the drop glue. We've already asserted that this is an integer
    let b = ManuallyDrop::new(b);

    match a.partial_cmp(&*b) {
        Some(ordering) => ordering == std::cmp::Ordering::Less,
        None => {
            let e = SteelErr::new(
                ErrorKind::TypeMismatch,
                format!("< expected comparable values, found: {} and {}", a, *b),
            );

            ctx.result = Some(Err(e));
            ctx.is_native = false;

            false
        }
    }
}

#[cross_platform_fn]
fn extern_c_gt_register_int(ctx: *mut VmCore, reg: usize, b: SteelVal) -> bool {
    assert!(matches!(b, SteelVal::IntV(_)));
    let ctx = unsafe { &mut *ctx };
    let offset = ctx.get_offset();
    let a = &ctx.thread.stack[reg + offset];

    // Avoid the drop glue. We've already asserted that this is an integer
    let b = ManuallyDrop::new(b);

    match a.partial_cmp(&*b) {
        Some(ordering) => ordering == std::cmp::Ordering::Greater,
        None => {
            let e = SteelErr::new(
                ErrorKind::TypeMismatch,
                format!("> expected comparable values, found: {} and {}", a, *b),
            );

            ctx.result = Some(Err(e));
            ctx.is_native = false;

            false
        }
    }
}

#[cross_platform_fn]
fn extern_c_lt_two_int(ctx: *mut VmCore, a: SteelVal, b: SteelVal) -> SteelVal {
    assert!(matches!(b, SteelVal::IntV(_)));
    match lt_primitive(&[a, b]) {
        Ok(v) => v,
        Err(e) => {
            let guard = unsafe { &mut *ctx };
            guard.is_native = false;
            guard.result = Some(Err(e));
            SteelVal::Void
        }
    }
}

#[cross_platform_fn]
fn extern_c_null_handler(a: SteelVal) -> SteelVal {
    let result = match a {
        SteelVal::ListV(l) => l.is_empty(),
        SteelVal::VectorV(v) => v.is_empty(),
        SteelVal::MutableVector(v) => {
            let ptr = v.strong_ptr();
            let guard = &ptr.lock().value;
            guard.is_empty()
        }
        _ => false,
    };

    SteelVal::BoolV(result)
}

extern_binop!(extern_c_gt_two, gt_primitive);

extern_binop!(extern_c_gte_two, gte_primitive);

extern_binop!(extern_c_mult_two, multiply_primitive);

extern_binop!(extern_c_div_two, divide_primitive);

#[cross_platform_fn]
fn extern_c_div_one(ctx: *mut VmCore, a: SteelVal) -> SteelVal {
    let guard = unsafe { &mut *ctx };

    let res = divide_primitive(&[a]);

    match res {
        Ok(v) => v,
        Err(e) => {
            guard.is_native = false;
            guard.result = Some(Err(e));
            SteelVal::Void
        }
    }
}

// Arithmetic or a comparison at an arity the jit has no fixed helper for. The
// operands are the top `arity` values of the vm stack. `code` is
// `variadic_numeric_code` in the jit.
#[cross_platform_fn]
fn variadic_numeric_spilled(ctx: *mut VmCore<'_>, code: usize, arity: usize) -> SteelVal {
    let ctx = unsafe { &mut *ctx };

    let func: fn(&[SteelVal]) -> Result<SteelVal> = match code {
        0 => add_primitive,
        1 => subtract_primitive,
        2 => multiply_primitive,
        3 => divide_primitive,
        4 => lt_primitive,
        5 => crate::steel_vm::primitives::lte_primitive,
        6 => gt_primitive,
        _ => gte_primitive,
    };

    let len = ctx.thread.stack.len();
    let result = func(&ctx.thread.stack[len - arity..]);
    let _ = ctx.thread.stack.truncate(len - arity);

    match result {
        Ok(value) => value,
        Err(e) => {
            ctx.result = Some(Err(e));
            ctx.is_native = false;
            SteelVal::Void
        }
    }
}
