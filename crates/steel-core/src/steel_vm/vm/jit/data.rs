//! Lists, pairs, vectors, boxes, predicates and constructors for jitted code.

use super::*;

#[cross_platform_fn]
fn car_handler_value(ctx: *mut VmCore, arg: SteelVal) -> SteelVal {
    unsafe { &mut *ctx }.ip += 2;
    match car(&arg) {
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
fn car_handler_reg(ctx: *mut VmCore, reg: usize) -> SteelVal {
    let guard = unsafe { &mut *ctx };
    guard.ip += 2;

    let offset = guard.get_offset();
    let arg = &guard.thread.stack[offset + reg];

    match car(&arg) {
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
fn list_ref_handler_c(ctx: *mut VmCore, list: SteelVal, index: SteelVal) -> SteelVal {
    fn inner_list_ref(list: SteelVal, index: SteelVal) -> Result<SteelVal> {
        if let SteelVal::ListV(l) = list {
            if let SteelVal::IntV(i) = index {
                list_ref(&l, i)
            } else {
                stop!(TypeMismatch => "list-ref expected an integer, found: {}", index);
            }
        } else {
            stop!(TypeMismatch => "list-ref expected a list, found: {}", list);
        }
    }

    match inner_list_ref(list, index) {
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
fn is_string_c_reg(ctx: *mut VmCore, register: usize) -> SteelVal {
    use crate::steel_vm::primitives::stringp;

    let guard = unsafe { &mut *ctx };
    let offset = guard.get_offset();
    let value = &guard.thread.stack[offset + register];
    SteelVal::BoolV(stringp(value))
}

// TODOO: Abstract these into a macro?
#[cross_platform_fn]
fn is_symbol_c_reg(ctx: *mut VmCore, register: usize) -> SteelVal {
    use crate::steel_vm::primitives::symbolp;

    let guard = unsafe { &mut *ctx };
    let offset = guard.get_offset();
    let value = &guard.thread.stack[offset + register];
    SteelVal::BoolV(symbolp(value))
}

#[cross_platform_fn]
fn is_pair_c_reg(ctx: *mut VmCore, register: usize) -> SteelVal {
    let guard = unsafe { &mut *ctx };
    let offset = guard.get_offset();
    let value = &guard.thread.stack[offset + register];
    SteelVal::BoolV(crate::primitives::lists::pair(value))
}

#[cross_platform_fn]
fn is_void_c_reg(ctx: *mut VmCore, register: usize) -> SteelVal {
    use crate::steel_vm::primitives::voidp;

    let guard = unsafe { &mut *ctx };
    let offset = guard.get_offset();
    let value = &guard.thread.stack[offset + register];
    SteelVal::BoolV(voidp(value))
}

#[cross_platform_fn]
fn is_pair_value(value: SteelVal) -> SteelVal {
    SteelVal::BoolV(crate::primitives::lists::pair(&value))
}

#[cross_platform_fn]
fn is_list_value(value: SteelVal) -> SteelVal {
    SteelVal::BoolV(listp(&value))
}

#[cross_platform_fn]
fn is_void_value(value: SteelVal) -> SteelVal {
    use crate::steel_vm::primitives::voidp;

    SteelVal::BoolV(voidp(&value))
}

#[cross_platform_fn]
fn is_string_value(value: SteelVal) -> SteelVal {
    use crate::steel_vm::primitives::stringp;

    SteelVal::BoolV(stringp(&value))
}

#[cross_platform_fn]
fn is_symbol_value(value: SteelVal) -> SteelVal {
    use crate::steel_vm::primitives::symbolp;

    SteelVal::BoolV(symbolp(&value))
}

#[cross_platform_fn]
fn is_empty_c_reg(ctx: *mut VmCore, register: usize) -> SteelVal {
    let guard = unsafe { &mut *ctx };
    let offset = guard.get_offset();
    let value = &guard.thread.stack[offset + register];
    SteelVal::BoolV(crate::primitives::lists::is_empty(value))
}

#[cross_platform_fn]
fn is_empty_value(value: SteelVal) -> SteelVal {
    SteelVal::BoolV(crate::primitives::lists::is_empty(&value))
}

// `list_cursor_materialize`, consuming the head the cursor walked from (the
// value in its slot, which is being replaced). A head whose cells nothing else
// shares is reused like an in-place `cdr`, repositioned onto the target without
// a copy. Cloning from the head would copy the target cell on every step, since
// the head still shares it.
#[cross_platform_fn]
fn list_cursor_materialize_owned(head: SteelVal, cell: usize, index: usize) -> SteelVal {
    if index == 0 {
        drop(head);
        return SteelVal::ListV(crate::values::lists::List::new());
    }

    debug_assert!(cell != 0, "a non-empty cursor always has a cell");

    match head {
        SteelVal::ListV(list) => {
            SteelVal::ListV(unsafe { list.reposition_from_raw_parts(cell as *const u8, index) })
        }
        other => {
            let out = list_cursor_materialize(cell, index);
            drop(other);
            out
        }
    }
}

// Turn a `(cell, index)` cursor that jitted code has been walking in registers
// back into a real list. Only called when the cursor escapes (is stored,
// returned, passed to a call, or has to be visible to the interpreter at a
// deopt), so the cell copy is paid once instead of once per step.
//
// The cell is kept alive by the head of the list, which stays in its vm stack
// slot untouched for as long as a cursor is walking it.
#[cross_platform_fn]
fn list_cursor_materialize(cell: usize, index: usize) -> SteelVal {
    // Index 0 means the cursor walked off the end and is the empty list; `cell`
    // is the last cell it was in. Return a real empty list, since an inline cell
    // has no position 0 to reposition to.
    if index == 0 {
        return SteelVal::ListV(crate::values::lists::List::new());
    }

    debug_assert!(cell != 0, "a non-empty cursor always has a cell");

    SteelVal::ListV(unsafe {
        crate::values::lists::List::<SteelVal>::from_raw_parts(cell as *const u8, index)
    })
}

#[cross_platform_fn]
fn vector_ref_handler_c(ctx: *mut VmCore, mut vec: SteelVal, index: SteelVal) -> SteelVal {
    match vec_ref(&mut vec, &index) {
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

// A char class predicate on anything the inline test does not cover: a
// non-ascii char, or a value that is not a char, which raises. `class` is a
// `CharClass` discriminant.
#[cross_platform_fn]
fn char_class_handler_c(ctx: *mut VmCore, c: SteelVal, class: i64) -> SteelVal {
    use crate::primitives::strings::*;
    let predicate = match class {
        0 => steel_char_is_whitespace,
        1 => steel_char_is_alphabetic,
        2 => steel_char_is_numeric,
        3 => steel_char_is_upper_case,
        _ => steel_char_is_lower_case,
    };
    match predicate(&[c]) {
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
fn vector_length_handler_c(ctx: *mut VmCore, vec: SteelVal) -> SteelVal {
    match crate::primitives::vectors::vec_length(&[vec]) {
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
fn vector_ref_handler_register(ctx: *mut VmCore, vec_reg: u16, index: SteelVal) -> SteelVal {
    let guard = unsafe { &mut *ctx };

    let offset = guard.get_offset();
    let vec = &mut guard.thread.stack[vec_reg as usize + offset];

    match vec_ref(vec, &index) {
        Ok(v) => v,
        Err(e) => {
            guard.result = Some(Err(e));
            guard.is_native = false;

            SteelVal::Void
        }
    }
}

#[cross_platform_fn]
fn vector_ref_handler_register_two(ctx: *mut VmCore, vec_reg: usize, index: usize) -> SteelVal {
    let guard = unsafe { &mut *ctx };

    let offset = guard.get_offset();

    let mut thunk = || {
        let temp_idx = &guard.thread.stack[index + offset];
        if let SteelVal::IntV(i) = temp_idx {
            let i = *i;

            if i < 0 {
                stop!(Generic => "vector-ref expects a positive integer, found: {:?}", i);
            }

            let idx_usize = i as usize;

            let vec = &mut guard.thread.stack[vec_reg + offset];

            // Now match on the vector type
            match vec {
                SteelVal::MutableVector(v) => {
                    // TODO: If we move this into a context aware function,
                    // then we can avoid the lookup cost since we won't be in a safepoint.
                    if let Some(guard) = unsafe { v.inner_ref() } {
                        if idx_usize >= guard.len() {
                            stop!(Generic => "index out of bounds, index given: {:?}, length of vector: {:?}", i, guard.len());
                        }

                        Ok(guard[idx_usize].clone())
                    } else {
                        let guard = &(unsafe { &(*v.inner.as_ptr()) }.lock());

                        let guard = &guard.value;

                        if idx_usize >= guard.len() {
                            stop!(Generic => "index out of bounds, index given: {:?}, length of vector: {:?}", i, guard.len());
                        }

                        Ok(guard[idx_usize].clone())
                    }
                }

                SteelVal::VectorV(v) => {
                    if idx_usize < v.len() {
                        Ok(v[idx_usize].clone())
                    } else {
                        let e = format!(
                            "Index out of bounds - attempted to access index: {} with length: {}",
                            idx_usize,
                            v.len()
                        );
                        stop!(Generic => e);
                    }
                }
                SteelVal::FlatVector(v) => {
                    if idx_usize < v.len() {
                        Ok(v[idx_usize].clone())
                    } else {
                        let e = format!(
                            "Index out of bounds - attempted to access index: {} with length: {}",
                            idx_usize,
                            v.len()
                        );
                        stop!(Generic => e);
                    }
                }

                _ => stop!(TypeMismatch => format!(
                    "vector-ref expected a vector and a number, found: {:?} and {:?}",
                    &guard.thread.stack[vec_reg + offset], guard.thread.stack[index + offset]
                )),
            }
        } else {
            let idx = guard.thread.stack[index + offset].clone();
            stop!(TypeMismatch => "vector-ref expects an integer index, found: {:?}", idx);
        }
    };

    match thunk() {
        Ok(v) => v,
        Err(e) => {
            guard.result = Some(Err(e));
            guard.is_native = false;

            SteelVal::Void
        }
    }
}

#[cross_platform_fn]
fn vector_ref_handler_register_two_unboxed(ctx: *mut VmCore, vec_reg: usize, i: usize) -> SteelVal {
    let guard = unsafe { &mut *ctx };

    let offset = guard.get_offset();

    let mut thunk = || {
        let idx_usize = i as usize;

        let vec = &mut guard.thread.stack[vec_reg + offset];

        // Now match on the vector type
        match vec {
            SteelVal::MutableVector(v) => {
                // TODO: If we move this into a context aware function,
                // then we can avoid the lookup cost since we won't be in a safepoint.
                if let Some(guard) = unsafe { v.inner_ref() } {
                    if idx_usize >= guard.len() {
                        stop!(Generic => "index out of bounds, index given: {:?}, length of vector: {:?}", i, guard.len());
                    }

                    Ok(guard[idx_usize].clone())
                } else {
                    let guard = &(unsafe { &(*v.inner.as_ptr()) }.lock());

                    let guard = &guard.value;

                    if idx_usize >= guard.len() {
                        stop!(Generic => "index out of bounds, index given: {:?}, length of vector: {:?}", i, guard.len());
                    }

                    Ok(guard[idx_usize].clone())
                }
            }

            SteelVal::VectorV(v) => {
                if idx_usize < v.len() {
                    Ok(v[idx_usize].clone())
                } else {
                    let e = format!(
                        "Index out of bounds - attempted to access index: {} with length: {}",
                        idx_usize,
                        v.len()
                    );
                    stop!(Generic => e);
                }
            }
            SteelVal::FlatVector(v) => {
                if idx_usize < v.len() {
                    Ok(v[idx_usize].clone())
                } else {
                    let e = format!(
                        "Index out of bounds - attempted to access index: {} with length: {}",
                        idx_usize,
                        v.len()
                    );
                    stop!(Generic => e);
                }
            }

            _ => stop!(TypeMismatch => format!(
                "vector-ref expected a vector and a number, found: {:?} and {:?}",
                &guard.thread.stack[vec_reg + offset], i )),
        }
    };

    match thunk() {
        Ok(v) => v,
        Err(e) => {
            guard.result = Some(Err(e));
            guard.is_native = false;

            SteelVal::Void
        }
    }
}

#[cross_platform_fn]
fn eq_reg_2(ctx: *mut VmCore, left: usize, right: usize) -> bool {
    let guard = unsafe { &mut *ctx };
    let offset = guard.get_offset();
    let l = &guard.thread.stack[left + offset];
    let r = &guard.thread.stack[right + offset];

    l.ptr_eq(r)
}

#[cross_platform_fn]
fn symbol_equal_no_drop(left: SteelString, right: SteelString) -> bool {
    let res = left == right;
    std::mem::forget(left);
    std::mem::forget(right);
    res
}

#[cross_platform_fn]
fn eq_reg_1(ctx: *mut VmCore, left: usize, right: SteelVal) -> bool {
    let guard = unsafe { &mut *ctx };
    let offset = guard.get_offset();
    let l = &guard.thread.stack[left + offset];
    l.ptr_eq(&right)
}

#[cross_platform_fn]
fn eq_value(left: SteelVal, right: SteelVal) -> bool {
    left.ptr_eq(&right)
}

// `eq?` on values the caller still owns.
#[cross_platform_fn]
fn eq_value_no_drop(left: SteelVal, right: SteelVal) -> bool {
    let res = left.ptr_eq(&right);
    std::mem::forget(left);
    std::mem::forget(right);
    res
}

#[cross_platform_fn]
fn vector_set_handler_stack(
    ctx: *mut VmCore,
    vec_reg: SteelVal,
    index: SteelVal,
    value: SteelVal,
) -> SteelVal {
    match steel_mut_vec_set(&[vec_reg, index, value]) {
        Ok(v) => v,
        Err(e) => {
            let guard = unsafe { &mut *ctx };
            guard.result = Some(Err(e));
            guard.is_native = false;

            SteelVal::Void
        }
    }
}

#[cross_platform_fn]
fn vector_push_handler_stack(ctx: *mut VmCore, vec_reg: SteelVal, value: SteelVal) -> SteelVal {
    match mut_vec_push(&[vec_reg, value]) {
        Ok(v) => v,
        Err(e) => {
            let guard = unsafe { &mut *ctx };
            guard.result = Some(Err(e));
            guard.is_native = false;

            SteelVal::Void
        }
    }
}

#[cross_platform_fn]
fn vector_set_handler_register_one(
    ctx: *mut VmCore,
    vec_reg: usize,
    index: SteelVal,
    value: SteelVal,
) -> SteelVal {
    let guard = unsafe { &mut *ctx };

    let offset = guard.get_offset();
    let vec = &guard.thread.stack[vec_reg + offset];

    match (vec, index) {
        (SteelVal::MutableVector(h), SteelVal::IntV(i)) => {
            match mut_vec_set(h, i as usize, value) {
                Ok(v) => v,
                Err(e) => {
                    guard.result = Some(Err(e));
                    guard.is_native = false;

                    SteelVal::Void
                }
            }
        }

        _ => {
            guard.result = Some(Err(
                throw!(TypeMismatch => "vector-set! expects a vector and an index, found: {} and {}")(
                ),
            ));
            guard.is_native = false;

            SteelVal::Void
        }
    }
}

#[cross_platform_fn]
fn list_contains_reg(ctx: *mut VmCore, reg: usize, lst: List<SteelVal>) -> bool {
    let guard = unsafe { &mut *ctx };
    let offset = guard.get_offset();
    let value = &guard.thread.stack[reg + offset];

    for item in lst.iter() {
        if value == item {
            return true;
        }
    }

    false
}

#[cross_platform_fn]
fn memq_value(ctx: *mut VmCore, value: SteelVal, lst: SteelVal) -> SteelVal {
    let guard = unsafe { &mut *ctx };
    match steel_memq(&[value, lst]) {
        Ok(v) => v,
        Err(e) => {
            guard.result = Some(Err(e));
            guard.is_native = false;
            SteelVal::Void
        }
    }
}

// The tail of a baked constant list after `n` elements, which the list still
// owns.
#[cross_platform_fn]
fn list_tail_constant(lst: List<SteelVal>, n: i64) -> SteelVal {
    let lst = core::mem::ManuallyDrop::new(lst);
    let mut list = List::clone(&lst);
    for _ in 0..n {
        list.cdr_mut();
    }
    SteelVal::ListV(list)
}

#[cross_platform_fn]
fn memq_unchecked_list(value: SteelVal, lst: List<SteelVal>) -> SteelVal {
    let mut list = lst.clone();
    std::mem::forget(lst);

    while let Some(first) = list.first() {
        if SteelVal::ptr_eq(first, &value) {
            return SteelVal::ListV(list);
        }

        list.cdr_mut();
    }

    SteelVal::BoolV(false)
}

#[cross_platform_fn]
fn list_contains_reg_constant(ctx: *mut VmCore, reg: usize, lst: List<SteelVal>) -> bool {
    let guard = unsafe { &mut *ctx };
    let offset = guard.get_offset();
    let value = &guard.thread.stack[reg + offset];

    for item in lst.iter() {
        if value == item {
            return true;
        }
    }

    std::mem::forget(lst);

    false
}

#[cross_platform_fn]
fn list_contains_value(ctx: *mut VmCore, value: SteelVal, lst: SteelVal) -> bool {
    let guard = unsafe { &mut *ctx };

    match steel_list_contains(&[value, lst]) {
        Ok(SteelVal::BoolV(b)) => b,
        Err(e) => {
            guard.result = Some(Err(e));
            guard.is_native = false;

            false
        }

        _ => unreachable!(),
    }
}

#[cross_platform_fn]
fn vector_set_handler_register_three(
    ctx: *mut VmCore,
    vec_reg: usize,
    index: usize,
    value: usize,
) -> SteelVal {
    let guard = unsafe { &mut *ctx };
    let offset = guard.get_offset();
    let vec = &guard.thread.stack[vec_reg + offset];
    let index = &guard.thread.stack[index + offset];
    let value = guard.thread.stack[value + offset].clone();

    match (vec, index) {
        (SteelVal::MutableVector(h), SteelVal::IntV(i)) => {
            match mut_vec_set(h, *i as usize, value) {
                Ok(v) => v,
                Err(e) => {
                    guard.result = Some(Err(e));
                    guard.is_native = false;

                    SteelVal::Void
                }
            }
        }

        _ => {
            guard.result = Some(Err(
                throw!(TypeMismatch => "vector-set! expects a vector and an index, found: {} and {}")(
                ),
            ));
            guard.is_native = false;

            SteelVal::Void
        }
    }
}

#[cross_platform_fn]
fn vector_set_handler_register_two(
    ctx: *mut VmCore,
    vec_reg: usize,
    index: usize,
    value: SteelVal,
) -> SteelVal {
    let guard = unsafe { &mut *ctx };

    let offset = guard.get_offset();
    let vec = &guard.thread.stack[vec_reg + offset];
    let index = &guard.thread.stack[index + offset];

    match (vec, index) {
        (SteelVal::MutableVector(h), SteelVal::IntV(i)) => {
            match mut_vec_set(h, *i as usize, value) {
                Ok(v) => v,
                Err(e) => {
                    guard.result = Some(Err(e));
                    guard.is_native = false;

                    SteelVal::Void
                }
            }
        }

        _ => {
            guard.result = Some(Err(
                throw!(TypeMismatch => "vector-set! expects a vector and an index, found: {} and {}")(
                ),
            ));
            guard.is_native = false;

            SteelVal::Void
        }
    }
}

#[cross_platform_fn]
fn box_handler_c(ctx: *mut VmCore, arg: SteelVal) -> SteelVal {
    let this = unsafe { &mut *ctx };

    let allocated_var = this.thread.heap.lock().allocate(
        arg,
        &this.thread.stack,
        this.thread.stack_frames.iter().map(|x| x.function.as_ref()),
        this.thread.global_env.roots(),
        &this.thread.thread_local_storage,
        &mut this.thread.synchronizer,
    );

    SteelVal::HeapAllocated(allocated_var)
}

#[cross_platform_fn]
fn unbox_handler_c(ctx: *mut VmCore, arg: SteelVal) -> SteelVal {
    let this = unsafe { &mut *ctx };

    if let SteelVal::HeapAllocated(h) = arg {
        h.get()
    } else {
        let err = || stop!(TypeMismatch => "unbox expected a boxed value, found: {}", arg);

        this.is_native = false;
        this.result = Some(err());
        SteelVal::Void
    }
}

#[cross_platform_fn]
fn setbox_handler_c(ctx: *mut VmCore, arg: SteelVal, value: SteelVal) -> SteelVal {
    let this = unsafe { &mut *ctx };

    if let SteelVal::HeapAllocated(h) = arg {
        h.set_and_return(value)
    } else {
        let err = || stop!(TypeMismatch => "unbox expected a boxed value, found: {}", arg);

        this.is_native = false;
        this.result = Some(err());
        SteelVal::Void
    }
}

// TODO: Lets optimize this?
#[cross_platform_fn]
fn cons_handler_value_register(ctx: *mut VmCore, arg: SteelVal, arg2: usize) {
    use crate::values::lists::Pair;

    let guard = unsafe { &mut *ctx };
    guard.ip += 2;

    let offset = guard.get_offset();
    let arg2 = &mut guard.thread.stack[offset + arg2];

    // This might be faster?
    match (arg, arg2) {
        (left, SteelVal::ListV(right)) => {
            // Leave it in place
            right.cons_mut(left);
        }
        // Silly, but this then gives us a special "pair" that is different
        // from a real bonafide list
        (left, right) => {
            let res = SteelVal::Pair(Gc::new(Pair::cons(left, right.clone())));
            *right = res;
        }
    }
}

#[cross_platform_fn]
fn cons_handler_register_register(ctx: *mut VmCore, arg: usize, arg2: usize) {
    use crate::values::lists::Pair;

    let guard = unsafe { &mut *ctx };
    guard.ip += 2;

    let offset = guard.get_offset();
    let arg1 = std::mem::replace(&mut guard.thread.stack[offset + arg], SteelVal::Void);
    let arg2 = &mut guard.thread.stack[offset + arg2];

    // This might be faster?
    match arg2 {
        SteelVal::ListV(right) => {
            // Leave it in place
            right.cons_mut(arg1);
        }
        // Silly, but this then gives us a special "pair" that is different
        // from a real bonafide list
        right => {
            let res = SteelVal::Pair(Gc::new(Pair::cons(arg1, right.clone())));
            *right = res;
        }
    }
}

#[cross_platform_fn]
fn cons_handler_value(ctx: *mut VmCore, mut arg: SteelVal, mut arg2: SteelVal) -> SteelVal {
    unsafe { &mut *ctx }.ip += 2;

    // The problem here, is that arg2 is expecting
    // a reference to the stack slot. If its _actually_
    // the last value, then we'll have a clone of it.
    //
    // Otherwise, we might have a problem. We also need
    // to be sure that we're not _reusing_ variables off
    // of the stack here, and that their destructors are
    // getting called after, if they exist.
    match cons(&mut arg, &mut arg2) {
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
fn cdr_handler_value(ctx: *mut VmCore, arg: SteelVal) -> SteelVal {
    unsafe { &mut *ctx }.ip += 2;
    let mut arg = arg;

    match cdr(&mut arg) {
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
fn cdr_handler_reg(ctx: *mut VmCore, arg: usize) -> SteelVal {
    let guard = unsafe { &mut *ctx };

    let offset = guard.get_offset();
    let arg = &guard.thread.stack[offset + arg];

    fn cdr_copy(arg: &SteelVal) -> Result<SteelVal> {
        match arg {
            SteelVal::ListV(l) => {
                if l.is_empty() {
                    stop!(Generic => "cdr expects a non empty list");
                }

                match l.cdr() {
                    Some(n) => Ok(SteelVal::ListV(n)),
                    None => Ok(SteelVal::ListV(List::new())),
                }
            }

            SteelVal::Pair(p) => Ok(p.cdr()),
            arg => {
                stop!(TypeMismatch => format!("cdr expects a list, found: {}", arg))
            }
        }
    }

    match cdr_copy(arg) {
        Ok(v) => v,
        Err(e) => {
            guard.result = Some(Err(e));
            guard.is_native = false;

            SteelVal::Void
        }
    }
}

#[cross_platform_fn]
fn cdr_handler_mut_reg(ctx: *mut VmCore, arg: usize) -> SteelVal {
    let guard = unsafe { &mut *ctx };

    let offset = guard.get_offset();
    let mut arg = &mut guard.thread.stack[offset + arg];

    match cdr(&mut arg) {
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
fn cdr_handler_reg_no_check(ctx: *mut VmCore, arg: usize) -> SteelVal {
    let guard = unsafe { &mut *ctx };

    let offset = guard.get_offset();
    let arg = &guard.thread.stack[offset + arg];

    fn cdr_copy(arg: &SteelVal) -> SteelVal {
        match arg {
            SteelVal::ListV(l) => match l.cdr() {
                Some(n) => SteelVal::ListV(n),
                None => SteelVal::ListV(List::new()),
            },

            _ => unsafe { unreachable_unchecked() },
        }
    }

    cdr_copy(&arg)
}

#[cross_platform_fn]
fn cdr_handler_mut_reg_no_check(ctx: *mut VmCore, arg: usize) {
    let guard = unsafe { &mut *ctx };

    let offset = guard.get_offset();
    let mut arg = &mut guard.thread.stack[offset + arg];

    unsafe { cdr_no_check_two(&mut arg) };
}

// TODO:
// This is tricky, since we need to remove a range from the stack, where the value on the stack
// could be genuinely there, versus on the native stack.
//
// We could have it so that we don't remove the last value?
// Or pass in the

helper_family! {
    ListHandlerDefinitions, mod list_handlers,
    names "make_list_" "",
    add_func fn(_ctx: *mut VmCore) -> SteelVal,
    arities [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16],
    |args| SteelVal::ListV(Vec::from(args).into())
}

#[cross_platform_fn]
fn list_handler_c(ctx: *mut VmCore<'_>, payload: usize) -> SteelVal {
    let ctx = unsafe { &mut *ctx };
    let last_index = ctx.thread.stack.len() - payload;

    let remaining = ctx.thread.stack.drain(last_index..).collect();

    SteelVal::ListV(remaining)
}

#[cross_platform_fn]
fn vec_handler_c(ctx: *mut VmCore<'_>, payload: usize) -> SteelVal {
    let ctx = unsafe { &mut *ctx };

    let len = payload / 2;
    let bytes = payload % 2 != 0;

    let args = ctx
        .thread
        .stack
        .drain(ctx.thread.stack.len() - len..)
        .collect::<Vec<_>>();

    let val = if bytes {
        let buffer: Vec<_> = args
            .into_iter()
            .flat_map(|val| {
                let int = val.int_or_else(|| "unexpected non integer");

                debug_assert!(int.is_ok());

                int.ok()
            })
            .flat_map(|int| {
                let byte = u8::try_from(int);

                debug_assert!(byte.is_ok());
                byte.ok()
            })
            .collect();

        SteelVal::ByteVector(crate::rvals::SteelByteVector::new(buffer))
    } else {
        SteelVal::VectorV(crate::rvals::SteelVector(Gc::new(args.into())))
    };

    val
}

helper_family! {
    CallStructConstructorsDefinitions, mod call_struct_constructor,
    names "call_struct_constructor_" "",
    add_func2 fn(type_descriptor: StructTypeDescriptor) -> SteelVal,
    arities [0 1 2 3 4 5 6 7 8],
    |args| SteelVal::CustomStruct(StructRef::from_parts(type_descriptor, args.into_iter()))
}

// `#%make-mutable-struct` in tail position, called directly from jitted code
// to skip the generic tail call handler's global lookup and `BuiltIn` calling
// convention. The first argument is the struct type descriptor; the rest are
// the fields. On an error the frame is handed back to the interpreter at
// `fallback_ip`, which `check_deopt` after the call looks for.
helper_family! {
    CallMutableStructConstructorsDefinitions, mod call_mutable_struct_constructor,
    names "call_mutable_struct_constructor_" "",
    add_func fn(ctx: *mut VmCore, fallback_ip: usize) -> SteelVal,
    arities [1 2 3 4 5 6 7 8],
    |args| {
        let guard = unsafe { &mut *ctx };
        let args: smallvec::SmallVec<[SteelVal; 8]> = args.into_iter().collect();
        match crate::steel_vm::primitives::make_mutable_struct_from_owned(guard, args) {
            Ok(v) => v,
            Err(e) => {
                guard.ip = fallback_ip;
                guard.result = Some(Err(e.set_span_if_none(guard.current_span())));
                guard.is_native = false;
                SteelVal::Void
            }
        }
    }
}

// `#%make-mutable-struct` with a descriptor the jit baked in, so the call
// only boxes the fields. It cannot fail.
helper_family! {
    CallMutableStructKnownDefinitions, mod call_mutable_struct_known,
    names "call_mutable_struct_known_" "",
    add_func fn(ctx: *mut VmCore, descriptor: usize) -> SteelVal,
    arities [0 1 2 3 4 5 6 7 8],
    |args| {
        let guard = unsafe { &mut *ctx };
        crate::steel_vm::primitives::make_mutable_struct_known(
            guard,
            crate::values::structs::StructTypeDescriptor::from_key(descriptor),
            args,
        )
    }
}

// `mutable-vector` in tail position, called directly from jitted code. It is a
// `BuiltIn`, so otherwise every `(vector a b c)` goes through the generic tail
// call handler and deopts to the interpreter.
//
// The allocation can collect, and the collector's roots are the vm stack plus
// these arguments. The caller checks that nothing else in the frame is live in
// tail position before emitting this.
helper_family! {
    CallMutableVectorConstructorsDefinitions, mod call_mutable_vector_constructor,
    names "call_mutable_vector_constructor_" "",
    add_func fn(ctx: *mut VmCore) -> SteelVal,
    arities [1 2 3 4 5 6 7 8],
    |args| {
        let guard = unsafe { &mut *ctx };
        guard.make_mutable_vector(args.into_iter().collect())
    }
}

// `cdr` in tail position. `cdr` is a `MutFunc`, which the tail call path has no
// inline case for, so `(loop ... (cdr x))` style tails would otherwise go through
// the generic handler into the interpreter. Unlike `cdr_handler_value` this
// leaves `ip` alone. In tail position the frame is returning, and the only
// reader of `ip` is the error path, which sets `fallback_ip` itself.
#[cross_platform_fn]
fn cdr_tail_value(ctx: *mut VmCore, fallback_ip: usize, arg: SteelVal) -> SteelVal {
    let mut arg = arg;

    match crate::primitives::lists::cdr(&mut arg) {
        Ok(v) => v,
        Err(e) => {
            let guard = unsafe { &mut *ctx };
            guard.ip = fallback_ip;
            guard.result = Some(Err(e.set_span_if_none(guard.current_span())));
            guard.is_native = false;
            SteelVal::Void
        }
    }
}

// `reverse` in tail position. Another `MutFunc`, common in tail position from
// the accumulate-then-reverse idiom.
#[cross_platform_fn]
fn reverse_tail_value(ctx: *mut VmCore, fallback_ip: usize, arg: SteelVal) -> SteelVal {
    let mut arg = arg;

    let out = match crate::primitives::lists::reverse(&mut arg) {
        Ok(v) => v,
        Err(e) => {
            let guard = unsafe { &mut *ctx };
            guard.ip = fallback_ip;
            guard.result = Some(Err(e.set_span_if_none(guard.current_span())));
            guard.is_native = false;
            SteelVal::Void
        }
    };

    out
}

// `(make-vector n)` / `(make-vector n fill)`, called directly from jitted code
// instead of through the generic handler. As with the other allocating
// builtins, the caller guarantees the vm stack and these arguments are the only
// roots it needs.
#[cross_platform_fn]
fn call_make_vector_1(ctx: *mut VmCore, fallback_ip: usize, size: SteelVal) -> SteelVal {
    make_vector_checked(ctx, fallback_ip, size, SteelVal::IntV(0))
}

#[cross_platform_fn]
fn call_make_vector_2(
    ctx: *mut VmCore,
    fallback_ip: usize,
    size: SteelVal,
    fill: SteelVal,
) -> SteelVal {
    make_vector_checked(ctx, fallback_ip, size, fill)
}

pub(super) fn make_vector_checked(
    ctx: *mut VmCore,
    fallback_ip: usize,
    size: SteelVal,
    fill: SteelVal,
) -> SteelVal {
    let guard = unsafe { &mut *ctx };

    match size {
        SteelVal::IntV(n) if n >= 0 => {
            let values = crate::primitives::vectors::filled_heap_vec(fill, n as usize);
            guard.make_mutable_vector(values)
        }
        other => {
            // Everything else, like a negative length or a non-integer, raises
            // with the primitive's own message.
            let args = [other, fill];
            match crate::primitives::vectors::make_vector(guard, &args) {
                Some(Err(e)) => {
                    guard.ip = fallback_ip;
                    guard.result = Some(Err(e.set_span_if_none(guard.current_span())));
                    guard.is_native = false;
                }
                _ => unreachable!("the success paths are handled above"),
            }
            SteelVal::Void
        }
    }
}

helper_family! {
    CallFlatVectorConstructorsDefinitions, mod call_flat_vector_constructor,
    names "call_flat_vector_constructor_" "",
    add_func2 fn() -> SteelVal,
    arities [1 2 3 4 5 6 7 8],
    |args| {
        let mut vec = steel_vec::Vec::with_capacity(args.len());
        for arg in args {
            vec.push(arg);
        }
        SteelVal::FlatVector(Gc::new(vec))
    }
}

#[cross_platform_fn]
fn if_handler_register(ctx: *mut VmCore, index: u64) -> bool {
    let ctx = unsafe { &mut *ctx };
    let offset = ctx.get_offset();
    ctx.thread.stack[index as usize + offset].is_truthy()
}

#[cross_platform_fn]
fn not_handler_raw_value(_ctx: *mut VmCore, value: SteelVal) -> SteelVal {
    SteelVal::BoolV(!value.is_truthy())
}
