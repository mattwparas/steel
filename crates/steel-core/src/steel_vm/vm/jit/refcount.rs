use super::*;

#[cross_platform_fn]
fn drop_value(ctx: *mut VmCore, arg: SteelVal) {
    unsafe { &mut *ctx }.ip += 1;
    drop(arg);
}

#[cross_platform_fn]
fn drop_box(arg: crate::values::closed::HeapRef<SteelVal>) {
    drop(arg);
}

#[cross_platform_fn]
fn drop_one(arg: SteelVal) {
    drop(arg);
}

#[cross_platform_fn]
fn clone_one(arg: SteelVal) {
    // Manually drop, means the destructor won't get called.
    let arg = ManuallyDrop::new(arg);

    // Clone it, don't call the destructor
    std::mem::forget((&*arg).clone());
}

#[cross_platform_fn]
fn drop_value_post_fast_decrement(arg: SteelVal) {
    use SteelVal::*;

    let mut arg = ManuallyDrop::new(arg);

    match &mut *arg {
        Closure(gc) => {
            // Fast decrement
            gc.0.fast_decrement_post_ref_count_dec();
        }

        VectorV(v) => {
            v.0 .0.fast_decrement_post_ref_count_dec();
        }

        StringV(s) => {
            s.0 .0.fast_decrement_post_ref_count_dec();
        }

        SymbolV(s) => {
            s.0 .0.fast_decrement_post_ref_count_dec();
        }

        Custom(gc) => {
            gc.0.fast_decrement_post_ref_count_dec();
        }

        HashMapV(hm) => {
            hm.0 .0.fast_decrement_post_ref_count_dec();
        }

        HashSetV(hs) => {
            hs.0 .0.fast_decrement_post_ref_count_dec();
        }

        CustomStruct(gc) => {
            gc.fast_decrement_post_ref_count_dec();
        }

        MutableStruct(gc) => {
            gc.fast_decrement_post_ref_count_dec();
        }

        PortV(gc) => {
            gc.port.0.fast_decrement_post_ref_count_dec();
        }

        IterV(gc) => {
            gc.0.fast_decrement_post_ref_count_dec();
        }

        ReducerV(r) => {
            r.0.fast_decrement_post_ref_count_dec();
        }

        StreamV(s) => {
            s.0.fast_decrement_post_ref_count_dec();
        }

        BoxedFunction(f) => {
            f.0.fast_decrement_post_ref_count_dec();
        }

        FutureFunc(f) => {
            f.fast_decrement_post_ref_count_dec();
        }

        FutureV(v) => {
            v.0.fast_decrement_post_ref_count_dec();
        }

        BoxedIterator(i) => {
            i.0.fast_decrement_post_ref_count_dec();
        }

        SyntaxObject(s) => {
            s.0.fast_decrement_post_ref_count_dec();
        }

        Reference(r) => {
            r.0.fast_decrement_post_ref_count_dec();
        }

        ListV(l) => {
            l.inner_ptr_mut().0.fast_decrement_post_ref_count_dec();
        }

        Pair(gc) => {
            gc.0.fast_decrement_post_ref_count_dec();
        }

        Boxed(gc) => {
            gc.0.fast_decrement_post_ref_count_dec();
        }

        BigNum(gc) => {
            gc.0.fast_decrement_post_ref_count_dec();
        }

        BigRational(gc) => {
            gc.0.fast_decrement_post_ref_count_dec();
        }

        Complex(gc) => {
            gc.0.fast_decrement_post_ref_count_dec();
        }

        ByteVector(bv) => {
            bv.vec.0.fast_decrement_post_ref_count_dec();
        }

        other => {
            panic!(
                "Calling fast decrement post ref count on a non pointer value: {}",
                other
            );
        }
    };
}

#[cross_platform_fn]
fn drop_value_post_fast_decrement_closure(arg: Gc<ByteCodeLambda>) {
    let mut arg = ManuallyDrop::new(arg);
    arg.0.fast_decrement_post_ref_count_dec();
}

#[cross_platform_fn]
fn increment_ref_count_slow_closure(arg: Gc<ByteCodeLambda>) {
    let mut arg = ManuallyDrop::new(arg);

    arg.0.raw_slow_increment();
}

#[cross_platform_fn]
fn drop_value_slow_decrement(arg: SteelVal) {
    use SteelVal::*;

    let mut arg = ManuallyDrop::new(arg);

    match &mut *arg {
        Closure(gc) => {
            // Fast decrement
            gc.0.raw_slow_decrement();
        }

        VectorV(v) => {
            v.0 .0.raw_slow_decrement();
        }

        StringV(s) => {
            s.0 .0.raw_slow_decrement();
        }

        SymbolV(s) => {
            s.0 .0.raw_slow_decrement();
        }

        Custom(gc) => {
            gc.0.raw_slow_decrement();
        }

        HashMapV(hm) => {
            hm.0 .0.raw_slow_decrement();
        }

        HashSetV(hs) => {
            hs.0 .0.raw_slow_decrement();
        }

        CustomStruct(gc) => {
            gc.raw_slow_decrement();
        }

        MutableStruct(gc) => {
            gc.raw_slow_decrement();
        }

        PortV(gc) => {
            gc.port.0.raw_slow_decrement();
        }

        IterV(gc) => {
            gc.0.raw_slow_decrement();
        }

        ReducerV(r) => {
            r.0.raw_slow_decrement();
        }

        StreamV(s) => {
            s.0.raw_slow_decrement();
        }

        BoxedFunction(f) => {
            f.0.raw_slow_decrement();
        }

        FutureFunc(f) => {
            f.raw_slow_decrement();
        }

        FutureV(v) => {
            v.0.raw_slow_decrement();
        }

        BoxedIterator(i) => {
            i.0.raw_slow_decrement();
        }

        SyntaxObject(s) => {
            s.0.raw_slow_decrement();
        }

        Reference(r) => {
            r.0.raw_slow_decrement();
        }

        ListV(l) => {
            l.inner_ptr_mut().0.raw_slow_decrement();
        }

        Pair(gc) => {
            gc.0.raw_slow_decrement();
        }

        Boxed(gc) => {
            gc.0.raw_slow_decrement();
        }

        BigNum(gc) => {
            gc.0.raw_slow_decrement();
        }

        BigRational(gc) => {
            gc.0.raw_slow_decrement();
        }

        Complex(gc) => {
            gc.0.raw_slow_decrement();
        }

        ByteVector(bv) => {
            bv.vec.0.raw_slow_decrement();
        }

        _ => {
            panic!("Calling fast decrement post ref count on a non pointer value");
        }
    };
}

#[cross_platform_fn]
fn drop_value_slow_decrement_closure(arg: Gc<ByteCodeLambda>) {
    let mut arg = ManuallyDrop::new(arg);
    arg.0.raw_slow_decrement();
}
