use crate::values::{
    closed::{HeapAllocated, HeapVec},
    lock::SpinLock,
    structs::{StructConstructorRefSpec, StructFunctionType},
};

use super::*;
use crate::values::structs::StructStorage;

// Byte offset from the pointer a HeapRef holds to the boxed value's lock.
const fn heap_box_lock_offset() -> i64 {
    steel_rc::weak::Weak::<SpinLock<HeapAllocated<SteelVal>>>::data_offset() as i64
}

// The header counters are loaded directly, so the width has to track the type.
// A mismatch reads both counters as a single value instead.
fn ref_count_type() -> Type {
    match steel_rc::weak::ref_count_width() {
        8 => types::I64,
        4 => types::I32,
        other => unreachable!("unsupported refcount width: {other}"),
    }
}

// Byte offset from the pointer a HeapRef holds to the steel_vec::Vec itself.
const fn heap_vec_offset() -> i32 {
    (steel_rc::weak::Weak::<SpinLock<HeapAllocated<HeapVec>>>::data_offset()
        + SpinLock::<HeapAllocated<HeapVec>>::data_offset()
        + core::mem::offset_of!(HeapAllocated<HeapVec>, value)) as i32
}

// A FlatVector carries the BiasedRc box pointer, and the steel_vec::Vec sits in
// its data. The elements are contiguous, so an element is at the buffer pointer
// plus a scaled index.
const fn flat_vec_offset() -> i32 {
    steel_rc::BiasedRc::<steel_vec::Vec<SteelVal>>::data_offset() as i32
}

const fn flat_vec_len_offset() -> i32 {
    flat_vec_offset() + steel_vec::Vec::<SteelVal>::len_offset() as i32
}

const fn flat_vec_buf_offset() -> i32 {
    flat_vec_offset() + steel_vec::Vec::<SteelVal>::buf_offset() as i32
}

// A bytevector is `Gc<SpinLock<steel_vec::Vec<u8>>>`, so the vec header is at
// the BiasedRc box pointer, plus the lock word. Unlike the vector paths the
// element is a plain `u8`, so there is no refcount to bump on a read and no old
// value to drop on a write.
const fn byte_vec_offset() -> i32 {
    (steel_rc::BiasedRc::<SpinLock<steel_vec::Vec<u8>>>::data_offset()
        + SpinLock::<steel_vec::Vec<u8>>::data_offset()) as i32
}

const fn byte_vec_len_offset() -> i32 {
    byte_vec_offset() + steel_vec::Vec::<u8>::len_offset() as i32
}

const fn byte_vec_buf_offset() -> i32 {
    byte_vec_offset() + steel_vec::Vec::<u8>::buf_offset() as i32
}

const fn byte_vec_lock_offset() -> i32 {
    steel_rc::BiasedRc::<SpinLock<steel_vec::Vec<u8>>>::data_offset() as i32
        + SpinLock::<steel_vec::Vec<u8>>::lock_offset() as i32
}

const fn heap_vec_len_offset() -> i32 {
    heap_vec_offset() + HeapVec::len_offset() as i32
}

const fn heap_vec_buf_offset() -> i32 {
    heap_vec_offset() + HeapVec::buf_offset() as i32
}

const fn heap_vec_cap_offset() -> i32 {
    heap_vec_offset() + HeapVec::capacity_offset() as i32
}

// Both arms of a two way branch start from the same abstract state.
// converging_if merges only `properties`, so the arms have to agree on the rest
// when they finish, and nothing else catches a mismatch.
// STEEL_JIT_BRANCH_CHECK=1 reports one.
fn branch_check_enabled() -> bool {
    crate::config::config().jit_branch_check
}

fn branch_state_fingerprint(ctx: &FunctionTranslator<'_>) -> Option<(usize, String, usize)> {
    if !branch_check_enabled() {
        return None;
    }

    Some((
        ctx.ip,
        format!("{:?}", ctx.shadow_stack),
        ctx.let_var_stack.len(),
    ))
}

fn report_branch_divergence(
    then_state: Option<(usize, String, usize)>,
    else_state: Option<(usize, String, usize)>,
) {
    let (Some(then_state), Some(else_state)) = (then_state, else_state) else {
        return;
    };

    if then_state == else_state {
        return;
    }

    eprintln!("BRANCH-DIVERGENCE");
    if then_state.0 != else_state.0 {
        eprintln!("  ip:    then={} else={}", then_state.0, else_state.0);
    }
    if then_state.2 != else_state.2 {
        eprintln!("  lets:  then={} else={}", then_state.2, else_state.2);
    }
    if then_state.1 != else_state.1 {
        eprintln!("  stack: then={}", then_state.1);
        eprintln!("         else={}", else_state.1);
    }
}

pub(super) struct BranchState {
    ip: usize,
    shadow_stack: Vec<MaybeStackValue>,
    let_var_stack: Vec<usize>,
    value_to_local_map: HashMap<Value, usize>,
    local_to_value_map: HashMap<usize, InferredType>,
    properties: PropertyMap,
}

impl<'a> FunctionTranslator<'a> {
    fn shadow_last_cloned(&self) -> MaybeStackValue {
        self.shadow_stack.last().unwrap().clone()
    }

    /// `char=?` of two chars is a compare of their code points. Anything else
    /// goes to the primitive, which raises for a non-char.
    pub(super) fn char_equals(&mut self, arity: usize) {
        let name = CallPrimitiveFixedDefinitions::arity_to_name(arity).unwrap();

        let args: Vec<Value> = self.split_off(arity).into_iter().map(|x| x.0).collect();
        let ip = self.ip;

        let checks: Vec<(Value, u8)> = args.iter().map(|v| (*v, SteelVal::CHAR_TAG)).collect();
        let all_chars = self.tags_are(&checks);

        let result = self.converging_if(
            all_chars,
            |ctx| {
                let points: Vec<Value> = args
                    .iter()
                    .map(|v| {
                        let payload = ctx.unbox_value(*v);
                        ctx.builder.ins().ireduce(types::I32, payload)
                    })
                    .collect();
                let mut all = ctx.builder.ins().iconst(types::I8, 1);
                for pair in points.windows(2) {
                    let eq = ctx.builder.ins().icmp(IntCC::Equal, pair[0], pair[1]);
                    all = ctx.builder.ins().band(all, eq);
                }
                let all = ctx.builder.ins().uextend(types::I64, all);
                ctx.encode_value(SteelVal::BOOL_TAG as i64, all)
            },
            |ctx| {
                let function = ctx.builder.ins().iconst(
                    ctx.module.target_config().pointer_type(),
                    crate::primitives::strings::char_equals_binop as *const () as i64,
                );
                let fallback_ip = ctx.i64_const(ip as i64);
                let mut call_args = vec![function, fallback_ip];
                call_args.extend(args.iter().copied());
                ctx.call_function_returns_value_args(name, &call_args)
            },
            types::I128,
        );

        self.push(result, InferredType::Bool);
        self.ip += 1;
        self.check_deopt();
    }

    pub(super) fn vector_set(&mut self) {
        use MaybeStackValue::*;

        let args = self
            .shadow_stack
            .get(self.shadow_stack.len() - 3..)
            .unwrap();

        match args {
            &[MutRegister(v) | Register(v), MutRegister(i) | Register(i), MutRegister(a) | Register(a)] =>
            {
                // Pop them off
                self.shadow_stack.pop();
                self.shadow_stack.pop();
                self.shadow_stack.pop();

                let vector_value = self.read_from_vm_stack(v);
                let index_value = self.read_from_vm_stack(i);
                let new_value = self.read_from_vm_stack(a);

                let fallback = move |ctx: &mut Self| {
                    let vector = ctx.register_index(v);
                    let index = ctx.register_index(i);
                    let value = ctx.register_index(a);

                    // Checked by the caller, after the result is pushed.
                    ctx.call_helper_unchecked(VECTOR_SET_REG_3, [vector, index, value])
                };

                let res = self.inline_mut_vector_set(
                    vector_value,
                    index_value,
                    new_value,
                    false,
                    false,
                    false,
                    fallback,
                );

                self.push(res, InferredType::Any);

                self.ip += 1;
                self.check_deopt();
            }

            &[MutRegister(v) | Register(v), MutRegister(i) | Register(i), Value(_)] => {
                let value = self.shadow_pop();

                // Pop them off
                self.shadow_stack.pop();
                self.shadow_stack.pop();

                let vector_value = self.read_from_vm_stack(v);
                let index_value = self.read_from_vm_stack(i);
                let new_value = value.0;

                let fallback = move |ctx: &mut Self| {
                    let vector = ctx.register_index(v);
                    let index = ctx.register_index(i);

                    // Checked by the caller, after the result is pushed.
                    ctx.call_helper_unchecked(VECTOR_SET_REG_2, [vector, index, new_value])
                };

                let res = self.inline_mut_vector_set(
                    vector_value,
                    index_value,
                    new_value,
                    false,
                    false,
                    true,
                    fallback,
                );

                self.push(res, InferredType::Any);
                self.ip += 1;
                self.check_deopt();
            }
            &[MutRegister(v) | Register(v), Value(_), Value(_)] => {
                let value = self.shadow_pop();
                let index = self.shadow_pop();
                self.shadow_stack.pop();

                let vector_value = self.read_from_vm_stack(v);
                let (index_value, new_value) = (index.0, value.0);

                let fallback = move |ctx: &mut Self| {
                    let vector = ctx.register_index(v);

                    // Checked by the caller, after the result is pushed.
                    ctx.call_helper_unchecked(VECTOR_SET_REG_1, [vector, index_value, new_value])
                };

                let res = self.inline_mut_vector_set(
                    vector_value,
                    index_value,
                    new_value,
                    false,
                    true,
                    true,
                    fallback,
                );

                self.push(res, InferredType::Any);
                self.ip += 1;
                self.check_deopt();
            }

            &[MutRegister(i) | Register(i), _, _] => {
                // The register keeps the vector alive, so it is read in place
                // and only the helper gets a clone. Read before the register is
                // marked as a vector below, which only holds once this succeeds.
                let vector = self.read_from_vm_stack(i);

                self.properties.add_property(
                    ValueOrRegister::Register(i),
                    Properties::InferredType(InferredType::MutableVector),
                );

                let rest = self
                    .split_off(2)
                    .into_iter()
                    .map(|x| x.0)
                    .collect::<Vec<_>>();
                self.shadow_stack.pop();
                let (index, value) = (rest[0], rest[1]);

                let fallback = move |ctx: &mut Self| {
                    ctx.clone_value(vector);
                    // Checked by the caller, after the result is pushed.
                    ctx.call_helper_unchecked(VECTOR_SET_ARGS, [vector, index, value])
                };

                let res =
                    self.inline_mut_vector_set(vector, index, value, false, true, true, fallback);

                self.push(res, InferredType::Any);
                self.ip += 1;
                self.check_deopt();
            }

            // A closed global, read from its slot. The global keeps the vector
            // alive, so the write needs no clone, and only the helper gets one.
            &[Constant(ConstantValue::Global(global)), _, _] => {
                let rest = self
                    .split_off(2)
                    .into_iter()
                    .map(|x| x.0)
                    .collect::<Vec<_>>();
                self.shadow_stack.pop();

                let vector = self.inline_lookup_global(global);
                let (index, value) = (rest[0], rest[1]);

                let fallback = move |ctx: &mut Self| {
                    ctx.clone_value(vector);
                    // Checked by the caller, after the result is pushed.
                    ctx.call_helper_unchecked(VECTOR_SET_ARGS, [vector, index, value])
                };

                let res =
                    self.inline_mut_vector_set(vector, index, value, false, true, true, fallback);

                self.push(res, InferredType::Any);
                self.ip += 1;
                self.check_deopt();
            }

            // A borrowed vector, e.g. a struct field. A slow path that clones it
            // owns it afterwards, and the helper takes that clone.
            &[Borrowed(borrowed), _, _] => {
                let rest = self
                    .split_off(2)
                    .into_iter()
                    .map(|x| x.0)
                    .collect::<Vec<_>>();
                self.shadow_stack.pop();

                let vector = borrowed.value;
                let (index, value) = (rest[0], rest[1]);

                let fallback = move |ctx: &mut Self| {
                    let vector = ctx.materialize(borrowed);
                    // Checked by the caller, after the result is pushed.
                    let res = ctx.call_helper_unchecked(VECTOR_SET_ARGS, [vector, index, value]);
                    let zero = ctx.builder.ins().iconst(types::I8, 0);
                    ctx.builder.def_var(borrowed.owned, zero);
                    res
                };

                let res =
                    self.inline_mut_vector_set(vector, index, value, false, true, true, fallback);
                self.release_borrowed(borrowed);

                self.push(res, InferredType::Any);
                self.ip += 1;
                self.check_deopt();
            }

            // Spill all by value
            _ => {
                let args = self
                    .split_off(3)
                    .into_iter()
                    .map(|x| x.0)
                    .collect::<Vec<_>>();

                let (vector, index, value) = (args[0], args[1], args[2]);

                let fallback = move |ctx: &mut Self| {
                    // Checked by the caller, after the result is pushed.
                    ctx.call_helper_unchecked(VECTOR_SET_ARGS, args.as_slice().try_into().unwrap())
                };

                let res =
                    self.inline_mut_vector_set(vector, index, value, true, true, true, fallback);

                self.push(res, InferredType::Any);
                self.ip += 1;
                self.check_deopt();
            }
        }
    }

    /// `vector-length` of a mutable or flat vector, read in place. A register or
    /// borrowed operand is left alone and an owned one released after.
    pub(super) fn vector_length(&mut self) {
        use MaybeStackValue::*;

        let operand = match self.shadow_stack.last().copied() {
            Some(Borrowed(b)) => {
                self.shadow_stack.pop();
                b
            }
            Some(Register(r)) => {
                self.shadow_stack.pop();
                let value = self.read_from_vm_stack(r);
                BorrowedValue {
                    value,
                    owned: self.owned_flag(false),
                }
            }
            _ => {
                let value = self.pop_boxed();
                BorrowedValue {
                    value,
                    owned: self.owned_flag(true),
                }
            }
        };
        let vector = operand.value;

        let fallback = move |ctx: &mut Self| {
            let vector = ctx.materialize(operand);
            let res = ctx.call_helper_unchecked(VECTOR_LENGTH_VALUE, [vector]);
            let zero = ctx.builder.ins().iconst(types::I8, 0);
            ctx.builder.def_var(operand.owned, zero);
            res
        };

        let is_mutable = self.is_type(vector, SteelVal::HEAP_REF_VECTOR_TAG);
        let res = self.converging_if(
            is_mutable,
            |ctx| {
                let ptr = ctx.unbox_value_to_pointer(vector);
                let len = ctx.heap_vec_len(ptr);
                ctx.encode_value(SteelVal::INT_TAG as _, len)
            },
            |ctx| {
                let is_flat = ctx.is_type(vector, SteelVal::FLAT_VECTOR_TAG);
                ctx.converging_if(
                    is_flat,
                    |ctx| {
                        let ptr = ctx.unbox_value_to_pointer(vector);
                        let len = ctx.builder.ins().load(
                            types::I64,
                            MemFlagsData::trusted(),
                            ptr,
                            flat_vec_len_offset(),
                        );
                        ctx.encode_value(SteelVal::INT_TAG as _, len)
                    },
                    fallback,
                    types::I128,
                )
            },
            types::I128,
        );
        self.release_borrowed(operand);

        self.push(res, InferredType::Any);
        self.ip += 1;
        self.check_deopt();
    }

    pub(super) fn eq(&mut self) {
        use MaybeStackValue::*;

        // Borrowed operands only go through the inline compare, which knows not
        // to release them.
        let operands = &self.shadow_stack[self.shadow_stack.len() - 2..];
        if operands.iter().any(|v| matches!(v, Borrowed(_))) {
            let all_inlinable = operands
                .iter()
                .all(|v| matches!(v, MutRegister(_) | Register(_) | Value(_) | Borrowed(_)));
            if all_inlinable && super::inline_eq_enabled() {
                self.inline_eq_operands();
                return;
            }
            self.materialize_borrowed();
        }

        let args = self
            .shadow_stack
            .get(self.shadow_stack.len() - 2..)
            .unwrap();

        match args {
            // Okay, so for constants, we can wait to actually reify them
            &[MutRegister(v) | Register(v), MutRegister(i) | Register(i)] => {
                let left = self.register_index(v);
                let right = self.register_index(i);

                // Pop them off
                self.shadow_stack.pop();
                self.shadow_stack.pop();

                let res = self.call_helper(EQ_P_REG_2, [left, right]);

                self.push(res, InferredType::UnboxedBool);
                self.ip += 1;
            }

            &[MutRegister(v) | Register(v), Value(StackValue {
                value: _,
                inferred_type: InferredType::Symbol,
                ..
            })] => {
                let right = self.shadow_pop();

                // Pop them off
                self.shadow_stack.pop();

                // If they're the same type, compare the bytes. Don't do a lookup.

                let left_value = self.read_from_vm_stack(v);
                let is_symbol = self.is_type(left_value, SteelVal::SYMBOL_TAG);

                let res = self.converging_if(
                    is_symbol,
                    |ctx| {
                        let lvalue = ctx.unbox_value_to_pointer(left_value);
                        let rvalue = ctx.unbox_value_to_pointer(right.0);

                        // Just compare the two values directly since we're looking
                        // at the pointers.
                        ctx.builder.ins().icmp(IntCC::Equal, lvalue, rvalue)
                    },
                    |ctx| ctx.builder.ins().iconst(types::I8, 0),
                    types::I8,
                );

                self.push(res, InferredType::UnboxedBool);
                self.ip += 1;
            }

            // A char, fixnum or boolean constant compares by value: the tag and
            // the payload. Only the payload's own width is compared, since the
            // rest of the word isn't written for a char or a boolean.
            &[MutRegister(v) | Register(v), Constant(
                c @ (ConstantValue::Char(_) | ConstantValue::Int(_) | ConstantValue::Bool(_)),
            )]
            | &[Constant(
                c @ (ConstantValue::Char(_) | ConstantValue::Int(_) | ConstantValue::Bool(_)),
            ), MutRegister(v) | Register(v)] => {
                self.shadow_stack.pop();
                self.shadow_stack.pop();

                let left_value = self.read_from_vm_stack(v);
                let payload = self.unbox_value(left_value);
                let (tag, same_payload) = match c {
                    ConstantValue::Char(ch) => {
                        let low = self.builder.ins().ireduce(types::I32, payload);
                        let eq = self.builder.ins().icmp_imm_u(IntCC::Equal, low, ch as i64);
                        (SteelVal::CHAR_TAG, eq)
                    }
                    ConstantValue::Int(n) => {
                        let eq = self
                            .builder
                            .ins()
                            .icmp_imm_s(IntCC::Equal, payload, n as i64);
                        (SteelVal::INT_TAG, eq)
                    }
                    ConstantValue::Bool(b) => {
                        let low = self.builder.ins().ireduce(types::I8, payload);
                        let eq = self.builder.ins().icmp_imm_u(IntCC::Equal, low, b as i64);
                        (SteelVal::BOOL_TAG, eq)
                    }
                    _ => unreachable!(),
                };
                let same_tag = self.is_type(left_value, tag);
                let res = self.builder.ins().band(same_tag, same_payload);

                self.push(res, InferredType::UnboxedBool);
                self.ip += 1;
            }

            &[MutRegister(v) | Register(v), Constant(ConstantValue::Symbol(i))]
            | &[Constant(ConstantValue::Symbol(i)), MutRegister(v) | Register(v)] => {
                let _ = self.shadow_stack.pop();

                // Pop them off
                self.shadow_stack.pop();

                let constant = self.constants.get(i);
                let SteelVal::SymbolV(s) = constant else {
                    panic!()
                };

                let as_ptr: i64 = unsafe { std::mem::transmute::<SteelString, _>(s.clone()) };

                // If they're the same type, compare the bytes. Don't do a lookup.
                let left_value = self.read_from_vm_stack(v);
                let is_symbol = self.is_type(left_value, SteelVal::SYMBOL_TAG);

                let lvalue = self.unbox_value_to_pointer(left_value);

                // Just compare the two values directly since we're looking
                // at the pointers.
                let res = self.builder.ins().icmp_imm_s(IntCC::Equal, lvalue, as_ptr);

                let res = self.builder.ins().band(is_symbol, res);

                self.push(res, InferredType::UnboxedBool);
                self.ip += 1;
            }

            &[MutRegister(_) | Register(_) | Value(_), MutRegister(_) | Register(_) | Value(_)]
                if super::inline_eq_enabled() =>
            {
                self.inline_eq_operands();
            }

            &[MutRegister(v) | Register(v), Value(_)] => {
                let left = self.register_index(v);
                let right = self.shadow_pop();

                // Pop them off
                self.shadow_stack.pop();

                let res = self.call_helper(EQ_P_REG_1, [left, right.0]);

                self.push(res, InferredType::UnboxedBool);
                self.ip += 1;
            }

            // Spill all by value
            _ => {
                let args = self
                    .split_off(2)
                    .into_iter()
                    .map(|x| x.0)
                    .collect::<Vec<_>>();

                let res = self.call_helper(EQ_P_ARGS, args.try_into().unwrap());

                self.push(res, InferredType::UnboxedBool);
                self.ip += 1;
            }
        }
    }

    /// `eq?` on two non-constant operands, without a call.
    ///
    /// Per tag, `SteelVal::ptr_eq` is a payload compare for pointers and
    /// fixnums, a compare at the real width for bools, chars and floats (the
    /// payload's unused bytes are not zeroed, and float `eq?` is `==`), always
    /// true for void, and always false for the tags it has no arm for. Only
    /// lists need the out-of-line check, since two handles can share storage.
    /// Owned operands are released inline afterwards.
    fn inline_eq_operands(&mut self) {
        // None: borrowed from a register. Some(flag): owned when the flag is 1.
        let operand = |ctx: &mut Self| -> (Value, Option<Value>) {
            match ctx.shadow_stack.pop().unwrap() {
                MaybeStackValue::MutRegister(i) | MaybeStackValue::Register(i) => {
                    (ctx.read_from_vm_stack(i), None)
                }
                MaybeStackValue::Borrowed(b) => {
                    let owned = ctx.builder.use_var(b.owned);
                    (b.value, Some(owned))
                }
                v @ MaybeStackValue::Value(_) => {
                    ctx.shadow_stack.push(v);
                    let MaybeStackValue::Value(v) = ctx.shadow_stack_pop().unwrap() else {
                        unreachable!()
                    };
                    ctx.value_to_local_map.remove(&v.value);
                    let one = ctx.builder.ins().iconst(types::I8, 1);
                    (v.as_steelval(ctx), Some(one))
                }
                MaybeStackValue::Constant(_) => unreachable!(),
            }
        };

        let (right, right_owned) = operand(self);
        let (left, left_owned) = operand(self);

        let left_tag = self.get_tag(left);
        let right_tag = self.get_tag(right);
        let same_tag = self.builder.ins().icmp(IntCC::Equal, left_tag, right_tag);

        let left_payload = self.unbox_value_to_pointer(left);
        let right_payload = self.unbox_value_to_pointer(right);

        // Tags whose ptr_eq is a payload compare.
        let payload_tags: u64 = [
            SteelVal::CLOSURE_TAG,
            SteelVal::INT_TAG,
            SteelVal::VECTOR_TAG,
            SteelVal::STRING_TAG,
            SteelVal::FUNCTION_POINTER_TAG,
            SteelVal::SYMBOL_TAG,
            SteelVal::CUSTOM_TAG,
            SteelVal::HASHMAP_TAG,
            SteelVal::HASHSET_TAG,
            SteelVal::STRUCT_TAG,
            SteelVal::PORT_TAG,
            SteelVal::ITER_TAG,
            SteelVal::REDUCER_TAG,
            SteelVal::ASYNC_FUNCTION_POINTER_TAG,
            SteelVal::BOXED_FUTURE_TAG,
            SteelVal::STREAM_TAG,
            SteelVal::BOXED_FUNCTION_TAG,
            SteelVal::CONTINUATION_TAG,
            SteelVal::PAIR_TAG,
            SteelVal::MUT_FUNCTION_TAG,
            SteelVal::BUILTIN_FUNCTION_TAG,
            SteelVal::HEAP_REF_VECTOR_TAG,
            SteelVal::BIG_NUM_TAG,
            SteelVal::BYTEVECTOR_TAG,
            SteelVal::FLAT_VECTOR_TAG,
        ]
        .iter()
        .fold(0, |mask, tag| mask | (1u64 << tag));

        let mask = self.i64_const(payload_tags as i64);
        let shifted = self.builder.ins().ushr(mask, left_tag);
        let is_payload_tag = self.builder.ins().band_imm_u(shifted, 1);
        let is_payload_tag = self.builder.ins().ireduce(types::I8, is_payload_tag);
        let same_payload = self
            .builder
            .ins()
            .icmp(IntCC::Equal, left_payload, right_payload);
        let by_payload = self.builder.ins().band(is_payload_tag, same_payload);

        let is_bool =
            self.builder
                .ins()
                .icmp_imm_s(IntCC::Equal, left_tag, SteelVal::BOOL_TAG as i64);
        let lb = self.builder.ins().ireduce(types::I8, left_payload);
        let rb = self.builder.ins().ireduce(types::I8, right_payload);
        let same_bool = self.builder.ins().icmp(IntCC::Equal, lb, rb);
        let by_bool = self.builder.ins().band(is_bool, same_bool);

        let is_char =
            self.builder
                .ins()
                .icmp_imm_s(IntCC::Equal, left_tag, SteelVal::CHAR_TAG as i64);
        let lc = self.builder.ins().ireduce(types::I32, left_payload);
        let rc = self.builder.ins().ireduce(types::I32, right_payload);
        let same_char = self.builder.ins().icmp(IntCC::Equal, lc, rc);
        let by_char = self.builder.ins().band(is_char, same_char);

        let is_float =
            self.builder
                .ins()
                .icmp_imm_s(IntCC::Equal, left_tag, SteelVal::FLOAT_TAG as i64);
        let lf = self.unbox_value_to_float(left);
        let rf = self.unbox_value_to_float(right);
        let same_float = self.builder.ins().fcmp(FloatCC::Equal, lf, rf);
        let by_float = self.builder.ins().band(is_float, same_float);

        let is_void =
            self.builder
                .ins()
                .icmp_imm_s(IntCC::Equal, left_tag, SteelVal::VOID_TAG as i64);

        let any = self.builder.ins().bor(by_payload, by_bool);
        let any = self.builder.ins().bor(any, by_char);
        let any = self.builder.ins().bor(any, by_float);
        let any = self.builder.ins().bor(any, is_void);
        let inline_result = self.builder.ins().band(same_tag, any);

        let is_list =
            self.builder
                .ins()
                .icmp_imm_s(IntCC::Equal, left_tag, SteelVal::LIST_TAG as i64);
        let needs_call = self.builder.ins().band(same_tag, is_list);

        let res = self.converging_if(
            needs_call,
            |ctx| ctx.call_helper(EQ_P_NO_DROP, [left, right]),
            |_| inline_result,
            types::I8,
        );

        for (value, owned) in [(left, left_owned), (right, right_owned)] {
            if let Some(owned) = owned {
                self.converging_if_no_value(owned, |ctx| ctx.drop_tagged_value(value), |_| {});
            }
        }

        self.push(res, InferredType::UnboxedBool);
        self.ip += 1;
    }

    // If its a constant with small amounts, we can test it via ptr eq fast path
    pub(super) fn list_contains(&mut self) {
        use MaybeStackValue::*;

        let args = self
            .shadow_stack
            .get(self.shadow_stack.len() - 2..)
            .unwrap();

        match args {
            // List contains the value
            &[Register(reg), Value(StackValue {
                value: _,
                inferred_type: InferredType::List,
                ..
            })] => {
                let left = self.register_index(reg);
                let right = self.shadow_pop().0;
                let list = self.unbox_value_to_pointer(right);
                self.shadow_pop();
                let res = self.call_helper(LIST_CONTAINS_REG, [left, list]);
                self.push(res, InferredType::UnboxedBool);

                self.ip += 1;
            }

            &[Register(reg), Constant(ConstantValue::List(l))] => {
                let left = self.register_index(reg);

                // Get the list without dropping it. It leaks, but it doesn't need a drop.
                let _ = self.shadow_pop().0;
                self.shadow_pop();

                let value = self.constants.get(l);
                let SteelVal::ListV(value) = value else {
                    panic!()
                };
                let as_ptr: usize = unsafe {
                    std::mem::transmute::<crate::values::lists::List<_>, _>(value.clone())
                };

                let value_ptr = self.i64_const(as_ptr as i64);

                let res = self.call_helper(LIST_CONTAINS_REG_CONSTANT, [left, value_ptr]);
                self.push(res, InferredType::UnboxedBool);

                self.ip += 1;
            }

            _ => {
                let args = self
                    .split_off(2)
                    .into_iter()
                    .map(|x| x.0)
                    .collect::<Vec<_>>();

                let res = self.call_helper_checked(LIST_CONTAINS_VALUE, args.try_into().unwrap());

                self.push(res, InferredType::UnboxedBool);

                self.ip += 1;
            }
        }
    }

    /// `pair?` on a boxed value, as an unboxed bool: a pair, or a list that
    /// isn't empty. Doesn't consume the value.
    fn pair_test(&mut self, value: Value) -> Value {
        let tag = self.get_tag(value);

        let mut switch = Switch::new();
        let pair_block = self.builder.create_block();
        let list_block = self.builder.create_block();
        let else_block = self.builder.create_block();
        let merge_block = self.builder.create_block();
        self.builder.append_block_param(merge_block, types::I8);

        switch.set_entry(SteelVal::LIST_TAG as _, list_block);
        switch.set_entry(SteelVal::PAIR_TAG as _, pair_block);

        switch.emit(&mut self.builder, tag, else_block);
        {
            // Is a pair
            self.builder.switch_to_block(pair_block);
            self.builder.seal_block(pair_block);
            let true_val = BlockArg::Value(self.builder.ins().iconst(types::I8, 1));
            self.builder.ins().jump(merge_block, &[true_val]);
        }

        {
            // Is a list: a pair unless it's empty
            self.builder.switch_to_block(list_block);
            self.builder.seal_block(list_block);

            let value = self.unbox_value_to_pointer(value);
            let length = self.list_cell_index(value);

            let not_empty =
                BlockArg::Value(self.builder.ins().icmp_imm_s(IntCC::NotEqual, length, 0));

            self.builder.ins().jump(merge_block, &[not_empty]);
        }

        {
            // Neither a list nor a pair
            self.builder.switch_to_block(else_block);
            self.builder.seal_block(else_block);
            let false_val = BlockArg::Value(self.builder.ins().iconst(types::I8, 0));
            self.builder.ins().jump(merge_block, &[false_val]);
        }

        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);
        self.builder.block_params(merge_block)[0]
    }

    pub(super) fn is_pair(&mut self) {
        use MaybeStackValue::*;

        let last = self.shadow_stack.last().unwrap().clone();

        match last {
            // TODO: Encode the result of the evaluation into the
            // branching. If this is used in the test position
            // of an if statement, we should encode the type checking
            // through.
            Value(stack_value) => {
                // TODO: Still need to invoke drop on this thing though!
                self.shadow_stack.pop();
                // If we've already inferrred this type as a pair,
                // we can skip the code generation for checking the tags
                // and actually invoking the function since we know
                // it will be a pair.
                // match stack_value.inferred_type {
                //     InferredType::List | InferredType::Pair | InferredType::ListOrPair if false => {
                //         let res = self.i64_const(1);

                //         let boolean =
                //             self.encode_value(discriminant(&SteelVal::BoolV(true)) as i64, res);

                //         // TODO: Also - we'll need to check the length of the list!
                //         // this should be able to be done inline as well, we just have to load
                //         // the index of the list.
                //         self.push(boolean, InferredType::Bool);
                //         self.ip += 1;

                //         self.drop_tagged_value(stack_value.value);

                //         return;
                //     }

                //     _ => {}
                // }

                let value = stack_value.as_steelval(self);
                let result = self.pair_test(value);
                self.push(result, InferredType::UnboxedBool);

                self.drop_tagged_value(value);

                self.ip += 1;
            }
            MutRegister(p) | Register(p) if !crate::config::config().jit_inline_pair_p => {
                let register = self.register_index(p);
                self.shadow_stack.pop();
                let res = self.call_helper(PAIR_P, [register]);
                self.push(res, InferredType::Bool);
                self.ip += 1;
            }
            MutRegister(p) | Register(p) => {
                self.shadow_stack.pop();

                // A register being walked as a cursor is a list, and is a pair
                // when the cursor isn't at the end. Reading the slot would
                // materialise it and undo the walk.
                let is_pair =
                    if let Some(cursor) = self.properties.cached_lookups.cursors.get(&p).copied() {
                        let index = self.builder.use_var(cursor.index);
                        self.builder.ins().icmp_imm_s(IntCC::NotEqual, index, 0)
                    } else {
                        // Only reads the slot; the register keeps its value.
                        let value = self.read_from_vm_stack(p);
                        self.pair_test(value)
                    };

                // So that the arm where it holds knows `car` / `cdr` succeed.
                self.properties.add_property(
                    ValueOrRegister::Value(is_pair),
                    Properties::CheckedPair(ValueOrRegister::Register(p)),
                );

                self.push(is_pair, InferredType::UnboxedBool);
                self.ip += 1;
            }

            // Depending on what the constant is, we can do this evaluation here
            // Constant(constant_value) => todo!(),
            _ => {
                // TODO: Check the inferred type here as well, maybe do unboxed bools
                let (value, _inferred_type) = self.shadow_pop();
                let res = self.call_helper(PAIR_P_VALUE, [value]);
                self.push(res, InferredType::Bool);
                self.ip += 1;
            }
        }
    }

    pub(super) fn is_list(&mut self) {
        use MaybeStackValue::*;

        let last = self.shadow_stack.last().unwrap().clone();

        match last {
            Borrowed(_) => unreachable!("borrowed values are materialized before this instruction"),
            Value(stack_value) => {
                self.shadow_stack.pop();
                let value = stack_value.as_steelval(self);
                let is_list = self.is_type(value, SteelVal::LIST_TAG);
                self.drop_tagged_value(value);
                self.push(is_list, InferredType::UnboxedBool);
                self.ip += 1;
            }
            MutRegister(p) | Register(p) => {
                // let register = self.register_index(p);

                match self.properties.get(&ValueOrRegister::Register(p)) {
                    // Elide the call entirely if its a non empty list. NOTE: We can't do this here.
                    // Some(Properties::NonEmptyList) if false => {
                    //     self.shadow_stack.pop();
                    //     let res = self.builder.ins().iconst(types::I8, 1);
                    //     self.push(res, InferredType::UnboxedBool);
                    //     self.ip += 1;
                    // }

                    // TODO: Figure out how to align the inferred types and properties
                    // so that there is some semblance of structure
                    Some(Properties::ProperList) => {
                        self.shadow_stack.pop();
                        let res = self.builder.ins().iconst(types::I8, 1);
                        self.push(res, InferredType::UnboxedBool);
                        self.ip += 1;
                    }

                    _ => {
                        self.shadow_stack.pop();
                        let value = self.read_from_vm_stack(p);
                        let is_list = self.is_type(value, SteelVal::LIST_TAG);

                        // Mark that this was a list
                        self.properties.add_property(
                            ValueOrRegister::Value(is_list),
                            Properties::CheckedList(ValueOrRegister::Register(p)),
                        );

                        self.push(is_list, InferredType::UnboxedBool);

                        // Now, we also can attach the

                        self.ip += 1;
                    }
                }
            }
            Constant(_constant_value) => {
                let (value, _inferred_type) = self.shadow_pop();
                let res = self.call_helper(LIST_P_VALUE, [value]);
                self.push(res, InferredType::Bool);
                self.ip += 1;
            }
        }
    }

    /// `memq` against a short constant list of symbols, chars, fixnums or
    /// booleans, as a chain of `eq?` compares. A hit returns the matching tail
    /// of the list, which is baked in like the list itself.
    pub(super) fn memq_constant_list(&mut self) -> Option<Value> {
        let Some(&MaybeStackValue::Constant(ConstantValue::List(l))) = self.shadow_stack.last()
        else {
            return None;
        };
        // Read through the constant map, so nothing is cloned or dropped here.
        // This can run on the background compile thread.
        let (elements, list_ptr) = self.constants.get_map(l, |constant| match constant {
            SteelVal::ListV(list) if memq_inline_elements(list) => {
                let elements: Vec<MemqElement> = list.iter().map(MemqElement::of).collect();
                let as_ptr: usize = unsafe {
                    std::mem::transmute::<crate::values::lists::List<_>, _>(jit_leak(list))
                };
                Some((elements, as_ptr))
            }
            _ => None,
        })?;

        // The list is a constant, so there is nothing to drop.
        self.shadow_stack.pop();
        let (value, owned) = match self.shadow_stack.last().copied() {
            Some(MaybeStackValue::Register(index)) => {
                self.shadow_stack.pop();
                (self.read_from_vm_stack(index), false)
            }
            _ => (self.shadow_pop().0, true),
        };

        let res = self.memq_chain(value, &elements, 0, list_ptr);
        if owned {
            self.drop_tagged_value(value);
        }
        Some(res)
    }

    fn memq_chain(
        &mut self,
        value: Value,
        elements: &[MemqElement],
        index: usize,
        list_ptr: usize,
    ) -> Value {
        let Some(element) = elements.get(index) else {
            return self.encode_false();
        };
        let matches = self.eq_constant_element(value, *element);
        let typ = self.int;
        self.converging_if(
            matches,
            |ctx| {
                let list = ctx.i64_const(list_ptr as i64);
                if index == 0 {
                    ctx.increment_ref_count_closure(list);
                    ctx.encode_value(SteelVal::LIST_TAG as _, list)
                } else {
                    let n = ctx.i64_const(index as i64);
                    ctx.call_helper(LIST_TAIL_CONSTANT, [list, n])
                }
            },
            |ctx| ctx.memq_chain(value, elements, index + 1, list_ptr),
            typ,
        )
    }

    /// `#%case-member?` against a constant list, as an unboxed bool. With
    /// enough chars or fixnums below 128 they are tested against a bitmap, so
    /// a clause costs the same however many datums it has. Up to 16 other
    /// elements are compared one by one.
    pub(super) fn case_member_constant(&mut self) -> Option<Value> {
        let Some(&MaybeStackValue::Constant(ConstantValue::List(l))) = self.shadow_stack.last()
        else {
            return None;
        };
        // Read through the constant map, so nothing is cloned or dropped here.
        let set = self.constants.get_map(l, |constant| match constant {
            SteelVal::ListV(list) => CaseSet::of(list),
            _ => None,
        })?;

        self.shadow_stack.pop();
        let (value, owned) = match self.shadow_stack.last().copied() {
            Some(MaybeStackValue::Register(index)) => {
                self.shadow_stack.pop();
                (self.read_from_vm_stack(index), false)
            }
            _ => (self.shadow_pop().0, true),
        };

        let mut hit = self.builder.ins().iconst(types::I8, 0);
        if set.chars != 0 {
            let in_set = self.in_ascii_bitmap(value, SteelVal::CHAR_TAG, set.chars, true);
            hit = self.builder.ins().bor(hit, in_set);
        }
        if set.ints != 0 {
            let in_set = self.in_ascii_bitmap(value, SteelVal::INT_TAG, set.ints, false);
            hit = self.builder.ins().bor(hit, in_set);
        }
        for element in set.others {
            let eq = self.eq_constant_element(value, element);
            hit = self.builder.ins().bor(hit, eq);
        }

        if owned {
            self.drop_tagged_value(value);
        }
        Some(hit)
    }

    /// Whether `value` has `tag` and a payload below 128 whose bit is set in
    /// `bitmap`, as an i8. A char's payload is only its low 32 bits.
    fn in_ascii_bitmap(&mut self, value: Value, tag: u8, bitmap: u128, char: bool) -> Value {
        let same_tag = self.is_type(value, tag);
        let payload = self.unbox_value(value);
        let payload = if char {
            let low = self.builder.ins().ireduce(types::I32, payload);
            self.builder.ins().uextend(types::I64, low)
        } else {
            payload
        };
        let in_range = self
            .builder
            .ins()
            .icmp_imm_u(IntCC::UnsignedLessThan, payload, 128);
        let low_half = self.i64_const(bitmap as u64 as i64);
        let high_half = self.i64_const((bitmap >> 64) as u64 as i64);
        let is_low = self
            .builder
            .ins()
            .icmp_imm_u(IntCC::UnsignedLessThan, payload, 64);
        let word = self.builder.ins().select(is_low, low_half, high_half);
        let shift = self.builder.ins().band_imm_u(payload, 63);
        let shifted = self.builder.ins().ushr(word, shift);
        let bit = self.builder.ins().band_imm_u(shifted, 1);
        let bit = self.builder.ins().ireduce(types::I8, bit);
        let found = self.builder.ins().band(same_tag, in_range);
        self.builder.ins().band(found, bit)
    }

    /// `eq?` of a value against one element of a constant list, as an i8.
    fn eq_constant_element(&mut self, value: Value, element: MemqElement) -> Value {
        let (tag, same_payload) = match element {
            MemqElement::Symbol(as_ptr) => {
                let pointer = self.unbox_value_to_pointer(value);
                let eq = self.builder.ins().icmp_imm_s(IntCC::Equal, pointer, as_ptr);
                (SteelVal::SYMBOL_TAG, eq)
            }
            MemqElement::Char(ch) => {
                let payload = self.unbox_value(value);
                let low = self.builder.ins().ireduce(types::I32, payload);
                let eq = self.builder.ins().icmp_imm_u(IntCC::Equal, low, ch as i64);
                (SteelVal::CHAR_TAG, eq)
            }
            MemqElement::Int(n) => {
                let payload = self.unbox_value(value);
                let eq = self
                    .builder
                    .ins()
                    .icmp_imm_s(IntCC::Equal, payload, n as i64);
                (SteelVal::INT_TAG, eq)
            }
            MemqElement::Bool(b) => {
                let payload = self.unbox_value(value);
                let low = self.builder.ins().ireduce(types::I8, payload);
                let eq = self.builder.ins().icmp_imm_u(IntCC::Equal, low, b as i64);
                (SteelVal::BOOL_TAG, eq)
            }
        };
        let same_tag = self.is_type(value, tag);
        self.builder.ins().band(same_tag, same_payload)
    }

    pub(super) fn memq(&mut self) -> Value {
        use MaybeStackValue::*;

        let args = self
            .shadow_stack
            .get(self.shadow_stack.len() - 2..)
            .unwrap();

        match args {
            &[Value(StackValue { value, .. }), Constant(ConstantValue::List(l))] => {
                // Constant list
                let _ = self.shadow_stack.pop();
                // Stack value:
                let _ = self.shadow_stack.pop();

                let SteelVal::ListV(list_value) = self.constants.get(l) else {
                    panic!()
                };

                let as_ptr: usize = unsafe {
                    std::mem::transmute::<crate::values::lists::List<_>, _>(list_value.clone())
                };

                let value_ptr = self.i64_const(as_ptr as i64);

                let res = self.call_helper(MEMQ_UNCHECKED_LIST, [value, value_ptr]);

                res
            }

            _ => {
                let args = self
                    .split_off(2)
                    .into_iter()
                    .map(|x| x.0)
                    .collect::<Vec<_>>();

                let res = self.call_helper_checked(MEMQ_VALUE, args.try_into().unwrap());

                // TODO: Put the ip in the check deopt!

                res
            }
        }
    }

    /// `char-whitespace?`. An ASCII char is answered inline, anything else, or
    /// a value that isn't a char, goes to the primitive.
    /// A type predicate that only looks at the tag: whether the value has one of
    /// `tags`, or, with `integral_float`, is a float with no fractional part, as
    /// `integer?` counts those. An unboxed bool.
    pub(super) fn tag_predicate(&mut self, tags: &[u8], integral_float: bool) -> Value {
        let (value, owned) = match self.shadow_stack.last().copied() {
            Some(MaybeStackValue::Register(r) | MaybeStackValue::MutRegister(r)) => {
                self.shadow_stack.pop();
                (self.read_from_vm_stack(r), false)
            }
            _ => (self.pop_boxed(), true),
        };

        let mut hit = self.builder.ins().iconst(types::I8, 0);
        for &tag in tags {
            let has_tag = self.is_type(value, tag);
            hit = self.builder.ins().bor(hit, has_tag);
        }
        if integral_float {
            // `x - trunc(x)` is zero exactly when `fract` is, and NaN for an
            // infinity or a NaN.
            let is_float = self.is_type(value, SteelVal::FLOAT_TAG);
            let float = self.unbox_value_to_float(value);
            let whole = self.builder.ins().trunc(float);
            let fraction = self.builder.ins().fsub(float, whole);
            let zero = self.builder.ins().f64const(0.0);
            let integral = self.builder.ins().fcmp(FloatCC::Equal, fraction, zero);
            let float_hit = self.builder.ins().band(is_float, integral);
            hit = self.builder.ins().bor(hit, float_hit);
        }

        if owned {
            self.drop_tagged_value(value);
        }
        hit
    }

    /// A char class predicate. An ascii char is classified inline, and
    /// anything else goes to the primitive, which raises for a non-char.
    pub(super) fn char_class(&mut self, class: CharClass) {
        let (value, owned) = match self.shadow_stack.last().copied() {
            Some(MaybeStackValue::Register(r) | MaybeStackValue::MutRegister(r)) => {
                self.shadow_stack.pop();
                (self.read_from_vm_stack(r), false)
            }
            _ => (self.pop_boxed(), true),
        };

        let is_char = self.is_type(value, SteelVal::CHAR_TAG);
        let payload = self.unbox_value(value);
        let c = self.builder.ins().ireduce(types::I32, payload);
        let ascii = self
            .builder
            .ins()
            .icmp_imm_u(IntCC::UnsignedLessThan, c, 128);
        let fast = self.builder.ins().band(is_char, ascii);

        let res = self.converging_if(
            fast,
            |ctx| {
                // `c - first <= last - first`, unsigned, for a range of chars.
                let in_range = |ctx: &mut Self, c: Value, first: char, last: char| {
                    let from = ctx.builder.ins().iadd_imm_s(c, -(first as i64));
                    ctx.builder.ins().icmp_imm_u(
                        IntCC::UnsignedLessThanOrEqual,
                        from,
                        last as i64 - first as i64,
                    )
                };
                let hit = match class {
                    // Space, or tab through carriage return.
                    CharClass::Whitespace => {
                        let space = ctx.builder.ins().icmp_imm_u(IntCC::Equal, c, 32);
                        let control = in_range(ctx, c, '\t', '\r');
                        ctx.builder.ins().bor(space, control)
                    }
                    // Setting bit 5 lowercases an ascii letter.
                    CharClass::Alphabetic => {
                        let lower = ctx.builder.ins().bor_imm(c, 0x20);
                        in_range(ctx, lower, 'a', 'z')
                    }
                    CharClass::Numeric => in_range(ctx, c, '0', '9'),
                    CharClass::UpperCase => in_range(ctx, c, 'A', 'Z'),
                    CharClass::LowerCase => in_range(ctx, c, 'a', 'z'),
                };
                let hit = ctx.builder.ins().uextend(types::I64, hit);
                ctx.encode_value(SteelVal::BOOL_TAG as i64, hit)
            },
            |ctx| {
                // The primitive takes the value.
                if !owned {
                    ctx.clone_value(value);
                }
                let class = ctx.i64_const(class as i64);
                ctx.call_helper_checked(CHAR_CLASS_VALUE, [value, class])
            },
            types::I128,
        );

        self.push(res, InferredType::Bool);
        self.ip += 1;
    }

    pub(super) fn is_string(&mut self) {
        use MaybeStackValue::*;

        let last = self.shadow_stack.last().unwrap().clone();

        match last {
            Borrowed(_) => unreachable!("borrowed values are materialized before this instruction"),
            Value(stack_value) => {
                self.shadow_stack.pop();
                let value = stack_value.as_steelval(self);
                let is_list = self.is_type(value, SteelVal::STRING_TAG);
                self.drop_tagged_value(value);
                self.push(is_list, InferredType::UnboxedBool);
                self.ip += 1;
            }
            MutRegister(p) | Register(p) => {
                let register = self.register_index(p);

                match self.local_to_value_map.get(&p) {
                    // Elide the call entirely if its a non empty list
                    Some(InferredType::String) => {
                        self.shadow_stack.pop();
                        let res = self.builder.ins().iconst(types::I8, 1);
                        self.push(res, InferredType::UnboxedBool);
                        self.ip += 1;
                    }
                    _ => {
                        self.shadow_stack.pop();
                        let res = self.call_helper(STRING_P, [register]);

                        self.push(res, InferredType::Bool);
                        self.ip += 1;
                    }
                }
            }
            Constant(_constant_value) => {
                let (value, _inferred_type) = self.shadow_pop();
                let res = self.call_helper(STRING_P_VALUE, [value]);
                self.push(res, InferredType::Bool);
                self.ip += 1;
            }
        }
    }

    pub(super) fn is_empty(&mut self) {
        use MaybeStackValue::*;

        let last = self.shadow_stack.last().unwrap().clone();

        match last {
            // TODO: Encode the result of the evaluation into the
            // branching. If this is used in the test position
            // of an if statement, we should encode the type checking
            // through.
            Value(stack_value) => {
                self.shadow_stack.pop();
                // If we've already inferrred this type as a pair,
                // we can skip the code generation for checking the tags
                // and actually invoking the function since we know
                // it will be a pair.
                // match stack_value.inferred_type {
                //     InferredType::List | InferredType::Pair | InferredType::ListOrPair if false => {
                //         let res = self.i64_const(1);
                //         let boolean =
                //             self.encode_value(discriminant(&SteelVal::BoolV(true)) as i64, res);
                //         // TODO: Also - we'll need to check the length of the list!
                //         // this should be able to be done inline as well, we just have to load
                //         // the index of the list.
                //         self.push(boolean, InferredType::Bool);
                //         self.ip += 1;
                //         self.drop_tagged_value(stack_value.value);
                //         return;
                //     }
                //     _ => {}
                // }

                let value = stack_value.as_steelval(self);
                let result = self.check_null_no_drop(value);
                self.push(result, InferredType::UnboxedBool);
                self.drop_tagged_value(value);

                self.ip += 1;
            }

            MutRegister(p) | Register(p) => {
                // A register being walked as a cursor is empty when the cursor
                // index is 0. Reading the slot would materialise it and undo
                // the walk.
                if let Some(cursor) = self.properties.cached_lookups.cursors.get(&p).copied() {
                    self.shadow_stack.pop();
                    let index = self.builder.use_var(cursor.index);
                    let is_empty = self.builder.ins().icmp_imm_s(IntCC::Equal, index, 0);
                    self.properties.add_property(
                        ValueOrRegister::Value(is_empty),
                        Properties::CheckedNull(ValueOrRegister::Register(p)),
                    );
                    self.push(is_empty, InferredType::UnboxedBool);
                    self.ip += 1;
                    return;
                }

                let register = self.register_index(p);
                self.shadow_stack.pop();
                let res = self.call_helper(EMPTY_P, [register]);
                self.push(res, InferredType::Bool);
                self.ip += 1;
            }

            // Depending on what the constant is, we can do this evaluation here
            // Constant(constant_value) => todo!(),
            _ => {
                // TODO: Check the inferred type here as well, maybe do unboxed bools
                let (value, _inferred_type) = self.shadow_pop();
                let res = self.call_helper(EMPTY_P_VALUE, [value]);
                self.push(res, InferredType::Bool);
                self.ip += 1;
            }
        }
    }

    /// Emit `then` when `test_condition` holds, then `merge` either way.
    pub(super) fn converging_if_no_else_no_value(
        &mut self,
        test_condition: Value,
        then: impl Fn(&mut Self),
        merge: impl Fn(&mut Self),
    ) {
        self.branch_then_merge(test_condition, then, merge, false)
    }

    /// `converging_if_no_else_no_value`, with `then` laid out as cold.
    pub(super) fn converging_if_no_else_no_value_cold(
        &mut self,
        test_condition: Value,
        then: impl Fn(&mut Self),
        merge: impl Fn(&mut Self),
    ) {
        self.branch_then_merge(test_condition, then, merge, true)
    }

    fn branch_then_merge(
        &mut self,
        test_condition: Value,
        then: impl Fn(&mut Self),
        merge: impl Fn(&mut Self),
        then_cold: bool,
    ) {
        if let Some(taken) = self.const_bool(test_condition) {
            if taken {
                then(self);
            }
            merge(self);
            return;
        }
        let then_block = self.builder.create_block();
        if then_cold {
            self.builder.set_cold_block(then_block);
        }

        let merge_block = self.builder.create_block();

        self.builder
            .ins()
            .brif(test_condition, then_block, &[], merge_block, &[]);

        self.builder.switch_to_block(then_block);
        self.builder.seal_block(then_block);

        let skipped = self.snapshot_branch_state();
        then(self);

        self.builder.ins().jump(merge_block, &[]);

        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);

        // The branch that skipped `then` reaches here with the state from before
        // it, so only what `then` left in agreement with that survives.
        self.cursor_bail |= self.properties.meet(&skipped.properties);
        self.meet_register_maps(&skipped.local_to_value_map, &skipped.value_to_local_map);

        merge(self);
    }

    // Both arms run from the same program state, so both have to be translated
    // from the same one. Emitting then mutates it in place, so roll it back before
    // the else arm. Otherwise else gets a stack / ip / property set that only the
    // then arm reaches.
    //
    // State after the merge is left as whatever else produced, same as
    // translate_if_else_value.
    fn snapshot_branch_state(&self) -> BranchState {
        BranchState {
            ip: self.ip,
            shadow_stack: self.shadow_stack.clone(),
            let_var_stack: self.let_var_stack.clone(),
            value_to_local_map: self.value_to_local_map.clone(),
            local_to_value_map: self.local_to_value_map.clone(),
            properties: self.properties.clone(),
        }
    }

    fn restore_branch_state(&mut self, state: BranchState) {
        let BranchState {
            ip,
            shadow_stack,
            let_var_stack,
            value_to_local_map,
            local_to_value_map,
            properties,
        } = state;

        self.ip = ip;
        self.shadow_stack = shadow_stack;
        self.let_var_stack = let_var_stack;
        self.value_to_local_map = value_to_local_map;
        self.local_to_value_map = local_to_value_map;
        self.properties = properties;
    }

    pub(super) fn converging_if_no_value(
        &mut self,
        test_condition: Value,
        then: impl Fn(&mut Self),
        else_thunk: impl Fn(&mut Self),
    ) {
        self.converging_branches(
            test_condition,
            |ctx| {
                then(ctx);
                None
            },
            |ctx| {
                else_thunk(ctx);
                None
            },
            None,
            false,
        );
    }

    pub(super) fn converging_if_no_value_else_cold(
        &mut self,
        test_condition: Value,
        then: impl Fn(&mut Self),
        else_thunk: impl Fn(&mut Self),
    ) {
        self.converging_branches(
            test_condition,
            |ctx| {
                then(ctx);
                None
            },
            |ctx| {
                else_thunk(ctx);
                None
            },
            None,
            true,
        );
    }

    pub(super) fn converging_if(
        &mut self,
        test_condition: Value,
        then: impl Fn(&mut Self) -> Value,
        else_thunk: impl Fn(&mut Self) -> Value,
        typ: Type,
    ) -> Value {
        self.converging_branches(
            test_condition,
            |ctx| Some(then(ctx)),
            |ctx| Some(else_thunk(ctx)),
            Some(typ),
            false,
        )
        .unwrap()
    }

    pub(super) fn converging_if_else_cold(
        &mut self,
        test_condition: Value,
        then: impl Fn(&mut Self) -> Value,
        else_thunk: impl Fn(&mut Self) -> Value,
        typ: Type,
    ) -> Value {
        self.converging_branches(
            test_condition,
            |ctx| Some(then(ctx)),
            |ctx| Some(else_thunk(ctx)),
            Some(typ),
            true,
        )
        .unwrap()
    }

    /// Emit `then` and `else_thunk` on the two sides of `test_condition` and
    /// join them. With a `result_type` both arms produce a value of that type,
    /// which is the result; without one they produce nothing.
    fn converging_branches(
        &mut self,
        test_condition: Value,
        then: impl Fn(&mut Self) -> Option<Value>,
        else_thunk: impl Fn(&mut Self) -> Option<Value>,
        result_type: Option<Type>,
        else_cold: bool,
    ) -> Option<Value> {
        if let Some(taken) = self.const_bool(test_condition) {
            return if taken { then(self) } else { else_thunk(self) };
        }
        let then_block = self.builder.create_block();
        let else_block = self.builder.create_block();
        let merge_block = self.builder.create_block();

        if else_cold {
            self.builder.set_cold_block(else_block);
        }

        if let Some(typ) = result_type {
            self.builder.append_block_param(merge_block, typ);
        }

        self.builder
            .ins()
            .brif(test_condition, then_block, &[], else_block, &[]);

        self.builder.switch_to_block(then_block);
        self.builder.seal_block(then_block);

        let entry = self.snapshot_branch_state();
        let then_result: Vec<BlockArg> = then(self).map(BlockArg::Value).into_iter().collect();
        let then_properties = self.properties.clone();
        let then_locals = self.local_to_value_map.clone();
        let then_values = self.value_to_local_map.clone();
        let then_state = result_type.and_then(|_| branch_state_fingerprint(self));

        self.builder.ins().jump(merge_block, &then_result);

        self.builder.switch_to_block(else_block);
        self.builder.seal_block(else_block);

        self.restore_branch_state(entry);
        let else_result: Vec<BlockArg> =
            else_thunk(self).map(BlockArg::Value).into_iter().collect();
        if result_type.is_some() {
            report_branch_divergence(then_state, branch_state_fingerprint(self));
        }
        self.builder.ins().jump(merge_block, &else_result);
        self.builder.switch_to_block(merge_block);

        // Only what both arms agree on survives the merge
        self.cursor_bail |= self.properties.meet(&then_properties);
        self.meet_register_maps(&then_locals, &then_values);

        result_type.map(|_| self.builder.block_params(merge_block)[0])
    }

    // TODO: Implement binop add directly. Assume naively that the values are probably integers,
    // and then implement the checked addition. We can branch for addition for ints / floats, anything else
    // should fall through to the generic case.
    pub(super) fn binop_add_value(
        &mut self,
        left: StackValue,
        right: StackValue,
    ) -> (Value, InferredType) {
        match (left, right) {
            // If they're both int 64, then we can continue comfortably. No error handling
            // necessary
            // (
            //     StackValue {
            //         inferred_type: InferredType::Int64,
            //         value: lv,
            //         ..
            //     },
            //     StackValue {
            //         inferred_type: InferredType::Int64,
            //         value: rv,
            //         ..
            //     },
            // ) => {
            //     todo!()
            // }
            (StackValue { value: lv, .. }, StackValue { value: rv, .. }) => {
                let both_int = self.tags_are(&[(lv, SteelVal::INT_TAG), (rv, SteelVal::INT_TAG)]);

                let sp = |ctx: &mut Self| ctx.call_helper_unchecked(ADD_BINOP, [lv, rv]);

                let typ = self.int;

                let res = self.converging_if(
                    both_int,
                    |ctx| {
                        // This is pointer sized, we're good to shrink it down
                        // to a pointer. both have to be int tag, otherwise we fall back to
                        // a function, and we'll return the usual
                        let left_payload = ctx.unbox_value_to_pointer(lv);
                        let right_payload = ctx.unbox_value_to_pointer(rv);

                        // Add the values, did they overflow?
                        let (added, overflow_flag) =
                            ctx.builder.ins().sadd_overflow(left_payload, right_payload);

                        ctx.converging_if(
                            overflow_flag,
                            sp,
                            |ctx| {
                                // Happy path, return the boxed integer value.
                                ctx.encode_value(SteelVal::INT_TAG as _, added)
                            },
                            typ,
                        )
                    },
                    |ctx| {
                        let res = sp(ctx);
                        ctx.check_deopt();
                        res
                    },
                    typ,
                );

                (res, InferredType::Number)
            }
        }
    }

    pub(super) fn binop_add_value_both(
        &mut self,
        left: Value,
        right: Value,
    ) -> (Value, InferredType) {
        let both_int = self.tags_are(&[(left, SteelVal::INT_TAG), (right, SteelVal::INT_TAG)]);

        let sp = |ctx: &mut Self| {
            let args = [left, right];
            let result = ctx.call_helper_checked(ADD_BINOP, args);

            result
        };

        let typ = self.int;

        let res = self.converging_if(
            both_int,
            |ctx| {
                // This is pointer sized, we're good to shrink it down
                // to a pointer. both have to be int tag, otherwise we fall back to
                // a function, and we'll return the usual
                let left_payload = ctx.unbox_value_to_pointer(left);
                let right_payload = ctx.unbox_value_to_pointer(right);

                // Add the values, did they overflow?
                let (added, overflow_flag) =
                    ctx.builder.ins().sadd_overflow(left_payload, right_payload);

                ctx.converging_if(
                    overflow_flag,
                    sp,
                    |ctx| {
                        // Happy path, return the boxed integer value.
                        ctx.encode_value(SteelVal::INT_TAG as _, added)
                    },
                    typ,
                )
            },
            |ctx| ctx.float_add_or(left, right, sp),
            typ,
        );

        (res, InferredType::Number)
    }

    /// Adds two floats inline, and calls `slow` for anything else.
    fn float_add_or(
        &mut self,
        left: Value,
        right: Value,
        slow: impl Fn(&mut Self) -> Value,
    ) -> Value {
        self.float_binop_or(OpCode::ADD, left, right, slow)
    }

    /// `+` or `-` of two floats inline, and `slow` for anything else.
    pub(super) fn float_binop_or(
        &mut self,
        op: OpCode,
        left: Value,
        right: Value,
        slow: impl Fn(&mut Self) -> Value,
    ) -> Value {
        if !super::float_inline_enabled() {
            return slow(self);
        }
        let both_float =
            self.tags_are(&[(left, SteelVal::FLOAT_TAG), (right, SteelVal::FLOAT_TAG)]);
        let typ = self.int;
        self.converging_if(
            both_float,
            |ctx| {
                let l = ctx.unbox_value_to_float(left);
                let r = ctx.unbox_value_to_float(right);
                let res = match op {
                    OpCode::SUB => ctx.builder.ins().fsub(l, r),
                    _ => ctx.builder.ins().fadd(l, r),
                };
                ctx.encode_float_value(res)
            },
            slow,
            typ,
        )
    }

    pub(super) fn binop_add_value_register(
        &mut self,
        left: usize,
        right: usize,
    ) -> (Value, InferredType) {
        let lv = self.read_from_vm_stack(left);
        let rv = self.read_from_vm_stack(right);

        let both_int = self.tags_are(&[(lv, SteelVal::INT_TAG), (rv, SteelVal::INT_TAG)]);

        let sp = |ctx: &mut Self| {
            let register_r = ctx.i64_const(right as i64);
            let register_l = ctx.i64_const(left as i64);

            let args = [register_l, register_r];
            let result = ctx.call_helper_checked(ADD_BINOP_REG_2, args);

            // The helper reports a type error by flagging the vm, so the call
            // has to be checked before the jitted code continues.

            result
        };

        let typ = self.int;

        let res = self.converging_if(
            both_int,
            |ctx| {
                // This is pointer sized, we're good to shrink it down
                // to a pointer. both have to be int tag, otherwise we fall back to
                // a function, and we'll return the usual
                let left_payload = ctx.unbox_value_to_pointer(lv);
                let right_payload = ctx.unbox_value_to_pointer(rv);

                // Add the values, did they overflow?
                let (added, overflow_flag) =
                    ctx.builder.ins().sadd_overflow(left_payload, right_payload);

                ctx.converging_if(
                    overflow_flag,
                    sp,
                    |ctx| {
                        // Happy path, return the boxed integer value.
                        ctx.encode_value(SteelVal::INT_TAG as _, added)
                    },
                    typ,
                )
            },
            |ctx| {
                ctx.float_add_or(lv, rv, |ctx| {
                    let res = sp(ctx);
                    ctx.check_deopt();
                    res
                })
            },
            typ,
        );

        (res, InferredType::Number)
    }

    // pub(super) fn reverse(&mut self) {
    //     use MaybeStackValue::*;
    // }

    pub(super) fn is_symbol(&mut self) {
        use MaybeStackValue::*;

        let last = self.shadow_stack.last().unwrap().clone();

        match last {
            Borrowed(_) => unreachable!("borrowed values are materialized before this instruction"),
            Value(stack_value) => {
                self.shadow_stack.pop();
                let value = stack_value.as_steelval(self);
                let is_symbol = self.is_type(value, SteelVal::SYMBOL_TAG);
                self.drop_tagged_value(value);
                self.push(is_symbol, InferredType::UnboxedBool);
                self.ip += 1;
            }
            MutRegister(p) | Register(p) => {
                let register = self.register_index(p);

                match self.local_to_value_map.get(&p) {
                    // Elide the call entirely if its a non empty list
                    Some(InferredType::Symbol) => {
                        self.shadow_stack.pop();
                        let res = self.builder.ins().iconst(types::I8, 1);
                        self.push(res, InferredType::UnboxedBool);
                        self.ip += 1;
                    }
                    _ => {
                        self.shadow_stack.pop();
                        let res = self.call_helper(SYMBOL_P, [register]);

                        self.push(res, InferredType::Bool);
                        self.ip += 1;
                    }
                }
            }
            // TODO: We should probably elide this somehow?
            Constant(_constant_value) => {
                let (value, _inferred_type) = self.shadow_pop();
                let res = self.call_helper(SYMBOL_P_VALUE, [value]);
                self.push(res, InferredType::Bool);
                self.ip += 1;
            }
        }
    }

    pub(super) fn is_void(&mut self) {
        use MaybeStackValue::*;

        let last = self.shadow_last_cloned();

        match last {
            Borrowed(_) => unreachable!("borrowed values are materialized before this instruction"),
            Value(stack_value) => {
                self.shadow_stack.pop();
                let value = stack_value.as_steelval(self);
                let is_void = self.is_type(value, SteelVal::VOID_TAG);
                self.drop_tagged_value(value);
                self.push(is_void, InferredType::UnboxedBool);
                self.ip += 1;
            }

            MutRegister(p) | Register(p) => {
                let register = self.register_index(p);

                let maybe_typ = self.local_to_value_map.get(&p);

                match (
                    self.properties.get(&ValueOrRegister::Register(p)),
                    maybe_typ,
                ) {
                    // Elide the call entirely if its a non empty list, or if
                    // we know that the value is something else at this point based
                    // on the inferred type.
                    (
                        Some(
                            Properties::NonEmptyListOrPair
                            | Properties::ProperNonEmptyList
                            | Properties::ProperList,
                        ),
                        _,
                    ) => {
                        self.shadow_stack.pop();
                        let res = self.builder.ins().iconst(types::I8, 0);
                        self.push(res, InferredType::UnboxedBool);
                        self.ip += 1;
                    }

                    (_, Some(t)) if *t != InferredType::Void => {
                        self.shadow_stack.pop();
                        let res = self.builder.ins().iconst(types::I8, 0);
                        self.push(res, InferredType::UnboxedBool);
                        self.ip += 1;
                    }

                    (_, Some(t)) if *t == InferredType::Void => {
                        self.shadow_stack.pop();
                        let res = self.builder.ins().iconst(types::I8, 1);
                        self.push(res, InferredType::UnboxedBool);
                        self.ip += 1;
                    }

                    _ => {
                        self.shadow_stack.pop();
                        let res = self.call_helper(VOID_P, [register]);
                        self.push(res, InferredType::Bool);
                        self.ip += 1;
                    }
                }
            }
            Constant(_constant_value) => {
                let (value, _inferred_type) = self.shadow_pop();
                let res = self.call_helper(VOID_P_VALUE, [value]);
                self.push(res, InferredType::Bool);
                self.ip += 1;
            }
        }
    }

    // Given a value, unbox it. This assumes this is coming from a register, so
    // we don't clone it in the happy path case where it is in fact a box.
    pub(super) fn unbox_value_checked_register(
        &mut self,
        value: Value,
        should_drop: bool,
    ) -> Value {
        // TODO: Check that this is actually a box!
        let is_box = self.is_type(value, SteelVal::HEAP_REF_VALUE_TAG);
        let typ = self.int;

        self.converging_if(
            is_box,
            |ctx| {
                // Lets upgrade the pointer first. Otherwise, we're doomed since the
                // value could not be around during the process. We can probably
                // relax this constraint because we won't be in a safepoint during unboxing
                // now. But, considering we could be in an embedded environment, we could
                // have some rogue values around.
                let ptr = ctx.unbox_value_to_pointer(value);

                let strong_count =
                    ctx.builder
                        .ins()
                        .atomic_load(ref_count_type(), MemFlagsData::trusted(), ptr);

                let is_one = ctx.builder.ins().icmp_imm_s(IntCC::Equal, strong_count, 1);
                let offset = heap_box_lock_offset();

                let data = ctx.converging_if(
                    is_one,
                    |ctx| {
                        let lock_pointer = ctx.builder.ins().iadd_imm_s(ptr, offset);

                        let data = ctx.builder.ins().load(
                            types::I128,
                            MemFlagsData::trusted(),
                            lock_pointer,
                            SpinLock::<SteelVal>::data_offset() as i32,
                        );

                        ctx.clone_value(data);

                        data
                    },
                    |ctx| {
                        let lock_pointer = ctx.builder.ins().iadd_imm_s(ptr, offset);

                        let data = ctx.with_spinlock(lock_pointer, |ctx| {
                            let data = ctx.builder.ins().load(
                                types::I128,
                                MemFlagsData::trusted(),
                                lock_pointer,
                                SpinLock::<SteelVal>::data_offset() as i32,
                            );

                            ctx.clone_value(data);

                            data
                        });

                        data
                    },
                    typ,
                );

                // The data lives at an offset of 16 from the pointer

                if should_drop {
                    ctx.drop_heap_box(ptr);
                }

                data
            },
            |ctx| {
                // Slow path, check deopt too
                ctx.clone_value(value);
                let res = ctx.call_helper_checked(UNBOX_HANDLER, [value]);
                res
            },
            typ,
        )
    }

    /// `unbox_value_checked_register` for a box borrowed from something that
    /// stays alive, leaving the contents borrowed too where it can. On the
    /// unshared fast path nothing is cloned and `owned` is set to 0. The locked
    /// and slow paths clone and set it to 1.
    pub(super) fn unbox_value_borrowed(&mut self, value: Value, owned: Variable) -> Value {
        let is_box = self.is_type(value, SteelVal::HEAP_REF_VALUE_TAG);
        let typ = self.int;

        self.converging_if(
            is_box,
            |ctx| {
                let ptr = ctx.unbox_value_to_pointer(value);

                let strong_count =
                    ctx.builder
                        .ins()
                        .atomic_load(ref_count_type(), MemFlagsData::trusted(), ptr);

                let is_one = ctx.builder.ins().icmp_imm_s(IntCC::Equal, strong_count, 1);
                let offset = heap_box_lock_offset();

                ctx.converging_if(
                    is_one,
                    |ctx| {
                        let lock_pointer = ctx.builder.ins().iadd_imm_s(ptr, offset);
                        let data = ctx.builder.ins().load(
                            types::I128,
                            MemFlagsData::trusted(),
                            lock_pointer,
                            SpinLock::<SteelVal>::data_offset() as i32,
                        );
                        let zero = ctx.builder.ins().iconst(types::I8, 0);
                        ctx.builder.def_var(owned, zero);
                        data
                    },
                    |ctx| {
                        let lock_pointer = ctx.builder.ins().iadd_imm_s(ptr, offset);
                        let data = ctx.with_spinlock(lock_pointer, |ctx| {
                            let data = ctx.builder.ins().load(
                                types::I128,
                                MemFlagsData::trusted(),
                                lock_pointer,
                                SpinLock::<SteelVal>::data_offset() as i32,
                            );
                            ctx.clone_value(data);
                            data
                        });
                        let one = ctx.builder.ins().iconst(types::I8, 1);
                        ctx.builder.def_var(owned, one);
                        data
                    },
                    typ,
                )
            },
            |ctx| {
                ctx.clone_value(value);
                let res = ctx.call_helper_checked(UNBOX_HANDLER, [value]);
                let one = ctx.builder.ins().iconst(types::I8, 1);
                ctx.builder.def_var(owned, one);
                res
            },
            typ,
        )
    }

    /// `(vector-ref (unbox boxed) index)` without cloning the vector out of the
    /// box. The element is read with the box held the way `#%unbox` holds it,
    /// so a `set-box!` can't drop the vector part way through. Anything the
    /// inline read doesn't take clones the contents and calls the helper once
    /// the box is let go, since the helper can raise.
    pub(super) fn box_contents_vector_ref(&mut self, boxed: Value, index: Value) -> Value {
        let is_box = self.is_type(boxed, SteelVal::HEAP_REF_VALUE_TAG);
        let typ = self.int;

        // Set when `res` below is the owned contents, left for the helper.
        let slow = self.builder.declare_var(types::I8);
        let no = self.builder.ins().iconst(types::I8, 0);
        self.builder.def_var(slow, no);

        let res = self.converging_if(
            is_box,
            |ctx| {
                let ptr = ctx.unbox_value_to_pointer(boxed);

                let strong_count =
                    ctx.builder
                        .ins()
                        .atomic_load(ref_count_type(), MemFlagsData::trusted(), ptr);

                let is_one = ctx.builder.ins().icmp_imm_s(IntCC::Equal, strong_count, 1);
                let lock_pointer = ctx.builder.ins().iadd_imm_s(ptr, heap_box_lock_offset());

                let read = |ctx: &mut Self| {
                    let vector = ctx.builder.ins().load(
                        types::I128,
                        MemFlagsData::trusted(),
                        lock_pointer,
                        SpinLock::<SteelVal>::data_offset() as i32,
                    );

                    ctx.inline_vector_ref(vector, index, false, |ctx| {
                        ctx.clone_value(vector);
                        let yes = ctx.builder.ins().iconst(types::I8, 1);
                        ctx.builder.def_var(slow, yes);
                        vector
                    })
                };

                // One copy of the read, with the lock around it only when the
                // box is shared.
                let lock = ctx.builder.create_block();
                let locked = ctx.builder.create_block();
                ctx.builder.set_cold_block(lock);
                ctx.builder.ins().brif(is_one, locked, &[], lock, &[]);
                ctx.builder.switch_to_block(lock);
                ctx.builder.seal_block(lock);
                emit_spinlock_inline(&mut ctx.builder, lock_pointer);
                ctx.builder.ins().jump(locked, &[]);
                ctx.builder.switch_to_block(locked);
                ctx.builder.seal_block(locked);

                let res = read(ctx);

                let unlock = ctx.builder.create_block();
                let done = ctx.builder.create_block();
                ctx.builder.set_cold_block(unlock);
                ctx.builder.ins().brif(is_one, done, &[], unlock, &[]);
                ctx.builder.switch_to_block(unlock);
                ctx.builder.seal_block(unlock);
                emit_spinlock_unlock_inline(&mut ctx.builder, lock_pointer);
                ctx.builder.ins().jump(done, &[]);
                ctx.builder.switch_to_block(done);
                ctx.builder.seal_block(done);

                res
            },
            |ctx| {
                // Not a box, so this raises.
                ctx.clone_value(boxed);
                let res = ctx.call_helper_checked(UNBOX_HANDLER, [boxed]);
                let yes = ctx.builder.ins().iconst(types::I8, 1);
                ctx.builder.def_var(slow, yes);
                res
            },
            typ,
        );

        let slow = self.builder.use_var(slow);

        self.converging_if_else_cold(
            slow,
            |ctx| ctx.call_helper_checked(VECTOR_REF_VALUE, [res, index]),
            |_| res,
            typ,
        )
    }

    /// Inline `#%set-box!`: store into the box and yield the value that was
    /// there, which is what `HeapRef::set_and_return` does.
    ///
    /// Same tag check and strong count probe as `unbox_value_checked_register`
    /// to decide whether the spinlock is needed. The operation is a
    /// `mem::replace`, so the old value is moved out and the new one moved in
    /// with no `clone_value` on either side. The caller owns the old value, as
    /// with the out-of-line handler.
    pub(super) fn set_box_value_checked_register(
        &mut self,
        boxed: Value,
        new_value: Value,
        should_drop: bool,
    ) -> Value {
        let is_box = self.is_type(boxed, SteelVal::HEAP_REF_VALUE_TAG);
        let typ = self.int;

        self.converging_if(
            is_box,
            |ctx| {
                let ptr = ctx.unbox_value_to_pointer(boxed);

                let strong_count =
                    ctx.builder
                        .ins()
                        .atomic_load(ref_count_type(), MemFlagsData::trusted(), ptr);

                let is_one = ctx.builder.ins().icmp_imm_s(IntCC::Equal, strong_count, 1);
                let offset = heap_box_lock_offset();

                let old = ctx.converging_if(
                    is_one,
                    |ctx| {
                        let lock_pointer = ctx.builder.ins().iadd_imm_s(ptr, offset);
                        let data_offset = SpinLock::<SteelVal>::data_offset() as i32;

                        let old = ctx.builder.ins().load(
                            types::I128,
                            MemFlagsData::trusted(),
                            lock_pointer,
                            data_offset,
                        );
                        ctx.builder.ins().store(
                            MemFlagsData::trusted(),
                            new_value,
                            lock_pointer,
                            data_offset,
                        );

                        old
                    },
                    |ctx| {
                        let lock_pointer = ctx.builder.ins().iadd_imm_s(ptr, offset);
                        let data_offset = SpinLock::<SteelVal>::data_offset() as i32;

                        ctx.with_spinlock(lock_pointer, |ctx| {
                            let old = ctx.builder.ins().load(
                                types::I128,
                                MemFlagsData::trusted(),
                                lock_pointer,
                                data_offset,
                            );
                            ctx.builder.ins().store(
                                MemFlagsData::trusted(),
                                new_value,
                                lock_pointer,
                                data_offset,
                            );

                            old
                        })
                    },
                    typ,
                );

                if should_drop {
                    ctx.drop_heap_box(ptr);
                }

                old
            },
            |ctx| {
                // Not a box: let the existing handler raise, with the operands
                // in the shape it expects. It takes the box, so a borrowed one
                // is cloned for it.
                if !should_drop {
                    ctx.clone_value(boxed);
                }
                ctx.call_helper_checked(SET_BOX_HANDLER, [boxed, new_value])
            },
            typ,
        )
    }

    // TODO: Replace the spin lock with an actual mutex implementation,
    // eventually.
    pub(super) fn with_spinlock<O>(
        &mut self,
        lock_pointer: Value,
        thunk: impl FnOnce(&mut Self) -> O,
    ) -> O {
        emit_spinlock_inline(&mut self.builder, lock_pointer);
        let res = thunk(self);
        emit_spinlock_unlock_inline(&mut self.builder, lock_pointer);

        res
    }

    fn heap_vec_len(&mut self, ptr: Value) -> Value {
        self.builder.ins().load(
            types::I64,
            MemFlagsData::trusted(),
            ptr,
            heap_vec_len_offset(),
        )
    }

    fn heap_vec_cap(&mut self, ptr: Value) -> Value {
        self.builder.ins().load(
            types::I64,
            MemFlagsData::trusted(),
            ptr,
            heap_vec_cap_offset(),
        )
    }

    fn heap_vec_element_address(&mut self, ptr: Value, index: Value) -> Value {
        let buf = self.builder.ins().load(
            types::I64,
            MemFlagsData::trusted(),
            ptr,
            heap_vec_buf_offset(),
        );

        debug_assert_eq!(std::mem::size_of::<SteelVal>(), 16);
        let byte_offset = self.builder.ins().ishl_imm_u(index, 4);
        self.builder.ins().iadd(buf, byte_offset)
    }

    // Hands back the old value. The caller bounds checks, and drops outside
    // whatever lock it holds.
    fn heap_vec_replace(&mut self, ptr: Value, index: Value, value: Value, owned: bool) -> Value {
        let slot = self.heap_vec_element_address(ptr, index);
        let old = self
            .builder
            .ins()
            .load(types::I128, MemFlagsData::trusted(), slot, 0);
        if !owned {
            self.clone_value(value);
        }
        self.builder
            .ins()
            .store(MemFlagsData::trusted(), value, slot, 0);
        old
    }

    fn heap_vec_lock_pointer(&mut self, ptr: Value) -> Value {
        let offset = (steel_rc::weak::Weak::<SpinLock<HeapAllocated<HeapVec>>>::data_offset()
            + SpinLock::<HeapAllocated<HeapVec>>::lock_offset()) as i64;
        self.builder.ins().iadd_imm_s(ptr, offset)
    }

    // Nothing else can reach the value at a strong count of one.
    fn with_heap_vec_lock(&mut self, ptr: Value, body: impl Fn(&mut Self) -> Value) -> Value {
        let strong = self
            .builder
            .ins()
            .atomic_load(ref_count_type(), MemFlagsData::trusted(), ptr);
        let is_one = self.builder.ins().icmp_imm_s(IntCC::Equal, strong, 1);

        self.converging_if(
            is_one,
            |ctx| body(ctx),
            |ctx| {
                let lock = ctx.heap_vec_lock_pointer(ptr);
                ctx.with_spinlock(lock, |ctx| body(ctx))
            },
            types::I128,
        )
    }

    // vector-set! with no call: check the tags, bounds check, then store.
    // Ownership is per argument because the shapes mix the two. A register
    // argument is a borrow, while a spilled one is ours to move in or drop.
    // In (vector-set! v i (vector-ref x i)), v and i are registers and the
    // value is not.
    pub(super) fn inline_mut_vector_set(
        &mut self,
        vector: Value,
        index: Value,
        value: Value,
        vector_owned: bool,
        index_owned: bool,
        value_owned: bool,
        fallback: impl Fn(&mut Self) -> Value,
    ) -> Value {
        let both = self.tags_are(&[
            (vector, SteelVal::HEAP_REF_VECTOR_TAG),
            (index, SteelVal::INT_TAG),
        ]);

        let body = |ctx: &mut Self| {
            let ptr = ctx.unbox_value_to_pointer(vector);
            let idx = ctx.unbox_value_to_pointer(index);
            let len = ctx.heap_vec_len(ptr);

            // Unsigned, so a negative index fails the same comparison
            let in_bounds = ctx.builder.ins().icmp(IntCC::UnsignedLessThan, idx, len);

            ctx.converging_if_else_cold(
                in_bounds,
                |ctx| {
                    let old = ctx.with_heap_vec_lock(ptr, |ctx| {
                        ctx.heap_vec_replace(ptr, idx, value, value_owned)
                    });

                    // Drop outside the lock, since a destructor runs arbitrary code
                    ctx.drop_tagged_value(old);

                    if vector_owned {
                        ctx.drop_tagged_value(vector);
                    }

                    if index_owned {
                        ctx.drop_tagged_value(index);
                    }

                    ctx.encode_void()
                },
                |ctx| fallback(ctx),
                types::I128,
            )
        };

        self.converging_if_else_cold(both, body, |ctx| fallback(ctx), types::I128)
    }

    // vector-ref with no call, for either representation. Flat is checked first
    // since its read is cheaper, and a mutable vector falls through to its own
    // inline path before the handler.
    ///
    /// `vector_owned` releases the vector after the inline read. The fallback
    /// is responsible for it otherwise.
    pub(super) fn inline_vector_ref(
        &mut self,
        vector: Value,
        index: Value,
        vector_owned: bool,
        fallback: impl Fn(&mut Self) -> Value,
    ) -> Value {
        self.inline_flat_vector_ref(vector, index, vector_owned, |ctx| {
            ctx.inline_mut_vector_ref(vector, index, vector_owned, &fallback)
        })
    }

    // vector-ref on a mutable vector with no call: check the tags, bounds check,
    // then load under the lock. A clone only bumps a refcount, so unlike a drop it
    // is safe to run while the lock is held.
    pub(super) fn inline_mut_vector_ref(
        &mut self,
        vector: Value,
        index: Value,
        vector_owned: bool,
        fallback: impl Fn(&mut Self) -> Value,
    ) -> Value {
        let both = self.tags_are(&[
            (vector, SteelVal::HEAP_REF_VECTOR_TAG),
            (index, SteelVal::INT_TAG),
        ]);

        self.converging_if_else_cold(
            both,
            |ctx| {
                let ptr = ctx.unbox_value_to_pointer(vector);
                let idx = ctx.unbox_value_to_pointer(index);
                let len = ctx.heap_vec_len(ptr);

                // Unsigned, so a negative index fails the same comparison
                let in_bounds = ctx.builder.ins().icmp(IntCC::UnsignedLessThan, idx, len);

                ctx.converging_if_else_cold(
                    in_bounds,
                    |ctx| {
                        let value = ctx.with_heap_vec_lock(ptr, |ctx| {
                            let slot = ctx.heap_vec_element_address(ptr, idx);

                            let value = ctx.builder.ins().load(
                                types::I128,
                                MemFlagsData::trusted(),
                                slot,
                                0,
                            );

                            ctx.clone_value(value);
                            value
                        });

                        // Outside the lock, since a destructor runs arbitrary code
                        if vector_owned {
                            ctx.drop_tagged_value(vector);
                        }

                        value
                    },
                    |ctx| fallback(ctx),
                    types::I128,
                )
            },
            |ctx| fallback(ctx),
            types::I128,
        )
    }

    /// Whether nothing but this thread can be looking at the bytevector, in
    /// which case the payload can be touched without taking the lock.
    ///
    /// Biased reference counting splits the count in two. The owner thread's
    /// half is a plain `Cell`, and every other thread's acquire lands in the
    /// shared atomic half. So a zero shared word means every live reference
    /// belongs to the owner, and since the caller holds one, the caller is the
    /// owner. The whole word is compared with zero, including the flag bits, so
    /// this also rules out `FLAG_MERGED` / `FLAG_QUEUED`, after which the
    /// biased half is meaningless.
    ///
    /// `strict` also requires the owner to hold one reference. That is not
    /// needed for soundness, since several references on one thread cannot
    /// race. It is there for comparison.
    fn byte_vec_unshared(&mut self, ptr: Value, strict: bool) -> Value {
        let shared_ptr = self
            .builder
            .ins()
            .iadd_imm_s(ptr, steel_rc::shared_offset() as i64);
        let shared =
            self.builder
                .ins()
                .atomic_load(types::I32, MemFlagsData::trusted(), shared_ptr);
        let unshared = self.builder.ins().icmp_imm_s(IntCC::Equal, shared, 0);

        if !strict {
            return unshared;
        }

        let biased = self.builder.ins().load(
            types::I32,
            MemFlagsData::trusted(),
            ptr,
            steel_rc::biased_offset() as i32,
        );
        let count = self
            .builder
            .ins()
            .ushr_imm_u(biased, steel_rc::biased_counter_shift() as i64);
        let only_one = self.builder.ins().icmp_imm_s(IntCC::Equal, count, 1);

        self.builder.ins().band(unshared, only_one)
    }

    /// Run `body` against the bytevector's payload, taking the spin lock only
    /// when another thread could be holding a reference. Mirrors
    /// `with_heap_vec_lock`.
    fn with_byte_vec_lock(&mut self, ptr: Value, body: impl Fn(&mut Self) -> Value) -> Value {
        let unshared = self.byte_vec_unshared(ptr, byte_vec_strict_unshared());

        self.converging_if(
            unshared,
            |ctx| body(ctx),
            |ctx| {
                let lock_pointer = ctx
                    .builder
                    .ins()
                    .iadd_imm_s(ptr, byte_vec_lock_offset() as i64);
                ctx.with_spinlock(lock_pointer, |ctx| body(ctx))
            },
            types::I64,
        )
    }

    /// `(bytes-ref bv i)` with no call.
    ///
    /// The element is a `u8`, so there is no refcount to bump, only a `uload8`
    /// and a retag as a fixnum.
    ///
    /// The locked region hands back a single value because the fallback must
    /// not run inside it. The spin lock is not reentrant and the primitive the
    /// fallback calls takes the same lock. `-1` is the out-of-bounds sentinel,
    /// which a real byte can never be.
    pub(super) fn inline_bytes_ref(
        &mut self,
        bytevector: Value,
        index: Value,
        owned: bool,
        fallback: impl Fn(&mut Self) -> Value,
    ) -> Value {
        let both = self.tags_are(&[
            (bytevector, SteelVal::BYTEVECTOR_TAG),
            (index, SteelVal::INT_TAG),
        ]);

        self.converging_if_else_cold(
            both,
            |ctx| {
                let ptr = ctx.unbox_value_to_pointer(bytevector);
                let idx = ctx.unbox_value_to_pointer(index);

                let byte = ctx.with_byte_vec_lock(ptr, |ctx| {
                    let len = ctx.builder.ins().load(
                        types::I64,
                        MemFlagsData::trusted(),
                        ptr,
                        byte_vec_len_offset(),
                    );

                    // Unsigned, so a negative index fails the same comparison
                    let in_bounds = ctx.builder.ins().icmp(IntCC::UnsignedLessThan, idx, len);

                    ctx.converging_if(
                        in_bounds,
                        |ctx| {
                            let buf = ctx.builder.ins().load(
                                types::I64,
                                MemFlagsData::trusted(),
                                ptr,
                                byte_vec_buf_offset(),
                            );
                            let slot = ctx.builder.ins().iadd(buf, idx);
                            ctx.builder
                                .ins()
                                .uload8(types::I64, MemFlagsData::trusted(), slot, 0)
                        },
                        |ctx| ctx.i64_const(-1),
                        types::I64,
                    )
                });

                let in_bounds =
                    ctx.builder
                        .ins()
                        .icmp_imm_s(IntCC::SignedGreaterThanOrEqual, byte, 0);

                ctx.converging_if_else_cold(
                    in_bounds,
                    |ctx| {
                        // Only one of this arm and the fallback runs, and the
                        // fallback hands the value to a helper that consumes it,
                        // so this is the only release needed.
                        if owned {
                            ctx.drop_tagged_value(bytevector);
                        }
                        ctx.encode_value(discriminant(&SteelVal::IntV(0)) as i64, byte)
                    },
                    |ctx| fallback(ctx),
                    types::I128,
                )
            },
            |ctx| fallback(ctx),
            types::I128,
        )
    }

    /// `(bytes-set! bv i b)` with no call. The slot is a byte, so there is no
    /// old value to drop, only a bounds check and an `istore8`. Returns 1 from
    /// the locked region when the store happened.
    pub(super) fn inline_bytes_set(
        &mut self,
        bytevector: Value,
        index: Value,
        byte: Value,
        owned: bool,
        fallback: impl Fn(&mut Self) -> Value,
    ) -> Value {
        let tags_ok = self.tags_are(&[
            (bytevector, SteelVal::BYTEVECTOR_TAG),
            (index, SteelVal::INT_TAG),
            (byte, SteelVal::INT_TAG),
        ]);

        self.converging_if_else_cold(
            tags_ok,
            |ctx| {
                let ptr = ctx.unbox_value_to_pointer(bytevector);
                let idx = ctx.unbox_value_to_pointer(index);
                let val = ctx.unbox_value_to_pointer(byte);

                // Unsigned, so a negative byte is a very large unsigned one
                // and fails this the same way 256 does.
                let byte_in_range =
                    ctx.builder
                        .ins()
                        .icmp_imm_u(IntCC::UnsignedLessThanOrEqual, val, 255);

                let stored = ctx.with_byte_vec_lock(ptr, |ctx| {
                    let len = ctx.builder.ins().load(
                        types::I64,
                        MemFlagsData::trusted(),
                        ptr,
                        byte_vec_len_offset(),
                    );

                    let in_bounds = ctx.builder.ins().icmp(IntCC::UnsignedLessThan, idx, len);
                    let ok = ctx.builder.ins().band(in_bounds, byte_in_range);

                    ctx.converging_if(
                        ok,
                        |ctx| {
                            let buf = ctx.builder.ins().load(
                                types::I64,
                                MemFlagsData::trusted(),
                                ptr,
                                byte_vec_buf_offset(),
                            );
                            let slot = ctx.builder.ins().iadd(buf, idx);
                            ctx.builder
                                .ins()
                                .istore8(MemFlagsData::trusted(), val, slot, 0);
                            ctx.i64_const(1)
                        },
                        |ctx| ctx.i64_const(0),
                        types::I64,
                    )
                });

                let ok = ctx.builder.ins().icmp_imm_s(IntCC::Equal, stored, 1);

                ctx.converging_if_else_cold(
                    ok,
                    |ctx| {
                        if owned {
                            ctx.drop_tagged_value(bytevector);
                        }
                        ctx.encode_void()
                    },
                    |ctx| fallback(ctx),
                    types::I128,
                )
            },
            |ctx| fallback(ctx),
            types::I128,
        )
    }

    // vector-ref on a flat vector with no call: check the tags, bounds check, then
    // load. The element is cloned because the vector keeps its own copy.
    pub(super) fn inline_flat_vector_ref(
        &mut self,
        vector: Value,
        index: Value,
        vector_owned: bool,
        fallback: impl Fn(&mut Self) -> Value,
    ) -> Value {
        let both = self.tags_are(&[
            (vector, SteelVal::FLAT_VECTOR_TAG),
            (index, SteelVal::INT_TAG),
        ]);

        self.converging_if_else_cold(
            both,
            |ctx| {
                let ptr = ctx.unbox_value_to_pointer(vector);
                let idx = ctx.unbox_value_to_pointer(index);

                let len = ctx.builder.ins().load(
                    types::I64,
                    MemFlagsData::trusted(),
                    ptr,
                    flat_vec_len_offset(),
                );

                // Unsigned, so a negative index fails the same comparison
                let in_bounds = ctx.builder.ins().icmp(IntCC::UnsignedLessThan, idx, len);

                ctx.converging_if_else_cold(
                    in_bounds,
                    |ctx| {
                        let buf = ctx.builder.ins().load(
                            types::I64,
                            MemFlagsData::trusted(),
                            ptr,
                            flat_vec_buf_offset(),
                        );

                        debug_assert_eq!(std::mem::size_of::<SteelVal>(), 16);
                        let byte_offset = ctx.builder.ins().ishl_imm_u(idx, 4);
                        let slot = ctx.builder.ins().iadd(buf, byte_offset);

                        let value =
                            ctx.builder
                                .ins()
                                .load(types::I128, MemFlagsData::trusted(), slot, 0);

                        ctx.clone_value(value);

                        if vector_owned {
                            ctx.drop_tagged_value(vector);
                        }

                        value
                    },
                    |ctx| fallback(ctx),
                    types::I128,
                )
            },
            |ctx| fallback(ctx),
            types::I128,
        )
    }

    // vector-push! with no call while the buffer has room. Growing reallocates
    // and a shared vector needs the lock, so both take the fallback.
    pub(super) fn inline_mut_vector_push(
        &mut self,
        vector: Value,
        value: Value,
        fallback: impl Fn(&mut Self) -> Value,
    ) -> Value {
        let is_vec = self.is_type(vector, SteelVal::HEAP_REF_VECTOR_TAG);

        self.converging_if_else_cold(
            is_vec,
            |ctx| {
                let ptr = ctx.unbox_value_to_pointer(vector);

                // At a strong count of one, len and cap can't move under us
                let strong =
                    ctx.builder
                        .ins()
                        .atomic_load(ref_count_type(), MemFlagsData::trusted(), ptr);
                let exclusive = ctx.builder.ins().icmp_imm_s(IntCC::Equal, strong, 1);

                let len = ctx.heap_vec_len(ptr);
                let cap = ctx.heap_vec_cap(ptr);
                let has_room = ctx.builder.ins().icmp(IntCC::UnsignedLessThan, len, cap);

                let can_inline = ctx.builder.ins().band(exclusive, has_room);

                ctx.converging_if_else_cold(
                    can_inline,
                    |ctx| {
                        let slot = ctx.heap_vec_element_address(ptr, len);
                        ctx.builder
                            .ins()
                            .store(MemFlagsData::trusted(), value, slot, 0);

                        let new_len = ctx.builder.ins().iadd_imm_s(len, 1);
                        ctx.builder.ins().store(
                            MemFlagsData::trusted(),
                            new_len,
                            ptr,
                            heap_vec_len_offset(),
                        );

                        ctx.drop_tagged_value(vector);
                        ctx.encode_void()
                    },
                    |ctx| fallback(ctx),
                    types::I128,
                )
            },
            |ctx| fallback(ctx),
            types::I128,
        )
    }

    pub(super) fn vector_push(&mut self) {
        let args = self
            .split_off(2)
            .into_iter()
            .map(|x| x.0)
            .collect::<Vec<_>>();

        let (vector, value) = (args[0], args[1]);

        let fallback = move |ctx: &mut Self| {
            // Checked by the caller, after the result is pushed.
            ctx.call_helper_unchecked(VECTOR_PUSH_ARGS, [vector, value])
        };

        let res = self.inline_mut_vector_push(vector, value, fallback);

        self.push(res, InferredType::Any);

        self.ip += 1;
        self.check_deopt();
    }

    // The constructor only moves the arguments into a fresh allocation, so it
    // cannot deopt and does not need the thread context.
    pub(super) fn flat_vector_construct(&mut self, arity: usize) {
        let name = CallFlatVectorConstructorsDefinitions::arity_to_name(arity).unwrap();

        let args = self
            .split_off(arity)
            .into_iter()
            .map(|x| x.0)
            .collect::<Vec<_>>();

        let res = self.call_function_returns_value_args_no_context(name, &args);

        self.push(res, InferredType::Any);

        self.ip += 1;
    }

    /// A `#:mutable` struct field is a box, and its accessor is
    /// `(lambda (this) (#%unbox (getter-proto this i)))`, so the getter call is
    /// followed immediately by a one argument call to `#%unbox`. Reading the
    /// box out of the struct only to unbox it costs a refcount increment on the
    /// box (an out-of-line call, since boxes are not biased) and a drop right
    /// after. When that pair comes next, this returns the instruction to resume
    /// at once both are done. That is past the unbox's FUNC, or at the POPPURE
    /// after a tail call's TAILCALL.
    ///
    /// Called with `ip` at the getter's FUNC. Nothing between the two calls may
    /// be a branch target, since the pair is compiled as one operation.
    fn fused_unbox_target(&self) -> Option<usize> {
        if !getter_unbox_fusion_enabled() {
            return None;
        }

        let call = self.ip + 1;
        let apply = call + 1;
        if self.join_targets.contains(&call) || self.join_targets.contains(&apply) {
            return None;
        }

        let call_ins = self.instructions.get(call)?;
        let apply_ins = self.instructions.get(apply)?;

        let calls_unbox = matches!(
            call_ins.op_code,
            OpCode::CALLGLOBAL
                | OpCode::CALLGLOBALNOARITY
                | OpCode::CALLGLOBALTAIL
                | OpCode::CALLGLOBALTAILNOARITY
                | OpCode::CALLPRIMITIVE
                | OpCode::CALLPRIMITIVETAIL
        ) && matches!(
            self._globals.get(call_ins.payload_size.to_usize()),
            Some(SteelVal::FuncV(f))
                if *f as usize == crate::steel_vm::primitives::steel_unbox_mutable as *const () as usize
        );

        if !calls_unbox || apply_ins.payload_size.to_usize() != 1 {
            return None;
        }

        match apply_ins.op_code {
            OpCode::FUNC | OpCode::FUNCNOARITY => Some(apply + 1),
            OpCode::TAILCALL | OpCode::TAILCALLNOARITY => {
                let ret = apply + 1;
                (!self.join_targets.contains(&ret)
                    && matches!(self.instructions.get(ret)?.op_code, OpCode::POPPURE))
                .then_some(ret)
            }
            _ => None,
        }
    }

    pub(super) fn inline_struct_call_no_drop(
        &mut self,
        spec: StructConstructorRefSpec,
        arity: usize,
        function_index: usize,
    ) -> Option<(Value, InferredType)> {
        // A captured operand is read without a clone, as a borrow.
        if arity == 1
            && matches!(
                spec.typ,
                StructFunctionType::Predicate | StructFunctionType::GetterProtoVec(_)
            )
        {
            if let Some(&MaybeStackValue::Constant(ConstantValue::Captured(c))) =
                self.shadow_stack.last()
            {
                let value = self.inline_read_captured(c, false);
                let owned = self.owned_flag(false);
                *self.shadow_stack.last_mut().unwrap() =
                    MaybeStackValue::Borrowed(BorrowedValue { value, owned });
            }
        }
        let res = self.inline_struct_call_inner(spec, arity, function_index);
        // A borrowed operand was kept for an inline lowering that didn't
        // happen, and the real call takes it owned.
        if res.is_none() {
            self.materialize_borrowed();
        }
        res
    }

    fn inline_struct_call_inner(
        &mut self,
        spec: StructConstructorRefSpec,
        arity: usize,
        function_index: usize,
    ) -> Option<(Value, InferredType)> {
        // Only the fused getter-proto arm below sets this, and every exit from
        // it returns a result its caller takes. Clear it here so a stale one is
        // never read as another call's.
        self.pending_borrow = None;

        let args = self.shadow_stack.get(self.shadow_stack.len() - arity..)?;

        match spec.typ {
            StructFunctionType::Constructor
                if CallStructConstructorsDefinitions::arity_to_name(arity).is_some() =>
            {
                Some(self.call_struct_constructor(&spec, arity))
            }
            StructFunctionType::GetterProtoVec(i)
                if matches!(
                    args,
                    &[MaybeStackValue::Register(_)
                        | MaybeStackValue::MutRegister(_)
                        | MaybeStackValue::Borrowed(_)]
                ) =>
            {
                // A field a vector operation reads right away is borrowed, since
                // the struct keeps it alive. A moved struct is left in its slot
                // instead, so it still does.
                let borrow = (super::borrow_enabled() && self.field_feeds_vector_op())
                    .then(|| self.builder.declare_var(types::I8));
                if borrow.is_some() {
                    if let Some(MaybeStackValue::MutRegister(p)) = self.shadow_stack.last().copied()
                    {
                        *self.shadow_stack.last_mut().unwrap() = MaybeStackValue::Register(p);
                    }
                }
                self.pending_borrow = borrow;

                Some(self.inline_struct_getter_call(
                    &spec,
                    arity,
                    function_index,
                    i,
                    None,
                    None,
                    borrow,
                ))
            }
            // A `#:mutable` struct's accessors are generated as closures over a
            // box, `(lambda (this) (#%unbox (getter-proto this 0)))`. After the
            // wrapper is inlined, the call left behind is the generic two
            // argument prototype with a literal index, instead of the one
            // argument specialised prototype an immutable struct gets. Past
            // pulling that index out, this is the same as `GetterProtoVec` above.
            StructFunctionType::GetterProto
                if getter_proto_inlining_enabled()
                    && matches!(
                        args,
                        &[
                            MaybeStackValue::Register(_) | MaybeStackValue::MutRegister(_),
                            MaybeStackValue::Constant(ConstantValue::Int(_))
                        ]
                    ) =>
            {
                let index_operand = *self.shadow_stack.last().unwrap();
                let i = match index_operand.into_constant_int(self) {
                    Some(i) if i >= 0 => i as usize,
                    _ => return None,
                };

                let fuse = self.fused_unbox_target();

                // A fused read of a struct that stays in its register leaves the
                // box's contents borrowed.
                let borrow = (fuse.is_some()
                    && super::borrow_enabled()
                    && matches!(
                        self.shadow_stack.get(self.shadow_stack.len() - 2),
                        Some(MaybeStackValue::Register(_))
                    ))
                .then(|| self.builder.declare_var(types::I8));
                self.pending_borrow = borrow;

                // The index is a compile time constant, so nothing is lost by
                // never materialising it.
                self.shadow_stack.pop();

                Some(self.inline_struct_getter_call(
                    &spec,
                    GETTER_PROTO_ARITY,
                    function_index,
                    i,
                    Some(index_operand),
                    fuse,
                    borrow,
                ))
            }
            // A struct predicate is a tag check and a descriptor compare, with no
            // field access, no allocation, and at the correct arity no error
            // path.
            //
            // Restricted to a borrowed register operand. The predicate does not
            // retain its argument, so with a register there is no ownership to
            // transfer and nothing to drop. A moved-out or owned operand would
            // need that bookkeeping and is rare, so it takes the generic path.
            StructFunctionType::Predicate
                if predicate_inlining_enabled()
                    && matches!(
                        args,
                        &[MaybeStackValue::Register(_) | MaybeStackValue::Borrowed(_)]
                    ) =>
            {
                Some(self.inline_struct_predicate(&spec))
            }

            _ => None,
        }
    }

    /// Structs are always built by the helper, which takes the descriptor and
    /// the fields.
    fn call_struct_constructor(
        &mut self,
        spec: &StructConstructorRefSpec,
        arity: usize,
    ) -> (Value, InferredType) {
        let name = CallStructConstructorsDefinitions::arity_to_name(arity).unwrap();

        let mut args = self
            .split_off(arity)
            .into_iter()
            .map(|x| x.0)
            .collect::<Vec<_>>();

        let descriptor = self.i64_const(spec.descriptor.key() as i64);

        args.insert(0, descriptor);

        let res = self.call_function_returns_value_args_no_context(name, &args);

        self.ip += 1;

        (res, InferredType::Any)
    }

    /// A struct predicate on a register operand.
    fn inline_struct_predicate(
        &mut self,
        spec: &StructConstructorRefSpec,
    ) -> (Value, InferredType) {
        let (value, borrowed) = match self.shadow_stack.last().copied().unwrap() {
            MaybeStackValue::Borrowed(b) => {
                self.shadow_stack.pop();
                (b.value, Some(b))
            }
            operand => {
                let value = self.read_from_vm_stack(operand.into_index());
                self.shadow_stack_pop();
                (value, None)
            }
        };

        let is_struct = self.is_type(value, SteelVal::STRUCT_TAG);
        let typ = self.int;
        let descriptor = spec.descriptor.key();

        let res = self.converging_if(
            is_struct,
            |ctx| {
                let ptr = ctx.unbox_value_to_pointer(value);
                let on_heap = ctx.builder.ins().load(
                    types::I64,
                    MemFlagsData::trusted(),
                    ptr,
                    StructStorage::header_offset() as i32,
                );
                let matches =
                    ctx.builder
                        .ins()
                        .icmp_imm_s(IntCC::Equal, on_heap, descriptor as i64);
                let widened = ctx.builder.ins().uextend(types::I64, matches);
                ctx.encode_value(discriminant(&SteelVal::BoolV(true)) as i64, widened)
            },
            // Not a struct at all, so certainly not this one.
            |ctx| {
                let zero = ctx.i64_const(0);
                ctx.encode_value(discriminant(&SteelVal::BoolV(true)) as i64, zero)
            },
            typ,
        );

        if let Some(b) = borrowed {
            self.release_borrowed(b);
        }

        self.ip += 1;

        (res, InferredType::Bool)
    }
    /// A struct field read with the struct in a register. The operands the
    /// getter doesn't need, like the prototype's literal index, are already
    /// popped and passed as `dropped`, so a slow arm can put them back.
    #[allow(clippy::too_many_arguments)]
    fn inline_struct_getter_call(
        &mut self,
        spec: &StructConstructorRefSpec,
        slow_arity: usize,
        function_index: usize,
        i: usize,
        dropped: Option<MaybeStackValue>,
        fuse: Option<usize>,
        borrow: Option<Variable>,
    ) -> (Value, InferredType) {
        let (struct_ref, struct_arg_index, borrowed) = match self.shadow_stack.last().copied() {
            Some(MaybeStackValue::Borrowed(b)) => (b.value, None, Some(b)),
            operand => {
                let index = operand.unwrap().into_index();
                (self.read_from_vm_stack(index), Some(index), None)
            }
        };

        let maybe_inferred_type = struct_arg_index
            .and_then(|index| self.properties.get(&ValueOrRegister::Register(index)));

        if let (
            Some(Properties::InferredType(InferredType::Struct(desc))),
            Some(struct_arg_index),
        ) = (maybe_inferred_type, struct_arg_index)
        {
            if spec.descriptor == desc {
                let last_kind = self.shadow_stack.pop().unwrap();
                let struct_ref_ptr = self.unbox_value_to_pointer(struct_ref);
                let res = fast_path_struct_matches(i, struct_ref_ptr, self, fuse, borrow);

                match last_kind {
                    // The read was a move, so the register gives up the struct,
                    // and with it this reference. The field was cloned, since
                    // only a struct left in its register is read borrowed.
                    // `mut_register_to_value` also forgets any cached read of
                    // the slot, which would otherwise hand the dropped struct
                    // out again.
                    MaybeStackValue::MutRegister(r) => {
                        let (moved, _) = self.mut_register_to_value(r);
                        self.drop_tagged_value(moved);
                    }
                    MaybeStackValue::Register(_) => {
                        self.properties.add_property(
                            ValueOrRegister::Register(struct_arg_index),
                            Properties::InferredType(InferredType::Struct(spec.descriptor)),
                        );
                    }
                    _ => unreachable!(),
                }

                return (res, InferredType::Any);
            }
        }

        let is_struct = self.is_type(struct_ref, SteelVal::STRUCT_TAG);
        let typ = self.int;

        // Take the operand out of its register up front so every arm starts
        // from the same stack. Otherwise a slow arm would move it again, out of
        // a register that already holds void.
        let owned = matches!(
            self.shadow_stack.last(),
            Some(MaybeStackValue::MutRegister(_))
        );
        if let Some(MaybeStackValue::MutRegister(p)) = self.shadow_stack.last().copied() {
            let (value, inferred_type) = self.mut_register_to_value(p);
            *self.shadow_stack.last_mut().unwrap() = MaybeStackValue::Value(StackValue {
                value,
                inferred_type,
                spilled: false,
            });
        }

        // What every slow arm restores to re-enter the real call.
        let old_ip = self.ip;
        let mut stack = self.shadow_stack.clone();
        stack.extend(dropped);
        let inner_stack = stack.clone();

        let res = self.converging_if(
            is_struct,
            |ctx| {
                inline_struct_getter(
                    spec,
                    slow_arity,
                    function_index,
                    i,
                    struct_ref,
                    owned,
                    borrowed,
                    typ,
                    &inner_stack,
                    old_ip,
                    fuse,
                    borrow,
                    ctx,
                )
            },
            move |ctx| {
                ctx.ip = old_ip;
                ctx.shadow_stack = stack.clone();
                ctx.materialize_borrowed();
                let value = slow_path_struct_getter(slow_arity, function_index, ctx);
                finish_slow_getter(value, fuse, borrow, ctx)
            },
            typ,
        );

        (res, InferredType::Any)
    }
}

/// The generic struct getter prototype always takes `(struct, index)`.
const GETTER_PROTO_ARITY: usize = 2;

/// `STEEL_JIT_PREDICATE=0` turns off inlining of struct predicates.
fn predicate_inlining_enabled() -> bool {
    crate::config::config().jit_predicate
}

/// `STEEL_JIT_FUSE_GETTER_UNBOX=0` turns off reading a mutable struct field
/// straight through its box. On by default.
fn getter_unbox_fusion_enabled() -> bool {
    crate::config::config().jit_fuse_getter_unbox
}

/// `STEEL_JIT_GETTER_PROTO=0` turns off inlining of the two argument struct
/// getter prototype, for A/B measurement.
fn getter_proto_inlining_enabled() -> bool {
    crate::config::config().jit_getter_proto
}

/// Checks the descriptor, then reads field `i`. The slow arm restores the
/// saved operand stack, since the call may have had operands the getter
/// already dropped.
#[allow(clippy::too_many_arguments)]
fn inline_struct_getter(
    spec: &StructConstructorRefSpec,
    slow_arity: usize,
    function_index: usize,
    i: usize,
    struct_ref: Value,
    owned: bool,
    borrowed: Option<BorrowedValue>,
    typ: Type,
    saved_stack: &[MaybeStackValue],
    old_ip: usize,
    fuse: Option<usize>,
    borrow: Option<Variable>,
    ctx: &mut FunctionTranslator,
) -> Value {
    let descriptor = spec.descriptor.key();

    let struct_ref_ptr = ctx.unbox_value_to_pointer(struct_ref);

    let descriptor_on_stack = ctx.builder.ins().load(
        types::I64,
        MemFlagsData::trusted(),
        struct_ref_ptr,
        StructStorage::header_offset() as i32,
    );

    let struct_matches =
        ctx.builder
            .ins()
            .icmp_imm_s(IntCC::Equal, descriptor_on_stack, descriptor as i64);

    ctx.shadow_stack.pop();

    let saved = saved_stack.to_vec();

    ctx.converging_if(
        struct_matches,
        |ctx| {
            let res = fast_path_struct_matches(i, struct_ref_ptr, ctx, fuse, borrow);
            // A moved struct is released once its field is read. The slow arm
            // hands it to the call instead.
            if owned {
                ctx.drop_tagged_value(struct_ref);
            }
            if let Some(b) = borrowed {
                ctx.release_borrowed(b);
            }
            res
        },
        move |ctx| {
            // call_global_function takes self.ip as the deopt fallback and then
            // advances one, so this has to enter on the call itself, the same
            // as the outer slow path does.
            ctx.ip = old_ip;
            ctx.shadow_stack = saved.clone();
            ctx.materialize_borrowed();
            let value = slow_path_struct_getter(slow_arity, function_index, ctx);
            finish_slow_getter(value, fuse, borrow, ctx)
        },
        typ,
    )
}

fn fast_path_struct_matches(
    i: usize,
    struct_ref_ptr: Value,
    ctx: &mut FunctionTranslator<'_>,
    fuse: Option<usize>,
    borrow: Option<Variable>,
) -> Value {
    // The fields sit inline right after the header, so the element address is a
    // constant displacement off the struct pointer, with no buffer to load.
    let size: i64 = std::mem::size_of::<SteelVal>() as _;

    let offset = StructStorage::data_offset() as i64 + (i as i64 * size);

    let slot_ptr = ctx.builder.ins().iadd_imm_s(struct_ref_ptr, offset);

    let local_value = ctx
        .builder
        .ins()
        .load(types::I128, MemFlagsData::trusted(), slot_ptr, 0);

    if let Some(resume) = fuse {
        // The field is the box and the unbox comes straight after. Read through
        // it while it is still borrowed from the struct, which keeps it alive
        // for the read, so only the value inside is cloned. The box itself is
        // never cloned, and so never dropped.
        let value = match borrow {
            // With the struct still in its register, the contents can stay
            // borrowed as well.
            Some(owned) => ctx.unbox_value_borrowed(local_value, owned),
            None => ctx.unbox_value_checked_register(local_value, false),
        };
        ctx.ip = resume;
        return value;
    }

    // Clone whatever comes out of this, unless it is borrowed
    match borrow {
        Some(owned) => {
            let zero = ctx.builder.ins().iconst(types::I8, 0);
            ctx.builder.def_var(owned, zero);
        }
        None => ctx.clone_value(local_value),
    }

    // TODO: @Matt clean up these ip advances
    ctx.ip += 1;

    local_value
}

/// The slow arm of a fused `(#%unbox (getter-proto ...))`: the real getter call
/// hands back an owned box, so unbox it here and release it. Both arms then end
/// on the same instruction holding the same kind of value, which the
/// `converging_if` around them requires.
fn finish_slow_getter(
    value: Value,
    fuse: Option<usize>,
    borrow: Option<Variable>,
    ctx: &mut FunctionTranslator<'_>,
) -> Value {
    if let Some(owned) = borrow {
        let one = ctx.builder.ins().iconst(types::I8, 1);
        ctx.builder.def_var(owned, one);
    }
    match fuse {
        Some(resume) => {
            let unboxed = ctx.unbox_value_checked_register(value, true);
            ctx.ip = resume;
            unboxed
        }
        None => value,
    }
}

fn slow_path_struct_getter(
    arity: usize,
    function_index: usize,
    ctx: &mut FunctionTranslator<'_>,
) -> Value {
    // Slow path. Just call the function directly
    let name = CallGlobalFunctionDefinitions::arity_to_name(arity);

    if let Some(name) = name {
        // A pure error path, so the interpreter never resumes from it. There is
        // no need to materialise the operand stack, so the fast arm skips it too.
        let result = ctx.call_global_function_no_spill(arity, name, function_index);
        ctx.check_deopt();

        // Assuming this worked, we'll want to push this result on to the stack.
        result
    } else {
        let v = ctx.call_global_function_spilled(arity, CALL_GLOBAL_SPILLED, function_index, false);
        ctx.check_deopt();

        v
    }
}

// `emit_spinlock_inline`'s lock_ptr is the base of the SpinLock, so the flag has
// to live at offset 0.
//
// unbox_value_checked_register reaches the payload with SpinLock<SteelVal>'s
// data offset, which only lands right while HeapAllocated keeps value first.
const _: () = {
    assert!(SpinLock::<SteelVal>::lock_offset() == 0);
    assert!(core::mem::offset_of!(HeapAllocated<SteelVal>, value) == 0);
};

// TODO: Use this now for anything that is boxed:
// i.e. Vectors and boxes
//
// Box get / unbox should now be inlineable.
// Vector ref / set should now be inlineable (once we swap the vector implementation)
// to the custom one.
//
// Struct refs should now be inlineable.
//
// The jit's own acquire/release, matching values::lock::SpinLock. Cranelift's
// atomics are sequentially consistent, so this is stronger than the rust side's
// Acquire/Release. They interoperate, and generated code pays a bit more.
fn emit_spinlock_inline(builder: &mut FunctionBuilder, lock_ptr: Value) {
    let spin_entry = builder.create_block();
    let spin_wait = builder.create_block();
    let acquired = builder.create_block();

    builder.ins().jump(spin_entry, &[]);

    // Attempt CAS
    builder.switch_to_block(spin_entry);
    let zero = builder.ins().iconst(types::I32, 0);
    let one = builder.ins().iconst(types::I32, 1);
    let old = builder
        .ins()
        .atomic_cas(MemFlagsData::trusted(), lock_ptr, zero, one);
    let cas_ok = builder.ins().icmp(IntCC::Equal, old, zero);
    builder.ins().brif(cas_ok, acquired, &[], spin_wait, &[]);

    // load until zero
    builder.switch_to_block(spin_wait);
    let val = builder
        .ins()
        .atomic_load(types::I32, MemFlagsData::trusted(), lock_ptr);
    let is_free = builder.ins().icmp(IntCC::Equal, val, zero);
    builder.ins().brif(is_free, spin_entry, &[], spin_wait, &[]);

    // Acquired
    builder.switch_to_block(acquired);
    builder.seal_block(spin_entry);
    builder.seal_block(spin_wait);
    builder.seal_block(acquired);
}

fn emit_spinlock_unlock_inline(builder: &mut FunctionBuilder, lock_ptr: Value) {
    let zero = builder.ins().iconst(types::I32, 0);
    builder
        .ins()
        .atomic_store(MemFlagsData::trusted(), zero, lock_ptr);
}

struct ForkedState {
    ip: usize,
    shadow_stack: Vec<MaybeStackValue>,
    value_to_local_map: HashMap<Value, usize>,
    local_to_value_map: HashMap<usize, InferredType>,
    properties: PropertyMap,
    tco: bool,
    let_var_stack: Vec<usize>,
    fake_entry_block: Option<Block>,
    exit_block: Block,
    visited: HashSet<usize>,
    depth: usize,
    if_bound: Option<usize>,
    if_stack: Vec<usize>,
    if_merge_blocks: Vec<Block>,
    if_merge_flags: Vec<Variable>,
    if_merge_lazy: Vec<Option<LazyRun>>,
    vm_context: Value,
    function_context: Option<usize>,
    thread_pointer: Option<Value>,
    should_trampoline: Option<Value>,
    sp: Option<Value>,
    pop_count: Option<Value>,
    pop_count_plus_one: Option<Value>,
    pop_count_minus_one: Option<Value>,
    thread_id: Option<Value>,
}

impl<'a> FunctionTranslator<'a> {
    // Do a converging if; attempt to unify the values?
    pub(super) fn branch_on_condition_and_property(
        &mut self,
        test_condition: Value,
        register: usize,
        positive_property: Properties,
        negative_property: Properties,
        positive_thunk: impl FnOnce(&mut Self) -> (Value, InferredType),
        negative_thunk: impl FnOnce(&mut Self) -> (Value, InferredType),
        merge_thunk: impl Fn(&mut Self, Value, InferredType),
    ) {
        // Branch, but don't merge
        let then_block = self.builder.create_block();
        let else_block = self.builder.create_block();

        self.builder
            .ins()
            .brif(test_condition, then_block, &[], else_block, &[]);

        self.builder.switch_to_block(then_block);
        self.builder.seal_block(then_block);

        // Fork on that condition:
        self.fork(|ctx| {
            ctx.properties
                .set_property(ValueOrRegister::Register(register), positive_property);
            let (value, typ) = positive_thunk(ctx);
            merge_thunk(ctx, value, typ);
            ctx.stack_to_ssa();

            if ctx.if_bound == Some(ctx.ip) {
                let merge = *ctx
                    .if_merge_blocks
                    .last()
                    .expect("if_bound is set but no enclosing if merge block was registered - check translate_if_else_value");
                let return_value = ctx.shadow_pop().0;
                if let Some(Some(run)) = ctx.if_merge_lazy.last().cloned() {
                    ctx.restore_lazy_run(&run);
                }
                // `shadow_pop` has materialized it, so it reaches the merge owned.
                let flag = *ctx.if_merge_flags.last().unwrap();
                let one = ctx.builder.ins().iconst(types::I8, 1);
                ctx.builder.def_var(flag, one);
                ctx.builder
                    .ins()
                    .jump(merge, &[BlockArg::Value(return_value)]);
            } else if ctx.ip > ctx.instructions.len() {
                let value = ctx.encode_void();
                ctx.builder.ins().return_(&[value]);
            }

            // Decrement the depth which is implicitly incremented by the stack to ssa call
            ctx.depth -= 1;
        });

        // Mark the else block as cold
        // self.builder.set_cold_block(else_block);

        self.builder.switch_to_block(else_block);
        self.builder.seal_block(else_block);
        self.properties
            .set_property(ValueOrRegister::Register(register), negative_property);

        let (value, typ) = negative_thunk(self);
        merge_thunk(self, value, typ);
        // Do the rest of the thing, from the usual position:
    }

    fn fork(&mut self, func: impl FnOnce(&mut Self)) {
        let state = ForkedState {
            ip: self.ip,
            shadow_stack: self.shadow_stack.clone(),
            value_to_local_map: self.value_to_local_map.clone(),
            local_to_value_map: self.local_to_value_map.clone(),
            properties: self.properties.clone(),
            tco: self.tco,
            let_var_stack: self.let_var_stack.clone(),
            fake_entry_block: self.fake_entry_block,
            exit_block: self.exit_block,
            visited: self.visited.clone(),
            depth: self.depth,
            if_bound: self.if_bound,
            if_stack: self.if_stack.clone(),
            if_merge_blocks: self.if_merge_blocks.clone(),
            if_merge_flags: self.if_merge_flags.clone(),
            if_merge_lazy: self.if_merge_lazy.clone(),
            vm_context: self.vm_context,
            function_context: self.function_context,
            thread_pointer: self.thread_pointer,
            should_trampoline: self.should_trampoline,
            sp: self.sp,
            pop_count: self.pop_count,
            pop_count_plus_one: self.pop_count_plus_one,
            pop_count_minus_one: self.pop_count_minus_one,
            thread_id: self.thread_id,
        };

        // Run the whole show, and then reset back:
        func(self);

        self.ip = state.ip;
        self.shadow_stack = state.shadow_stack;
        self.value_to_local_map = state.value_to_local_map;
        self.local_to_value_map = state.local_to_value_map;
        self.properties = state.properties;
        self.tco = state.tco;
        self.let_var_stack = state.let_var_stack;
        self.fake_entry_block = state.fake_entry_block;
        self.exit_block = state.exit_block;
        self.visited = state.visited;
        self.depth = state.depth;
        self.if_bound = state.if_bound;
        self.if_stack = state.if_stack;
        self.if_merge_blocks = state.if_merge_blocks;
        self.if_merge_flags = state.if_merge_flags;
        self.if_merge_lazy = state.if_merge_lazy;
        self.vm_context = state.vm_context;
        self.function_context = state.function_context;
        self.thread_pointer = state.thread_pointer;
        self.should_trampoline = state.should_trampoline;
        self.sp = state.sp;
        self.pop_count = state.pop_count;
        self.pop_count_plus_one = state.pop_count_plus_one;
        self.pop_count_minus_one = state.pop_count_minus_one;
        self.thread_id = state.thread_id;
    }
}

/// Whether `memq` over this list is short enough to unroll, with every
/// element compared by its tag and payload alone.
pub(super) fn memq_inline_elements(list: &crate::values::lists::List<SteelVal>) -> bool {
    (1..=8).contains(&list.len())
        && list.iter().all(|e| {
            matches!(
                e,
                SteelVal::SymbolV(_) | SteelVal::CharV(_) | SteelVal::IntV(_) | SteelVal::BoolV(_)
            )
        })
}

/// One element of an unrolled `memq`: what its `eq?` compares against.
#[derive(Clone, Copy)]
pub(super) enum MemqElement {
    // The symbol's pointer, which the constant list keeps alive.
    Symbol(i64),
    Char(char),
    Int(isize),
    Bool(bool),
}

impl MemqElement {
    fn of(value: &SteelVal) -> Self {
        match value {
            // The handle's own bits, which is the payload a symbol carries.
            SteelVal::SymbolV(s) => MemqElement::Symbol(unsafe {
                core::ptr::read(s as *const SteelString as *const i64)
            }),
            SteelVal::CharV(c) => MemqElement::Char(*c),
            SteelVal::IntV(n) => MemqElement::Int(*n),
            SteelVal::BoolV(b) => MemqElement::Bool(*b),
            _ => unreachable!("checked by memq_inline_elements"),
        }
    }
}

/// The datums of a `case` clause, split for `case_member_constant`.
pub(super) struct CaseSet {
    // Bit `n` is set for the char, or the fixnum, `n`.
    chars: u128,
    ints: u128,
    others: Vec<MemqElement>,
}

impl CaseSet {
    // Below this many, compares are cheaper than the bitmap test.
    const MIN_BITMAP: usize = 4;
    const MAX_COMPARES: usize = 16;

    fn of(list: &crate::values::lists::List<SteelVal>) -> Option<Self> {
        let ascii = |e: &SteelVal| match e {
            SteelVal::CharV(c) => (*c as u32) < 128,
            SteelVal::IntV(n) => (0..128).contains(n),
            _ => false,
        };
        let use_bitmap = list.iter().filter(|e| ascii(e)).count() >= Self::MIN_BITMAP;
        let mut set = CaseSet {
            chars: 0,
            ints: 0,
            others: Vec::new(),
        };
        for element in list.iter() {
            match element {
                SteelVal::CharV(c) if use_bitmap && ascii(element) => set.chars |= 1 << (*c as u32),
                SteelVal::IntV(n) if use_bitmap && ascii(element) => set.ints |= 1 << *n,
                SteelVal::SymbolV(_)
                | SteelVal::CharV(_)
                | SteelVal::IntV(_)
                | SteelVal::BoolV(_) => set.others.push(MemqElement::of(element)),
                _ => return None,
            }
        }
        (set.others.len() <= Self::MAX_COMPARES).then_some(set)
    }
}

/// The char predicates `char_class` lowers. The discriminant is what
/// `char_class_handler_c` dispatches on.
#[derive(Clone, Copy)]
pub(super) enum CharClass {
    Whitespace = 0,
    Alphabetic = 1,
    Numeric = 2,
    UpperCase = 3,
    LowerCase = 4,
}
