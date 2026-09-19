use crate::values::{
    closed::{HeapAllocated, HeapVec},
    lock::SpinLock,
    structs::{StructConstructorRefSpec, StructFunctionType, UserDefinedStruct},
};

use crate::values::structs::StructStorage;
use super::*;

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

// Byte offset from the pointer a HeapRef holds to the steel_vec::Vec itself
const fn heap_vec_offset() -> i32 {
    (steel_rc::weak::Weak::<SpinLock<HeapAllocated<HeapVec>>>::data_offset()
        + SpinLock::<HeapAllocated<HeapVec>>::data_offset()
        + core::mem::offset_of!(HeapAllocated<HeapVec>, value)) as i32
}

// A FlatVector carries the BiasedRc box pointer, and the steel_vec::Vec sits in
// its data. Contiguous, so an element is a load and an index
const fn flat_vec_offset() -> i32 {
    steel_rc::BiasedRc::<steel_vec::Vec<SteelVal>>::data_offset() as i32
}

const fn flat_vec_len_offset() -> i32 {
    flat_vec_offset() + steel_vec::Vec::<SteelVal>::len_offset() as i32
}

const fn flat_vec_buf_offset() -> i32 {
    flat_vec_offset() + steel_vec::Vec::<SteelVal>::buf_offset() as i32
}

// A bytevector is `Gc<SpinLock<steel_vec::Vec<u8>>>`, so: the BiasedRc box
// pointer, then past the lock word, then the vec header. Unlike the vector
// paths the element is a plain `u8` - no refcount to bump on a read, and no old
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

// The abstract state both arms of a two way branch start from
// converging_if merges only `properties`, so the arms have to agree on the rest
// or the mismatch is silent. STEEL_JIT_BRANCH_CHECK=1 reports one.
fn branch_check_enabled() -> bool {
    static ENABLED: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ENABLED.get_or_init(|| std::env::var("STEEL_JIT_BRANCH_CHECK").is_ok())
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

    // Check the tags, see if they're equal, etc.
    pub(super) fn value_equals_binop(&mut self) {
        todo!()
    }

    pub(super) fn char_equals(&mut self, arity: usize) {
        let name = CallPrimitiveFixedDefinitions::arity_to_name(arity).unwrap();

        let args = self
            .shadow_stack
            .get(self.shadow_stack.len() - arity..)
            .unwrap()
            .to_vec();

        // dbg!(args);

        // attempt to move forward with it
        let additional_args = self.split_off(arity);

        // dbg!(&additional_args);

        // let f = crate::primitives::ports::read_char_single
        //     as fn(SteelVal) -> Result<SteelVal, crate::SteelErr>;
        //

        let all_chars = additional_args.iter().all(|x| x.1 == InferredType::Char);

        if all_chars && false {
            // println!("Found all characters, applying equality");
            // Just... compare for equality?

            let left = additional_args[0].0;
            let right = additional_args[1].0;

            let left = self.unbox_value(left);
            let right = self.unbox_value(right);

            let left = self.builder.ins().ireduce(types::I8, left);
            let right = self.builder.ins().ireduce(types::I8, right);

            let comparison = self.builder.ins().icmp(IntCC::Equal, left, right);
            let res = self.builder.ins().uextend(types::I64, comparison);
            let boolean = self.encode_value(discriminant(&SteelVal::BoolV(true)) as i64, res);
            self.push(boolean, InferredType::Bool);
            self.ip += 1;

            // No need to check deopt here, we're good.
        } else {
            let function = self.builder.ins().iconst(
                self.module.target_config().pointer_type(),
                crate::primitives::strings::char_equals_binop as *const () as i64,
            );

            let fallback_ip = self
                .builder
                .ins()
                .iconst(Type::int(64).unwrap(), self.ip as i64);

            let mut args = vec![function, fallback_ip];

            args.extend(additional_args.into_iter().map(|x| x.0));

            let result = self.call_function_returns_value_args(name, &args);
            self.push(result, InferredType::Bool);
            self.ip += 1;
            self.check_deopt();
        }
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

                    ctx.call_function_returns_value_args(
                        "vector-set-reg-3",
                        &[vector, index, value],
                    )
                };

                let res = if INLINE_MUTABLE_VECTOR_OPS {
                    self.inline_mut_vector_set(
                        vector_value,
                        index_value,
                        new_value,
                        false,
                        false,
                        false,
                        fallback,
                    )
                } else {
                    fallback(self)
                };

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

                    ctx.call_function_returns_value_args(
                        "vector-set-reg-2",
                        &[vector, index, new_value],
                    )
                };

                let res = if INLINE_MUTABLE_VECTOR_OPS {
                    self.inline_mut_vector_set(
                        vector_value,
                        index_value,
                        new_value,
                        false,
                        false,
                        true,
                        fallback,
                    )
                } else {
                    fallback(self)
                };

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

                    ctx.call_function_returns_value_args(
                        "vector-set-reg-1",
                        &[vector, index_value, new_value],
                    )
                };

                let res = if INLINE_MUTABLE_VECTOR_OPS {
                    self.inline_mut_vector_set(
                        vector_value,
                        index_value,
                        new_value,
                        false,
                        true,
                        true,
                        fallback,
                    )
                } else {
                    fallback(self)
                };

                self.push(res, InferredType::Any);
                self.ip += 1;
                self.check_deopt();
            }

            &[MutRegister(i) | Register(i), _, _] => {
                self.properties.add_property(
                    ValueOrRegister::Register(i),
                    Properties::InferredType(InferredType::MutableVector),
                );

                let args = self
                    .split_off(3)
                    .into_iter()
                    .map(|x| x.0)
                    .collect::<Vec<_>>();

                let (vector, index, value) = (args[0], args[1], args[2]);

                let fallback = move |ctx: &mut Self| {
                    ctx.call_function_returns_value_args("vector-set-args", &args)
                };

                let res = if INLINE_MUTABLE_VECTOR_OPS {
                    self.inline_mut_vector_set(vector, index, value, true, true, true, fallback)
                } else {
                    fallback(self)
                };

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
                    ctx.call_function_returns_value_args("vector-set-args", &args)
                };

                let res = if INLINE_MUTABLE_VECTOR_OPS {
                    self.inline_mut_vector_set(vector, index, value, true, true, true, fallback)
                } else {
                    fallback(self)
                };

                self.push(res, InferredType::Any);
                self.ip += 1;
                self.check_deopt();
            }
        }
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

                let res = self.call_function_returns_value_args("eq?-reg-2", &[left, right]);

                self.push(res, InferredType::UnboxedBool);
                self.ip += 1;
            }

            &[MutRegister(v) | Register(v), Value(StackValue {
                value,
                inferred_type: InferredType::Symbol,
                ..
            })] => {
                let right = self.shadow_pop();

                // Pop them off
                self.shadow_stack.pop();

                // If they're the same type, just compare the bytes. Don't do a lookup.

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
                    |ctx| {
                        // let left = ctx.register_index(v);
                        // ctx.call_function_returns_value_args("eq?-reg-1", &[left, right.0])
                        ctx.builder.ins().iconst(types::I8, 0)
                    },
                    types::I8,
                );

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

                // If they're the same type, just compare the bytes. Don't do a lookup.
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

                let res = self.call_function_returns_value_args("eq?-reg-1", &[left, right.0]);

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

                let res = self.call_function_returns_value_args_no_context("eq?-args", &args);

                self.push(res, InferredType::UnboxedBool);
                self.ip += 1;
            }
        }
    }

    /// `eq?` on two non-constant operands, without a call.
    ///
    /// `SteelVal::ptr_eq` is, per tag: a payload compare for everything that
    /// is a pointer or a fixnum; a compare at the real width for bools, chars
    /// and floats (the payload's unused bytes are not zeroed, and float `eq?`
    /// is `==`); always true for void; and always false for the tags it has no
    /// arm for. Only lists need the out-of-line check, since two handles can
    /// share storage. Owned operands are released inline afterwards, rather
    /// than being moved into a helper that drops them.
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

        // Tags whose ptr_eq is exactly a payload compare.
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

        let mask = self.builder.ins().iconst(types::I64, payload_tags as i64);
        let shifted = self.builder.ins().ushr(mask, left_tag);
        let is_payload_tag = self.builder.ins().band_imm_u(shifted, 1);
        let is_payload_tag = self.builder.ins().ireduce(types::I8, is_payload_tag);
        let same_payload = self
            .builder
            .ins()
            .icmp(IntCC::Equal, left_payload, right_payload);
        let by_payload = self.builder.ins().band(is_payload_tag, same_payload);

        let is_bool = self
            .builder
            .ins()
            .icmp_imm_s(IntCC::Equal, left_tag, SteelVal::BOOL_TAG as i64);
        let lb = self.builder.ins().ireduce(types::I8, left_payload);
        let rb = self.builder.ins().ireduce(types::I8, right_payload);
        let same_bool = self.builder.ins().icmp(IntCC::Equal, lb, rb);
        let by_bool = self.builder.ins().band(is_bool, same_bool);

        let is_char = self
            .builder
            .ins()
            .icmp_imm_s(IntCC::Equal, left_tag, SteelVal::CHAR_TAG as i64);
        let lc = self.builder.ins().ireduce(types::I32, left_payload);
        let rc = self.builder.ins().ireduce(types::I32, right_payload);
        let same_char = self.builder.ins().icmp(IntCC::Equal, lc, rc);
        let by_char = self.builder.ins().band(is_char, same_char);

        let is_float = self
            .builder
            .ins()
            .icmp_imm_s(IntCC::Equal, left_tag, SteelVal::FLOAT_TAG as i64);
        let lf = self.unbox_value_to_float(left);
        let rf = self.unbox_value_to_float(right);
        let same_float = self.builder.ins().fcmp(FloatCC::Equal, lf, rf);
        let by_float = self.builder.ins().band(is_float, same_float);

        let is_void = self
            .builder
            .ins()
            .icmp_imm_s(IntCC::Equal, left_tag, SteelVal::VOID_TAG as i64);

        let any = self.builder.ins().bor(by_payload, by_bool);
        let any = self.builder.ins().bor(any, by_char);
        let any = self.builder.ins().bor(any, by_float);
        let any = self.builder.ins().bor(any, is_void);
        let inline_result = self.builder.ins().band(same_tag, any);

        let is_list = self
            .builder
            .ins()
            .icmp_imm_s(IntCC::Equal, left_tag, SteelVal::LIST_TAG as i64);
        let needs_call = self.builder.ins().band(same_tag, is_list);

        let res = self.converging_if(
            needs_call,
            |ctx| ctx.call_function_returns_value_args_no_context("eq?-no-drop", &[left, right]),
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

    // Load just the symbols, and eq against each of them, unrolled?
    pub(super) fn list_contains_constant_symbols_fast_path(
        &mut self,
        lhs_value: Value,
        list: List<SteelVal>,
    ) {
        for item in list.iter() {
            let v = item.clone();

            let SteelVal::SymbolV(sym) = v else { panic!() };

            // self.builder.ins().
        }
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
                value,
                inferred_type: InferredType::List,
                ..
            })] => {
                let left = self.register_index(reg);
                let right = self.shadow_pop().0;
                let list = self.unbox_value_to_pointer(right);
                self.shadow_pop();
                let res = self.call_function_returns_value_args("list-contains-reg", &[left, list]);
                self.push(res, InferredType::UnboxedBool);

                self.ip += 1;
            }

            &[Register(reg), Constant(ConstantValue::List(l))] => {
                let left = self.register_index(reg);

                // Get the list, don't drop it - leak it, but we don't need to drop it.
                let _ = self.shadow_pop().0;
                self.shadow_pop();

                let value = self.constants.get(l);
                let SteelVal::ListV(value) = value else {
                    panic!()
                };
                let as_ptr: usize = unsafe {
                    std::mem::transmute::<crate::values::lists::List<_>, _>(value.clone())
                };

                let value_ptr = self.builder.ins().iconst(types::I64, as_ptr as i64);

                let res = self.call_function_returns_value_args(
                    "list-contains-reg-constant",
                    &[left, value_ptr],
                );
                self.push(res, InferredType::UnboxedBool);

                self.ip += 1;
            }

            _ => {
                let args = self
                    .split_off(2)
                    .into_iter()
                    .map(|x| x.0)
                    .collect::<Vec<_>>();

                let res = self.call_function_returns_value_args("list-contains-value", &args);

                self.check_deopt();

                self.push(res, InferredType::UnboxedBool);

                self.ip += 1;
            }
        }
    }

    pub(super) fn is_pair(&mut self) {
        use MaybeStackValue::*;

        let last = self.shadow_stack.last().unwrap().clone();

        match last {
            // TODO: Encode the result of the evaluation into the
            // branching - if this is used in the test position
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
                //         let res = self.builder.ins().iconst(types::I64, 1);

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

                // Encode this manually:
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
                    // Is a list
                    self.builder.switch_to_block(list_block);
                    self.builder.seal_block(list_block);

                    let value = self.unbox_value_to_pointer(value);

                    // Its not a pair if its an empty list
                    let length = self.list_cell_index(value);

                    let not_empty =
                        BlockArg::Value(self.builder.ins().icmp_imm_s(IntCC::NotEqual, length, 0));

                    self.builder.ins().jump(merge_block, &[not_empty]);
                }

                {
                    // Else case, not a list or pair
                    self.builder.switch_to_block(else_block);
                    self.builder.seal_block(else_block);
                    let false_val = BlockArg::Value(self.builder.ins().iconst(types::I8, 0));
                    self.builder.ins().jump(merge_block, &[false_val]);
                }

                {
                    self.builder.switch_to_block(merge_block);
                    self.builder.seal_block(merge_block);
                    let result = self.builder.block_params(merge_block)[0];
                    self.push(result, InferredType::UnboxedBool);
                }

                self.drop_tagged_value(value);

                self.ip += 1;
            }
            MutRegister(p) | Register(p) => {
                let register = self.register_index(p);

                match self.properties.get(&ValueOrRegister::Register(p)) {
                    // Elide the call entirely if its a non empty list
                    // Some(Properties::NonEmptyList) if false => {
                    //     println!("Found non empty list!");

                    //     self.shadow_stack.pop();
                    //     let res = self.builder.ins().iconst(types::I8, 1);
                    //     self.push(res, InferredType::UnboxedBool);
                    //     self.ip += 1;
                    // }
                    _ => {
                        self.shadow_stack.pop();
                        let res = self.call_function_returns_value_args("pair?", &[register]);

                        self.push(res, InferredType::Bool);
                        self.ip += 1;
                    }
                }
            }

            // Depending on what the constant is, we can do this evaluation here
            // Constant(constant_value) => todo!(),
            _ => {
                // TODO: Check the inferred type here as well, maybe do unboxed bools
                let (value, inferred_type) = self.shadow_pop();
                let res = self.call_function_returns_value_args_no_context("pair?-value", &[value]);
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
                        // let res = self.call_function_returns_value_args("list?", &[register]);
                        // self.push(res, InferredType::Bool);

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
            Constant(constant_value) => {
                let (value, inferred_type) = self.shadow_pop();
                let res = self.call_function_returns_value_args_no_context("list?-value", &[value]);
                self.push(res, InferredType::Bool);
                self.ip += 1;
            }
        }
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

                let value_ptr = self.builder.ins().iconst(types::I64, as_ptr as i64);

                let res = self.call_function_returns_value_args_no_context(
                    "memq-unchecked-list",
                    &[value, value_ptr],
                );

                res
            }

            _ => {
                let args = self
                    .split_off(2)
                    .into_iter()
                    .map(|x| x.0)
                    .collect::<Vec<_>>();

                let res = self.call_function_returns_value_args("memq-value", &args);

                // TODO: Put the ip in the check deopt!
                self.check_deopt();

                res
            }
        }
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
                        let res = self.call_function_returns_value_args("string?", &[register]);

                        self.push(res, InferredType::Bool);
                        self.ip += 1;
                    }
                }
            }
            Constant(constant_value) => {
                let (value, inferred_type) = self.shadow_pop();
                let res =
                    self.call_function_returns_value_args_no_context("string?-value", &[value]);
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
            // branching - if this is used in the test position
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
                //         let res = self.builder.ins().iconst(types::I64, 1);
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
                let register = self.register_index(p);
                self.shadow_stack.pop();
                let res = self.call_function_returns_value_args("empty?", &[register]);
                self.push(res, InferredType::Bool);
                self.ip += 1;
            }

            // Depending on what the constant is, we can do this evaluation here
            // Constant(constant_value) => todo!(),
            _ => {
                // TODO: Check the inferred type here as well, maybe do unboxed bools
                let (value, inferred_type) = self.shadow_pop();
                let res =
                    self.call_function_returns_value_args_no_context("empty?-value", &[value]);
                self.push(res, InferredType::Bool);
                self.ip += 1;
            }
        }
    }

    pub(super) fn converging_if_no_else_no_value_else_cold(
        &mut self,
        test_condition: Value,
        then: impl Fn(&mut Self),
        merge: impl Fn(&mut Self),
    ) {
        if let Some(taken) = self.const_bool(test_condition) {
            if taken {
                then(self);
            }
            merge(self);
            return;
        }
        let then_block = self.builder.create_block();
        self.builder.set_cold_block(then_block);

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
        self.properties.meet(&skipped.properties);
        self.meet_register_maps(&skipped.local_to_value_map, &skipped.value_to_local_map);

        merge(self);
    }

    pub(super) fn converging_if_no_else_no_value(
        &mut self,
        test_condition: Value,
        then: impl Fn(&mut Self),
        merge: impl Fn(&mut Self),
    ) {
        if let Some(taken) = self.const_bool(test_condition) {
            if taken {
                then(self);
            }
            merge(self);
            return;
        }
        let then_block = self.builder.create_block();

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
        self.properties.meet(&skipped.properties);
        self.meet_register_maps(&skipped.local_to_value_map, &skipped.value_to_local_map);

        merge(self);
    }

    pub(super) fn converging_if_no_else_no_value_then_cold(
        &mut self,
        test_condition: Value,
        then: impl Fn(&mut Self),
        merge: impl Fn(&mut Self),
    ) {
        if let Some(taken) = self.const_bool(test_condition) {
            if taken {
                then(self);
            }
            merge(self);
            return;
        }
        let then_block = self.builder.create_block();
        self.builder.set_cold_block(then_block);

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
        self.properties.meet(&skipped.properties);
        self.meet_register_maps(&skipped.local_to_value_map, &skipped.value_to_local_map);

        merge(self);
    }

    // Both arms run from the same program state, so both have to be translated
    // from the same one. Emitting then mutates it in place, so roll it back before
    // the else arm - otherwise else gets a stack / ip / property set that only the
    // then arm ever reaches.
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
        if let Some(taken) = self.const_bool(test_condition) {
            if taken { then(self) } else { else_thunk(self) }
            return;
        }
        let then_block = self.builder.create_block();
        let else_block = self.builder.create_block();
        let merge_block = self.builder.create_block();

        self.builder
            .ins()
            .brif(test_condition, then_block, &[], else_block, &[]);

        self.builder.switch_to_block(then_block);
        self.builder.seal_block(then_block);

        let entry = self.snapshot_branch_state();
        then(self);
        let then_properties = self.properties.clone();
        let then_locals = self.local_to_value_map.clone();
        let then_values = self.value_to_local_map.clone();

        self.builder.ins().jump(merge_block, &[]);

        self.builder.switch_to_block(else_block);
        self.builder.seal_block(else_block);

        self.restore_branch_state(entry);
        else_thunk(self);
        self.builder.ins().jump(merge_block, &[]);
        self.builder.switch_to_block(merge_block);

        // Only what both arms agree on survives the merge
        self.properties.meet(&then_properties);
        self.meet_register_maps(&then_locals, &then_values);
    }

    pub(super) fn converging_if_no_value_else_cold(
        &mut self,
        test_condition: Value,
        then: impl Fn(&mut Self),
        else_thunk: impl Fn(&mut Self),
    ) {
        if let Some(taken) = self.const_bool(test_condition) {
            if taken { then(self) } else { else_thunk(self) }
            return;
        }
        let then_block = self.builder.create_block();
        let else_block = self.builder.create_block();

        self.builder.set_cold_block(else_block);

        let merge_block = self.builder.create_block();

        self.builder
            .ins()
            .brif(test_condition, then_block, &[], else_block, &[]);

        self.builder.switch_to_block(then_block);
        self.builder.seal_block(then_block);

        let entry = self.snapshot_branch_state();
        then(self);
        let then_properties = self.properties.clone();
        let then_locals = self.local_to_value_map.clone();
        let then_values = self.value_to_local_map.clone();

        self.builder.ins().jump(merge_block, &[]);

        self.builder.switch_to_block(else_block);
        self.builder.seal_block(else_block);

        self.restore_branch_state(entry);
        else_thunk(self);
        self.builder.ins().jump(merge_block, &[]);
        self.builder.switch_to_block(merge_block);

        // Only what both arms agree on survives the merge
        self.properties.meet(&then_properties);
        self.meet_register_maps(&then_locals, &then_values);
    }

    pub(super) fn converging_if(
        &mut self,
        test_condition: Value,
        then: impl Fn(&mut Self) -> Value,
        else_thunk: impl Fn(&mut Self) -> Value,
        typ: Type,
    ) -> Value {
        if let Some(taken) = self.const_bool(test_condition) {
            return if taken { then(self) } else { else_thunk(self) };
        }
        let then_block = self.builder.create_block();
        let else_block = self.builder.create_block();
        let merge_block = self.builder.create_block();

        self.builder.append_block_param(merge_block, typ);

        self.builder
            .ins()
            .brif(test_condition, then_block, &[], else_block, &[]);

        self.builder.switch_to_block(then_block);
        self.builder.seal_block(then_block);

        let entry = self.snapshot_branch_state();
        let res = BlockArg::Value(then(self));
        let then_properties = self.properties.clone();
        let then_locals = self.local_to_value_map.clone();
        let then_values = self.value_to_local_map.clone();
        let then_state = branch_state_fingerprint(self);

        self.builder.ins().jump(merge_block, &[res]);

        self.builder.switch_to_block(else_block);
        self.builder.seal_block(else_block);

        self.restore_branch_state(entry);
        let then_res = BlockArg::Value(else_thunk(self));
        report_branch_divergence(then_state, branch_state_fingerprint(self));
        self.builder.ins().jump(merge_block, &[then_res]);
        self.builder.switch_to_block(merge_block);

        // Only what both arms agree on survives the merge
        self.properties.meet(&then_properties);
        self.meet_register_maps(&then_locals, &then_values);

        let result = self.builder.block_params(merge_block)[0];

        result
    }

    pub(super) fn converging_if_else_cold(
        &mut self,
        test_condition: Value,
        then: impl Fn(&mut Self) -> Value,
        else_thunk: impl Fn(&mut Self) -> Value,
        typ: Type,
    ) -> Value {
        if let Some(taken) = self.const_bool(test_condition) {
            return if taken { then(self) } else { else_thunk(self) };
        }
        let then_block = self.builder.create_block();
        let else_block = self.builder.create_block();
        let merge_block = self.builder.create_block();

        self.builder.set_cold_block(else_block);

        self.builder.append_block_param(merge_block, typ);

        self.builder
            .ins()
            .brif(test_condition, then_block, &[], else_block, &[]);

        self.builder.switch_to_block(then_block);
        self.builder.seal_block(then_block);

        let entry = self.snapshot_branch_state();
        let res = BlockArg::Value(then(self));
        let then_properties = self.properties.clone();
        let then_locals = self.local_to_value_map.clone();
        let then_values = self.value_to_local_map.clone();
        let then_state = branch_state_fingerprint(self);

        self.builder.ins().jump(merge_block, &[res]);

        self.builder.switch_to_block(else_block);
        self.builder.seal_block(else_block);

        self.restore_branch_state(entry);
        let then_res = BlockArg::Value(else_thunk(self));
        report_branch_divergence(then_state, branch_state_fingerprint(self));
        self.builder.ins().jump(merge_block, &[then_res]);
        self.builder.switch_to_block(merge_block);

        // Only what both arms agree on survives the merge
        self.properties.meet(&then_properties);
        self.meet_register_maps(&then_locals, &then_values);

        let result = self.builder.block_params(merge_block)[0];

        result
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
                let left_is_int = self.is_type(lv, SteelVal::INT_TAG);
                let right_is_int = self.is_type(rv, SteelVal::INT_TAG);
                let both_int = self.builder.ins().band(left_is_int, right_is_int);

                let sp = |ctx: &mut Self| {
                    let function_name = op_to_name_payload(OpCode::ADD, 2);
                    let args = [lv, rv];
                    let result = ctx.call_function_returns_value_args(function_name, &args);
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
                                // Happy path, just return the boxed integer value.
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
        let left_is_int = self.is_type(left, SteelVal::INT_TAG);
        let right_is_int = self.is_type(right, SteelVal::INT_TAG);
        let both_int = self.builder.ins().band(left_is_int, right_is_int);

        let sp = |ctx: &mut Self| {
            let args = [left, right];
            let result = ctx.call_function_returns_value_args("add-binop", &args);

            ctx.check_deopt();

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
                        // Happy path, just return the boxed integer value.
                        ctx.encode_value(SteelVal::INT_TAG as _, added)
                    },
                    typ,
                )
            },
            |ctx| {
                let res = sp(ctx);
                res
            },
            typ,
        );

        (res, InferredType::Number)
    }

    pub(super) fn binop_add_value_register(
        &mut self,
        left: usize,
        right: usize,
    ) -> (Value, InferredType) {
        let lv = self.read_from_vm_stack(left);
        let rv = self.read_from_vm_stack(right);

        let left_is_int = self.is_type(lv, SteelVal::INT_TAG);
        let right_is_int = self.is_type(rv, SteelVal::INT_TAG);
        let both_int = self.builder.ins().band(left_is_int, right_is_int);

        let sp = |ctx: &mut Self| {
            let register_r = ctx.builder.ins().iconst(types::I64, right as i64);
            let register_l = ctx.builder.ins().iconst(types::I64, left as i64);

            let args = [register_l, register_r];
            let result = ctx.call_function_returns_value_args("add-binop-reg-2", &args);

            // The helper reports a type error by flagging the vm rather than
            // returning one; without this the jitted code kept going and the
            // error surfaced after `with-handler` had already been unwound.
            ctx.check_deopt();

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
                        // Happy path, just return the boxed integer value.
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
                        let res = self.call_function_returns_value_args("symbol?", &[register]);

                        self.push(res, InferredType::Bool);
                        self.ip += 1;
                    }
                }
            }
            // TODO: We should probably elide this somehow?
            Constant(constant_value) => {
                let (value, inferred_type) = self.shadow_pop();
                let res =
                    self.call_function_returns_value_args_no_context("symbol?-value", &[value]);
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
                        let res = self.call_function_returns_value_args("void?", &[register]);
                        self.push(res, InferredType::Bool);
                        self.ip += 1;
                    }
                }
            }
            Constant(constant_value) => {
                let (value, inferred_type) = self.shadow_pop();
                let res = self.call_function_returns_value_args_no_context("void?-value", &[value]);
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
                let res = ctx.call_function_returns_value_args("unbox-handler", &[value]);
                ctx.check_deopt();
                res
            },
            typ,
        )
    }

    /// `unbox_value_checked_register` for a box borrowed from something that
    /// stays alive, leaving the contents borrowed too where it can: on the
    /// unshared fast path nothing is cloned and `owned` is set to 0. The locked
    /// and slow paths clone as before and set it to 1.
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
                let res = ctx.call_function_returns_value_args("unbox-handler", &[value]);
                ctx.check_deopt();
                let one = ctx.builder.ins().iconst(types::I8, 1);
                ctx.builder.def_var(owned, one);
                res
            },
            typ,
        )
    }

    /// Inline `#%set-box!`: store into the box and yield the value that was
    /// there, which is what `HeapRef::set_and_return` does.
    ///
    /// This mirrors `unbox_value_checked_register` exactly - same tag check,
    /// same Arc strong-count probe to decide whether the spinlock is needed -
    /// but it is *cheaper*, because the underlying operation is a
    /// `mem::replace`: the old value is moved out and the new one moved in, so
    /// unlike the read path there is no `clone_value` on either side. The
    /// caller receives ownership of the old value, exactly as the out-of-line
    /// handler returned it.
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
                // in the shape it expects.
                ctx.clone_value(boxed);
                let res = ctx
                    .call_function_returns_value_args("set-box-handler", &[boxed, new_value]);
                ctx.check_deopt();
                res
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
        self.builder
            .ins()
            .load(types::I64, MemFlagsData::trusted(), ptr, heap_vec_len_offset())
    }

    fn heap_vec_cap(&mut self, ptr: Value) -> Value {
        self.builder
            .ins()
            .load(types::I64, MemFlagsData::trusted(), ptr, heap_vec_cap_offset())
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
    // whatever lock it holds
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

    // Nothing else can reach the value at a strong count of one
    fn with_heap_vec_lock(
        &mut self,
        ptr: Value,
        body: impl Fn(&mut Self) -> Value,
    ) -> Value {
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
    // Ownership is per argument because the shapes mix the two - a register
    // argument is a borrow, while a spilled one is ours to move in or drop.
    // (vector-set! v i (vector-ref x i)) is the mixed case: v and i are
    // registers, the value is not
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
        let is_vec = self.is_type(vector, SteelVal::HEAP_REF_VECTOR_TAG);
        let is_int = self.is_type(index, SteelVal::INT_TAG);
        let both = self.builder.ins().band(is_vec, is_int);

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

                    // Outside the lock - a destructor runs arbitrary code
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

    // vector-ref with no call, for either representation - flat is checked first
    // since its read is the cheaper one, and a mutable vector falls through to its
    // own inline rather than straight to the handler
    pub(super) fn inline_vector_ref(
        &mut self,
        vector: Value,
        index: Value,
        fallback: impl Fn(&mut Self) -> Value,
    ) -> Value {
        self.inline_flat_vector_ref(vector, index, |ctx| {
            ctx.inline_mut_vector_ref(vector, index, &fallback)
        })
    }

    // vector-ref on a mutable vector with no call: check the tags, bounds check,
    // then load under the lock. A clone only bumps a refcount, so unlike a drop it
    // is safe to run while the lock is held
    pub(super) fn inline_mut_vector_ref(
        &mut self,
        vector: Value,
        index: Value,
        fallback: impl Fn(&mut Self) -> Value,
    ) -> Value {
        let is_vec = self.is_type(vector, SteelVal::HEAP_REF_VECTOR_TAG);
        let is_int = self.is_type(index, SteelVal::INT_TAG);
        let both = self.builder.ins().band(is_vec, is_int);

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
                        ctx.with_heap_vec_lock(ptr, |ctx| {
                            let slot = ctx.heap_vec_element_address(ptr, idx);

                            let value = ctx.builder.ins().load(
                                types::I128,
                                MemFlagsData::trusted(),
                                slot,
                                0,
                            );

                            ctx.clone_value(value);
                            value
                        })
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
    // load. The element is cloned because the vector keeps its own copy
    /// Whether nothing but this thread can be looking at the bytevector, in
    /// which case the payload can be touched without taking the lock.
    ///
    /// Biased reference counting splits the count in two: the owner thread's
    /// half is a plain `Cell`, and every *other* thread's acquire lands in the
    /// shared atomic half. So a zero shared word means every live reference
    /// belongs to the owner - and since the caller is holding one, the caller
    /// *is* the owner. Zero rather than "counter is zero" on purpose: it also
    /// rules out `FLAG_MERGED` / `FLAG_QUEUED`, after which the biased half
    /// stops meaning anything.
    ///
    /// `strict` additionally demands the owner hold exactly one reference,
    /// which is the conservative reading. It is not needed for soundness -
    /// several references on one thread still cannot race - so it exists to be
    /// measured against.
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
    fn with_byte_vec_lock(
        &mut self,
        ptr: Value,
        body: impl Fn(&mut Self) -> Value,
    ) -> Value {
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
    /// Simpler than the vector equivalent: the element is a `u8`, so there is
    /// no refcount to bump - just a `uload8` and a retag as a fixnum.
    ///
    /// The locked region hands back a single value because the *fallback* must
    /// not run inside it: the spin lock is not reentrant and the primitive it
    /// calls takes the same lock. `-1` is the out-of-bounds sentinel, which a
    /// real byte can never be.
    pub(super) fn inline_bytes_ref(
        &mut self,
        bytevector: Value,
        index: Value,
        owned: bool,
        fallback: impl Fn(&mut Self) -> Value,
    ) -> Value {
        let is_bytes = self.is_type(bytevector, SteelVal::BYTEVECTOR_TAG);
        let is_int = self.is_type(index, SteelVal::INT_TAG);
        let both = self.builder.ins().band(is_bytes, is_int);

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
                        |ctx| ctx.builder.ins().iconst(types::I64, -1),
                        types::I64,
                    )
                });

                let in_bounds = ctx
                    .builder
                    .ins()
                    .icmp_imm_s(IntCC::SignedGreaterThanOrEqual, byte, 0);

                ctx.converging_if_else_cold(
                    in_bounds,
                    |ctx| {
                        // Exactly one of this arm and the fallback runs, and the
                        // fallback hands the value to a helper that consumes it,
                        // so releasing here is the whole story.
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

    /// `(bytes-set! bv i b)` with no call. No old value to drop - the slot is a
    /// byte - so this is a bounds check and an `istore8`. Returns 1 from the
    /// locked region when the store happened.
    pub(super) fn inline_bytes_set(
        &mut self,
        bytevector: Value,
        index: Value,
        byte: Value,
        owned: bool,
        fallback: impl Fn(&mut Self) -> Value,
    ) -> Value {
        let is_bytes = self.is_type(bytevector, SteelVal::BYTEVECTOR_TAG);
        let is_int = self.is_type(index, SteelVal::INT_TAG);
        let byte_is_int = self.is_type(byte, SteelVal::INT_TAG);
        let tags_ok = self.builder.ins().band(is_bytes, is_int);
        let tags_ok = self.builder.ins().band(tags_ok, byte_is_int);

        self.converging_if_else_cold(
            tags_ok,
            |ctx| {
                let ptr = ctx.unbox_value_to_pointer(bytevector);
                let idx = ctx.unbox_value_to_pointer(index);
                let val = ctx.unbox_value_to_pointer(byte);

                // Unsigned again: a negative byte is a very large unsigned one
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
                            ctx.builder.ins().iconst(types::I64, 1)
                        },
                        |ctx| ctx.builder.ins().iconst(types::I64, 0),
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

    pub(super) fn inline_flat_vector_ref(
        &mut self,
        vector: Value,
        index: Value,
        fallback: impl Fn(&mut Self) -> Value,
    ) -> Value {
        let is_flat = self.is_type(vector, SteelVal::FLAT_VECTOR_TAG);
        let is_int = self.is_type(index, SteelVal::INT_TAG);
        let both = self.builder.ins().band(is_flat, is_int);

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

                        let value = ctx.builder.ins().load(
                            types::I128,
                            MemFlagsData::trusted(),
                            slot,
                            0,
                        );

                        ctx.clone_value(value);
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
    // and a shared vector needs the lock, so both take the fallback
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
            ctx.call_function_returns_value_args("vector-push-args", &[vector, value])
        };

        let res = if INLINE_MUTABLE_VECTOR_OPS {
            self.inline_mut_vector_push(vector, value, fallback)
        } else {
            fallback(self)
        };

        self.push(res, InferredType::Any);

        self.ip += 1;
        self.check_deopt();
    }

    // The constructor just moves the arguments into a fresh allocation, so it
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
    /// `(lambda (this) (#%unbox (getter-proto this i)))`: the getter call is
    /// followed immediately by a one argument call to `#%unbox`. Reading the
    /// box out of the struct just to unbox it cost a refcount increment on the
    /// box (an out-of-line call, since boxes are not biased) and a drop right
    /// after - a quarter of conform's time. When that pair is what comes next,
    /// this returns the instruction to resume at once both are done: past the
    /// unbox's FUNC, or at the POPPURE after a tail call's TAILCALL.
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
                if *f as usize == crate::steel_vm::primitives::steel_unbox_mutable as usize
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
        // Only the fused getter-proto arm below sets this, and every exit from
        // it returns a result its caller takes; clearing it here means a stale
        // one can never be read as another call's.
        self.pending_borrow = None;

        let args = self.shadow_stack.get(self.shadow_stack.len() - arity..)?;

        match spec.typ {
            // TODO: We need to include the arity checks properly! The constructor / spec should be able to include
            // it and then we can make this happen properly with avoiding the checks for the arity!
            StructFunctionType::Constructor
                if CallStructConstructorsDefinitions::arity_to_name(arity).is_some() =>
            {
                // Fetch the name, then split off the args, and then do the thing.
                let name = CallStructConstructorsDefinitions::arity_to_name(arity).unwrap();

                let mut args = self
                    .split_off(arity)
                    .into_iter()
                    .map(|x| x.0)
                    .collect::<Vec<_>>();

                let descriptor = self
                    .builder
                    .ins()
                    .iconst(types::I64, spec.descriptor.key() as i64);

                args.insert(0, descriptor);

                let res = self.call_function_returns_value_args_no_context(name, &args);

                self.ip += 1;

                Some((res, InferredType::Any))
            }
            // crate::values::structs::StructFunctionType::Predicate => todo!(),
            // crate::values::structs::StructFunctionType::GetterProto => todo!(),

            // TODO: Check the type descriptor, and if the type descriptor
            // matches, then we continue on.
            StructFunctionType::GetterProtoVec(i)
                if matches!(
                    args,
                    &[MaybeStackValue::Register(_) | MaybeStackValue::MutRegister(_)]
                ) =>
            {
                let struct_arg_index = self.shadow_stack.last().unwrap().into_index();
                // Reference to the struct pointer on the stack itself
                let struct_ref = self.read_from_vm_stack(struct_arg_index);

                // Check the inferred type:
                let maybe_inferred_type = self
                    .properties
                    .get(&ValueOrRegister::Register(struct_arg_index));

                // Only fast path on immutable registers?
                // if matches!(self.shadow_stack.last(), Some(MaybeStackValue::Register(_))) {
                if let Some(Properties::InferredType(InferredType::Struct(desc))) =
                    maybe_inferred_type
                {
                    if spec.descriptor == desc {
                        let last_kind = self.shadow_stack.pop().unwrap();
                        let struct_ref_ptr = self.unbox_value_to_pointer(struct_ref);
                        let res = fast_path_struct_matches(i, struct_ref_ptr, self, None, None);

                        match last_kind {
                            // Move the value out, call drop on it
                            MaybeStackValue::MutRegister(i) => {
                                let void = self.encode_void();
                                self.write_to_vm_stack(i, void);

                                self.properties.props.insert(
                                    ValueOrRegister::Register(i),
                                    vec![Properties::InferredType(InferredType::Void)],
                                );
                            }
                            // Otherwise, we're fine?
                            MaybeStackValue::Register(_) => {
                                // Add the property
                                self.properties.add_property(
                                    ValueOrRegister::Register(struct_arg_index),
                                    Properties::InferredType(InferredType::Struct(spec.descriptor)),
                                );
                            }
                            _ => unreachable!(),
                        }

                        return Some((res, InferredType::Any));
                    }
                }
                // }

                let is_struct = self.is_type(struct_ref, SteelVal::STRUCT_TAG);
                let typ = self.int;

                // Take the operand out of its register up front so both arms start
                // from the same stack - the slow arm would otherwise do it in its
                // call's split_off and the fast arm not at all
                if let Some(MaybeStackValue::MutRegister(p)) = self.shadow_stack.last().copied() {
                    let (value, inferred_type) = self.mut_register_to_value(p);
                    *self.shadow_stack.last_mut().unwrap() = MaybeStackValue::Value(StackValue {
                        value,
                        inferred_type,
                        spilled: false,
                    });
                }

                let old_ip = self.ip;
                let stack = self.shadow_stack.clone();

                let res = self.converging_if(
                    is_struct,
                    |ctx| {
                        inline_struct_getter(&spec, arity, function_index, i, struct_ref, typ, ctx)
                    },
                    // TODO: This should share the same branch with the false code?
                    // Basically, we're raising an error
                    move |ctx| {
                        ctx.ip = old_ip;
                        ctx.shadow_stack = stack.clone();
                        slow_path_struct_getter(arity, function_index, ctx)
                    },
                    typ,
                );

                Some((res, InferredType::Any))
            }
            // A `#:mutable` struct's accessors are generated as closures over a
            // box - `(lambda (this) (#%unbox (getter-proto this 0)))` - so after
            // the wrapper is inlined the call left behind is the *generic*
            // two-argument prototype with a literal index, rather than the
            // one-argument specialised prototype an immutable struct gets.
            // Everything past pulling that index out is identical to the
            // `GetterProtoVec` case above.
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

                // Saved before the index operand is dropped: every slow arm has
                // to re-enter the real two argument call.
                let old_ip = self.ip;
                let stack = self.shadow_stack.clone();

                // The index is a compile time constant, so nothing is lost by
                // never materialising it.
                self.shadow_stack.pop();

                let struct_arg_index = self.shadow_stack.last().unwrap().into_index();
                let struct_ref = self.read_from_vm_stack(struct_arg_index);

                let maybe_inferred_type = self
                    .properties
                    .get(&ValueOrRegister::Register(struct_arg_index));

                if let Some(Properties::InferredType(InferredType::Struct(desc))) =
                    maybe_inferred_type
                {
                    if spec.descriptor == desc {
                        let last_kind = self.shadow_stack.pop().unwrap();
                        let struct_ref_ptr = self.unbox_value_to_pointer(struct_ref);
                        let res = fast_path_struct_matches(i, struct_ref_ptr, self, fuse, borrow);

                        match last_kind {
                            MaybeStackValue::MutRegister(r) => {
                                let void = self.encode_void();
                                self.write_to_vm_stack(r, void);

                                self.properties.props.insert(
                                    ValueOrRegister::Register(r),
                                    vec![Properties::InferredType(InferredType::Void)],
                                );
                            }
                            MaybeStackValue::Register(_) => {
                                self.properties.add_property(
                                    ValueOrRegister::Register(struct_arg_index),
                                    Properties::InferredType(InferredType::Struct(spec.descriptor)),
                                );
                            }
                            _ => unreachable!(),
                        }

                        return Some((res, InferredType::Any));
                    }
                }

                let is_struct = self.is_type(struct_ref, SteelVal::STRUCT_TAG);
                let typ = self.int;

                // Take the operand out of its register up front so both arms
                // start from the same stack.
                if let Some(MaybeStackValue::MutRegister(p)) = self.shadow_stack.last().copied() {
                    let (value, inferred_type) = self.mut_register_to_value(p);
                    *self.shadow_stack.last_mut().unwrap() = MaybeStackValue::Value(StackValue {
                        value,
                        inferred_type,
                        spilled: false,
                    });
                }

                let inner_stack = stack.clone();

                let res = self.converging_if(
                    is_struct,
                    |ctx| {
                        inline_struct_getter_proto(
                            &spec,
                            function_index,
                            i,
                            struct_ref,
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
                        let value = slow_path_struct_getter(GETTER_PROTO_ARITY, function_index, ctx);
                        finish_slow_getter(value, fuse, borrow, ctx)
                    },
                    typ,
                );

                Some((res, InferredType::Any))
            }
            // A struct predicate is a tag check and a descriptor compare - no
            // field access, no allocation, and at the correct arity no error
            // path at all. This arm used to be a `todo!()`, so all of them
            // deopted.
            //
            // Restricted to a borrowed register operand: the predicate does not
            // retain its argument, so with a register there is no ownership to
            // transfer and nothing to drop. A moved-out or owned operand would
            // need that bookkeeping, and is rare enough not to be worth the
            // risk here - it still takes the old path.
            StructFunctionType::Predicate
                if predicate_inlining_enabled()
                    && matches!(args, &[MaybeStackValue::Register(_)]) =>
            {
                let operand_index = self.shadow_stack.last().unwrap().into_index();
                let value = self.read_from_vm_stack(operand_index);
                self.shadow_stack_pop();

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
                        let matches = ctx.builder.ins().icmp_imm_s(
                            IntCC::Equal,
                            on_heap,
                            descriptor as i64,
                        );
                        let widened = ctx.builder.ins().uextend(types::I64, matches);
                        ctx.encode_value(discriminant(&SteelVal::BoolV(true)) as i64, widened)
                    },
                    // Not a struct at all, so certainly not this one.
                    |ctx| {
                        let zero = ctx.builder.ins().iconst(types::I64, 0);
                        ctx.encode_value(discriminant(&SteelVal::BoolV(true)) as i64, zero)
                    },
                    typ,
                );

                self.ip += 1;

                Some((res, InferredType::Bool))
            }

            _ => {
                return None;
            }
        }
    }
}

/// The generic struct getter prototype always takes `(struct, index)`.
const GETTER_PROTO_ARITY: usize = 2;

/// `STEEL_JIT_GETTER_PROTO=0` turns off inlining of the two argument struct
/// getter prototype, for A/B measurement.
/// `STEEL_JIT_PREDICATE=0` turns off inlining of struct predicates.
fn predicate_inlining_enabled() -> bool {
    static ENABLED: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ENABLED.get_or_init(|| {
        !matches!(
            std::env::var("STEEL_JIT_PREDICATE").ok().as_deref(),
            Some("0") | Some("false")
        )
    })
}

/// `STEEL_JIT_FUSE_GETTER_UNBOX=0` turns off reading a mutable struct field
/// straight through its box. On by default.
fn getter_unbox_fusion_enabled() -> bool {
    static ENABLED: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ENABLED.get_or_init(|| {
        !matches!(
            std::env::var("STEEL_JIT_FUSE_GETTER_UNBOX").ok().as_deref(),
            Some("0") | Some("false")
        )
    })
}

fn getter_proto_inlining_enabled() -> bool {
    static ENABLED: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ENABLED.get_or_init(|| {
        !matches!(
            std::env::var("STEEL_JIT_GETTER_PROTO").ok().as_deref(),
            Some("0") | Some("false")
        )
    })
}

/// `inline_struct_getter`, but for the two argument prototype: its slow arm has
/// to re-enter a call whose index operand this pass already dropped, so it
/// restores the saved operand stack rather than pushing back the one value it
/// popped.
#[allow(clippy::too_many_arguments)]
fn inline_struct_getter_proto(
    spec: &StructConstructorRefSpec,
    function_index: usize,
    i: usize,
    struct_ref: Value,
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
        |ctx| fast_path_struct_matches(i, struct_ref_ptr, ctx, fuse, borrow),
        move |ctx| {
            ctx.ip = old_ip;
            ctx.shadow_stack = saved.clone();
            let value = slow_path_struct_getter(GETTER_PROTO_ARITY, function_index, ctx);
            finish_slow_getter(value, fuse, borrow, ctx)
        },
        typ,
    )
}

fn inline_struct_getter(
    spec: &StructConstructorRefSpec,
    arity: usize,
    function_index: usize,
    i: usize,
    struct_ref: Value,
    typ: Type,
    ctx: &mut FunctionTranslator,
) -> Value {
    // Check the descriptor first
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

    let last = ctx.shadow_stack.pop().unwrap();


    let res = ctx.converging_if(
        struct_matches,
        |ctx| fast_path_struct_matches(i, struct_ref_ptr, ctx, None, None),
        |ctx| {
            // call_global_function takes self.ip as the deopt fallback and then
            // advances one, so this has to enter on the call itself - the same
            // as the outer slow path does
            ctx.shadow_stack.push(last);
            slow_path_struct_getter(arity, function_index, ctx)
        },
        typ,
    );

    res
}

fn fast_path_struct_matches(
    i: usize,
    struct_ref_ptr: Value,
    ctx: &mut FunctionTranslator<'_>,
    fuse: Option<usize>,
    borrow: Option<Variable>,
) -> Value {
    // The fields sit inline right after the header, so the element address is a
    // constant displacement off the struct pointer - no separate buffer to load.
    let size: i64 = std::mem::size_of::<SteelVal>() as _;

    let offset = StructStorage::data_offset() as i64 + (i as i64 * size);

    let slot_ptr = ctx.builder.ins().iadd_imm_s(struct_ref_ptr, offset);

    let local_value = ctx
        .builder
        .ins()
        .load(types::I128, MemFlagsData::trusted(), slot_ptr, 0);

    if let Some(resume) = fuse {
        // The field is the box and the unbox comes straight after: read through
        // it while it is still borrowed from the struct - which holds it alive
        // for the length of the read - so only the value inside is cloned. The
        // box itself is never cloned, and so never dropped.
        let value = match borrow {
            // With the struct still in its register, the contents can stay
            // borrowed as well.
            Some(owned) => ctx.unbox_value_borrowed(local_value, owned),
            None => ctx.unbox_value_checked_register(local_value, false),
        };
        ctx.ip = resume;
        return value;
    }

    // Clone whatever comes out of this
    ctx.clone_value(local_value);

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
        // A pure error path, so the interpreter never resumes from it - no need
        // to materialise the operand stack, which lets the fast arm skip it too
        let result = ctx.call_global_function_no_spill(arity, name, function_index);
        ctx.check_deopt();

        // Assuming this worked, we'll want to push this result on to the stack.
        result
    } else {
        let name = "call-global-spilled";

        let v = ctx.call_global_function_spilled(arity, name, function_index, false);
        ctx.check_deopt();

        v
    }
}

// TODO: Use this now for anything that is boxed:
// i.e. Vectors and boxes
//
// Box get / unbox should now be inlineable.
// Vector ref / set should now be inlineable (once we swap the vector implementation)
// to the custom one.
//
// Struct refs should now be inlineable.
// The jit's own acquire/release, matching values::lock::SpinLock. Cranelift's
// atomics are sequentially consistent, so this is stronger than the rust side's
// Acquire/Release - they interoperate, generated code just pays a bit more.
//
// lock_ptr is the base of the SpinLock, so the flag has to live at offset 0.
//
// unbox_value_checked_register reaches the payload with SpinLock<SteelVal>'s
// data offset, which only lands right while HeapAllocated keeps value first.
const _: () = {
    assert!(SpinLock::<SteelVal>::lock_offset() == 0);
    assert!(core::mem::offset_of!(HeapAllocated<SteelVal>, value) == 0);
};

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
    vm_context: Value,
    function_context: Option<usize>,
    potentially_could_deopt: bool,
    tier: JitTier,
    thread_pointer: Option<Value>,
    should_trampoline: Option<Value>,
    sp: Option<Value>,
    pop_count: Option<Value>,
    pop_count_plus_one: Option<Value>,
    pop_count_minus_one: Option<Value>,
    thread_id: Option<Value>,
    compilation_stats: CompilationStats,
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
            vm_context: self.vm_context,
            function_context: self.function_context,
            potentially_could_deopt: self.potentially_could_deopt,
            tier: self.tier,
            thread_pointer: self.thread_pointer,
            should_trampoline: self.should_trampoline,
            sp: self.sp,
            pop_count: self.pop_count,
            pop_count_plus_one: self.pop_count_plus_one,
            pop_count_minus_one: self.pop_count_minus_one,
            thread_id: self.thread_id,
            compilation_stats: self.compilation_stats.clone(),
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
        self.vm_context = state.vm_context;
        self.function_context = state.function_context;
        self.potentially_could_deopt = state.potentially_could_deopt;
        self.tier = state.tier;
        self.thread_pointer = state.thread_pointer;
        self.should_trampoline = state.should_trampoline;
        self.sp = state.sp;
        self.pop_count = state.pop_count;
        self.pop_count_plus_one = state.pop_count_plus_one;
        self.pop_count_minus_one = state.pop_count_minus_one;
        self.thread_id = state.thread_id;
        self.compilation_stats = state.compilation_stats;
    }
}

