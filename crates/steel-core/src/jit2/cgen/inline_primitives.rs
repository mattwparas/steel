//! Which primitive calls the jit lowers inline, in one table.
//!
//! A call to a primitive (a global bound to a native function) normally goes
//! out through a generic helper. The primitives in [`inline_primitives`] have
//! lowerings of their own. Each entry says which function it is, for which
//! arity, whether it applies in operand position and in tail position (each
//! behind its own switch), any further shape check, and how it lowers.
//! `try_inline_primitive` and `try_inline_primitive_tail` are the only
//! dispatchers, so recognising a new primitive is one entry here.

use super::*;

type PushLowering = Box<dyn for<'a> Fn(&mut FunctionTranslator<'a>, usize) + Send + Sync>;
type ReturnLowering =
    Box<dyn for<'a> Fn(&mut FunctionTranslator<'a>, usize) -> Option<Value> + Send + Sync>;

/// How an entry emits its code, given the call's arity. Either way it consumes
/// its operands off the operand stack.
pub(super) enum Lowering {
    /// Pushes the result and steps past the call itself, as in operand
    /// position. In tail position the result is popped back off and returned.
    Pushes(PushLowering),
    /// Returns the result, or `None` with nothing emitted when this site
    /// doesn't suit the lowering. In operand position it is pushed
    /// with `InferredType::Any`.
    Returns(ReturnLowering),
}

fn pushes(
    lower: impl for<'a> Fn(&mut FunctionTranslator<'a>, usize) + Send + Sync + 'static,
) -> Lowering {
    Lowering::Pushes(Box::new(lower))
}

fn returns(
    lower: impl for<'a> Fn(&mut FunctionTranslator<'a>, usize) -> Option<Value> + Send + Sync + 'static,
) -> Lowering {
    Lowering::Returns(Box::new(lower))
}

pub(super) struct PrimitiveInline {
    /// For reading the table. Dispatch uses `target`.
    #[allow(dead_code)]
    pub(super) name: &'static str,
    /// The native function, as a pointer.
    pub(super) target: usize,
    /// `None` takes any arity and leaves it to `guard`.
    pub(super) arity: Option<usize>,
    /// Whether it applies in operand position, and in tail position.
    pub(super) operand: fn() -> bool,
    pub(super) tail: fn() -> bool,
    /// A check on the operands at this site.
    pub(super) guard: for<'a> fn(&FunctionTranslator<'a>, usize) -> bool,
    pub(super) lower: Lowering,
    /// The type a `Returns` lowering's result is pushed with.
    pub(super) result: InferredType,
}

fn always() -> bool {
    true
}

fn never() -> bool {
    false
}

fn any_shape(_: &FunctionTranslator<'_>, _: usize) -> bool {
    true
}

fn extra() -> bool {
    extra_primitive_tail_calls_enabled()
}

fn extra_pred() -> bool {
    extra_primitive_tail_calls_enabled() && tail_pred_enabled()
}

fn extra_cdr() -> bool {
    extra_primitive_tail_calls_enabled() && tail_cdr_enabled()
}

fn extra_rev() -> bool {
    extra_primitive_tail_calls_enabled() && tail_reverse_enabled()
}

fn extra_vec() -> bool {
    extra_primitive_tail_calls_enabled() && tail_vec_enabled()
}

fn divmod() -> bool {
    inline_divmod_enabled()
}

fn bitwise() -> bool {
    inline_bitwise_enabled()
}

fn rounding() -> bool {
    inline_rounding_enabled()
}

fn bytevector() -> bool {
    inline_bytevector_enabled()
}

// Reached from an inlined accessor's `(#%unbox (getter x))`, which is an
// operand position call, so the tail call switch also gates operand position.
fn boxes() -> bool {
    inline_primitive_tail_calls_enabled()
}

/// An entry for `target` lowered by `lower`. By default it applies in operand
/// position, not in tail position, at any arity, with no further shape check.
/// Override with the methods below.
fn inline(name: &'static str, target: *const (), lower: Lowering) -> PrimitiveInline {
    PrimitiveInline {
        name,
        target: target as usize,
        arity: None,
        operand: always,
        tail: never,
        guard: any_shape,
        lower,
        result: InferredType::Any,
    }
}

impl PrimitiveInline {
    fn arity(mut self, arity: usize) -> Self {
        self.arity = Some(arity);
        self
    }

    fn operand(mut self, gate: fn() -> bool) -> Self {
        self.operand = gate;
        self
    }

    fn tail(mut self, gate: fn() -> bool) -> Self {
        self.tail = gate;
        self
    }

    fn guard(mut self, guard: for<'a> fn(&FunctionTranslator<'a>, usize) -> bool) -> Self {
        self.guard = guard;
        self
    }

    fn result(mut self, result: InferredType) -> Self {
        self.result = result;
        self
    }
}

fn divmod_entry(name: &'static str, target: FunctionSignature, mode: DivMode) -> PrimitiveInline {
    // The fallback calls the primitive itself.
    inline(
        name,
        target as *const (),
        returns(move |t, _| Some(t.inline_int_divmod(mode, target))),
    )
    .arity(2)
    .operand(divmod)
    .tail(divmod)
}

fn rounding_entry(name: &'static str, target: FunctionSignature, op: RoundOp) -> PrimitiveInline {
    inline(
        name,
        target as *const (),
        returns(move |t, _| Some(t.inline_rounding(op, target))),
    )
    .arity(1)
    .operand(rounding)
    .tail(rounding)
}

fn bitwise_entry(name: &'static str, target: FunctionSignature, op: BitwiseOp) -> PrimitiveInline {
    inline(
        name,
        target as *const (),
        returns(move |t, _| Some(t.inline_int_bitwise(op, target))),
    )
    .arity(2)
    .operand(bitwise)
    .tail(bitwise)
    .result(InferredType::Int)
}

/// Every primitive with an inline lowering.
pub(super) fn inline_primitives() -> &'static [PrimitiveInline] {
    static TABLE: std::sync::LazyLock<Vec<PrimitiveInline>> = std::sync::LazyLock::new(|| {
        use crate::primitives::lists::{steel_cdr, steel_reverse};
        use crate::primitives::vectors::{list_vec_null, mut_vec_construct};
        use crate::steel_vm::primitives::{
            make_mutable_struct, steel_set_box_mutable, steel_unbox_mutable,
        };

        vec![
            // Type predicates.
            inline(
                "string?",
                steel_stringp as *const (),
                pushes(|t, _| t.is_string()),
            )
            .arity(1),
            inline(
                "list?",
                steel_listp as *const (),
                pushes(|t, _| t.is_list()),
            )
            .arity(1),
            inline(
                "void?",
                steel_voidp as *const (),
                pushes(|t, _| t.is_void()),
            )
            .arity(1),
            inline(
                "eof-object?",
                steel_eof_objectp as *const (),
                pushes(|t, _| t.eof_object()),
            )
            .arity(1),
            inline(
                "symbol?",
                steel_symbolp as *const (),
                pushes(|t, _| t.is_symbol()),
            )
            .arity(1),
            inline("pair?", steel_pair as *const (), pushes(|t, _| t.is_pair()))
                .arity(1)
                .tail(extra_pred),
            inline(
                "empty?",
                steel_is_empty as *const (),
                pushes(|t, _| t.is_empty()),
            )
            .arity(1)
            .tail(extra_pred),
            // `null?` in operand position is its own opcode. In tail position
            // the empty list is the case worth inlining; the primitive also
            // answers for empty vectors, which the helper handles.
            inline(
                "null?",
                list_vec_null as *const (),
                returns(|t, _| Some(t.inline_null_predicate())),
            )
            .arity(1)
            .operand(never)
            .tail(extra_pred),
            // Equality and membership.
            inline("eq?", steel_eq as *const (), pushes(|t, _| t.eq()))
                .arity(2)
                .tail(extra),
            inline(
                "char-whitespace?",
                crate::primitives::strings::steel_char_is_whitespace as *const (),
                pushes(|t, _| t.char_whitespace()),
            )
            .arity(1)
            .tail(extra_pred),
            // The same comparison as `eq?`, which is what `case` expands to.
            inline("eqv?", steel_eqv as *const (), pushes(|t, _| t.eq()))
                .arity(2)
                .tail(extra),
            inline(
                "char=?",
                steel_char_equals as *const (),
                pushes(|t, arity| t.char_equals(arity)),
            )
            .arity(2),
            inline(
                "#%case-member?",
                crate::steel_vm::primitives::steel_case_member as *const (),
                returns(|t, _| t.case_member_constant()),
            )
            .arity(2)
            .result(InferredType::UnboxedBool),
            inline(
                "memq",
                steel_memq as *const (),
                returns(|t, _| t.memq_constant_list()),
            )
            .arity(2),
            inline(
                "list-contains",
                steel_list_contains as *const (),
                pushes(|t, _| t.list_contains()),
            )
            .arity(2),
            // Vectors.
            inline(
                "vector-set!",
                steel_mut_vec_set as *const (),
                pushes(|t, _| t.vector_set()),
            )
            .arity(3)
            .tail(extra),
            inline(
                "vector-length",
                crate::primitives::vectors::vec_length as *const (),
                pushes(|t, _| t.vector_length()),
            )
            .arity(1)
            .tail(extra_vec),
            inline(
                "vector-push!",
                mut_vec_push as *const (),
                pushes(|t, _| t.vector_push()),
            )
            .arity(2),
            inline(
                "flat vector",
                flat_vector_construct as *const (),
                pushes(|t, arity| t.flat_vector_construct(arity)),
            )
            .guard(|_, arity| {
                CallFlatVectorConstructorsDefinitions::arity_to_name(arity).is_some()
            }),
            inline(
                "mutable-vector",
                mut_vec_construct as *const (),
                returns(|t, arity| {
                    t.inline_mutable_vector_tail_call(
                        mut_vec_construct as *const () as usize,
                        arity,
                    )
                }),
            )
            .operand(never)
            .tail(extra_vec),
            inline(
                "bytes-ref",
                steel_bytes_ref as *const (),
                returns(|t, arity| Some(t.inline_byte_vector_op(steel_bytes_ref, arity))),
            )
            .arity(2)
            .operand(bytevector)
            .guard(|t, arity| t.byte_vector_shape_ok(arity)),
            inline(
                "bytes-set!",
                steel_bytes_set as *const (),
                returns(|t, arity| Some(t.inline_byte_vector_op(steel_bytes_set, arity))),
            )
            .arity(3)
            .operand(bytevector)
            .guard(|t, arity| t.byte_vector_shape_ok(arity)),
            // Integer division, in both operand and tail position.
            divmod_entry("quotient", quotient, DivMode::TruncQuotient),
            divmod_entry(
                "truncate-quotient",
                truncate_quotient,
                DivMode::TruncQuotient,
            ),
            divmod_entry("remainder", remainder, DivMode::TruncRemainder),
            divmod_entry(
                "truncate-remainder",
                truncate_remainder,
                DivMode::TruncRemainder,
            ),
            divmod_entry("floor-quotient", floor_quotient, DivMode::FloorQuotient),
            divmod_entry("modulo", modulo, DivMode::FloorRemainder),
            divmod_entry("floor-remainder", floor_remainder, DivMode::FloorRemainder),
            rounding_entry("floor", steel_floor, RoundOp::Floor),
            rounding_entry("ceiling", steel_ceiling, RoundOp::Ceiling),
            rounding_entry("truncate", steel_truncate, RoundOp::Truncate),
            rounding_entry("round", steel_round, RoundOp::Round),
            rounding_entry("exact", steel_exact, RoundOp::Exact),
            bitwise_entry("bitwise-and", bitwise_and, BitwiseOp::And),
            bitwise_entry("bitwise-ior", bitwise_ior, BitwiseOp::Ior),
            bitwise_entry("bitwise-xor", bitwise_xor, BitwiseOp::Xor),
            // Boxes. Mutable struct fields are boxes, so `(#%unbox (getter ...))`
            // and `#%set-box!` are common.
            inline(
                "#%unbox",
                steel_unbox_mutable as *const (),
                returns(|t, arity| {
                    t.inline_box_primitive(steel_unbox_mutable as *const () as usize, arity)
                }),
            )
            .arity(1)
            .operand(boxes)
            .tail(always)
            .guard(|t, arity| {
                t.box_primitive_inlinable(steel_unbox_mutable as *const () as usize, arity)
            }),
            inline(
                "#%set-box!",
                steel_set_box_mutable as *const (),
                returns(|t, arity| {
                    t.inline_box_primitive(steel_set_box_mutable as *const () as usize, arity)
                }),
            )
            .arity(2)
            .operand(boxes)
            .tail(always)
            .guard(|t, arity| {
                t.box_primitive_inlinable(steel_set_box_mutable as *const () as usize, arity)
            }),
            // Structs.
            inline(
                "#%make-mutable-struct",
                make_mutable_struct as *const (),
                returns(|t, arity| {
                    t.inline_mutable_struct_tail_call(
                        make_mutable_struct as *const () as usize,
                        arity,
                    )
                }),
            )
            .operand(never)
            .tail(extra),
            // Lists.
            inline(
                "cdr",
                steel_cdr as *const (),
                returns(|t, arity| {
                    t.inline_mut_func_tail_call(steel_cdr as *const () as usize, arity)
                }),
            )
            .operand(never)
            .tail(extra_cdr),
            inline(
                "reverse",
                steel_reverse as *const (),
                returns(|t, arity| {
                    t.inline_mut_func_tail_call(steel_reverse as *const () as usize, arity)
                }),
            )
            .operand(never)
            .tail(extra_rev),
        ]
    });
    &TABLE
}

pub(super) fn find(target: usize, arity: usize) -> Option<&'static PrimitiveInline> {
    inline_primitives()
        .iter()
        .find(|entry| entry.target == target && entry.arity.is_none_or(|a| a == arity))
}

impl FunctionTranslator<'_> {
    /// Lower a call to the primitive `target` in operand position, if it has
    /// an inline lowering here. `false` (nothing emitted) means use the
    /// generic call.
    pub(super) fn try_inline_primitive(&mut self, target: usize, arity: usize) -> bool {
        let Some(entry) = find(target, arity) else {
            return false;
        };
        if !(entry.operand)() || !(entry.guard)(self, arity) {
            return false;
        }
        match &entry.lower {
            Lowering::Pushes(lower) => lower(self, arity),
            Lowering::Returns(lower) => {
                let Some(value) = lower(self, arity) else {
                    return false;
                };
                self.push(value, entry.result);
                self.ip += 1;
            }
        }
        true
    }

    /// The same in tail position: the value to return, or `None` (nothing
    /// emitted) for the generic tail call.
    pub(super) fn try_inline_primitive_tail(
        &mut self,
        target: usize,
        arity: usize,
    ) -> Option<Value> {
        let entry = find(target, arity)?;
        if !(entry.tail)() || !(entry.guard)(self, arity) {
            return None;
        }
        match &entry.lower {
            // Run the operand position lowering and pop its result. Its `ip`
            // step doesn't matter, since the tail call arm moves past the end
            // of the function once this returns.
            Lowering::Pushes(lower) => {
                lower(self, arity);
                Some(self.pop_boxed())
            }
            Lowering::Returns(lower) => lower(self, arity),
        }
    }
}
