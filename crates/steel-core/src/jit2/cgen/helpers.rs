//! The runtime helpers jitted code calls, declared once.
//!
//! Each line of [`jit_helpers!`] below is one helper: the constant jitted code
//! refers to it by, the symbol it is registered under, the Rust function, and
//! its signature. The macro generates the registration (`register_helpers`) and
//! a typed handle. `FunctionTranslator::call_helper` takes the handle with as
//! many arguments as the helper has, so a wrong count is a compile error instead
//! of a verifier failure or a crash in generated code.
//!
//! A `ctx` helper takes the `*mut VmCore` first and gets it implicitly. A `plain`
//! one takes only what is listed. `ctx_raises` / `plain_raises` mark a helper
//! that can raise, which changes how it may be called (see [`Helper`]).
//! [`KEEPS_STACK`] lists the helpers that never move the vm's value stack.
//!
//! Helpers that come in one function per arity (`helper_family!` in
//! `steel_vm/vm/jit.rs`) are registered with their families, and called by the
//! name their family gives for an arity.

use super::*;

/// A helper declared in [`jit_helpers!`]. `N` is the argument count not counting
/// the vm context, `CTX` whether it takes the context first, `RET` whether it
/// returns a value, and `RAISES` whether it can raise an error or otherwise hand
/// control back to the interpreter (by clearing `VmCore::is_native`).
///
/// A raising helper can only be called through `call_helper_checked`, which
/// leaves native code right after the call if it raised, or through
/// `call_helper_unchecked`, where the caller checks later itself. Forgetting the
/// check doesn't compile. Continuing after a raise runs native code on a garbage
/// result.
#[derive(Clone, Copy, Debug)]
pub(super) struct Helper<const N: usize, const CTX: bool, const RET: bool, const RAISES: bool> {
    pub(super) name: &'static str,
}

macro_rules! helper_count {
    () => { 0usize };
    ($head:ty $(, $tail:ty)*) => { 1usize + helper_count!($($tail),*) };
}

macro_rules! helper_is_ctx {
    (ctx) => {
        true
    };
    (ctx_raises) => {
        true
    };
    (plain) => {
        false
    };
    (plain_raises) => {
        false
    };
}

macro_rules! helper_raises {
    (ctx) => {
        false
    };
    (ctx_raises) => {
        true
    };
    (plain) => {
        false
    };
    (plain_raises) => {
        true
    };
}

macro_rules! helper_returns {
    () => {
        false
    };
    ($ret:ty) => {
        true
    };
}

macro_rules! register_helper {
    ($map:ident, ctx_raises, $($rest:tt)*) => {
        register_helper!($map, ctx, $($rest)*)
    };
    ($map:ident, plain_raises, $($rest:tt)*) => {
        register_helper!($map, plain, $($rest)*)
    };
    ($map:ident, ctx, $name:literal, $f:ident, ($($p:ty),*), ($($ret:ty)?)) => {
        $map.add_func($name, abi! { $f as fn(*mut VmCore $(, $p)*) $(-> $ret)? })
    };
    ($map:ident, plain, $name:literal, $f:ident, ($($p:ty),*), ($($ret:ty)?)) => {
        $map.add_func2($name, abi! { $f as fn($($p),*) $(-> $ret)? })
    };
}

macro_rules! jit_helpers {
    ($(
        $konst:ident = $name:literal => $f:ident : $kind:ident fn($($p:ty),* $(,)?) $(-> $ret:ty)?;
    )*) => {
        $(
            // Some are only reached by a name built at run time, or not at all
            // (the debugging helpers); every registered helper is listed anyway.
            #[allow(dead_code)]
            pub(super) const $konst: Helper<
                { helper_count!($($p),*) },
                { helper_is_ctx!($kind) },
                { helper_returns!($($ret)?) },
                { helper_raises!($kind) },
            > = Helper { name: $name };
        )*

        pub(super) fn register_helpers(map: &mut FunctionMap) {
            $(register_helper!(map, $kind, $name, $f, ($($p),*), ($($ret)?));)*
        }
    };
}

jit_helpers! {
    DEBUG_STEEL_VALUE = "#%debug-steel-value" => debug_value: plain fn(SteelVal);
    DEBUG_VALUE = "#%debug-value" => debug_int: plain fn(i64);
    DEBUG_COUNT = "#%debug-count" => debug_count: plain fn(i32);
    RECORD_SPECULATION_DEOPT = "#%record-speculation-deopt" => record_speculation_deopt: plain fn(i64);
    DEBUG_TAG = "#%debug-tag" => debug_tag: plain fn(i8);
    DEBUG_INSTRUCTIONS = "#%debug-instructions" => debug_instructions: plain fn(RootedInstructions);
    DEBUG_INSTRUCTIONS2 = "#%debug-instructions2" => debug_instructions2: plain fn(RootedInstructions);
    DEBUG_STACK_FRAMES = "#%debug-stack-frames" => debug_stack_frames: ctx fn();
    DEBUG_INSTRUCTIONS_BEFORE = "#%debug-instructions-before" => debug_instructions_before: ctx fn();
    DEBUG_INSTRUCTIONS_AFTER = "#%debug-instructions-after" => debug_instructions_after: ctx fn();
    DEBUG_STACK_BEFORE = "#%debug-stack-before" => debug_stack_before: ctx fn();
    DEBUG_STACK_AFTER = "#%debug-stack-after" => debug_stack_after: ctx fn();
    DEBUG_IS_NATIVE = "#%debug-is-native" => debug_is_native: ctx fn();
    PAIR_P = "pair?" => is_pair_c_reg: ctx fn(usize) -> SteelVal;
    PAIR_P_VALUE = "pair?-value" => is_pair_value: plain fn(SteelVal) -> SteelVal;
    LIST_P_VALUE = "list?-value" => is_list_value: plain fn(SteelVal) -> SteelVal;
    VOID_P = "void?" => is_void_c_reg: ctx fn(usize) -> SteelVal;
    VOID_P_VALUE = "void?-value" => is_void_value: plain fn(SteelVal) -> SteelVal;
    STRING_P = "string?" => is_string_c_reg: ctx fn(usize) -> SteelVal;
    STRING_P_VALUE = "string?-value" => is_string_value: plain fn(SteelVal) -> SteelVal;
    SYMBOL_P = "symbol?" => is_symbol_c_reg: ctx fn(usize) -> SteelVal;
    SYMBOL_P_VALUE = "symbol?-value" => is_symbol_value: plain fn(SteelVal) -> SteelVal;
    EMPTY_P = "empty?" => is_empty_c_reg: ctx fn(usize) -> SteelVal;
    LIST_CURSOR_MATERIALIZE = "list-cursor-materialize" => list_cursor_materialize: plain fn(usize, usize) -> SteelVal;
    LIST_CURSOR_MATERIALIZE_OWNED = "list-cursor-materialize-owned" => list_cursor_materialize_owned: plain fn(SteelVal, usize, usize) -> SteelVal;
    EMPTY_P_VALUE = "empty?-value" => is_empty_value: plain fn(SteelVal) -> SteelVal;
    IF_BRANCH_REGISTER = "if-branch-register" => if_handler_register: ctx fn(u64) -> bool;
    NOT_VALUE = "not-value" => not_handler_raw_value: ctx fn(SteelVal) -> SteelVal;
    CALL_GLOBAL_TAIL_SPILLED = "call-global-tail-spilled" => callglobal_tail_handler_deopt_spilled: ctx_raises fn(usize, usize, usize) -> SteelVal;
    CALL_GLOBAL_SPILLED = "call-global-spilled" => call_global_function_deopt_spilled: ctx_raises fn(usize, usize, usize) -> SteelVal;
    CALL_FUNCTION_SPILLED = "call-function-spilled" => call_function_deopt_spilled: ctx_raises fn(SteelVal, usize, usize) -> SteelVal;
    CALL_FUNCTION_TAIL_SPILLED = "call-function-tail-spilled" => call_function_tail_deopt_spilled: ctx_raises fn(SteelVal, usize, usize) -> SteelVal;
    LIST_HANDLER_SPILLED = "list-handler-spilled" => list_handler_c: ctx fn(usize) -> SteelVal;
    VARIADIC_NUMERIC_SPILLED = "variadic-numeric-spilled" => variadic_numeric_spilled: ctx_raises fn(usize, usize) -> SteelVal;
    VEC_HANDLER_SPILLED = "vec-handler-spilled" => vec_handler_c: ctx fn(usize) -> SteelVal;
    NUM_EQUAL_VALUE_BOOL = "num-equal-value-bool" => num_equal_value_bool: ctx_raises fn(SteelVal, SteelVal) -> bool;
    NUM_EQUAL_INT = "num-equal-int" => num_equal_int: ctx_raises fn(SteelVal, SteelVal) -> SteelVal;
    NUM_EQUAL_INT_REGISTER = "num-equal-int-register" => num_equal_int_register: ctx_raises fn(usize, SteelVal) -> bool;
    EQUAL_BINOP_BOOL = "equal-binop-bool" => equal_binop_bool: ctx fn(SteelVal, SteelVal) -> bool;
    EQUAL_BINOP_REGISTER_BOOL = "equal-binop-register-bool" => equal_binop_register_bool: ctx fn(usize, SteelVal) -> bool;
    DROP_VALUE = "drop-value" => drop_value: ctx fn(SteelVal);
    DROP_ONE = "drop-one" => drop_one: plain fn(SteelVal);
    DROP_BOX = "drop-box" => drop_box: plain fn(crate::values::closed::HeapRef<SteelVal>);
    CLONE_STD_RC = "#%clone-std-rc" => clone_one: plain fn(SteelVal);
    DROP_VALUE_POST_FAST_DEC = "drop-value-post-fast-dec" => drop_value_post_fast_decrement: plain fn(SteelVal);
    DROP_VALUE_POST_FAST_DEC_CLOSURE = "drop-value-post-fast-dec-closure" => drop_value_post_fast_decrement_closure: plain fn(Gc<ByteCodeLambda>);
    DROP_VALUE_SLOW_DEC = "drop-value-slow-dec" => drop_value_slow_decrement: plain fn(SteelVal);
    DROP_VALUE_SLOW_DEC_CLOSURE = "drop-value-slow-dec-closure" => drop_value_slow_decrement_closure: plain fn(Gc<ByteCodeLambda>);
    RAW_SLOW_INCREMENT_CLOSURE = "raw-slow-increment-closure" => increment_ref_count_slow_closure: plain fn(Gc<ByteCodeLambda>);
    HANDLE_ATTACHMENTS = "#%handle-attachments" => handle_attachments_pop: ctx fn(Option<Box<StackFrameAttachments>>);
    POP_SLOW_PATH_FINISH = "#%pop-slow-path-finish" => pop_slow_path_finish: ctx fn(SteelVal);
    SLOW_GROW_STACK = "slow-grow-stack" => grow_stack_slow: ctx fn();
    SLOW_STACK_RESERVE_EXACT = "slow-stack-reserve-exact" => stack_ensure_capacity: ctx fn(usize);
    SLOW_GROW_FRAME_STACK = "slow-grow-frame-stack" => grow_frame_stack_slow: ctx fn();
    NEW_CLOSURE = "new-closure" => handle_new_start_closure: ctx fn(usize, usize) -> SteelVal;
    PURE_FUNC = "pure-func" => handle_pure_function: ctx fn(usize, usize) -> SteelVal;
    SETUP_CLOSURE_ARITY = "#%setup-closure-arity" => setup_closure_call_arity: ctx_raises fn(Gc<ByteCodeLambda>, usize, usize) -> SteelVal;
    COLLECT_REST_ARGS = "#%collect-rest-args" => collect_rest_args: ctx fn(usize);
    FRAMELESS_RECORD_FRAME = "#%frameless-record-frame" => frameless_record_frame: ctx fn(usize, usize, Gc<ByteCodeLambda>, usize, usize);
    FRAMELESS_SPLICE_FRAMES = "#%frameless-splice-frames" => frameless_splice_frames: ctx fn();
    CDR_TAIL_VALUE = "cdr-tail-value" => cdr_tail_value: ctx_raises fn(usize, SteelVal) -> SteelVal;
    REVERSE_TAIL_VALUE = "reverse-tail-value" => reverse_tail_value: ctx_raises fn(usize, SteelVal) -> SteelVal;
    CALL_MAKE_VECTOR_1 = "call-make-vector-1" => call_make_vector_1: ctx_raises fn(usize, SteelVal) -> SteelVal;
    CALL_MAKE_VECTOR_2 = "call-make-vector-2" => call_make_vector_2: ctx_raises fn(usize, SteelVal, SteelVal) -> SteelVal;
    CAR_HANDLER_VALUE = "car-handler-value" => car_handler_value: ctx_raises fn(SteelVal) -> SteelVal;
    CDR_HANDLER_VALUE = "cdr-handler-value" => cdr_handler_value: ctx_raises fn(SteelVal) -> SteelVal;
    CDR_REG = "cdr-reg" => cdr_handler_reg: ctx_raises fn(usize) -> SteelVal;
    CDR_MUT_REG = "cdr-mut-reg" => cdr_handler_mut_reg: ctx_raises fn(usize) -> SteelVal;
    CDR_REG_NO_CHECK = "cdr-reg-no-check" => cdr_handler_reg_no_check: ctx fn(usize) -> SteelVal;
    CDR_MUT_REG_NO_CHECK = "cdr-mut-reg-no-check" => cdr_handler_mut_reg_no_check: ctx fn(usize);
    CONS_HANDLER_VALUE = "cons-handler-value" => cons_handler_value: ctx_raises fn(SteelVal, SteelVal) -> SteelVal;
    CONS_HANDLER_VALUE_REGISTER = "cons-handler-value-register" => cons_handler_value_register: ctx fn(SteelVal, usize);
    CONS_HANDLER_REGISTER_REGISTER = "cons-handler-register-register" => cons_handler_register_register: ctx fn(usize, usize);
    CAR_REG = "car-reg" => car_handler_reg: ctx_raises fn(usize) -> SteelVal;
    BOX_HANDLER = "box-handler" => box_handler_c: ctx fn(SteelVal) -> SteelVal;
    UNBOX_HANDLER = "unbox-handler" => unbox_handler_c: ctx_raises fn(SteelVal) -> SteelVal;
    SET_BOX_HANDLER = "set-box-handler" => setbox_handler_c: ctx_raises fn(SteelVal, SteelVal) -> SteelVal;
    LIST_REF_VALUE = "list-ref-value" => list_ref_handler_c: ctx_raises fn(SteelVal, SteelVal) -> SteelVal;
    VECTOR_REF_VALUE = "vector-ref-value" => vector_ref_handler_c: ctx_raises fn(SteelVal, SteelVal) -> SteelVal;
    VECTOR_LENGTH_VALUE = "vector-length-value" => vector_length_handler_c: ctx_raises fn(SteelVal) -> SteelVal;
    VECTOR_REF_REG_1 = "vector-ref-reg-1" => vector_ref_handler_register: ctx_raises fn(u16, SteelVal) -> SteelVal;
    VECTOR_REF_REG_2 = "vector-ref-reg-2" => vector_ref_handler_register_two: ctx_raises fn(usize, usize) -> SteelVal;
    VECTOR_REF_REG_2_UNBOXED_INDEX = "vector-ref-reg-2-unboxed-index" => vector_ref_handler_register_two_unboxed: ctx_raises fn(usize, usize) -> SteelVal;
    VECTOR_SET_ARGS = "vector-set-args" => vector_set_handler_stack: ctx_raises fn(SteelVal, SteelVal, SteelVal) -> SteelVal;
    VECTOR_PUSH_ARGS = "vector-push-args" => vector_push_handler_stack: ctx_raises fn(SteelVal, SteelVal) -> SteelVal;
    VECTOR_SET_REG_1 = "vector-set-reg-1" => vector_set_handler_register_one: ctx_raises fn(usize, SteelVal, SteelVal) -> SteelVal;
    VECTOR_SET_REG_2 = "vector-set-reg-2" => vector_set_handler_register_two: ctx_raises fn(usize, usize, SteelVal) -> SteelVal;
    VECTOR_SET_REG_3 = "vector-set-reg-3" => vector_set_handler_register_three: ctx_raises fn(usize, usize, usize) -> SteelVal;
    LIST_CONTAINS_REG = "list-contains-reg" => list_contains_reg: ctx fn(usize, List<SteelVal>) -> bool;
    LIST_CONTAINS_REG_CONSTANT = "list-contains-reg-constant" => list_contains_reg_constant: ctx fn(usize, List<SteelVal>) -> bool;
    LIST_CONTAINS_VALUE = "list-contains-value" => list_contains_value: ctx_raises fn(SteelVal, SteelVal) -> bool;
    MEMQ_UNCHECKED_LIST = "memq-unchecked-list" => memq_unchecked_list: plain fn(SteelVal, List<SteelVal>) -> SteelVal;
    MEMQ_VALUE = "memq-value" => memq_value: ctx_raises fn(SteelVal, SteelVal) -> SteelVal;
    EQ_P_REG_2 = "eq?-reg-2" => eq_reg_2: ctx fn(usize, usize) -> bool;
    EQ_P_REG_1 = "eq?-reg-1" => eq_reg_1: ctx fn(usize, SteelVal) -> bool;
    SYMBOL_EQUAL_P_NO_DROP = "symbol-equal?-no-drop" => symbol_equal_no_drop: plain fn(SteelString, SteelString) -> bool;
    EQ_P_ARGS = "eq?-args" => eq_value: plain fn(SteelVal, SteelVal) -> bool;
    EQ_P_NO_DROP = "eq?-no-drop" => eq_value_no_drop: plain fn(SteelVal, SteelVal) -> bool;
    PUSH_CONST_INDEX = "push-const-index" => push_const_value_index_c: ctx fn(usize) -> SteelVal;
    ADD_BINOP = "add-binop" => extern_c_add_two: ctx_raises fn(SteelVal, SteelVal) -> SteelVal;
    ADD_THREE = "add-three" => extern_c_add_three: ctx_raises fn(SteelVal, SteelVal, SteelVal) -> SteelVal;
    ADD_FOUR = "add-four" => extern_c_add_four: ctx_raises fn(SteelVal, SteelVal, SteelVal, SteelVal) -> SteelVal;
    ADD_BINOP_REG = "add-binop-reg" => extern_c_add_two_binop_register: ctx_raises fn(usize, SteelVal) -> SteelVal;
    ADD_BINOP_REG_2 = "add-binop-reg-2" => extern_c_add_two_binop_register_both: ctx_raises fn(usize, usize) -> SteelVal;
    SUB_BINOP = "sub-binop" => extern_c_sub_two: ctx_raises fn(SteelVal, SteelVal) -> SteelVal;
    SUB_NEGATE = "sub-negate" => extern_c_negate: ctx_raises fn(SteelVal) -> SteelVal;
    SUB_BINOP_INT = "sub-binop-int" => extern_c_sub_two_int: ctx_raises fn(SteelVal, SteelVal) -> SteelVal;
    SUB_BINOP_BOTH_REG = "sub-binop-both-reg" => extern_c_sub_two_both_reg: ctx_raises fn(usize, usize) -> SteelVal;
    SUB_BINOP_INT_REG = "sub-binop-int-reg" => extern_c_sub_two_int_reg: ctx_raises fn(usize, SteelVal) -> SteelVal;
    ADD_BINOP_INT_REG = "add-binop-int-reg" => extern_c_add_two_int_reg: ctx_raises fn(usize, SteelVal) -> SteelVal;
    SUB_BINOP_FLOAT_REG = "sub-binop-float-reg" => extern_c_sub_two_float_reg: ctx_raises fn(usize, SteelVal) -> SteelVal;
    SUB_BINOP_REG = "sub-binop-reg" => extern_c_sub_two_reg: ctx_raises fn(usize, SteelVal) -> SteelVal;
    LT_BINOP = "lt-binop" => extern_c_lt_two: ctx_raises fn(SteelVal, SteelVal) -> SteelVal;
    LT_TWO_VALUE_BOOL = "lt-two-value-bool" => extern_c_lt_two_value_bool: ctx_raises fn(SteelVal, SteelVal) -> bool;
    LTE_TWO_VALUE_BOOL = "lte-two-value-bool" => extern_c_lte_two_value_bool: ctx_raises fn(SteelVal, SteelVal) -> bool;
    GT_TWO_VALUE_BOOL = "gt-two-value-bool" => extern_c_gt_two_value_bool: ctx_raises fn(SteelVal, SteelVal) -> bool;
    GTE_TWO_VALUE_BOOL = "gte-two-value-bool" => extern_c_gte_two_value_bool: ctx_raises fn(SteelVal, SteelVal) -> bool;
    LTE_BINOP = "lte-binop" => extern_c_lte_two: ctx_raises fn(SteelVal, SteelVal) -> SteelVal;
    LTE_BINOP_INT = "lte-binop-int" => extern_c_lte_two_int: ctx_raises fn(SteelVal, SteelVal) -> SteelVal;
    LTE_REGISTER = "lte-register" => extern_c_lte_register: ctx_raises fn(usize, SteelVal) -> bool;
    LTE_REGISTER_INT = "lte-register-int" => extern_c_lte_register_int: ctx_raises fn(usize, SteelVal) -> bool;
    GTE_REGISTER_INT = "gte-register-int" => extern_c_gte_register_int: ctx_raises fn(usize, SteelVal) -> bool;
    GTE_REGISTER_UNKNOWN = "gte-register-unknown" => extern_c_gte_register_unknown: ctx_raises fn(usize, SteelVal) -> bool;
    LT_REGISTER_INT = "lt-register-int" => extern_c_lt_register_int: ctx_raises fn(usize, SteelVal) -> bool;
    GT_REGISTER_INT = "gt-register-int" => extern_c_gt_register_int: ctx_raises fn(usize, SteelVal) -> bool;
    LT_BINOP_INT = "lt-binop-int" => extern_c_lt_two_int: ctx_raises fn(SteelVal, SteelVal) -> SteelVal;
    NULL_HANDLER = "null-handler" => extern_c_null_handler: plain fn(SteelVal) -> SteelVal;
    LT_THREE = "lt-three" => extern_c_lt_three: ctx_raises fn(SteelVal, SteelVal, SteelVal) -> SteelVal;
    LTE_THREE = "lte-three" => extern_c_lte_three: ctx_raises fn(SteelVal, SteelVal, SteelVal) -> SteelVal;
    GT_THREE = "gt-three" => extern_c_gt_three: ctx_raises fn(SteelVal, SteelVal, SteelVal) -> SteelVal;
    GTE_THREE = "gte-three" => extern_c_gte_three: ctx_raises fn(SteelVal, SteelVal, SteelVal) -> SteelVal;
    GT_BINOP = "gt-binop" => extern_c_gt_two: ctx_raises fn(SteelVal, SteelVal) -> SteelVal;
    GTE_BINOP = "gte-binop" => extern_c_gte_two: ctx_raises fn(SteelVal, SteelVal) -> SteelVal;
    MULT_TWO = "mult-two" => extern_c_mult_two: ctx_raises fn(SteelVal, SteelVal) -> SteelVal;
    MULT_THREE = "mult-three" => extern_c_mult_three: ctx_raises fn(SteelVal, SteelVal, SteelVal) -> SteelVal;
    SUB_THREE = "sub-three" => extern_c_sub_three: ctx_raises fn(SteelVal, SteelVal, SteelVal) -> SteelVal;
    DIV_TWO = "div-two" => extern_c_div_two: ctx_raises fn(SteelVal, SteelVal) -> SteelVal;
    DIV_ONE = "div-one" => extern_c_div_one: ctx_raises fn(SteelVal) -> SteelVal;
    READ_LOCAL_0 = "read-local-0" => read_local_0_value_c: ctx fn() -> SteelVal;
    READ_LOCAL_1 = "read-local-1" => read_local_1_value_c: ctx fn() -> SteelVal;
    READ_LOCAL_2 = "read-local-2" => read_local_2_value_c: ctx fn() -> SteelVal;
    READ_LOCAL_3 = "read-local-3" => read_local_3_value_c: ctx fn() -> SteelVal;
    SET_LOCAL_ANY = "set-local-any" => set_local_any_c: ctx fn(usize, SteelVal) -> SteelVal;
    MOVE_READ_LOCAL_0 = "move-read-local-0" => move_read_local_0_value_c: ctx fn() -> SteelVal;
    MOVE_READ_LOCAL_1 = "move-read-local-1" => move_read_local_1_value_c: ctx fn() -> SteelVal;
    MOVE_READ_LOCAL_2 = "move-read-local-2" => move_read_local_2_value_c: ctx fn() -> SteelVal;
    MOVE_READ_LOCAL_3 = "move-read-local-3" => move_read_local_3_value_c: ctx fn() -> SteelVal;
    TCO_JUMP = "tco-jump" => tcojmp_handler: ctx_raises fn(usize);
    SET_HANDLER = "set-handler" => set_handler_c: ctx fn(usize, SteelVal) -> SteelVal;
}

/// Helpers that never push to or reallocate the vm's value stack, so a cached
/// stack buffer pointer survives a call to one. Everything else is assumed to
/// move it.
pub(super) const KEEPS_STACK: &[&str] = &[
    // arithmetic and comparison slow paths, which only operate on values
    ADD_BINOP.name,
    ADD_BINOP_INT_REG.name,
    ADD_THREE.name,
    SUB_BINOP.name,
    SUB_BINOP_INT.name,
    SUB_BINOP_INT_REG.name,
    SUB_BINOP_BOTH_REG.name,
    SUB_NEGATE.name,
    SUB_THREE.name,
    LT_BINOP.name,
    LT_BINOP_INT.name,
    LT_REGISTER_INT.name,
    LTE_REGISTER_INT.name,
    GT_REGISTER_INT.name,
    GTE_REGISTER_INT.name,
    LT_TWO_VALUE_BOOL.name,
    LTE_TWO_VALUE_BOOL.name,
    GT_TWO_VALUE_BOOL.name,
    GTE_TWO_VALUE_BOOL.name,
    // builds a list cell; never touches the value stack
    LIST_CURSOR_MATERIALIZE.name,
    // refcount traffic only
    CLONE_STD_RC.name,
    RAW_SLOW_INCREMENT_CLOSURE.name,
];

/// Helpers that can allocate on the managed heap (boxes and mutable vectors),
/// and so start a collection. The collector's roots are the vm stack, the frame
/// stack and the globals. A value compiled code holds only in a register is not
/// a root, so across a call to one of these, nothing may be pending off the vm
/// stack. The call sites are checked when the code is generated
/// (`FunctionTranslator::check_collection_roots`).
pub(super) const COLLECTS: &[&str] = &[
    BOX_HANDLER.name,
    CALL_MAKE_VECTOR_1.name,
    CALL_MAKE_VECTOR_2.name,
];

/// The same for helpers named at run time: the per-arity constructor families.
pub(super) const COLLECTING_FAMILIES: &[&str] = &[
    "call_mutable_vector_constructor_",
    "call_mutable_struct_constructor_",
];

pub(super) fn helper_collects(name: &str) -> bool {
    COLLECTS.contains(&name)
        || COLLECTING_FAMILIES
            .iter()
            .any(|prefix| name.starts_with(prefix))
}
