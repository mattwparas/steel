use alloc::borrow::Cow;

use crate::steel_vm::engine::Engine;

mod assigned_locals;
#[cfg(all(feature = "sync", feature = "jit2"))]
mod compiled_shape;
mod continuations_from_host;
#[cfg(all(feature = "sync", feature = "jit2"))]
mod counted_loops;
#[cfg(feature = "sync")]
mod engines_side_by_side;
#[cfg(feature = "sync")]
mod history;
mod local_macros;
mod malformed_bindings;
mod module_contracts;
#[cfg(feature = "sync")]
mod serialization;

fn generate_asserting_machine() -> Engine {
    // Background compilation is on by default, so a test program could finish
    // before the worker compiles any of it. Settling after each top level form
    // keeps the compiled code covered. Run the suite again with
    // `STEEL_JIT_BACKGROUND=0` to cover synchronous compilation.
    #[cfg(feature = "jit2")]
    crate::steel_vm::vm::jit::settle_background_compiles(
        crate::steel_vm::vm::jit::jit_background_enabled(),
    );
    let vm = Engine::new();
    vm
}

pub(crate) fn assert_script<T: AsRef<str> + Into<Cow<'static, str>>>(script: T) {
    let mut vm = generate_asserting_machine();
    vm.compile_and_run_raw_program(script).unwrap();
}

pub(crate) fn assert_script_error<T: AsRef<str> + Into<Cow<'static, str>>>(script: T) {
    let mut vm = generate_asserting_machine();
    assert!(vm.compile_and_run_raw_program(script).is_err());
}

pub(crate) fn assert_script_module<T: AsRef<str> + Into<Cow<'static, str>>>(name: &str, script: T) {
    let mut vm = generate_asserting_machine();
    vm.register_steel_module(name.to_string(), script.into().to_string());

    vm.compile_and_run_raw_program(format!(r#"(require "{}")"#, name))
        .unwrap();
}

macro_rules! test_harness_success {
    ($($file_name:ident),* $(,)?) => {
        #[cfg(test)]
        mod integration_success {
            use super::*;
            $(
                #[test]
                fn $file_name() {
                    let script = include_str!(concat!("success/", stringify!($file_name), ".scm"));
                    assert_script(script);
                }
            )*
        }
    };
}

macro_rules! test_harness_success_module {
    ($($file_name:ident),* $(,)?) => {
        #[cfg(test)]
        mod integration_success_module {
            use super::*;
            $(
                #[test]
                fn $file_name() {
                    println!("RUNNING: {}", stringify!($file_name));
                    let script = include_str!(concat!("success/", stringify!($file_name), ".scm"));
                    assert_script_module(stringify!($file_name), script);
                }
            )*
        }
    };
}

macro_rules! test_harness_success_sync {
    ($($file_name:ident),* $(,)?) => {
        #[cfg(feature = "sync")]
        #[cfg(test)]
        mod integration_success_sync {
            use super::*;
            $(
                #[test]
                fn $file_name() {
                    let script = include_str!(concat!("success/", stringify!($file_name), ".scm"));
                    assert_script(script);
                }
            )*
        }
    };
}

macro_rules! test_harness_failure {
    ($($file_name:ident),* $(,)?) => {
        #[cfg(test)]
        mod integration_failure {
            use super::*;
            $(
                #[test]
                fn $file_name() {
                    let script = include_str!(concat!("failure/", stringify!($file_name), ".scm"));
                    assert_script_error(script);
                }
            )*
        }
    };
}

test_harness_success_sync! {
    native_threads,
    gc_deadlock
}

test_harness_success! {
    abc_problem,
    arity_mismatch_calls,
    box_release,
    jit_arithmetic_errors,
    jit_branch_in_argument_list,
    jit_compare_register_int,
    jit_struct_getter_moved_operand,
    jit_tail_apply_paths,
    jit_many_arg_calls,
    jit_captures,
    jit_control_flow,
    jit_higher_order,
    jit_indirect_calls,
    jit_mutation,
    jit_structs,
    jit_tail_calls,
    jit_type_facts,
    jit_fixnum_arith,
    jit_frameless_self_calls,
    jit_background_stress,
    jit_helper_error_exits,
    inline_mutated_parameter,
    case_eqv_and_builtin_tails,
    lift_arity_cap_group,
    captured_struct_reads,
    memq_constant_list_and_lt_constant,
    captured_reads_deferred,
    set_box_value_first,
    case_member_dispatch,
    char_class_predicates,
    apply_tail_native,
    deep_mutable_teardown,
    type_predicates_inline,
    macro_template_builtins,
    or_struct_predicate,
    packed_mutable_structs,
    cells_across_threads,
    threads_stop_in_loops,
    vectors_across_threads,
    structs_across_threads,
    boxes_across_threads,
    vectors_of_many_kinds_across_threads,
    counted_loops,
    functions_made_while_collecting,
    joins_while_collecting,
    channels_while_collecting,
    weak_boxes_while_collecting,
    wills_while_collecting,
    will_executors_and_weak_boxes,
    will_executors_registered_twice,
    will_procedures_keep_what_they_reach,
    wills_of_counted_values_while_collecting,
    wills_of_values_in_shared_lists,
    wills_over_shared_structure,
    wills_ready_together,
    continuations_kept_and_dropped,
    continuations_and_errors,
    continuations_through_native_calls,
    builtin_callbacks_in_tail_position,
    cells_pending_while_collecting,
    with_handler,
    wills_of_values_held_twice_by_a_vector,
    wills_of_values_in_maps_that_share,
    wills_behind_garbage_and_weak_boxes,
    shared_values_compare_hash_and_print,
    hashing_shared_values,
    default_arguments_in_order,
    let_values,
    local_macros,
    binding_macros_in_templates,
    interrupt_runs_dynamic_wind_exits,
    will_execute_with_nothing_else_collecting,
    serialization_struct_types_of_one_name,
    channels_held_by_their_pair,
    boxes_dropped_while_collecting,
    retired_values_and_macro_expansion,
    continuations_through_swallowing_builtins,
    rest_arguments_evaluated_once,
    continuations_and_dynamic_wind,
    continuations_reentered,
    continuations_edge_cases,
    values_read_while_collecting,
    structs_of_many_kinds_across_threads,
    vectors_reshaped_across_threads,
    collections_with_busy_threads,
    suspend_resume_interrupt,
    thread_local_storage_slots,
    values_dropped_on_other_threads,
    threads_come_and_go,
    swapped_values_are_released,
    will_executors_and_cycles,
    serialization_round_trips,
    serialization_refusals,
    serialization_sharing_and_cycles,
    serialization_across_threads,
    parameterize_per_thread,
    closure_captures_only_what_it_uses,
    contract_signatures,
    dotted_ellipsis_patterns,
    jit_type_test_narrowing,
    serialization_deep_values,
    identity_and_equality,
    immutable_structs,
    equal_takes_two,
    jit_pair_predicate,
    primitive_edge_cases,
    jit_primitive_arg_narrowing,
    drop_hash_vector_string,
    jit_loop_specialization,
    jit_varargs,
    apply,
    apply_more_complex,
    babbage_problem,
    balanced_brackets,
    basic_apply,
    binary_search,
    bytevectors,
    calculator,
    capture_upvalue,
    capture_upvalues_arity_two,
    close_upvalue,
    closure_value_capture,
    comma_quibbling,
    complex_lets,
    constant_eval_set,
    curried,
    contracts,
    define_normal,
    defmacro,
    delim_control,
    delim_control_n,
    dfs,
    dll,
    docs,
    ellipses,
    empty,
    fib,
    generator,
    generic_execution,
    generic_execution_dropping,
    generic_execution_output_different_type,
    generic_transducer,
    generic_transducer_with_different_functions,
    hailstone,
    hash,
    heap_sort,
    help,
    html_table,
    internal_define_ordering,
    into_string,
    letrec_mutual_recursion,
    letrec_simple_recursion,
    list_append,
    list_functions,
    local_struct,
    macro_quoted_names,
    matcher,
    maxsubseq,
    merge_sort,
    ncsubseq,
    numbers,
    pascals,
    print,
    permutations,
    ports,
    quicksort,
    read,
    require_alias,
    require_only_in,
    require_prefix,
    result,
    search,
    set_local,
    set_tail_call,
    shift_reset,
    local_reset_shadow,
    sicp_example,
    sieve,
    simple_stream,
    simple_stream_with_map,
    simple_stream_with_mapping,
    simple_stream_with_transduce_operation,
    simple_stream_with_transducer,
    stack_state,
    stack_struct,
    stack_test_with_contract,
    string,
    string_eq,
    structs,
    symbols,
    // TODO: @Matt 11/11/2023
    syntax_case,
    threads,
    transducer_over_streams,
    tree_traversal,
    trie_sort,
    y_combinator,
}

test_harness_success_module! {
    abc_problem,
    prefixed_require_builtin,
    macro_template_builtins,
    or_struct_predicate,
    packed_mutable_structs,
    cells_across_threads,
    threads_stop_in_loops,
    vectors_across_threads,
    structs_across_threads,
    boxes_across_threads,
    vectors_of_many_kinds_across_threads,
    counted_loops,
    functions_made_while_collecting,
    joins_while_collecting,
    channels_while_collecting,
    weak_boxes_while_collecting,
    wills_while_collecting,
    will_executors_and_weak_boxes,
    will_executors_registered_twice,
    will_procedures_keep_what_they_reach,
    wills_of_counted_values_while_collecting,
    wills_of_values_in_shared_lists,
    wills_over_shared_structure,
    wills_ready_together,
    continuations_kept_and_dropped,
    continuations_and_errors,
    continuations_through_native_calls,
    builtin_callbacks_in_tail_position,
    cells_pending_while_collecting,
    with_handler,
    wills_of_values_held_twice_by_a_vector,
    wills_of_values_in_maps_that_share,
    wills_behind_garbage_and_weak_boxes,
    shared_values_compare_hash_and_print,
    hashing_shared_values,
    default_arguments_in_order,
    let_values,
    local_macros,
    binding_macros_in_templates,
    interrupt_runs_dynamic_wind_exits,
    will_execute_with_nothing_else_collecting,
    serialization_struct_types_of_one_name,
    channels_held_by_their_pair,
    boxes_dropped_while_collecting,
    retired_values_and_macro_expansion,
    continuations_through_swallowing_builtins,
    rest_arguments_evaluated_once,
    continuations_and_dynamic_wind,
    continuations_reentered,
    continuations_edge_cases,
    values_read_while_collecting,
    structs_of_many_kinds_across_threads,
    vectors_reshaped_across_threads,
    collections_with_busy_threads,
    suspend_resume_interrupt,
    thread_local_storage_slots,
    values_dropped_on_other_threads,
    threads_come_and_go,
    swapped_values_are_released,
    will_executors_and_cycles,
    serialization_round_trips,
    serialization_refusals,
    serialization_sharing_and_cycles,
    serialization_across_threads,
    parameterize_per_thread,
    closure_captures_only_what_it_uses,
    contract_signatures,
    dotted_ellipsis_patterns,
    jit_type_test_narrowing,
    serialization_deep_values,
    identity_and_equality,
    immutable_structs,
    equal_takes_two,
    jit_arithmetic_errors,
    jit_branch_in_argument_list,
    jit_compare_register_int,
    jit_struct_getter_moved_operand,
    jit_tail_apply_paths,
    jit_many_arg_calls,
    jit_captures,
    jit_control_flow,
    jit_higher_order,
    jit_indirect_calls,
    jit_mutation,
    jit_structs,
    jit_tail_calls,
    jit_varargs,
    apply,
    apply_more_complex,
    babbage_problem,
    balanced_brackets,
    basic_apply,
    binary_search,
    bytevectors,
    calculator,
    capture_upvalue,
    capture_upvalues_arity_two,
    close_upvalue,
    closure_value_capture,
    comma_quibbling,
    complex_lets,
    constant_eval_set,
    curried,
    contracts,
    define_normal,
    // defmacro,
    delim_control,
    delim_control_n,
    dfs,
    dll,
    docs,
    ellipses,
    empty,
    fib,
    generator,
    generic_execution,
    generic_execution_dropping,
    generic_execution_output_different_type,
    generic_transducer,
    generic_transducer_with_different_functions,
    hailstone,
    hash,
    heap_sort,
    help,
    html_table,
    internal_define_ordering,
    into_string,
    letrec_mutual_recursion,
    letrec_simple_recursion,
    list_append,
    list_functions,
    local_struct,
    matcher,
    maxsubseq,
    merge_sort,
    ncsubseq,
    numbers,
    pascals,
    print,
    permutations,
    ports,
    quicksort,
    read,
    require_alias,
    require_only_in,
    require_prefix,
    result,
    search,
    set_local,
    set_tail_call,
    shift_reset,
    sicp_example,
    sieve,
    simple_stream,
    simple_stream_with_map,
    simple_stream_with_mapping,
    simple_stream_with_transduce_operation,
    simple_stream_with_transducer,
    stack_state,
    stack_struct,
    stack_test_with_contract,
    string,
    string_eq,
    structs,
    symbols,
    // TODO: @Matt 11/11/2023
    syntax_case,
    threads,
    transducer_over_streams,
    tree_traversal,
    trie_sort,
    y_combinator,
}

test_harness_failure! {
    capped_depth_defmacro,
    constant_fold_rest,
    define_syntax_ends_a_body,
    define_syntax_outside_a_body,
    function_used_before_definition,
    global_env,
    identifier_used_before_definition,
    local_define_does_not_escape,
    local_define_does_not_escape_non_const,
    local_macro_does_not_escape,
    template_define_values_does_not_escape,
    template_struct_does_not_escape,
    local_struct_inaccessible,
    require_only_in_missing_identifier,
    syntax_rules_empty_list,
}
