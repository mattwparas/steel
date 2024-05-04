use std::borrow::Cow;

use crate::steel_vm::engine::Engine;

fn generate_asserting_machine() -> Engine {
    let vm = Engine::new();
    vm
}

pub(crate) fn assert_script<T: AsRef<str> + Into<Cow<'static, str>>>(script: T) {
    let mut vm = generate_asserting_machine();
    assert!(vm.compile_and_run_raw_program(script).is_ok());
}

pub(crate) fn assert_script_error<T: AsRef<str> + Into<Cow<'static, str>>>(script: T) {
    let mut vm = generate_asserting_machine();
    assert!(vm.compile_and_run_raw_program(script).is_err());
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

test_harness_success! {
    abc_problem,
    apply_more_complex,
    babbage_problem,
    balanced_brackets,
    basic_apply,
    bytevectors,
    calculator,
    capture_upvalue,
    capture_upvalues_arity_two,
    close_upvalue,
    closure_value_capture,
    comma_quibbling,
    complex_lets,
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
    heap_sort,
    help,
    html_table,
    letrec_mutual_recursion,
    letrec_simple_recursion,
    list_functions,
    local_struct,
    matcher,
    maxsubseq,
    merge_sort,
    ncsubseq,
    numbers,
    pascals,
    permutations,
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
    string_append,
    structs,
    // TODO: @Matt 11/11/2023
    threads,
    transducer_over_streams,
    tree_traversal,
    trie_sort,
    y_combinator,
}

test_harness_failure! {
    capped_depth_defmacro,
    function_used_before_definition,
    global_env,
    identifier_used_before_definition,
    local_define_does_not_escape,
    local_define_does_not_escape_non_const,
    local_struct_inaccessible,
    require_only_in_missing_identifier,
}
