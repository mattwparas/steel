use crate::stdlib::{CONTRACTS, PRELUDE};
use crate::steel_vm::engine::Engine;

fn generate_asserting_machine() -> Engine {
    let mut vm = Engine::new();
    vm.parse_and_execute_without_optimizations(PRELUDE).unwrap();
    vm.parse_and_execute_without_optimizations(CONTRACTS)
        .unwrap();
    vm
}

pub(crate) fn assert_script<T: AsRef<str>>(script: T) {
    let mut vm = generate_asserting_machine();
    assert!(vm
        .parse_and_execute_without_optimizations(script.as_ref())
        .is_ok());
}

pub(crate) fn assert_script_error<T: AsRef<str>>(script: T) {
    let mut vm = generate_asserting_machine();
    assert!(vm
        .parse_and_execute_without_optimizations(script.as_ref())
        .is_err());
}

macro_rules! test_harness_success {
    ($($file_name:ident),* $(,)?) => {
        #[cfg(test)]
        mod integration_success {
            use super::*;
            $(
                #[test]
                fn $file_name() {
                    let script = include_str!(concat!("success/", stringify!($file_name), ".rkt"));
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
                    let script = include_str!(concat!("failure/", stringify!($file_name), ".rkt"));
                    assert_script_error(script);
                }
            )*
        }
    };
}

test_harness_success! {
    apply_more_complex,
    basic_apply,
    calculator,
    capture_upvalue,
    capture_upvalues_arity_two,
    close_upvalue,
    define_normal,
    dfs,
    fib,
    generator,
    generic_execution_dropping,
    generic_execution_output_different_type,
    generic_execution,
    generic_transducer_with_different_functions,
    generic_transducer,
    letrec_mutual_recursion,
    letrec_simple_recursion,
    local_struct,
    matcher,
    merge_sort,
    read,
    set_local,
    sieve,
    simple_stream_with_map,
    simple_stream_with_mapping,
    simple_stream_with_transduce_operation,
    simple_stream_with_transducer,
    simple_stream,
    stack_state,
    stack_struct,
    stack_test_with_contract,
    transducer_over_streams,
    trie_sort,
    y_combinator,
}

test_harness_failure! {
    function_used_before_definition,
    identifier_used_before_definition,
    local_struct_inaccessible
}
