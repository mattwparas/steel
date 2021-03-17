use super::engine::Engine;
use crate::stdlib::{CONTRACTS, PRELUDE};

#[test]
fn prelude_parses() {
    let mut vm = Engine::new();
    vm.parse_and_execute_without_optimizations(PRELUDE).unwrap();
}

#[test]
fn contract_parses() {
    let mut vm = Engine::new();
    vm.parse_and_execute_without_optimizations(PRELUDE).unwrap();
    vm.parse_and_execute_without_optimizations(CONTRACTS)
        .unwrap();
}

#[cfg(test)]
fn generate_asserting_machine() -> Engine {
    let mut vm = Engine::new();
    vm.parse_and_execute_without_optimizations(PRELUDE).unwrap();
    vm.parse_and_execute_without_optimizations(CONTRACTS)
        .unwrap();
    vm
}

#[cfg(test)]
pub(crate) fn assert_script<T: AsRef<str>>(script: T) {
    let mut vm = generate_asserting_machine();
    assert!(vm
        .parse_and_execute_without_optimizations(script.as_ref())
        .is_ok());
}

#[cfg(test)]
pub(crate) fn assert_script_error<T: AsRef<str>>(script: T) {
    let mut vm = generate_asserting_machine();
    assert!(vm
        .parse_and_execute_without_optimizations(script.as_ref())
        .is_err());
}
