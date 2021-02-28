#[cfg(test)]
use std::path::PathBuf;

#[cfg(test)]
use crate::engine::Engine;
#[cfg(test)]
use steel::stdlib::{CONTRACTS, PRELUDE};

#[test]
fn prelude_parses() {
    let mut vm = Engine::new();
    vm.parse_and_execute_without_optimizations(PRELUDE, PathBuf::from("test"))
        .unwrap();
}

#[test]
fn contract_parses() {
    let mut vm = Engine::new();
    vm.parse_and_execute_without_optimizations(PRELUDE, PathBuf::from("test"))
        .unwrap();
    vm.parse_and_execute_without_optimizations(CONTRACTS, PathBuf::from("test"))
        .unwrap();
}

#[cfg(test)]
fn generate_asserting_machine() -> Engine {
    let mut vm = Engine::new();
    vm.parse_and_execute_without_optimizations(PRELUDE, PathBuf::from("test"))
        .unwrap();
    vm.parse_and_execute_without_optimizations(CONTRACTS, PathBuf::from("test"))
        .unwrap();
    vm
}

#[cfg(test)]
pub(crate) fn assert_script<T: AsRef<str>>(script: T) {
    let mut vm = generate_asserting_machine();
    assert!(vm
        .parse_and_execute_without_optimizations(script.as_ref(), PathBuf::from("test"))
        .is_ok());
}

#[cfg(test)]
pub(crate) fn assert_script_error<T: AsRef<str>>(script: T) {
    let mut vm = generate_asserting_machine();
    assert!(vm
        .parse_and_execute_without_optimizations(script.as_ref(), PathBuf::from("test"))
        .is_err());
}
