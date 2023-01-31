use super::engine::Engine;
use crate::stdlib::{CONTRACTS, PRELUDE};

#[test]
fn prelude_parses() {
    let mut vm = Engine::new();
    vm.compile_and_run_raw_program(PRELUDE).unwrap();
}

#[test]
fn contract_parses() {
    let mut vm = Engine::new();
    vm.compile_and_run_raw_program(PRELUDE).unwrap();
    vm.compile_and_run_raw_program(CONTRACTS).unwrap();
}

#[cfg(test)]
fn generate_asserting_machine() -> Engine {
    
    // vm.compile_and_run_raw_program(PRELUDE).unwrap();
    // vm.compile_and_run_raw_program(CONTRACTS).unwrap();
    Engine::new()
}

#[cfg(test)]
pub(crate) fn assert_script<T: AsRef<str>>(script: T) {
    let mut vm = generate_asserting_machine();
    assert!(vm.compile_and_run_raw_program(script.as_ref()).is_ok());
}

#[cfg(test)]
pub(crate) fn assert_script_error<T: AsRef<str>>(script: T) {
    let mut vm = generate_asserting_machine();
    assert!(vm.compile_and_run_raw_program(script.as_ref()).is_err());
}
