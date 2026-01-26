use super::engine::Engine;
use crate::stdlib::PRELUDE;
use alloc::borrow::Cow;

#[test]
fn prelude_parses() {
    let mut vm = Engine::new();
    vm.compile_and_run_raw_program(PRELUDE).unwrap();
}

#[cfg(test)]
fn generate_asserting_machine() -> Engine {
    // vm.compile_and_run_raw_program(PRELUDE).unwrap();
    // vm.compile_and_run_raw_program(CONTRACTS).unwrap();
    Engine::new()
}

#[cfg(test)]
pub(crate) fn assert_script<T: AsRef<str> + Into<Cow<'static, str>>>(script: T) {
    let mut vm = generate_asserting_machine();
    assert!(vm.compile_and_run_raw_program(script).is_ok());
}

#[cfg(test)]
pub(crate) fn assert_script_error<T: AsRef<str> + Into<Cow<'static, str>>>(script: T) {
    let mut vm = generate_asserting_machine();
    assert!(vm.compile_and_run_raw_program(script).is_err());
}
