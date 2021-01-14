#[cfg(test)]
use crate::engine::Engine;
#[cfg(test)]
use steel::stdlib::PRELUDE;

#[test]
fn prelude_parses() {
    let mut vm = Engine::new();
    vm.parse_and_execute_without_optimizations(PRELUDE).unwrap();
}

#[cfg(test)]
fn generate_asserting_machine() -> Engine {
    let mut vm = Engine::new();
    vm.parse_and_execute_without_optimizations(PRELUDE).unwrap();
    vm
}

#[cfg(test)]
pub(crate) fn assert_script<T: AsRef<str>>(script: T) {
    let mut vm = generate_asserting_machine();
    assert!(vm
        .parse_and_execute_without_optimizations(script.as_ref())
        .is_ok());
}
