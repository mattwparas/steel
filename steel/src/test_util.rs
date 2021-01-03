use crate::stdlib::PRELUDE;
#[cfg(test)]
use crate::vm::VirtualMachine;

#[test]
fn prelude_parses() {
    let mut vm = VirtualMachine::new();
    vm.parse_and_execute_without_optimizations(PRELUDE).unwrap();
}

fn generate_asserting_machine() -> VirtualMachine {
    let mut vm = VirtualMachine::new();
    vm.parse_and_execute_without_optimizations(PRELUDE).unwrap();
    vm
}

pub(crate) fn assert_script<T: AsRef<str>>(script: T) {
    let mut vm = generate_asserting_machine();
    assert!(vm
        .parse_and_execute_without_optimizations(script.as_ref())
        .is_ok());
}
