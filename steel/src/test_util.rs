use crate::stdlib::PRELUDE;
#[cfg(test)]
use crate::vm::VirtualMachine;

#[test]
fn prelude_parses() {
    let mut vm = VirtualMachine::new();
    vm.parse_and_execute(PRELUDE).unwrap();
}

fn generate_asserting_machine() -> VirtualMachine {
    let mut vm = VirtualMachine::new();
    vm.parse_and_execute(PRELUDE).unwrap();
    vm
}

pub(crate) fn assert_script<T: AsRef<str>>(script: T) {
    let mut vm = generate_asserting_machine();
    assert!(vm.parse_and_execute(script.as_ref()).is_ok());
}
