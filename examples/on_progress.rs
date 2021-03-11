use steel_vm::engine::Engine;

pub fn main() {
    let mut vm = Engine::new();

    let core_libraries = &[steel::stdlib::PRELUDE, steel::stdlib::CONTRACTS];

    for core in core_libraries {
        let res = vm.parse_and_execute_without_optimizations(core);
        if let Err(e) = res {
            eprintln!("{}", e);
            return;
        }
    }
}
