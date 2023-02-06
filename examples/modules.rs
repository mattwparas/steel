use steel::steel_vm::engine::Engine;

// Steel allows for flexibility with modules as well. If you wanted to
// expose a module with a clean api, you can do so using the `provide` syntax.
// `require` is used to bring a module into scope - this requires the relative path
// (from the directory of whatever project main is in), or absolute path to the script.
// See the `modules` directory for the code used here
//
// Note: This should be run from the root of the project
// Otherwise, it will fatal with a 'file not found'
pub fn main() {
    let mut vm = Engine::new();

    vm.compile_and_run_raw_program(
        r#"
        (require "examples/modules/sort_export.scm")

        (define sorted (sort '(1000 342 1 5534 34243 223 23495)))

        (displayln sorted)
    "#,
    )
    .unwrap();
}
