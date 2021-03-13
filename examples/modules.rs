use steel_vm::engine::Engine;

// use env_logger::Builder;
// use log::LevelFilter;

// Steel allows for flexibility with modules as well. If you wanted to
// expose a module with a clean api, you can do so using the `provide` syntax.
// `require` is used to bring a module into scope - this requires the relative path
// (from the directory of whatever project main is in), or absolute path to the script.
// See the `modules` directory for the code used here
pub fn main() {
    // let mut builder = Builder::new();

    // builder.filter(Some("steel"), LevelFilter::Trace).init();

    let mut vm = Engine::new();

    vm.run(
        r#"
        (require "examples/modules/sort_export.rkt")

        (define sorted (sort '(1000 342 1 5534 34243 223 23495)))

        (displayln sorted)
    "#,
    )
    .unwrap();
}
