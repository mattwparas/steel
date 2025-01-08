#[macro_use]
mod repl;
mod highlight;

/// Run the Steel repl with the given `Engine`. Exits on IO error or when the user requests to exit.
pub fn run_repl(vm: steel::steel_vm::engine::Engine) -> std::io::Result<()> {
    repl::repl_base(vm)
}

pub fn register_readline_module(vm: &mut steel::steel_vm::engine::Engine) {
    repl::readline_module(vm)
}
