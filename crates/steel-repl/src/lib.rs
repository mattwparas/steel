#[macro_use]
mod repl;
mod editor;
#[cfg(feature = "rustyline")]
mod highlight;
#[cfg(test)]
mod line_editor_tests;
mod steel_editor;
pub mod terminal;

pub use colored;
pub use repl::Repl;

/// Run the Steel repl with the given `Engine`. Exits on IO error or when the user requests to exit.
pub fn run_repl(vm: steel::steel_vm::engine::Engine) -> std::io::Result<()> {
    repl::repl_base(vm)
}

/// `steel keys`: shows the bytes each key sends (src/scheme/keys.scm)
pub fn run_keys(vm: &mut steel::steel_vm::engine::Engine) -> Result<(), steel::rerrs::SteelErr> {
    repl::terminal_module(vm);
    vm.compile_and_run_raw_program(include_str!("scheme/keys.scm"))?;
    Ok(())
}

/// Registers `#%private/steel/readline`, `#%private/steel/terminal` and
/// `steel/line-editor`
pub fn register_readline_module(vm: &mut steel::steel_vm::engine::Engine) {
    repl::readline_module(vm);
    register_line_editor(vm);
}

/// `steel/line-editor` and the terminal primitives under it
pub fn register_line_editor(vm: &mut steel::steel_vm::engine::Engine) {
    repl::terminal_module(vm);
    vm.register_steel_module(
        "steel/line-editor".to_owned(),
        include_str!("scheme/line-editor.scm").to_owned(),
    );
}
