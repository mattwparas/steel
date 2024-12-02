use std::path::PathBuf;

use steel::steel_vm::engine::Engine;

fn main() {
    let out_dir = std::env::var_os("OUT_DIR").unwrap();
    let dest_path = std::path::Path::new(&out_dir).join("program.rs");
    let dest_bytes = std::path::Path::new(&out_dir).join("program.bin");

    let entrypoint = include_str!("../../cogs/installer/forge.scm");

    let non_interactive_program = Engine::create_non_interactive_program_image(
        entrypoint,
        PathBuf::from("../../cogs/installer/spm.scm"),
    )
    .unwrap();

    // Write the bytes out
    non_interactive_program.write_bytes_to_file(&dest_bytes);

    let rust_entrypoint = format!(
        r#"
fn main() {{
    steel::steel_vm::engine::Engine::execute_non_interactive_program_image(include_bytes!("{}")).unwrap();
}}
    "#,
        dest_bytes.as_os_str().to_str().unwrap()
    );

    std::fs::write(dest_path, rust_entrypoint).unwrap();
}
