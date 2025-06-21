use std::path::PathBuf;

use steel::steel_vm::engine::Engine;

fn main() {
    // Re run this if any of the files within the directory
    // have changed. Note - this may not pick up changes in any
    // dependencies, but it should be good enough.
    println!("cargo::rerun-if-changed=installer/");

    let out_dir = std::env::var_os("OUT_DIR").unwrap();
    let dest_path = std::path::Path::new(&out_dir).join("program.rs");
    let dest_bytes = std::path::Path::new(&out_dir).join("program.bin");

    let entrypoint = include_str!("installer/forge.scm");

    let non_interactive_program = Engine::create_non_interactive_program_image(
        entrypoint,
        PathBuf::from("installer/forge.scm"),
    )
    .unwrap();

    // Write the bytes out
    non_interactive_program.write_bytes_to_file(&dest_bytes);

    let rust_entrypoint = format!(
        r#"
fn main() {{
    steel::steel_vm::engine::Engine::execute_non_interactive_program_image(include_bytes!(r"{}")).unwrap();
}}
    "#,
        dest_bytes.as_os_str().to_str().unwrap()
    );

    std::fs::write(dest_path, rust_entrypoint).unwrap();
}
