use steel_rustls::build_module;

fn main() {
    std::process::Command::new("cargo-steel-lib")
        .spawn()
        .unwrap()
        .wait()
        .unwrap();

    build_module()
        .emit_package_to_file("libsteel_rustls", "rustls.scm")
        .unwrap()
}
