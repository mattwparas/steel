use std::env;

fn main() {
    export_var("HOST_PLATFORM", &env::var("HOST").unwrap());
    export_var("TARGET_PLATFORM", &env::var("TARGET").unwrap());
    println!("cargo:rerun-if-changed-env=TARGET")
}

fn export_var(name: &str, value: &str) {
    println!("cargo:rustc-env={}={}", name, value);
}
