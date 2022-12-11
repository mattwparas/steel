// build.rs

use std::env;
use std::fs;
use std::path::Path;

fn main() {
    let out_dir = env::var_os("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("hello.rs");
    fs::write(
        &dest_path,
        r#"
        

        pub fn message() -> &'static str {
            println!("{:?}", OpCode::FUNC);
            "Hello, World!"
        }
        "#,
    )
    .unwrap();
    println!("cargo:rerun-if-changed=build.rs");
}
