// build.rs

fn main() {
    #[cfg(feature = "dynamic")]
    {
        use std::env;
        use std::fs;
        use std::path::Path;

        let out_dir = env::var_os("OUT_DIR").unwrap();
        let dest_path = Path::new(&out_dir).join("generated.rs");

        fs::write(dest_path, steel_gen::permutations::code_gen()).unwrap();

        println!("cargo:rerun-if-changed=build.rs");
    }
}
