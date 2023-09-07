// build.rs

use std::env;
use std::fs;
use std::path::Path;

fn main() {
    #[cfg(feature = "dynamic")]
    {
        use steel_gen::generate_opcode_map;

        let out_dir = env::var_os("OUT_DIR").unwrap();
        let dest_path = Path::new(&out_dir).join("dynamic.rs");

        // TODO: Come up with better way for this to make it in
        // let patterns: &'static [&'static [(steel_gen::OpCode, usize)]] = &[
        //     &[
        //         (MOVEREADLOCAL0, 0),
        //         (LOADINT2, 225),
        //         (SUB, 2),
        //         (CALLGLOBAL, 1),
        //     ],
        //     &[(READLOCAL0, 0), (LOADINT1, 219), (SUB, 2), (CALLGLOBAL, 1)],
        //     &[(READLOCAL0, 0), (LOADINT2, 225), (LTE, 2), (IF, 7)],
        //     &[
        //         (READLOCAL0, 0),
        //         (LOADINT1, 219),
        //         (SUB, 2),
        //         (MOVEREADLOCAL0, 0),
        //         (MOVEREADLOCAL1, 1),
        //         (LOADINT1, 219),
        //         (SUB, 2),
        //         (CALLGLOBAL, 2),
        //     ],
        // ];

        fs::write(dest_path, generate_opcode_map()).unwrap();
    }

    let out_dir = env::var_os("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("generated.rs");

    fs::write(dest_path, steel_gen::permutations::code_gen()).unwrap();

    println!("cargo:rerun-if-changed=build.rs");
}
