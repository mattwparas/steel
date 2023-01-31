// build.rs

fn main() {
    #[cfg(feature = "dynamic")]
    {
        use std::env;
        use std::fs;
        use std::path::Path;

        use steel_gen::generate_opcode_map;
        use steel_gen::OpCode::*;

        let out_dir = env::var_os("OUT_DIR").unwrap();
        let dest_path = Path::new(&out_dir).join("dynamic.rs");

        // TODO: Come up with better way for this to make it in
        let patterns: Vec<Vec<(steel_gen::OpCode, usize)>> = vec![
            vec![
                (MOVEREADLOCAL0, 0),
                (LOADINT2, 225),
                (SUB, 2),
                (CALLGLOBAL, 1),
            ],
            vec![(READLOCAL0, 0), (LOADINT1, 219), (SUB, 2), (CALLGLOBAL, 1)],
            vec![(READLOCAL0, 0), (LOADINT2, 225), (LTE, 2), (IF, 7)],
            vec![
                (READLOCAL0, 0),
                (LOADINT1, 219),
                (SUB, 2),
                (MOVEREADLOCAL0, 0),
                (MOVEREADLOCAL1, 1),
                (LOADINT1, 219),
                (SUB, 2),
                (CALLGLOBAL, 2),
            ],
        ];

        fs::write(dest_path, generate_opcode_map(patterns)).unwrap();
    }

    println!("cargo:rerun-if-changed=build.rs");
}
