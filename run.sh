cd self_hosted;
cargo run --bin main --manifest-path /home/matt/Documents/steel/Cargo.toml --release compiler.rkt;
cd ..;
cargo run --bin runner self_hosted/test.txt;
