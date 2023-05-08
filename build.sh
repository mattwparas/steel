cd crates/

cd steel-toml && cargo-steel-lib && cd ..
cd steel-webserver && cargo-steel-lib && cd ..
cd example-dylib && cargo-steel-lib && cd ..

cargo build

cd ..