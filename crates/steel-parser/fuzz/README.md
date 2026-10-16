# Fuzzer for the parser


## Running

```sh
cargo install cargo-afl
cd crates/steel-parser/fuzz
cargo afl build --release
cargo afl fuzz -i in -o out -x steel.dict target/release/chunked # Or pick which one
```

## Replaying a crash

```sh
target/release/chunked < out/default/crashes/id:000000,...
cargo afl tmin -i <crash> -o minimized target/release/chunked
```
