# Fuzzing the reader

Three [afl.rs](https://github.com/rust-fuzz/afl.rs) targets:

| target | what it looks for |
| --- | --- |
| `chunked` | feeding the parser in chunks must produce what parsing the whole input at once does |
| `parse` | parsing arbitrary text produces a value or an error, never a panic |
| `lex` | the same, for the lexer on its own |

`chunked` is the interesting one. It takes the first input byte as the chunk size
(1-64) and the rest as the text, then compares a chunked parse against a single
pass. Suspending and resuming across a chunk boundary is the part of the reader
most likely to disagree, and it is what `steel_parser::differential` checks.

The release profile here turns `debug_assert!` on, since the parser states a
number of its invariants that way.

## Running

```sh
cargo install cargo-afl        # once
cd crates/steel-parser/fuzz
cargo afl build --release

cargo afl fuzz -i in -o out -x steel.dict target/release/chunked
```

Swap in `parse` or `lex` for the other targets. To use every core, run one
`-M main` instance and several `-S worker-N` ones against the same `-o out`.

## Triaging a crash

The targets read stdin when they are not running under afl, so a crash file
replays directly:

```sh
target/release/chunked < out/default/crashes/id:000000,...
```

`cargo afl tmin -i <crash> -o minimized target/release/chunked` shrinks it first,
which usually makes the input obvious. Once it is small enough to read, add it to
`CORPUS` in `crates/steel-parser/tests/incremental.rs` so it stays covered.

## Seeds

`in/` covers the constructs worth starting from: quote prefixes inside and outside
lists, stacked prefixes, `#;`, both comment forms, vectors and bytevectors, dotted
pairs, deep nesting, and a long quoted list. Each seed begins with a `\x08` byte so
the `chunked` target starts at a chunk size of 9; the other targets treat it as a
leading control character, which is itself worth covering.
