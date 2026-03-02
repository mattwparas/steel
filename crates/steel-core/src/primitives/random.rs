use crate::steel_vm::builtin::BuiltInModule;
use crate::steel_vm::register_fn::RegisterFn;

// pub(crate) const THREAD_RNG_DOC: DocTemplate<'static> = DocTemplate {
//     signature: "(thread-rng!) -> ThreadRng?",
//     params: &[],
//     description: r#"Constructs a `ThreadRng` object"#,
//     examples: &[],
// };

// pub(crate) const RNG_GEN_USIZE: DocTemplate<'static> = DocTemplate {
//     signature: "(rng->gen-usize rng) -> int?",
//     params: &["rng: ThreadRng?"],
//     description: r#"Generates a random unsigned integer"#,
//     examples: &[],
// };

/// Constructs the random module
pub fn random_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/random");

    module
        // .register_fn("thread-rng!", thread_rng)
        // .register_doc("thread-rng!", THREAD_RNG_DOC)
        .register_fn("rng->gen-usize", random_u64)
        // .register_doc("rng->gen-usize", RNG_GEN_USIZE)
        .register_fn("rng->gen-range", random_range_i64);

    module
}

fn random_u64() -> u64 {
    let mut bytes = [0u8; 8];
    getrandom::fill(&mut bytes).expect("getrandom failed");
    u64::from_ne_bytes(bytes)
}

fn random_range_i64(start: i64, end: i64) -> i64 {
    assert!(start < end, "random range is empty");
    let span = (end as i128 - start as i128) as u64;
    if span == 1 {
        return start;
    }
    let offset = random_range_u64(span) as i128;
    (start as i128 + offset) as i64
}

fn random_range_u64(span: u64) -> u64 {
    let zone = u64::MAX - (u64::MAX % span);
    loop {
        let value = random_u64();
        if value < zone {
            return value % span;
        }
    }
}
