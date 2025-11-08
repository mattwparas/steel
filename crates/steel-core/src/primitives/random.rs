use crate::rand;
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
        .register_fn("rng->gen-usize", rand::random_u64)
        // .register_doc("rng->gen-usize", RNG_GEN_USIZE)
        .register_fn("rng->gen-range", |x: i64, y: i64| rand::random_range(x..y));

    module
}
