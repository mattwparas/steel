use rand::prelude::*;

use crate::steel_vm::register_fn::RegisterFn;
use crate::{rvals::Custom, steel_vm::builtin::BuiltInModule};

impl Custom for ThreadRng {}

pub fn random_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/random");

    module
        .register_fn("thread-rng!", thread_rng)
        .register_fn("rng->gen-usize", ThreadRng::gen::<usize>)
        .register_fn(
            "rng->gen-range",
            |rng: &mut ThreadRng, x: isize, y: isize| ThreadRng::gen_range(rng, x..y),
        );

    module
}
