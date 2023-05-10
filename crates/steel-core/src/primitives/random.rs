use rand::prelude::*;

use im_lists::list::List;

use crate::{rvals::Custom, steel_vm::builtin::BuiltInModule};
use crate::{steel_vm::register_fn::RegisterFn, SteelErr};

impl Custom for ThreadRng {}

pub fn random_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/random".to_string());

    module
        .register_fn("thread-rng!", thread_rng)
        .register_fn("rng->gen-usize", ThreadRng::gen::<usize>)
        .register_fn(
            "rng->gen-range",
            |rng: &mut ThreadRng, x: isize, y: isize| ThreadRng::gen_range(rng, x..y),
        );

    module
}
