use crate::rvals::Custom;
use std::{time::Duration, time::Instant};
// use chrono::

// TODO fix this noise

use crate::steel_vm::builtin::BuiltInModule;
use crate::steel_vm::register_fn::RegisterFn;

fn duration_to_string(duration: Duration) -> String {
    format!("{:?}", duration)
}

pub fn time_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/time".to_string());

    module
        .register_fn("instant/now", Instant::now)
        .register_fn("instant/elapsed", Instant::elapsed)
        .register_fn("duration-since", Instant::duration_since)
        .register_fn("duration->string", duration_to_string);

    module
}

impl Custom for Instant {}
impl Custom for Duration {}
