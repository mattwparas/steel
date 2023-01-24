use crate::{rvals::Custom, steel_vm::builtin::MarkdownDoc};
use std::{time::Duration, time::Instant};
// use chrono::

// TODO fix this noise

use crate::steel_vm::builtin::BuiltInModule;
use crate::steel_vm::register_fn::RegisterFn;

pub(crate) const TIME_MODULE_DOC: MarkdownDoc<'static> = MarkdownDoc(
    r#"

# steel/time
    
Contains direct wrappers around the Rust `std::time::Instant` and `std::time::Duration` modules. For example, to measure the time something takes:

```scheme
(define t (instant/now))
(displayln "Hello world")
(displayln (instant/elapsed t))
```

"#,
);

fn duration_to_string(duration: Duration) -> String {
    format!("{:?}", duration)
}

pub fn time_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/time".to_string());

    module.register_doc("steel/time", TIME_MODULE_DOC);

    module
        .register_fn("instant/now", Instant::now)
        .register_fn("instant/elapsed", Instant::elapsed)
        .register_fn("duration-since", Instant::duration_since)
        .register_fn("duration->string", duration_to_string)
        .register_fn("duration->seconds", Duration::as_secs);

    module
}

impl Custom for Instant {}
impl Custom for Duration {}
