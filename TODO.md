# TODO:

* Implement `Rest` args on the Rust `register_fn` api. Let functions with multiple arguments be bound
* Implenent a `Trace` function on the `Custom` type. This will be necessary for any required GC passes
* Finish the C API for modules. Get real dylibs working.
* Figure out / plan the thread safety aspect.
  * Ideas: poisoned values, lazily instantiating an engine on a new thread. Channel to communicate values on
  * Naively deep clone the whole thing, fail quickly if we cannot do so.
