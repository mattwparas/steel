use crate::steel_vm::engine::Engine;

/// The Kernel is an engine context used to evaluate defmacro style macros
/// It lives inside the compiler, so in theory there could be tiers of kernels
pub struct Kernel {
    engine: Box<Engine>,
}

impl Kernel {
    pub fn new() -> Self {
        todo!()
    }

    pub fn expand_macros() {
        todo!()
    }
}
