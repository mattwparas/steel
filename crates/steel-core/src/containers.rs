use crate::{SteelVal};

mod private {
    use crate::steel_vm::{builtin::BuiltInModule, engine::Engine};

    pub trait Sealed {}

    // Implement for those same types, but no others.
    impl Sealed for BuiltInModule {}
    impl Sealed for Engine {}
}

/// Blanket implementation for things that can register values into the container,
/// used for RegisterFn
pub(crate) trait RegisterValue: private::Sealed {
    fn register_value_inner(&mut self, name: &str, value: SteelVal) -> &mut Self;
}
