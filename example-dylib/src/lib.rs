use steel::steel_vm::{builtin::BuiltInModule, register_fn::RegisterFn};
use steel::SteelVal;

fn hidden_function() -> usize {
    10
}

#[no_mangle]
fn generate_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("external-dylib".to_string());

    module.register_value("outside-value", SteelVal::StringV("Hello world!".into()));
    module.register_fn("hidden-function", hidden_function);

    module
}
