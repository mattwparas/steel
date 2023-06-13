

use steel::steel_vm::{builtin::BuiltInModule, register_fn::RegisterFn};
use steel::SteelVal;

fn hidden_function() -> usize {
    10
}

// thread_local! {
//     static MODULE: Rc<BuiltInModule> = create_module();
// }

// #[no_mangle]
pub fn create_module() -> Box<BuiltInModule> {
    let mut module = BuiltInModule::new("external-dylib".to_string());

    module.register_value("outside-value", SteelVal::StringV("Hello world!".into()));
    module.register_fn("hidden-function", hidden_function);

    Box::new(module)
    // module
}

#[no_mangle]
pub fn generate_module() -> *mut BuiltInModule {
    Box::into_raw(create_module())
}

#[no_mangle]
pub fn build_module(module: &mut BuiltInModule) {
    module.set_name("external-dylib".to_string());

    module.register_value("outside-value", SteelVal::StringV("Hello world!".into()));
    module.register_fn("hidden-function", hidden_function);
}

#[no_mangle]
pub fn free_module(ptr: *mut BuiltInModule) {
    unsafe {
        let _ = Box::from_raw(ptr);
    }
}
