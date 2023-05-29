use std::rc::Rc;

use steel::steel_vm::{builtin::BuiltInModule, register_fn::RegisterFn};
use steel::SteelVal;

use toml::Value;

struct SteelTomlValue(Value);

impl SteelTomlValue {
    fn as_value(&self) -> steel::rvals::Result<SteelVal> {
        as_native_steelval(&self.0)
    }
}

impl steel::rvals::Custom for SteelTomlValue {}

fn as_native_steelval(value: &Value) -> steel::rvals::Result<SteelVal> {
    match value {
        Value::String(s) => Ok(s.clone().into()),
        Value::Integer(i) => Ok((*i as i32).into()),
        Value::Float(f) => Ok((*f).into()),
        Value::Boolean(b) => Ok((*b).into()),
        Value::Datetime(_) => todo!(),
        Value::Array(a) => Ok(a
            .into_iter()
            .map(|x| as_native_steelval(x))
            .collect::<steel::rvals::Result<steel::List<steel::SteelVal>>>()?
            .into()),
        Value::Table(m) => Ok(SteelVal::HashMapV(steel::gc::Gc::new(
            m.into_iter()
                .map(|x| {
                    let key: steel::SteelVal = x.0.clone().into();
                    let value = as_native_steelval(x.1)?;

                    Ok((key, value))
                })
                .collect::<steel::rvals::Result<steel::HashMap<_, _>>>()?,
        ))),
    }
}

// impl FromSteelVal for SteelTomlValue {
//     fn from_steelval(val: &SteelVal) -> steel::rvals::Result<Self> {
//         todo!()
//     }
// }

// thread_local! {
//     static MODULE: Rc<BuiltInModule> = create_module();
// }

fn add_100(x: usize) -> usize {
    x + 100
}

// #[no_mangle]
fn create_module() -> Box<BuiltInModule> {
    let mut module = BuiltInModule::new("dylib/toml".to_string());

    module.register_owned_fn("toml->value".to_string(), SteelTomlValue::as_value);
    module.register_fn("add-100", add_100);

    // module.register_type::<SteelTomlValue>("toml?");

    // module.register_

    // module.register_value("outside-value", SteelVal::StringV("Hello world!".into()));
    // module.register_fn("hidden-function", hidden_function);

    Box::new(module)

    // module
}

#[no_mangle]
pub fn build_module(module: &mut BuiltInModule) {
    module.set_name("dylib/toml".to_string());

    module.register_fn("toml->value", SteelTomlValue::as_value);
}

#[no_mangle]
pub fn generate_module() -> *mut BuiltInModule {
    Box::into_raw(create_module())
}

#[no_mangle]
pub fn free_module(ptr: *mut BuiltInModule) {
    unsafe {
        let _ = Box::from_raw(ptr);
    }
}
