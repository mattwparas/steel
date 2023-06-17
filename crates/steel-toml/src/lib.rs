use abi_stable::std_types::RVec;

use toml::Value;

use steel::declare_module;
use steel::steel_vm::ffi::{FFIModule, FFIValue, RegisterFFIFn};

struct SteelTomlValue(Value);

impl SteelTomlValue {
    fn as_ffi_value(&self) -> FFIValue {
        as_ffi_value(&self.0)
    }
}

impl steel::rvals::Custom for SteelTomlValue {}

// fn as_native_steelval(value: &Value) -> steel::rvals::Result<SteelVal> {
//     match value {
//         Value::String(s) => Ok(s.clone().into()),
//         Value::Integer(i) => Ok((*i as i32).into()),
//         Value::Float(f) => Ok((*f).into()),
//         Value::Boolean(b) => Ok((*b).into()),
//         Value::Datetime(_) => todo!(),
//         Value::Array(a) => Ok(a
//             .into_iter()
//             .map(|x| as_native_steelval(x))
//             .collect::<steel::rvals::Result<steel::List<steel::SteelVal>>>()?
//             .into()),
//         Value::Table(m) => Ok(SteelVal::HashMapV(steel::gc::Gc::new(
//             m.into_iter()
//                 .map(|x| {
//                     let key: steel::SteelVal = x.0.clone().into();
//                     let value = as_native_steelval(x.1)?;

//                     Ok((key, value))
//                 })
//                 .collect::<steel::rvals::Result<steel::HashMap<_, _>>>()?,
//         ))),
//     }
// }

fn as_ffi_value(value: &Value) -> FFIValue {
    match value {
        Value::String(s) => s.clone().into(),
        Value::Integer(i) => (*i as isize).into(),
        Value::Float(f) => (*f).into(),
        Value::Boolean(b) => (*b).into(),
        Value::Datetime(_) => todo!(),
        Value::Array(a) => a
            .into_iter()
            .map(|x| as_ffi_value(x))
            .collect::<RVec<_>>()
            .into(),
        // Value::Table(m) => Ok(SteelVal::HashMapV(steel::gc::Gc::new(
        //     m.into_iter()
        //         .map(|x| {
        //             let key: steel::SteelVal = x.0.clone().into();
        //             let value = as_ffi_value(x.1);

        //             Ok((key, value))
        //         })
        //         .collect::<steel::rvals::Result<steel::HashMap<_, _>>>()?,
        // ))),
        _ => todo!(),
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

declare_module!(create_module);

fn create_module() -> FFIModule {
    let mut module = FFIModule::new("dylib/toml");

    module.register_fn("toml->value", SteelTomlValue::as_ffi_value);
    module
}
