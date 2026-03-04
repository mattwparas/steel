use core::any::Any;
use std::collections::HashMap;

use once_cell::sync::Lazy;
use parking_lot::Mutex;

use crate::{
    rvals::{Custom, HeapSerializer, Result, SerializationContext},
    SteelVal,
};

#[derive(Default)]
pub struct SerializationMap {
    map: std::collections::HashMap<&'static str, Box<dyn Any + Send + Sync + 'static>>,
}

impl SerializationMap {
    pub fn insert<T: 'static>(&mut self, f: fn(&mut SerializationContext, &T) -> Result<Vec<u8>>) {
        self.map.insert(core::any::type_name::<T>(), Box::new(f));
    }

    pub fn call<T: 'static + ?Sized>(
        &self,
        ctx: &mut SerializationContext,
        arg: &T,
    ) -> Option<Result<Vec<u8>>> {
        self.map
            .get(&core::any::type_name::<T>())
            .and_then(|f| f.downcast_ref::<fn(&mut SerializationContext, &T) -> Result<Vec<u8>>>())
            .map(|f| f(ctx, arg))
    }
}

// In the runtime, if we want to serialize it, we'll
// have to serialize it somehow in a way that the values
// can be recovered.

#[derive(Default)]
pub struct NativeSerdeHandlers {
    serializers: SerializationMap,
    deserializers: CustomFunctionConstructors,
}

impl NativeSerdeHandlers {
    pub fn register_serializer<T: 'static>(
        &mut self,
        f: fn(&mut SerializationContext, &T) -> Result<Vec<u8>>,
    ) {
        self.serializers.insert(f);
    }

    pub fn register_deserializer<T: Custom + 'static>(
        &mut self,
        func: fn(&mut HeapSerializer, &[u8]) -> Result<SteelVal>,
    ) {
        self.deserializers.register::<T>(func)
    }
}

#[derive(Default)]
pub struct CustomFunctionConstructors {
    deserializers: HashMap<&'static str, fn(ctx: &mut HeapSerializer, &[u8]) -> Result<SteelVal>>,
}

impl CustomFunctionConstructors {
    pub fn register<T: Custom + 'static>(
        &mut self,
        func: fn(&mut HeapSerializer, &[u8]) -> Result<SteelVal>,
    ) {
        self.deserializers.insert(core::any::type_name::<T>(), func);
    }

    pub fn call_by_name(
        &mut self,
        name: &str,
        serializer: &mut HeapSerializer,
        bytes: &[u8],
    ) -> Option<Result<SteelVal>> {
        self.deserializers.get(name).map(|x| x(serializer, bytes))
    }
}

static SERDE_HANDLERS: Lazy<Mutex<NativeSerdeHandlers>> =
    Lazy::new(|| Mutex::new(NativeSerdeHandlers::default()));

pub fn register_serializer<T: 'static>(f: fn(&mut SerializationContext, &T) -> Result<Vec<u8>>) {
    SERDE_HANDLERS.lock().register_serializer(f);
}

pub fn register_deserializer<T: Custom + 'static>(
    func: fn(&mut HeapSerializer, &[u8]) -> Result<SteelVal>,
) {
    SERDE_HANDLERS.lock().register_deserializer::<T>(func);
}

pub fn call_serializer<T: 'static + ?Sized>(
    ctx: &mut SerializationContext,
    arg: &T,
) -> Option<Result<Vec<u8>>> {
    SERDE_HANDLERS.lock().serializers.call(ctx, arg)
}

/*
pub fn call_deserializer<T: Custom + 'static>(
    serializer: &mut HeapSerializer,
    bytes: &[u8],
) -> Option<Result<SteelVal>> {
    SERDE_HANDLERS
        .lock()
        .deserializers
        .call::<T>(serializer, bytes)
}
*/

pub fn call_deserializer_by_name(
    serializer: &mut HeapSerializer,
    name: &str,
    bytes: &[u8],
) -> Option<Result<SteelVal>> {
    SERDE_HANDLERS
        .lock()
        .deserializers
        .call_by_name(name, serializer, bytes)
}

#[cfg(test)]
mod serialization_tests {
    use serde::{Deserialize, Serialize};

    use crate::{
        rvals::{
            from_serializable_value, into_serializable_value, FromSteelVal, IntoSteelVal,
            SerializableSteelVal,
        },
        steel_vm::{builtin::BuiltInModule, engine::Engine, register_fn::RegisterFn},
    };

    use super::*;

    #[derive(Clone, PartialEq, Eq, Debug)]
    struct TestSerializationValue {
        value: SteelVal,
        foo: u64,
        baz: String,
    }

    #[derive(Serialize, Deserialize)]
    struct SerializableTestValue {
        value: SerializableSteelVal,
        foo: u64,
        baz: String,
    }

    impl Custom for TestSerializationValue {}

    #[test]
    fn register_serializers() {
        let mut handlers = NativeSerdeHandlers::default();

        handlers.register_serializer(|ctx, value: &TestSerializationValue| {
            let steel_val = into_serializable_value(value.value.clone(), ctx)?;

            let the_rest = SerializableTestValue {
                value: steel_val,
                foo: value.foo,
                baz: value.baz.clone(),
            };

            bincode::serialize(&the_rest)
                .map_err(|e| throw!(Generic => "Unable to serialize value: {}", e)())
        });

        handlers.register_deserializer::<TestSerializationValue>(|ctx, bytes| {
            let value: SerializableTestValue = bincode::deserialize(bytes)
                .map_err(|e| throw!(Generic => "Unable to deserialize value: {}", e)())?;

            let new_value = from_serializable_value(ctx, value.value);

            TestSerializationValue {
                value: new_value,
                foo: value.foo,
                baz: value.baz,
            }
            .into_steelval()
        });
    }

    #[test]
    fn register_static_serializers() {
        register_serializer(|ctx, value: &TestSerializationValue| {
            let steel_val = into_serializable_value(value.value.clone(), ctx)?;

            let the_rest = SerializableTestValue {
                value: steel_val,
                foo: value.foo,
                baz: value.baz.clone(),
            };

            bincode::serialize(&the_rest)
                .map_err(|e| throw!(Generic => "Unable to serialize value: {}", e)())
        });

        register_deserializer::<TestSerializationValue>(|ctx, bytes| {
            let value: SerializableTestValue = bincode::deserialize(bytes)
                .map_err(|e| throw!(Generic => "Unable to deserialize value: {}", e)())?;

            let new_value = from_serializable_value(ctx, value.value);

            TestSerializationValue {
                value: new_value,
                foo: value.foo,
                baz: value.baz,
            }
            .into_steelval()
        });

        let mut engine = Engine::new();
        let mut module = BuiltInModule::new("#%private/test/serde");
        module.register_fn("make-test-value", || TestSerializationValue {
            value: SteelVal::ListV(
                vec![
                    SteelVal::IntV(1),
                    SteelVal::IntV(2),
                    SteelVal::IntV(3),
                    SteelVal::IntV(4),
                    SteelVal::IntV(5),
                ]
                .into(),
            ),
            foo: 12345,
            baz: "Hello world!".to_owned(),
        });

        engine.register_module(module);

        let results = engine
            .run(
                r#"
(require-builtin #%private/test/serde)
(define value (make-test-value))
(define serialized (serialize-value value))
(deserialize-value serialized)
            "#,
            )
            .unwrap();

        let value = results.last().unwrap();

        let expected = TestSerializationValue {
            value: SteelVal::ListV(
                vec![
                    SteelVal::IntV(1),
                    SteelVal::IntV(2),
                    SteelVal::IntV(3),
                    SteelVal::IntV(4),
                    SteelVal::IntV(5),
                ]
                .into(),
            ),
            foo: 12345,
            baz: "Hello world!".to_owned(),
        };

        assert_eq!(
            TestSerializationValue::from_steelval(value).unwrap(),
            expected
        );
    }

    #[test]
    fn round_trip_vectors_and_functions() {
        let mut engine = Engine::new();

        engine
            .run(
                r#"
(define (round-trip value)
  (~> value serialize-value deserialize-value))
            "#,
            )
            .unwrap();

        engine.run(r#"
(define foo (let ((a (vector 10 20 30))) (lambda () (vector-set! a 0 (+ (vector-ref a 0) 100)) (vector-ref a 0))))
            "#).unwrap();

        engine
            .run(
                r#"
(define new (round-trip foo))
            "#,
            )
            .unwrap();

        engine
            .run(
                r#"
(new)
(new)
(new)
(new)
(new)
(assert! (equal? 610 (new)))
            "#,
            )
            .unwrap();
    }

    #[test]
    fn round_trip_values() {
        let mut engine = Engine::new();

        engine
            .run(
                r#"
(define (round-trip value)
  (~> value serialize-value deserialize-value))
            "#,
            )
            .unwrap();

        engine
            .run(
                r#"
(define foo (list 10 20 30 40))
            "#,
            )
            .unwrap();

        engine
            .run(
                r#"
(define new (round-trip foo))
            "#,
            )
            .unwrap();

        engine
            .run(
                r#"(assert! (equal? (list 10 20 30 40) new))
            "#,
            )
            .unwrap();
    }
}
