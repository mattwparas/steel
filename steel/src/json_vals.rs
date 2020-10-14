use crate::env::{FALSE, TRUE, VOID};
use crate::gc::Gc;
use crate::rerrs::SteelErr;
use crate::rvals::Result;
use crate::rvals::SteelVal;
use serde_json::Map;
use serde_json::Number;
use serde_json::Value;

use crate::primitives::ListOperations;
use std::convert::TryFrom;
use std::convert::TryInto;

use im_rc::HashMap;

use crate::throw;

// use list

pub fn string_to_jsexpr() -> SteelVal {
    SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
        if args.len() != 1 {
            stop!(ArityMismatch => "string->jsexpr takes 1 argument");
        } else {
            let arg =
                &args[0].string_or_else(throw!(TypeMismatch => "string->jsexpr takes a string"))?;
            let unescaped = unescape(arg);
            let res: std::result::Result<Value, _> = serde_json::from_str(unescaped.as_str());
            match res {
                Ok(res) => res.try_into(),
                Err(e) => stop!(Generic => format!("string->jsexpr failed: {}", e.to_string())),
            }
        }
    })
}

pub fn serialize_val_to_string() -> SteelVal {
    SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
        if args.len() != 1 {
            stop!(ArityMismatch => "serialize value takes one argument");
        } else {
            let arg = Gc::clone(&args[0]);
            let serde_value: Value = arg.try_into()?;
            let serialized_value = serde_value.to_string();
            Ok(Gc::new(SteelVal::StringV(serialized_value)))
        }
    })
}

// required to parse each string
fn unescape(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut chars = s.chars();
    while let Some(ch) = chars.next() {
        result.push(if ch != '\\' {
            ch
        } else {
            match chars.next() {
                // Some('u') => {
                //     let value = chars
                //         .by_ref()
                //         .take(4)
                //         .fold(0, |acc, c| acc * 16 + c.to_digit(16).unwrap());
                //     char::from_u32(value).unwrap()
                // }
                Some('b') => '\x08',
                Some('f') => '\x0c',
                Some('n') => '\n',
                Some('r') => '\r',
                Some('t') => '\t',
                Some(ch) => ch,
                _ => panic!("Malformed escape"),
            }
        })
    }
    result
}

impl TryFrom<Map<String, Value>> for Gc<SteelVal> {
    type Error = SteelErr;
    fn try_from(map: Map<String, Value>) -> std::result::Result<Self, Self::Error> {
        let mut hm = HashMap::new();
        for (key, value) in map {
            hm.insert(Gc::new(SteelVal::SymbolV(key)), value.try_into()?);
        }
        Ok(Gc::new(SteelVal::HashMapV(hm)))
    }
}

impl TryFrom<Value> for Gc<SteelVal> {
    type Error = SteelErr;
    fn try_from(val: Value) -> std::result::Result<Self, Self::Error> {
        match val {
            Value::Null => Ok(VOID.with(|x| Gc::clone(x))),
            Value::Bool(t) => {
                if t {
                    Ok(TRUE.with(|x| Gc::clone(x)))
                } else {
                    Ok(FALSE.with(|x| Gc::clone(x)))
                }
            }
            Value::Number(n) => <Gc<SteelVal>>::try_from(n),
            Value::String(s) => Ok(s.into()),
            Value::Array(v) => {
                ListOperations::built_in_list_func_iter_result(v.into_iter().map(|x| x.try_into()))
            }
            Value::Object(m) => m.try_into(),
        }
    }
}

// TODO
impl TryFrom<Number> for Gc<SteelVal> {
    type Error = SteelErr;
    fn try_from(n: Number) -> std::result::Result<Self, Self::Error> {
        let result = n.as_f64().unwrap();
        Ok(Gc::new(SteelVal::NumV(result)))
    }
}

// Attempt to serialize to json?
// It would be better to straight implement the deserialize method
// Honestly... this is not great
impl TryFrom<Gc<SteelVal>> for Value {
    type Error = SteelErr;
    fn try_from(val: Gc<SteelVal>) -> std::result::Result<Self, Self::Error> {
        match val.as_ref() {
            SteelVal::BoolV(b) => Ok(Value::Bool(*b)),
            SteelVal::NumV(n) => Ok(Value::Number(Number::from_f64(*n).unwrap())),
            SteelVal::IntV(n) => Ok(Value::Number(Number::from(*n))),
            SteelVal::CharV(c) => Ok(Value::String(c.to_string())),
            SteelVal::Pair(_, _) => Ok(Value::Array(
                SteelVal::iter(val)
                    .map(|x| x.try_into())
                    .collect::<Result<Vec<_>>>()?,
            )),
            SteelVal::VectorV(v) => Ok(Value::Array(
                v.into_iter()
                    .map(|x| Gc::clone(x).try_into())
                    .collect::<Result<Vec<_>>>()?,
            )),
            SteelVal::Void => stop!(Generic => "void not serializable"),
            SteelVal::StringV(s) => Ok(Value::String(s.clone())),
            SteelVal::FuncV(_) => stop!(Generic => "function not serializable"),
            SteelVal::LambdaV(_) => stop!(Generic => "function not serializable"),
            SteelVal::MacroV(_) => stop!(Generic => "macro not serializable"),
            SteelVal::SymbolV(s) => Ok(Value::String(s.clone())),
            SteelVal::Custom(_) => stop!(Generic => "generic struct not serializable"),
            SteelVal::HashMapV(hm) => {
                let mut map: Map<String, Value> = Map::new();
                for (key, value) in hm {
                    map.insert(key.try_into()?, Gc::clone(value).try_into()?);
                }
                Ok(Value::Object(map))
            }
            SteelVal::HashSetV(hs) => Ok(Value::Array(
                hs.into_iter()
                    .map(|x| Gc::clone(x).try_into())
                    .collect::<Result<Vec<_>>>()?,
            )),
            SteelVal::StructV(_) => stop!(Generic => "built in struct not serializable yet"),
            _ => stop!(Generic => "type not serializable"),
            // SteelVal::StructClosureV(_, _) => {}
            // SteelVal::PortV(_) => {}
            // SteelVal::Closure(_) => {}
            // SteelVal::IterV(_) => {}
            // SteelVal::FutureFunc(_) => {}
            // SteelVal::FutureV(_) => {}
            // SteelVal::StreamV(_) => {}
        }

        // unimplemented!()
    }
}
