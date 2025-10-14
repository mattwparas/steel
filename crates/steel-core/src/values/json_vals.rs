use crate::rvals::SteelString;
use crate::values::lists::List;
use crate::values::HashMap;
use crate::{
    gc::Gc,
    rerrs::SteelErr,
    rvals::{FromSteelVal, IntoSteelVal, Result, SteelVal},
};
use core::convert::{TryFrom, TryInto};
use serde_json::{Map, Number, Value};
use steel_derive::function;

/// Deserializes a JSON string into a Steel value.
///
/// (string->jsexpr json) -> any/c
///
/// * json : string?
///
/// # Examples
/// ```scheme
/// (string->jsexpr "{\"foo\": [3]}") ;; => '#hash((foo . (3)))
/// ```
#[function(name = "string->jsexpr")]
pub fn string_to_jsexpr(value: &SteelString) -> Result<SteelVal> {
    // let unescaped = unescape(&value);
    let unescaped = value;
    let res: core::result::Result<Value, _> = serde_json::from_str(unescaped.as_str());

    match res {
        Ok(res) => res.try_into(),
        Err(e) => stop!(Generic => format!("string->jsexpr failed: {e}")),
    }
}

/// Serializes a Steel value into a string.
///
/// (value->jsexpr-string any/c) -> string?
///
/// # Examples
/// ```scheme
/// (value->jsexpr-string `(,(hash "foo" #t))) ;; => "[{\"foo\":true}]"
/// ```
#[function(name = "value->jsexpr-string")]
pub fn serialize_val_to_string(value: SteelVal) -> Result<SteelVal> {
    let serde_value: Value = value.try_into()?;
    let serialized_value = serde_value.to_string();
    Ok(SteelVal::StringV(serialized_value.into()))
}

// required to parse each string
#[allow(unused)]
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

impl TryFrom<Map<String, Value>> for SteelVal {
    type Error = SteelErr;
    fn try_from(map: Map<String, Value>) -> core::result::Result<Self, Self::Error> {
        let mut hm = HashMap::new();
        for (key, value) in map {
            hm.insert(SteelVal::SymbolV(key.into()), value.try_into()?);
        }
        Ok(SteelVal::HashMapV(Gc::new(hm).into()))
    }
}

impl TryFrom<Value> for SteelVal {
    type Error = SteelErr;
    fn try_from(val: Value) -> core::result::Result<Self, Self::Error> {
        match val {
            Value::Null => Ok(SteelVal::Void),
            Value::Bool(t) => Ok(SteelVal::BoolV(t)),
            Value::Number(n) => <SteelVal>::try_from(n),
            Value::String(s) => Ok(SteelVal::StringV(s.into())),
            Value::Array(v) => Ok(SteelVal::ListV(
                v.into_iter()
                    .map(<SteelVal>::try_from)
                    .collect::<Result<List<SteelVal>>>()?,
            )),
            Value::Object(m) => m.try_into(),
        }
    }
}

// TODO
impl TryFrom<Number> for SteelVal {
    type Error = SteelErr;
    fn try_from(n: Number) -> core::result::Result<Self, Self::Error> {
        let result = n.as_f64().unwrap();
        Ok(SteelVal::NumV(result))
    }
}

impl IntoSteelVal for Value {
    fn into_steelval(self) -> Result<SteelVal> {
        self.try_into()
    }
}

impl FromSteelVal for Value {
    fn from_steelval(val: &SteelVal) -> Result<Self> {
        val.clone().try_into()
    }
}

// Attempt to serialize to json?
// It would be better to straight implement the deserialize method
// Honestly... this is not great
impl TryFrom<SteelVal> for Value {
    type Error = SteelErr;
    fn try_from(val: SteelVal) -> core::result::Result<Self, Self::Error> {
        match val {
            SteelVal::BoolV(b) => Ok(Value::Bool(b)),
            SteelVal::NumV(n) => Ok(Value::Number(Number::from_f64(n).unwrap())),
            SteelVal::IntV(n) => Ok(Value::Number(Number::from(n))),
            SteelVal::CharV(c) => Ok(Value::String(c.to_string())),
            // SteelVal::Pair(_) => Ok(Value::Array(
            //     SteelVal::iter(val)
            //         .map(|x| x.try_into())
            //         .collect::<Result<Vec<_>>>()?,
            // )),
            SteelVal::ListV(l) => Ok(Value::Array(
                l.into_iter()
                    .map(|x| x.try_into())
                    .collect::<Result<Vec<_>>>()?,
            )),
            SteelVal::VectorV(v) => Ok(Value::Array(
                v.iter()
                    .map(|x| x.clone().try_into())
                    .collect::<Result<Vec<_>>>()?,
            )),
            SteelVal::Void => Ok(Value::Null),
            SteelVal::StringV(s) => Ok(Value::String(s.to_string())),
            SteelVal::FuncV(_) => stop!(Generic => "function not serializable"),
            // SteelVal::LambdaV(_) => stop!(Generic => "function not serializable"),
            // SteelVal::MacroV(_) => stop!(Generic => "macro not serializable"),
            SteelVal::SymbolV(s) => Ok(Value::String(s.to_string())),
            SteelVal::Custom(_) => stop!(Generic => "generic struct not serializable"),
            SteelVal::HashMapV(hm) => {
                let mut map: Map<String, Value> = Map::new();
                for (key, value) in hm.iter() {
                    map.insert(key.clone().try_into()?, value.clone().try_into()?);
                }
                Ok(Value::Object(map))
            }
            SteelVal::HashSetV(hs) => Ok(Value::Array(
                hs.iter()
                    .map(|x| x.clone().try_into())
                    .collect::<Result<Vec<_>>>()?,
            )),
            // SteelVal::StructV(_) => stop!(Generic => "built in struct not serializable yet"),
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

#[cfg(test)]
mod json_tests {
    use super::*;

    use crate::rvals::SteelVal::*;

    #[cfg(not(feature = "sync"))]
    use im_rc::hashmap;

    #[cfg(all(feature = "sync", not(feature = "imbl")))]
    use im::hashmap;

    #[cfg(all(feature = "sync", feature = "imbl"))]
    use imbl::hashmap;

    #[test]
    fn test_string_to_jsexpr() {
        let json_expr = r#"{"a":"applesauce","b":"bananas"}"#;

        let result = string_to_jsexpr(&json_expr.into());

        let expected = SteelVal::HashMapV(
            Gc::new(hashmap! {
                SymbolV("a".into()) => StringV("applesauce".into()),
                SymbolV("b".into()) => StringV("bananas".into())
            })
            .into(),
        );

        assert_eq!(result.unwrap(), expected);
    }
}
