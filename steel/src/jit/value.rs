use crate::gc::Gc;
use crate::rvals::{FromSteelVal, IntoSteelVal};
use crate::SteelVal;

use super::code_gen::JIT;

// pub fn is_negative_zero(number: f64) -> bool {
//     number == 0 && (number as isize) != 0
// }

// Tags
pub const PTR_TAG: u64 = 0xfffa000000000000;
pub const MAX_DOUBLE: u64 = 0xfff8000000000000;
pub const INT32_TAG: u64 = 0xfff9000000000000;

// pub const BOOLEAN_MASK: u64 = 0xfffb000000000002;
// pub const TRUE_VALUE: u64 = BOOLEAN_MASK | 3;
// pub const FALSE_VALUE: u64 = BOOLEAN_MASK | 2;

// pub const BOOELAN_MASK: 0xfff

// TODO should this be true - 6 false - 7 or the other way around
pub const TRUE_VALUE: u64 = 0x07;
pub const FALSE_VALUE: u64 = 0x06;

// pub const BOOL_TAG: u64 = 0x06;

pub fn is_bool(bits: u64) -> bool {
    bits == TRUE_VALUE || bits == FALSE_VALUE
}

pub fn get_as_bool(bits: u64) -> u64 {
    bits & 0x1
}

pub fn get_as_rust_bool(bits: u64) -> bool {
    is_bool(bits) && bits == TRUE_VALUE
}

pub fn encode_bool(b: bool) -> f64 {
    unsafe { std::mem::transmute(if b { TRUE_VALUE } else { FALSE_VALUE }) }
}

pub fn to_float(bits: u64) -> f64 {
    unsafe { std::mem::transmute(bits) }
}

pub fn is_double(bits: u64) -> bool {
    bits < MAX_DOUBLE
}

pub fn is_int32(bits: u64) -> bool {
    (bits & INT32_TAG) == INT32_TAG
}

pub fn is_pointer(bits: f64) -> bool {
    let bits = bits.to_bits();
    (bits & PTR_TAG) == PTR_TAG
}

fn coerce_value(bits: u64) -> f64 {
    assert!((bits & PTR_TAG) == 0);

    to_float(bits | PTR_TAG)
}

pub fn get_pointer(bits: f64) -> u64 {
    // If this isn't a pointer, things will go poorly
    assert!(is_pointer(bits));

    let bits = bits.to_bits();
    bits & !PTR_TAG
}

pub fn from_i32(value: i32) -> f64 {
    let bits = unsafe {
        let bits: u64 = std::mem::transmute(i64::from(value));
        bits | INT32_TAG
    };

    to_float(bits)
}

// inline Value(const int32_t number) {
//     asBits = number | Int32Tag;
// }

// If this is a double, just return it
pub fn get_double(bits: u64) -> f64 {
    assert!(is_double(bits));

    to_float(bits)
}

pub fn get_int32(value: f64) -> i64 {
    unsafe { std::mem::transmute(value.to_bits() & !INT32_TAG) }
}

pub(crate) trait ToNanTaggedValue {
    fn encode(self) -> f64;
}

impl ToNanTaggedValue for &Gc<SteelVal> {
    fn encode(self) -> f64 {
        to_encoded_double(self)
    }
}

impl ToNanTaggedValue for &SteelVal {
    fn encode(self) -> f64 {
        to_encoded_double_raw(self)
    }
}

impl<T: IntoSteelVal> ToNanTaggedValue for T {
    fn encode(self) -> f64 {
        let value = self
            .into_steelval()
            .expect("Value coercion to steelval failed!");

        let boxed = Gc::new(value);

        JIT::allocate(&boxed);

        boxed.encode()
    }
}

// Take a pointer to a value that implements from steel val and retrieve it
// Since these functions do coercion, this might be expensive and it may be better
// to move back and forth directly
pub(crate) fn from_nan_tagged_value<T: FromSteelVal>(ptr: f64) -> T {
    T::from_steelval(decode(ptr)).expect("Unable to coerce type from steelval")
}

// pub trait FromSteelVal: Sized {
//     fn from_steelval(val: SteelVal) -> Result<Self>;
// }

// inline int32_t getInt32() const {
//     assert(isInt32());

//     return static_cast<int32_t>(asBits & ~Int32Tag);
// }

pub fn to_encoded_double(value: &Gc<SteelVal>) -> f64 {
    match value.as_ref() {
        SteelVal::IntV(i) => from_i32(*i as i32),
        SteelVal::NumV(n) => *n,
        SteelVal::BoolV(b) => {
            if *b {
                to_float(TRUE_VALUE)
            } else {
                to_float(FALSE_VALUE)
            }
        }
        _ => coerce_value(value.as_ptr() as u64),
    }
}

pub fn to_encoded_double_raw(value: &SteelVal) -> f64 {
    match value {
        SteelVal::IntV(i) => from_i32(*i as i32),
        SteelVal::NumV(n) => *n,
        SteelVal::BoolV(b) => {
            if *b {
                to_float(TRUE_VALUE)
            } else {
                to_float(FALSE_VALUE)
            }
        }
        _ => coerce_value((value as *const SteelVal) as u64),
    }
}

// TODO move this to a trait
pub fn to_encoded_double_from_const_ptr(value: *const SteelVal) -> f64 {
    coerce_value(value as u64)
}

pub fn decode(ptr: f64) -> SteelVal {
    let bits = ptr.to_bits();
    if is_int32(bits) {
        // println!("Decoding an int");
        SteelVal::IntV(get_int32(ptr) as isize)
    } else if is_double(bits) {
        // println!("Decoding a double");
        SteelVal::NumV(ptr)
    } else if is_bool(bits) {
        SteelVal::BoolV(get_as_rust_bool(bits))
    } else {
        // println!("Decoding a steelval ref");
        unsafe { get_ref_from_double(ptr) }
    }
}

pub unsafe fn get_ref_from_double(ptr: f64) -> SteelVal {
    (&*(get_pointer(ptr) as *const SteelVal)).clone()
}

pub fn is_float_encoded_value(value: isize) -> bool {
    let bits: f64 = unsafe { std::mem::transmute(value) };
    is_pointer(bits)
}

#[cfg(test)]
mod value_tests {

    use super::*;
    use crate::gc::Gc;
    use crate::SteelVal;

    #[test]
    fn test_pointer_converstion() {
        let value = Gc::new(SteelVal::StringV(Gc::new("hello world".to_string())));
        let coerced = to_encoded_double(&value);
        let result = unsafe { get_ref_from_double(coerced) };
        assert_eq!(
            result,
            SteelVal::StringV(Gc::new("hello world".to_string()))
        );
    }

    #[test]
    fn test_is_float() {
        let value: f64 = 10.0;

        assert!(is_double(value.to_bits()))
    }

    #[test]
    fn test_ints() {
        let value: i32 = 1000;
        let encoded = from_i32(value);
        let decoded = decode(encoded);

        assert_eq!(SteelVal::IntV(1000), decoded);
    }

    #[test]
    fn test_bools() {
        let encoded_true: f64 = unsafe { std::mem::transmute(TRUE_VALUE) };

        let output = get_as_rust_bool(encoded_true.to_bits());

        assert_eq!(output, true);

        let encoded_false: f64 = unsafe { std::mem::transmute(FALSE_VALUE) };

        let output = get_as_rust_bool(encoded_false.to_bits());

        assert_eq!(output, false);
    }

    // #[test]
    // fn test_addition() {
    //     let left = 10;
    //     let right = 20;

    //     // let encoded_left =
    // }

    // #[test]
    // fn test_is_float_encoded_value()
}
