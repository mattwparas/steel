use crate::gc::Gc;
use crate::SteelVal;

// pub fn is_negative_zero(number: f64) -> bool {
//     number == 0 && (number as isize) != 0
// }

// Tags
pub const PTR_TAG: u64 = 0xfffa000000000000;
pub const MAX_DOUBLE: u64 = 0xfff8000000000000;
pub const INT32_TAG: u64 = 0xfff9000000000000;

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

// inline int32_t getInt32() const {
//     assert(isInt32());

//     return static_cast<int32_t>(asBits & ~Int32Tag);
// }

pub fn to_encoded_double(value: &Gc<SteelVal>) -> f64 {
    match value.as_ref() {
        SteelVal::IntV(i) => from_i32(*i as i32),
        SteelVal::NumV(n) => *n,
        _ => coerce_value(value.as_ptr() as u64),
    }
}

// TODO move this to a trait
pub fn to_encoded_double_from_const_ptr(value: *const SteelVal) -> f64 {
    coerce_value(value as u64)
}

pub fn decode(ptr: f64) -> SteelVal {
    let bits = ptr.to_bits();
    if is_int32(bits) {
        println!("Decoding an int");
        SteelVal::IntV(get_int32(ptr) as isize)
    } else if is_double(bits) {
        println!("Decoding a double");
        SteelVal::NumV(ptr)
    } else {
        println!("Decoding a steelval ref");
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

    // #[test]
    // fn test_addition() {
    //     let left = 10;
    //     let right = 20;

    //     // let encoded_left =
    // }

    // #[test]
    // fn test_is_float_encoded_value()
}
