use crate::gc::Gc;
use crate::SteelVal;

// pub fn is_negative_zero(number: f64) -> bool {
//     number == 0 && (number as isize) != 0
// }

// Tags
const PTR_TAG: u64 = 0xfffa000000000000;
const MAX_DOUBLE: u64 = 0xfff8000000000000;
const INT32_TAG: u64 = 0xfff9000000000000;

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

// If this is a double, just return it
pub fn get_double(bits: u64) -> f64 {
    assert!(is_double(bits));

    to_float(bits)
}

pub fn to_encoded_double(value: &Gc<SteelVal>) -> f64 {
    coerce_value(value.as_ptr() as u64)
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

    // #[test]
    // fn test_is_float_encoded_value()
}
