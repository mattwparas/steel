pub mod bytevectors;
pub mod contracts;
mod control;
mod fs;
pub mod hashmaps;
pub mod hashsets;
pub mod http;
mod io;
pub mod lists;
pub mod meta_ops;
/// Implements numbers as defined in section 6.2 of the R7RS spec.
pub mod numbers;

#[cfg(all(feature = "std", not(target_family = "wasm")))]
pub mod polling;

pub mod ports;
pub mod process;
pub mod random;
mod streams;
pub mod strings;
mod symbols;
pub mod tcp;
pub mod time;
pub mod transducers;
mod utils;
pub mod vectors;

// This is for boot strapping the package
// manager with an embedded git implementation,
// as to not require depending on the system git.
pub mod git;

pub mod hashes;

use crate::gc::{Gc, GcMut};

use core::convert::TryFrom;
use core::{any, result};

// When building without the standard library, alias `core` to `std` so
// common references like `std::mem`, `std::result`, `std::fmt` resolve to
// `core::mem`, `core::result`, `core::fmt`.
use crate::rvals::{FromSteelVal, IntoSteelVal, SteelByteVector};
use crate::rvals::{
    FunctionSignature, PrimitiveAsRef, PrimitiveAsRefMut, SteelHashMap, SteelHashSet, SteelVal,
    SteelVector,
};
use crate::values::closed::HeapRef;
use crate::values::lists::List;
use crate::values::port::SteelPort;
use crate::values::structs::UserDefinedStruct;
use crate::values::Vector;
use crate::{
    rerrs::{ErrorKind, SteelErr},
    rvals::SteelString,
};
pub use control::ControlOperations;
pub use fs::{fs_module, fs_module_sandbox};
pub use io::IoFunctions;
pub use lists::UnRecoverableResult;
pub use meta_ops::MetaOperations;
use num_bigint::BigInt;
use num_rational::{BigRational, Rational32};
use num_traits::ToPrimitive;
pub use numbers::{add_primitive, divide_primitive, multiply_primitive, subtract_primitive};
pub use ports::port_module;
pub use streams::StreamOperations;
pub use strings::string_module;
pub use symbols::symbol_module;

macro_rules! try_from_impl {
    ($type:ident => $($body:ty),*) => {
        $(
            impl TryFrom<SteelVal> for $body {
                type Error = SteelErr;
                #[inline]
                fn try_from(value: SteelVal) -> result::Result<Self, Self::Error> {
                    match value {
                        SteelVal::$type(x) => Ok(x.clone() as $body),
                        _ => Err(SteelErr::new(ErrorKind::ConversionError, format!("Expected number, found: {}", value))),
                    }
                }
            }

            impl TryFrom<&SteelVal> for $body {
                type Error = SteelErr;
                #[inline]
                fn try_from(value: &SteelVal) -> result::Result<Self, Self::Error> {
                    match value {
                        SteelVal::$type(x) => Ok(x.clone() as $body),
                        _ => Err(SteelErr::new(ErrorKind::ConversionError, format!("Expected number, found: {}", value))),
                    }
                }
            }

            impl FromSteelVal for $body {
                #[inline]
                fn from_steelval(value: &SteelVal) -> result::Result<Self, SteelErr> {
                    match value {
                        SteelVal::$type(x) => Ok(x.clone() as $body),
                        _ => Err(SteelErr::new(ErrorKind::ConversionError, format!("Expected number, found: {}", value))),
                    }
                }
            }

        )*
    };
}

macro_rules! from_f64 {
    ($($body:ty),*) => {
        $(
            impl From<$body> for SteelVal {
                #[inline]
                fn from(val: $body) -> SteelVal {
                    SteelVal::NumV(val as f64)
                }
            }

            impl IntoSteelVal for $body {
                #[inline]
                fn into_steelval(self) -> Result<SteelVal, SteelErr> {
                    Ok(SteelVal::NumV(self as f64))
                }
            }
        )*
    };
}

macro_rules! from_for_isize {
    ($($body:ty),*) => {
        $(
            impl From<$body> for SteelVal {
                #[inline]
                fn from(val: $body) -> SteelVal {
                    SteelVal::IntV(val as isize)
                }
            }

            impl IntoSteelVal for $body {
                #[inline]
                fn into_steelval(self) -> Result<SteelVal, SteelErr> {
                    Ok(SteelVal::IntV(self as isize))
                }
            }
        )*
    };
}

impl From<i64> for SteelVal {
    fn from(value: i64) -> Self {
        if let Ok(converted) = TryInto::<isize>::try_into(value) {
            SteelVal::IntV(converted)
        } else {
            SteelVal::BigNum(Gc::new(value.into()))
        }
    }
}

impl FromSteelVal for u8 {
    #[inline]
    fn from_steelval(val: &SteelVal) -> crate::rvals::Result<Self> {
        match val {
            SteelVal::IntV(v) => (*v).try_into().map_err(|_err| {
                SteelErr::new(
                    ErrorKind::ConversionError,
                    format!("Unable to convert isize to u8: {}", v),
                )
            }),
            SteelVal::BigNum(n) => n.as_ref().try_into().map_err(|_err| {
                SteelErr::new(
                    ErrorKind::ConversionError,
                    format!("Unable to convert bignum to u8: {:?}", n),
                )
            }),
            _ => Err(SteelErr::new(
                ErrorKind::ConversionError,
                format!("Unable to convert steelval to u8: {}", val),
            )),
        }
    }
}

impl From<usize> for SteelVal {
    #[inline]
    fn from(value: usize) -> Self {
        if value > isize::MAX as usize {
            SteelVal::BigNum(Gc::new(value.into()))
        } else {
            SteelVal::IntV(value as isize)
        }
    }
}

impl IntoSteelVal for usize {
    #[inline]
    fn into_steelval(self) -> crate::rvals::Result<SteelVal> {
        Ok(SteelVal::from(self))
    }
}

impl IntoSteelVal for u128 {
    fn into_steelval(self) -> crate::rvals::Result<SteelVal> {
        Ok(SteelVal::from(self))
    }
}

impl From<u128> for SteelVal {
    fn from(value: u128) -> Self {
        if value > isize::MAX as u128 {
            SteelVal::BigNum(Gc::new(value.into()))
        } else {
            SteelVal::IntV(value as isize)
        }
    }
}

impl IntoSteelVal for i64 {
    #[inline]
    fn into_steelval(self) -> crate::rvals::Result<SteelVal> {
        Ok(self.into())
    }
}

impl FromSteelVal for i64 {
    fn from_steelval(val: &SteelVal) -> crate::rvals::Result<Self> {
        match val {
            SteelVal::IntV(v) => (*v).try_into().map_err(|_err| {
                SteelErr::new(
                    ErrorKind::ConversionError,
                    format!("Unable to convert i64 to isize: {}", v),
                )
            }),
            SteelVal::BigNum(n) => n.as_ref().try_into().map_err(|_err| {
                SteelErr::new(
                    ErrorKind::ConversionError,
                    format!("Unable to convert bignum to isize: {:?}", n),
                )
            }),
            _ => Err(SteelErr::new(
                ErrorKind::ConversionError,
                format!("Unable to convert steelval to isize: {}", val),
            )),
        }
    }
}

impl From<char> for SteelVal {
    #[inline]
    fn from(val: char) -> SteelVal {
        SteelVal::CharV(val)
    }
}

impl IntoSteelVal for char {
    #[inline]
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(SteelVal::CharV(self))
    }
}

impl FromSteelVal for char {
    fn from_steelval(val: &SteelVal) -> Result<Self, SteelErr> {
        if let SteelVal::CharV(c) = val {
            Ok(*c)
        } else {
            Err(SteelErr::new(
                ErrorKind::ConversionError,
                "Expected character".to_string(),
            ))
        }
    }
}

impl<T: Into<SteelVal>> From<Option<T>> for SteelVal {
    #[inline]
    fn from(val: Option<T>) -> SteelVal {
        if let Some(s) = val {
            s.into()
        } else {
            SteelVal::BoolV(true)
        }
    }
}

impl<T: IntoSteelVal> IntoSteelVal for Option<T> {
    #[inline]
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        if let Some(s) = self {
            s.into_steelval()
        } else {
            Ok(SteelVal::BoolV(false))
        }
    }
}

impl<T: FromSteelVal> FromSteelVal for Option<T> {
    #[inline]
    fn from_steelval(val: &SteelVal) -> Result<Self, SteelErr> {
        if val.is_truthy() {
            Ok(Some(T::from_steelval(val)?))
        } else {
            Ok(None)
        }
    }
}

impl FromSteelVal for SteelVal {
    #[inline]
    fn from_steelval(val: &SteelVal) -> Result<Self, SteelErr> {
        Ok(val.clone())
    }
}

impl FromSteelVal for () {
    fn from_steelval(val: &SteelVal) -> Result<Self, SteelErr> {
        if let SteelVal::Void = val {
            Ok(())
        } else {
            crate::stop!(ConversionError => "could not convert value to unit type")
        }
    }
}

impl IntoSteelVal for () {
    #[inline]
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(SteelVal::Void)
    }
}

impl From<()> for SteelVal {
    #[inline]
    fn from(_: ()) -> SteelVal {
        SteelVal::Void
    }
}

impl IntoSteelVal for Rational32 {
    #[inline]
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        if self.is_integer() {
            self.numer().into_steelval()
        } else {
            Ok(SteelVal::Rational(self))
        }
    }
}

impl IntoSteelVal for BigInt {
    #[inline]
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        match self.to_isize() {
            Some(i) => i.into_steelval(),
            None => Ok(SteelVal::BigNum(crate::gc::Gc::new(self))),
        }
    }
}

impl IntoSteelVal for BigRational {
    #[inline]
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        if self.is_integer() {
            let (n, _) = self.into();
            return n.into_steelval();
        }
        match (self.numer().to_i32(), self.denom().to_i32()) {
            (Some(n), Some(d)) => Rational32::new(n, d).into_steelval(),
            _ => Ok(SteelVal::BigRational(Gc::new(self))),
        }
    }
}

from_f64!(f64, f32);
from_for_isize!(i32, i16, i8, u8, u16, u32, u64, isize);
try_from_impl!(NumV => f64, f32);
try_from_impl!(IntV => i32, i16, i8, u16, u32, u64, usize, isize);

impl TryFrom<SteelVal> for String {
    type Error = SteelErr;
    #[inline]
    fn try_from(value: SteelVal) -> result::Result<Self, Self::Error> {
        match value {
            SteelVal::StringV(ref x) => Ok(x.to_string()),
            SteelVal::SymbolV(ref x) => Ok(x.to_string()),
            _ => Err(SteelErr::new(
                ErrorKind::ConversionError,
                "Expected string".to_string(),
            )),
        }
    }
}

impl From<SteelVal> for Gc<SteelVal> {
    #[inline]
    fn from(val: SteelVal) -> Self {
        Gc::new(val)
    }
}

impl From<Gc<SteelVal>> for SteelVal {
    #[inline]
    fn from(val: Gc<SteelVal>) -> Self {
        (*val).clone()
    }
}

impl FromSteelVal for String {
    #[inline]
    fn from_steelval(val: &SteelVal) -> Result<Self, SteelErr> {
        match val {
            SteelVal::StringV(s) | SteelVal::SymbolV(s) => Ok(s.to_string()),
            _ => Err(SteelErr::new(
                ErrorKind::ConversionError,
                format!("Expected string, found: {val}"),
            )),
        }
    }
}

impl TryFrom<&SteelVal> for String {
    type Error = SteelErr;
    #[inline]
    fn try_from(value: &SteelVal) -> result::Result<Self, Self::Error> {
        match value {
            SteelVal::StringV(x) => Ok(x.to_string()),
            SteelVal::SymbolV(x) => Ok(x.to_string()),
            _ => Err(SteelErr::new(
                ErrorKind::ConversionError,
                "Expected string".to_string(),
            )),
        }
    }
}

impl From<String> for SteelVal {
    #[inline]
    fn from(val: String) -> SteelVal {
        SteelVal::StringV(val.into())
    }
}

impl IntoSteelVal for &str {
    #[inline]
    fn into_steelval(self) -> crate::rvals::Result<SteelVal> {
        Ok(SteelVal::StringV(self.into()))
    }
}

impl FromSteelVal for SteelString {
    #[inline]
    fn from_steelval(val: &SteelVal) -> crate::rvals::Result<Self> {
        if let SteelVal::StringV(s) = val {
            Ok(s.clone())
        } else {
            crate::stop!(ConversionError => format!("Cannot convert steel value: {} to steel string", val))
        }
    }
}

pub enum Either<L, R> {
    Left(L),
    Right(R),
}

impl<'a, L: PrimitiveAsRef<'a>, R: PrimitiveAsRef<'a>> PrimitiveAsRef<'a> for Either<L, R> {
    #[inline(always)]
    fn primitive_as_ref(val: &'a SteelVal) -> crate::rvals::Result<Self> {
        let left_type_name = any::type_name::<L>();
        let right_type_name = any::type_name::<R>();

        let error_thunk = crate::throw!(ConversionError => format!("Cannot convert steel value to the specified type: {} or {}", left_type_name, right_type_name));

        Self::maybe_primitive_as_ref(val).ok_or_else(error_thunk)
    }

    #[inline(always)]
    fn maybe_primitive_as_ref(val: &'a SteelVal) -> Option<Self> {
        L::maybe_primitive_as_ref(val)
            .map(Either::Left)
            .or_else(|| R::maybe_primitive_as_ref(val).map(Either::Right))
    }
}

impl<'a> PrimitiveAsRef<'a> for &'a SteelByteVector {
    fn primitive_as_ref(val: &'a SteelVal) -> crate::rvals::Result<Self> {
        Self::maybe_primitive_as_ref(val).ok_or_else(
            crate::throw!(ConversionError => format!("Cannot convert value to bytevector: {}", val)),
        )
    }

    fn maybe_primitive_as_ref(val: &'a SteelVal) -> Option<Self> {
        if let SteelVal::ByteVector(s) = val {
            Some(s)
        } else {
            None
        }
    }
}

impl<'a> PrimitiveAsRef<'a> for &'a UserDefinedStruct {
    fn primitive_as_ref(val: &'a SteelVal) -> crate::rvals::Result<Self> {
        Self::maybe_primitive_as_ref(val).ok_or_else(
            crate::throw!(ConversionError => format!("Cannot convert value to struct: {}", val)),
        )
    }

    fn maybe_primitive_as_ref(val: &'a SteelVal) -> Option<Self> {
        if let SteelVal::CustomStruct(s) = val {
            Some(s)
        } else {
            None
        }
    }
}

impl<'a> PrimitiveAsRef<'a> for &'a GcMut<SteelVal> {
    #[inline(always)]
    fn primitive_as_ref(val: &'a SteelVal) -> crate::rvals::Result<Self> {
        if let SteelVal::Boxed(c) = val {
            Ok(c)
        } else {
            crate::stop!(ConversionError => format!("Cannot convert steel value: {} to steel boxed value", val))
        }
    }

    #[inline(always)]
    fn maybe_primitive_as_ref(val: &'a SteelVal) -> Option<Self> {
        if let SteelVal::Boxed(c) = val {
            Some(c)
        } else {
            None
        }
    }
}

impl<'a> PrimitiveAsRef<'a> for &'a HeapRef<SteelVal> {
    #[inline(always)]
    fn primitive_as_ref(val: &'a SteelVal) -> crate::rvals::Result<Self> {
        if let SteelVal::HeapAllocated(b) = val {
            Ok(b)
        } else {
            crate::stop!(ConversionError => format!("Cannot convert steel value: {} to steel box", val))
        }
    }

    #[inline(always)]
    fn maybe_primitive_as_ref(val: &'a SteelVal) -> Option<Self> {
        if let SteelVal::HeapAllocated(b) = val {
            Some(b)
        } else {
            None
        }
    }
}

impl<'a> PrimitiveAsRef<'a> for &'a char {
    #[inline(always)]
    fn primitive_as_ref(val: &'a SteelVal) -> crate::rvals::Result<Self> {
        if let SteelVal::CharV(c) = val {
            Ok(c)
        } else {
            crate::stop!(ConversionError => format!("Cannot convert steel value: {} to steel character", val))
        }
    }

    #[inline(always)]
    fn maybe_primitive_as_ref(val: &'a SteelVal) -> Option<Self> {
        if let SteelVal::CharV(c) = val {
            Some(c)
        } else {
            None
        }
    }
}

impl<'a> PrimitiveAsRef<'a> for char {
    #[inline(always)]
    fn primitive_as_ref(val: &'a SteelVal) -> crate::rvals::Result<Self> {
        if let SteelVal::CharV(c) = val {
            Ok(*c)
        } else {
            crate::stop!(ConversionError => format!("Cannot convert steel value: {} to steel character", val))
        }
    }

    #[inline(always)]
    fn maybe_primitive_as_ref(val: &'a SteelVal) -> Option<Self> {
        if let SteelVal::CharV(c) = val {
            Some(*c)
        } else {
            None
        }
    }
}

impl<'a> PrimitiveAsRef<'a> for isize {
    #[inline(always)]
    fn primitive_as_ref(val: &'a SteelVal) -> crate::rvals::Result<Self> {
        if let SteelVal::IntV(i) = val {
            Ok(*i)
        } else {
            crate::stop!(ConversionError => format!("Cannot convert steel value: {} to steel int", val))
        }
    }

    #[inline(always)]
    fn maybe_primitive_as_ref(val: &'a SteelVal) -> Option<Self> {
        if let SteelVal::IntV(i) = val {
            Some(*i)
        } else {
            None
        }
    }
}

impl<'a> PrimitiveAsRef<'a> for &'a Gc<Vector<SteelVal>> {
    #[inline(always)]
    fn primitive_as_ref(val: &'a SteelVal) -> crate::rvals::Result<Self> {
        if let SteelVal::VectorV(p) = val {
            Ok(&p.0)
        } else {
            crate::stop!(ConversionError => format!("Cannot convert steel value: {} to steel vector", val))
        }
    }

    #[inline(always)]
    fn maybe_primitive_as_ref(val: &'a SteelVal) -> Option<Self> {
        if let SteelVal::VectorV(p) = val {
            Some(&p.0)
        } else {
            None
        }
    }
}

impl<'a> PrimitiveAsRef<'a> for &'a SteelVector {
    #[inline(always)]
    fn primitive_as_ref(val: &'a SteelVal) -> crate::rvals::Result<Self> {
        if let SteelVal::VectorV(p) = val {
            Ok(p)
        } else {
            crate::stop!(ConversionError => format!("Cannot convert steel value: {} to steel vector", val))
        }
    }

    #[inline(always)]
    fn maybe_primitive_as_ref(val: &'a SteelVal) -> Option<Self> {
        if let SteelVal::VectorV(p) = val {
            Some(p)
        } else {
            None
        }
    }
}

impl<'a> PrimitiveAsRef<'a> for &'a Gc<crate::values::HashSet<SteelVal>> {
    #[inline(always)]
    fn primitive_as_ref(val: &'a SteelVal) -> crate::rvals::Result<Self> {
        if let SteelVal::HashSetV(p) = val {
            Ok(&p.0)
        } else {
            crate::stop!(ConversionError => format!("Cannot convert steel value: {} to steel hashset", val))
        }
    }

    #[inline(always)]
    fn maybe_primitive_as_ref(val: &'a SteelVal) -> Option<Self> {
        if let SteelVal::HashSetV(p) = val {
            Some(&p.0)
        } else {
            None
        }
    }
}

impl<'a> PrimitiveAsRef<'a> for &'a SteelHashSet {
    #[inline(always)]
    fn primitive_as_ref(val: &'a SteelVal) -> crate::rvals::Result<Self> {
        if let SteelVal::HashSetV(p) = val {
            Ok(p)
        } else {
            crate::stop!(ConversionError => format!("Cannot convert steel value: {} to steel hashset", val))
        }
    }

    #[inline(always)]
    fn maybe_primitive_as_ref(val: &'a SteelVal) -> Option<Self> {
        if let SteelVal::HashSetV(p) = val {
            Some(p)
        } else {
            None
        }
    }
}

impl<'a> PrimitiveAsRef<'a> for &'a HeapRef<Vec<SteelVal>> {
    #[inline(always)]
    fn primitive_as_ref(val: &'a SteelVal) -> crate::rvals::Result<Self> {
        if let SteelVal::MutableVector(p) = val {
            Ok(p)
        } else {
            crate::stop!(ConversionError => format!("Cannot convert steel value: {} to steel mutable vector", val))
        }
    }

    fn maybe_primitive_as_ref(val: &'a SteelVal) -> Option<Self> {
        if let SteelVal::MutableVector(p) = val {
            Some(p)
        } else {
            None
        }
    }
}

impl<'a> PrimitiveAsRef<'a> for &'a SteelPort {
    #[inline(always)]
    fn primitive_as_ref(val: &'a SteelVal) -> crate::rvals::Result<Self> {
        if let SteelVal::PortV(p) = val {
            Ok(p)
        } else {
            crate::stop!(ConversionError => format!("Cannot convert steel value: {} to steel port", val))
        }
    }

    #[inline(always)]
    fn maybe_primitive_as_ref(val: &'a SteelVal) -> Option<Self> {
        if let SteelVal::PortV(p) = val {
            Some(p)
        } else {
            None
        }
    }
}

impl<'a> PrimitiveAsRef<'a> for &'a List<SteelVal> {
    #[inline(always)]
    fn primitive_as_ref(val: &'a SteelVal) -> crate::rvals::Result<Self> {
        if let SteelVal::ListV(l) = val {
            Ok(l)
        } else {
            crate::stop!(ConversionError => format!("Cannot convert steel value: {} to steel list", val))
        }
    }

    #[inline(always)]
    fn maybe_primitive_as_ref(val: &'a SteelVal) -> Option<Self> {
        if let SteelVal::ListV(l) = val {
            Some(l)
        } else {
            None
        }
    }
}

impl<'a> PrimitiveAsRef<'a> for &'a SteelVal {
    #[inline(always)]
    fn primitive_as_ref(val: &'a SteelVal) -> crate::rvals::Result<Self> {
        Ok(val)
    }

    #[inline(always)]
    fn maybe_primitive_as_ref(val: &'a SteelVal) -> Option<Self> {
        Some(val)
    }
}

impl<'a> PrimitiveAsRefMut<'a> for &'a mut SteelVal {
    #[inline(always)]
    fn primitive_as_ref(val: &'a mut SteelVal) -> crate::rvals::Result<Self> {
        Ok(val)
    }

    #[inline(always)]
    fn maybe_primitive_as_ref(val: &'a mut SteelVal) -> Option<Self> {
        Some(val)
    }
}

impl<'a> PrimitiveAsRef<'a> for &'a SteelString {
    #[inline(always)]
    fn primitive_as_ref(val: &'a SteelVal) -> crate::rvals::Result<Self> {
        if let SteelVal::StringV(s) = val {
            Ok(s)
        } else {
            crate::stop!(TypeMismatch => format!("Cannot convert steel value: {} to steel string", val))
        }
    }

    #[inline(always)]
    fn maybe_primitive_as_ref(val: &'a SteelVal) -> Option<Self> {
        if let SteelVal::StringV(s) = val {
            Some(s)
        } else {
            None
        }
    }
}

impl<'a> PrimitiveAsRef<'a> for &'a Gc<crate::values::HashMap<SteelVal, SteelVal>> {
    #[inline(always)]
    fn primitive_as_ref(val: &'a SteelVal) -> crate::rvals::Result<Self> {
        if let SteelVal::HashMapV(hm) = val {
            Ok(&hm.0)
        } else {
            crate::stop!(ConversionError => format!("Cannot convert steel value: {} to hashmap", val))
        }
    }

    #[inline(always)]
    fn maybe_primitive_as_ref(val: &'a SteelVal) -> Option<Self> {
        if let SteelVal::HashMapV(hm) = val {
            Some(&hm.0)
        } else {
            None
        }
    }
}

impl<'a> PrimitiveAsRefMut<'a> for &'a mut Gc<crate::values::HashMap<SteelVal, SteelVal>> {
    #[inline(always)]
    fn primitive_as_ref(val: &'a mut SteelVal) -> crate::rvals::Result<Self> {
        if let SteelVal::HashMapV(hm) = val {
            Ok(&mut hm.0)
        } else {
            crate::stop!(ConversionError => format!("Cannot convert steel value: {} to hashmap", val))
        }
    }

    #[inline(always)]
    fn maybe_primitive_as_ref(val: &'a mut SteelVal) -> Option<Self> {
        if let SteelVal::HashMapV(hm) = val {
            Some(&mut hm.0)
        } else {
            None
        }
    }
}

impl<'a> PrimitiveAsRefMut<'a> for &'a mut SteelByteVector {
    #[inline(always)]
    fn primitive_as_ref(val: &'a mut SteelVal) -> crate::rvals::Result<Self> {
        if let SteelVal::ByteVector(hm) = val {
            Ok(hm)
        } else {
            crate::stop!(ConversionError => format!("Cannot convert steel value: {} to bytevector", val))
        }
    }

    #[inline(always)]
    fn maybe_primitive_as_ref(val: &'a mut SteelVal) -> Option<Self> {
        if let SteelVal::ByteVector(hm) = val {
            Some(hm)
        } else {
            None
        }
    }
}

impl<'a> PrimitiveAsRef<'a> for &'a SteelHashMap {
    #[inline(always)]
    fn primitive_as_ref(val: &'a SteelVal) -> crate::rvals::Result<Self> {
        if let SteelVal::HashMapV(hm) = val {
            Ok(hm)
        } else {
            crate::stop!(ConversionError => format!("Cannot convert steel value: {} to hashmap", val))
        }
    }

    #[inline(always)]
    fn maybe_primitive_as_ref(val: &'a SteelVal) -> Option<Self> {
        if let SteelVal::HashMapV(hm) = val {
            Some(hm)
        } else {
            None
        }
    }
}

impl IntoSteelVal for String {
    #[inline(always)]
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(SteelVal::StringV(self.into()))
    }
}

impl IntoSteelVal for SteelString {
    #[inline(always)]
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(SteelVal::StringV(self))
    }
}

impl From<String> for Gc<SteelVal> {
    #[inline(always)]
    fn from(val: String) -> Gc<SteelVal> {
        Gc::new(val.into())
    }
}

impl From<bool> for SteelVal {
    #[inline(always)]
    fn from(val: bool) -> SteelVal {
        SteelVal::BoolV(val)
    }
}

impl FromSteelVal for bool {
    #[inline(always)]
    fn from_steelval(val: &SteelVal) -> crate::rvals::Result<bool> {
        if let SteelVal::BoolV(b) = val {
            Ok(*b)
        } else {
            crate::stop!(ConversionError => format!("Cannot convert steel value: {val} to boolean"))
        }
    }
}

impl IntoSteelVal for bool {
    #[inline(always)]
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(SteelVal::BoolV(self))
    }
}

impl From<Vector<SteelVal>> for SteelVal {
    #[inline(always)]
    fn from(val: Vector<SteelVal>) -> SteelVal {
        SteelVal::VectorV(Gc::new(val).into())
    }
}

impl From<FunctionSignature> for SteelVal {
    fn from(val: FunctionSignature) -> SteelVal {
        SteelVal::FuncV(val)
    }
}

#[cfg(test)]
mod try_from_tests {

    use super::*;

    #[test]
    fn from_char() {
        assert_eq!(SteelVal::from('c'), SteelVal::CharV('c'));
    }

    #[test]
    fn from_steelval_char() {
        assert_eq!(char::from_steelval(&SteelVal::CharV('c')).unwrap(), 'c')
    }

    #[test]
    fn into_steelval_char() {
        assert_eq!('c'.into_steelval().unwrap(), SteelVal::CharV('c'))
    }

    #[test]
    fn from_steelval_usize() {
        assert_eq!(usize::from_steelval(&SteelVal::IntV(10)).unwrap(), 10)
    }

    #[test]
    fn from_steelval_i32() {
        assert_eq!(i32::from_steelval(&SteelVal::IntV(32)).unwrap(), 32)
    }

    #[test]
    fn into_steelval_i32() {
        assert_eq!(32.into_steelval().unwrap(), SteelVal::IntV(32))
    }

    #[test]
    fn from_bool() {
        assert_eq!(SteelVal::from(true), SteelVal::BoolV(true));
    }

    #[test]
    fn try_from_steelval_string() {
        let expected = "foo".to_string();
        let input = SteelVal::StringV("foo".into());

        let res = String::try_from(input);
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn try_from_steelval_ref_string() {
        let expected = "foo".to_string();
        let input = SteelVal::StringV("foo".into());

        let res = String::try_from(&input);
        assert_eq!(res.unwrap(), expected);
    }
}
