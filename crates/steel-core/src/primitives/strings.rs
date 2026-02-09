use crate::gc::Gc;
use crate::values::lists::{List, SteelList};

use crate::rvals::{IntoSteelVal, RestArgsIter, Result, SteelByteVector, SteelString, SteelVal};
use crate::steel_vm::{builtin::BuiltInModule, vm::VmCore};
use crate::{builtin_stop, stop, Vector};

use std::hint::unreachable_unchecked;
use std::io::Write as _;

use icu_casemap::CaseMapper;
use steel_derive::{function, native};

/// Strings in Steel are immutable, fixed length arrays of characters. They are heap allocated, and
/// are implemented under the hood as referenced counted Rust `Strings`. Rust `Strings` are stored
/// as UTF-8 encoded bytes.
#[steel_derive::define_module(name = "steel/strings")]
pub fn string_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/strings");
    module
        .register_native_fn_definition(STRING_APPEND_DEFINITION)
        .register_native_fn_definition(TO_STRING_DEFINITION)
        .register_native_fn_definition(STRING_TO_LIST_DEFINITION)
        .register_native_fn_definition(STRING_UPCASE_DEFINITION)
        .register_native_fn_definition(STRING_DOWNCASE_DEFINITION)
        .register_native_fn_definition(STRING_FOLDCASE_DEFINITION)
        .register_native_fn_definition(STRING_LENGTH_DEFINITION)
        .register_native_fn_definition(UTF8_LENGTH_DEFINITION)
        .register_native_fn_definition(TRIM_DEFINITION)
        .register_native_fn_definition(TRIM_START_DEFINITION)
        .register_native_fn_definition(TRIM_END_DEFINITION)
        .register_native_fn_definition(SPLIT_WHITESPACE_DEFINITION)
        .register_native_fn_definition(SPLIT_ONCE_DEFINITION)
        .register_native_fn_definition(SPLIT_MANY_DEFINITION)
        .register_native_fn_definition(STRING_TO_INT_DEFINITION)
        .register_native_fn_definition(INT_TO_STRING_DEFINITION)
        .register_native_fn_definition(STRING_TO_SYMBOL_DEFINITION)
        .register_native_fn_definition(STRING_TO_UNINTERNED_SYMBOL_DEFINITION)
        .register_native_fn_definition(STARTS_WITH_DEFINITION)
        .register_native_fn_definition(ENDS_WITH_DEFINITION)
        .register_native_fn_definition(TRIM_END_MATCHES_DEFINITION)
        .register_native_fn_definition(TRIM_START_MATCHES_DEFINITION)
        .register_native_fn_definition(STRING_REF_DEFINITION)
        .register_native_fn_definition(SUBSTRING_DEFINITION)
        .register_native_fn_definition(MAKE_STRING_DEFINITION)
        .register_native_fn_definition(STRING_EQUALS_DEFINITION)
        .register_native_fn_definition(STRING_CI_EQUALS_DEFINITION)
        .register_native_fn_definition(STRING_LESS_THAN_DEFINITION)
        .register_native_fn_definition(STRING_CI_LESS_THAN_DEFINITION)
        .register_native_fn_definition(STRING_LESS_THAN_EQUAL_TO_DEFINITION)
        .register_native_fn_definition(STRING_CI_LESS_THAN_EQUAL_TO_DEFINITION)
        .register_native_fn_definition(STRING_GREATER_THAN_DEFINITION)
        .register_native_fn_definition(STRING_CI_GREATER_THAN_DEFINITION)
        .register_native_fn_definition(STRING_GREATER_THAN_EQUAL_TO_DEFINITION)
        .register_native_fn_definition(STRING_CI_GREATER_THAN_EQUAL_TO_DEFINITION)
        .register_native_fn_definition(STRING_CONSTRUCTOR_DEFINITION)
        .register_native_fn_definition(STRING_TO_NUMBER_DEFINITION)
        .register_native_fn_definition(NUMBER_TO_STRING_DEFINITION)
        .register_native_fn_definition(REPLACE_DEFINITION)
        .register_native_fn_definition(CHAR_UPCASE_DEFINITION)
        .register_native_fn_definition(CHAR_DOWNCASE_DEFINITION)
        .register_native_fn_definition(CHAR_FOLDCASE_DEFINITION)
        .register_native_fn_definition(CHAR_IS_DIGIT_DEFINITION)
        .register_native_fn_definition(CHAR_IS_WHITESPACE_DEFINITION)
        .register_native_fn_definition(CHAR_TO_NUMBER_DEFINITION)
        .register_native_fn_definition(CHAR_EQUALS_DEFINITION)
        .register_native_fn_definition(CHAR_CI_EQUALS_DEFINITION)
        .register_native_fn_definition(CHAR_GREATER_THAN_DEFINITION)
        .register_native_fn_definition(CHAR_CI_GREATER_THAN_DEFINITION)
        .register_native_fn_definition(CHAR_GREATER_THAN_EQUAL_TO_DEFINITION)
        .register_native_fn_definition(CHAR_CI_GREATER_THAN_EQUAL_TO_DEFINITION)
        .register_native_fn_definition(CHAR_LESS_THAN_DEFINITION)
        .register_native_fn_definition(CHAR_CI_LESS_THAN_DEFINITION)
        .register_native_fn_definition(CHAR_LESS_THAN_EQUAL_TO_DEFINITION)
        .register_native_fn_definition(CHAR_CI_LESS_THAN_EQUAL_TO_DEFINITION)
        .register_native_fn_definition(CHAR_TO_INTEGER_DEFINITION)
        .register_native_fn_definition(INTEGER_TO_CHAR_DEFINITION)
        .register_native_fn_definition(STRING_TO_BYTES_DEFINITION)
        .register_native_fn_definition(STRING_TO_VECTOR_DEFINITION)
        .register_native_fn_definition(STRING_JOIN_DEFINITION)
        .register_native_fn_definition(STRING_CONTAINS_DEFINITION)
        .register_native_fn_definition(STRING_PUSH_DEFINITION);

    module
}

macro_rules! monotonic {
    ($iter:expr, $compare:expr) => {{
        let mut iter = $iter;

        let Some(mut last)= iter.next().transpose()? else {
            stop!(ArityMismatch => "expected at least one argument");
        };

        let comparator = $compare;

        for maybe_item in iter {
            let item = maybe_item?;

            if comparator(&last, &item) {
                last = item;
            } else {
                return Ok(SteelVal::BoolV(false));
            }
        }

        Ok(SteelVal::BoolV(true))
    }};
}

mod radix_fmt {
    use num_bigint::BigInt;

    const DIGITS: [u8; 16] = *b"0123456789abcdef";

    pub fn small(acc: &mut Vec<u8>, value: isize, radix: usize) {
        let start = acc.len();
        let numbers = core::iter::successors(Some(value.unsigned_abs()), |n| match n / radix {
            0 => None,
            n => Some(n),
        });

        for number in numbers {
            let idx = number % radix;
            let digit = DIGITS[idx];
            acc.push(digit);
        }
        if value < 0 {
            acc.push(b'-');
        }

        acc[start..].reverse();
    }

    pub fn big(acc: &mut Vec<u8>, value: BigInt, radix: usize) {
        let fmt = value.to_str_radix(radix as u32);
        acc.extend(fmt.as_bytes());
    }
}

fn format_number(acc: &mut Vec<u8>, value: &SteelVal, radix: Option<usize>) -> Result<()> {
    match value {
        SteelVal::NumV(v) if v.is_nan() => acc.extend(b"+nan.0"),
        SteelVal::NumV(v) if *v == f64::INFINITY => acc.extend(b"+inf.0"),
        SteelVal::NumV(v) if *v == f64::NEG_INFINITY => acc.extend(b"-inf.0"),
        SteelVal::NumV(v) => {
            let _ = write!(acc, "{:?}", v);
        }
        SteelVal::IntV(v) => {
            if let Some(radix) = radix {
                radix_fmt::small(acc, *v, radix);
            } else {
                let _ = write!(acc, "{}", v);
            }
        }
        SteelVal::BigNum(v) => {
            if let Some(radix) = radix {
                radix_fmt::big(acc, v.unwrap(), radix);
            } else {
                let _ = write!(acc, "{}", **v);
            }
        }
        SteelVal::Rational(v) => {
            if let Some(radix) = radix {
                radix_fmt::small(acc, *v.numer() as isize, radix);
                acc.push(b'/');
                radix_fmt::small(acc, *v.denom() as isize, radix);
            } else {
                let _ = write!(acc, "{}", v.numer());
                acc.push(b'/');
                let _ = write!(acc, "{}", v.denom());
            }
        }
        SteelVal::BigRational(v) => {
            if let Some(radix) = radix {
                radix_fmt::big(acc, v.numer().clone(), radix);
                acc.push(b'/');
                radix_fmt::big(acc, v.denom().clone(), radix);
            } else {
                let _ = write!(acc, "{}", v.numer());
                acc.push(b'/');
                let _ = write!(acc, "{}", v.denom());
            }
        }
        SteelVal::Complex(c) => {
            format_number(acc, &c.re, radix)?;
            if !c.imaginary_is_negative() && c.imaginary_is_finite() {
                acc.push(b'+');
            }
            format_number(acc, &c.im, radix)?;
            acc.push(b'i');
        }
        _ => stop!(TypeMismatch => "number->string expects a number type, found: {}", value),
    }

    Ok(())
}

fn number_to_string_impl(value: &SteelVal, radix: Option<usize>) -> Result<SteelVal> {
    let mut accumulator = Vec::new();
    format_number(&mut accumulator, value, radix.filter(|x| *x != 10))?;

    let string = String::from_utf8(accumulator).expect("should just be ascii");
    string.into_steelval()
}

/// Converts the given number to a string, with an optional radix.
///
/// Returns an error, if the value given is not a number.
///
/// (number->string number? [radix]) -> string?
///
/// * radix: number?
///
/// ```scheme
/// > (number->string 10) ;; => "10"
/// > (number->string 1.0) ;; => "1.0"
/// > (number->string 1/2) ;; => "1.0"
/// > (number->string 1+2i) ;; => "1+2i"
/// > (number->string 255 16) ;; => "ff"
/// > (number->string 1/2 2) ;; => "1/10"
/// ```
#[function(name = "number->string", constant = true)]
pub fn number_to_string(value: &SteelVal, mut rest: RestArgsIter<'_, isize>) -> Result<SteelVal> {
    let radix = rest.next();

    let radix = if let Some(radix) = radix {
        let radix = radix?;

        if radix < 2 || radix > 16 {
            stop!(ContractViolation => "radix value given to string->number must be between 2 and 16, found: {}", radix);
        }

        Some(radix as usize)
    } else {
        None
    };

    number_to_string_impl(value, radix)
}

/// Converts the given string to a number, with an optional radix.
/// On failure, it returns `#f`
///
/// (string->number digits [radix]) -> (or/c number? boolean?)
///
/// * digits : string?
/// * radix : number?
///
/// # Examples
///
/// ```scheme
/// > (string->number "10") ;; => 10
/// > (string->number "1.0") ;; => 1.0
/// > (string->number "1/2") ;; => 1/2
/// > (string->number "1+2i") ;; => 1+2i
/// > (string->number "ff") ;; => #f
/// > (string->number "ff" 16) ;; => 255
/// > (string->number "1/10" 2) ;; => 1/2
/// > (string->number "not-a-number") ;; => #f
/// ```
#[function(name = "string->number", constant = true)]
pub fn string_to_number(
    value: &SteelString,
    mut rest: RestArgsIter<'_, isize>,
) -> Result<SteelVal> {
    let radix = rest.next();

    let radix = if let Some(radix) = radix {
        let radix = radix?;

        if radix < 2 || radix > 16 {
            stop!(ContractViolation => "radix value given to string->number must be between 2 and 16, found: {}", radix);
        }

        Some(radix as u32)
    } else {
        None
    };

    let number = steel_parser::lexer::parse_number(value, radix);
    number.into_steelval()
}

/// Constructs a string from the given characters
///
/// (string . char?) -> string?
///
/// # Examples
///
/// ```scheme
/// > (string #\h #\e #\l #\l #\o) ;; => "hello"
/// > (string #\λ) ;; => "λ"
/// > (string) ;; => ""
/// ```
#[function(name = "string")]
pub fn string_constructor(rest: RestArgsIter<'_, char>) -> Result<SteelVal> {
    rest.collect::<Result<String>>().map(|x| x.into())
}

// TODO: avoid allocation in `-ci` variants

macro_rules! impl_str_comparison {
    (-ci, $name:ident, $ext_name:literal, $mode:literal, $op:expr, $(#[$attr:meta])*) => {
        #[doc = concat!("Compares strings lexicographically (as in\"", $mode, "\"),")]
        #[doc = "in a case insensitive fashion."]
        #[doc = ""]
        #[doc = concat!("(", $ext_name, " s1 s2 ... ) -> bool?")]
        #[doc = "* s1 : string?"]
        #[doc = "* s2 : string?"]
        $(#[$attr])*
        #[function(name = $ext_name, constant = true)]
        pub fn $name(rest: RestArgsIter<&SteelString>) -> Result<SteelVal> {
            let cm = CaseMapper::new();
            monotonic!(rest.map(|val| val.map(|s| cm.fold_string(s))), $op)
        }
    };
    ($name:ident, $ext_name:literal, $mode:literal, $op:expr, $(#[$attr:meta])*) => {
        #[doc = concat!("Compares strings lexicographically (as in\"", $mode, "\").")]
        #[doc = ""]
        #[doc = concat!("(", $ext_name, " s1 s2 ... ) -> bool?")]
        #[doc = "* s1 : string?"]
        #[doc = "* s2 : string?"]
        $(#[$attr])*
        #[function(name = $ext_name, constant = true)]
        pub fn $name(rest: RestArgsIter<&SteelString>) -> Result<SteelVal> {
            monotonic!(rest, $op)
        }
    };
}

impl_str_comparison!(
    string_less_than,
    "string<?",
    "less-than",
    |s1: &_, s2: &_| s1 < s2,
    /// # Examples
    ///
    /// ```scheme
    /// > (string<? "a" "b") ;; => #t
    /// > (string<? "a" "b" "c") ;; => #t
    /// > (string<? "a" "b" "b") ;; => #f
    /// ```
);
impl_str_comparison!(
    string_less_than_equal_to,
    "string<=?",
    "less-than-equal-to",
    |s1: &_, s2: &_| s1 <= s2,
    /// # Examples
    ///
    /// ```scheme
    /// > (string<=? "a" "b") ;; => #t
    /// > (string<=? "a" "b" "c") ;; => #t
    /// > (string<=? "a" "b" "b") ;; => #t
    /// ```
);
impl_str_comparison!(
    string_greater_than,
    "string>?",
    "greater-than",
    |s1: &_, s2: &_| s1 > s2,
    /// # Examples
    ///
    /// ```scheme
    /// > (string>? "b" "a") ;; => #t
    /// > (string>? "c" "b" "a") ;; => #t
    /// > (string>? "c" "b" "b") ;; => #f
    /// ```
);
impl_str_comparison!(
    string_greater_than_equal_to,
    "string>=?",
    "greater-than-or-equal",
    |s1: &_, s2: &_| s1 >= s2,
    /// # Examples
    ///
    /// ```scheme
    /// > (string>=? "b" "a") ;; => #t
    /// > (string>=? "c" "b" "a") ;; => #t
    /// > (string>=? "c" "b" "b") ;; => #t
    /// ```
);
impl_str_comparison!(
    -ci,
    string_ci_less_than,
    "string-ci<?",
    "less-than",
    |s1: &_, s2: &_| s1 < s2,
    /// # Examples
    ///
    /// ```scheme
    /// > (string-ci<? "a" "b") ;; => #t
    /// > (string-ci<? "a" "B") ;; => #t
    /// > (string-ci<? "a" "B" "c") ;; => #t
    /// > (string-ci<? "a" "B" "b") ;; => #f
    /// > (string-ci<? "Straßburg" "STRASSE" "straßenbahn") ;; => #t
    /// ```
);
impl_str_comparison!(
    -ci,
    string_ci_less_than_equal_to,
    "string-ci<=?",
    "less-than-or-equal",
    |s1: &_, s2: &_| s1 <= s2,
    /// # Examples
    ///
    /// ```scheme
    /// > (string-ci<=? "a" "b") ;; => #t
    /// > (string-ci<=? "a" "B") ;; => #t
    /// > (string-ci<=? "a" "B" "c") ;; => #t
    /// > (string-ci<=? "a" "B" "b") ;; => #t
    /// > (string-ci<=? "Straßburg" "STRASSE" "straßenbahn") ;; => #t
    /// ```
);
impl_str_comparison!(
    -ci,
    string_ci_greater_than,
    "string-ci>?",
    "greater-than",
    |s1: &_, s2: &_| s1 > s2,
    /// # Examples
    ///
    /// ```scheme
    /// > (string-ci>? "b" "a") ;; => #t
    /// > (string-ci>? "B" "a") ;; => #t
    /// > (string-ci>? "c" "B" "a") ;; => #t
    /// > (string-ci>? "c" "B" "b") ;; => #f
    /// > (string-ci>? "straßenbahn" "STRASSE" "Straßburg") ;; => #t
    /// ```
);
impl_str_comparison!(
    -ci,
    string_ci_greater_than_equal_to,
    "string-ci>=?",
    "greater-than-or-equal",
    |s1: &_, s2: &_| s1 >= s2,
    /// # Examples
    ///
    /// ```scheme
    /// > (string-ci>=? "b" "a") ;; => #t
    /// > (string-ci>=? "B" "a") ;; => #t
    /// > (string-ci>=? "c" "B" "a") ;; => #t
    /// > (string-ci>=? "c" "B" "b") ;; => #f
    /// > (string-ci>=? "straßenbahn" "STRASSE" "Straßburg") ;; => #t
    /// ```
);

/// Compares strings for equality.
///
/// (string=? string1 string2 ...) -> bool?
///
/// * string1 : string?
/// * string2 : string?
///
/// # Examples
///
/// ```scheme
/// > (string=? "hello" "hello") ;; => #t
/// > (string=? "hello" "HELLO") ;; => #f
/// ```
#[function(name = "string=?", constant = true)]
pub fn string_equals(rest: RestArgsIter<&SteelString>) -> Result<SteelVal> {
    monotonic!(rest, |s1: &_, s2: &_| s1 == s2)
}

/// Compares strings for equality, in a case insensitive fashion.
///
/// (string-ci=? string? string? ...) -> bool?
///
/// # Examples
///
/// ```scheme
/// > (string-ci=? "hEllO WorLd" "HELLO worlD") ;; => #t
/// > (string-ci=? "Straße" "STRASSE" "strasse" "STRAẞE") ;; => #t
/// > (string-ci=? "ὈΔΥΣΣΕΎΣ" "ὀδυσσεύς" "ὀδυσσεύσ") ;; => #t
/// ```
#[function(name = "string-ci=?", constant = true)]
pub fn string_ci_equals(rest: RestArgsIter<&SteelString>) -> Result<SteelVal> {
    let cm = CaseMapper::new();
    monotonic!(
        rest.map(|val| val.map(|s| cm.fold_string(s))),
        |s1: &_, s2: &_| s1 == s2
    )
}

/// Extracts the nth character out of a given string, starting at 0.
///
/// (string-ref str n) -> char?
///
/// * str : string?
/// * n : int?
///
/// ```scheme
/// (string-ref "one" 1) ;; => #\n
/// (string-ref "αβγ" 1) ;; => #\β
/// ```
#[function(name = "string-ref", constant = true)]
pub fn string_ref(value: &SteelString, index: usize) -> Result<SteelVal> {
    let res = if index < value.len() {
        value.chars().nth(index)
    } else {
        None
    };

    if let Some(ch) = res {
        Ok(SteelVal::CharV(ch))
    } else {
        stop!(Generic => "string-ref: index out of bounds: index: {}, string length: {}", index, value.len());
    }
}

/// Creates a substring slicing the characters between two indices.
///
/// (substring str start end) -> string?
///
/// * str: string?
/// * start : int?
/// * end : int?
///
/// # Examples
/// ```scheme
/// (substring "hello" 1 4) ;; => "ell"
/// (substring "hello" 10 15) ;; => error
/// ```
#[function(name = "substring", constant = true)]
pub fn substring(
    value: &SteelString,
    i: usize,
    mut rest: RestArgsIter<'_, isize>,
) -> Result<SteelVal> {
    let j = rest.next().transpose()?;

    if rest.next().is_some() {
        stop!(ArityMismatch => "substring expects 1 or 2 arguments");
    }

    let range = bounds(value.as_str(), Some(i as isize), j, "substring")?;

    Ok(SteelVal::StringV(value[range].into()))
}

/// Creates a string of a given length, filled with an optional character
/// (which defaults to `#\0`).
///
/// (make-string len [char]) -> string?
///
/// * len : int?
/// * char : char? = #\0
///
/// # Examples
///
/// ```scheme
/// > (make-string 5 #\a) ;; => "aaaaa"
/// > (make-string 5) ;; => "\0\0\0\0\0"
/// ```
#[function(name = "make-string")]
pub fn make_string(k: usize, mut c: RestArgsIter<'_, char>) -> Result<SteelVal> {
    // If the char is there, we want to take it
    let char = c.next().transpose()?;

    // We want the iterator to be exhaused
    if let Some(next) = c.next() {
        stop!(ArityMismatch => format!("make-string expected 1 or 2 arguments, got an additional argument {}", next?))
    }

    let c = char.unwrap_or('\0');
    Ok(core::iter::repeat_n(c, k).collect::<String>().into())
}

/// Replaces all occurrences of a pattern into the given string
///
/// (string-replace str from to) -> string?
///
/// * str : string?
/// * from : string?
/// * to : string?
///
/// # Examples
/// ```scheme
/// (string-replace "hello world" "o" "@") ;; => "hell@ w@rld"
/// ```
#[function(name = "string-replace")]
pub fn replace(value: &SteelString, from: &SteelString, to: &SteelString) -> Result<SteelVal> {
    Ok(SteelVal::StringV(
        value.replace(from.as_str(), to.as_str()).into(),
    ))
}

/// Concatenates all of the inputs to their string representation, separated by spaces.
///
/// (to-string xs ...)
///
/// * xs : any/c
///
/// # Examples
/// ```scheme
/// > (to-string 10) ;; => "10"
/// > (to-string 10 20) ;; => "10 20"
/// > (to-string "hello" "world") ;; => "hello world"
/// ```
#[native(name = "to-string", arity = "AtLeast(0)")]
pub fn to_string(args: &[SteelVal]) -> Result<SteelVal> {
    let mut error_message = String::new();

    if let Some((first, rest)) = args.split_first() {
        error_message.push_str(first.to_string().trim_matches('\"'));

        for arg in rest {
            let error_val = arg.to_string();
            error_message.push(' ');
            error_message.push_str(error_val.trim_matches('\"'));
        }
    }

    Ok(SteelVal::StringV(error_message.into()))
}

#[function(name = "string-push")]
pub fn string_push(value: &mut SteelVal, character_or_string: SteelVal) -> Result<SteelVal> {
    if let SteelVal::StringV(_) = value {
        let SteelVal::StringV(mut s) = core::mem::take(value) else {
            unsafe { unreachable_unchecked() }
        };

        let updated = Gc::make_mut(&mut s.0);

        match character_or_string {
            SteelVal::CharV(c) => {
                updated.push(c);
            }

            SteelVal::StringV(s) => {
                updated.push_str(s.as_str());
            }

            _ => {
                stop!(TypeMismatch => "string-push expects either a string or character, found: {}", character_or_string)
            }
        }

        Ok(SteelVal::StringV(s))
    } else {
        stop!(TypeMismatch => "string-push expected a string, found: {}", value);
    }
}

/// Return an uninterned symbol from the given string.
///
/// (string->uninterned-symbol string?) -> symbol?
///
/// # Examples
///
/// ```scheme
/// (string->uninterned-symbol "abc") ;; => 'abc
/// (string->uninterned-symbol "pea pod") ;; => '|pea pod|
/// ```
#[function(name = "string->uninterned-symbol", constant = false)]
pub fn string_to_uninterned_symbol(value: SteelString) -> SteelVal {
    match Gc::strong_count(&value.0) {
        1 => SteelVal::SymbolV(value),
        _ => SteelVal::SymbolV(value.0.unwrap().into()),
    }
    // SteelVal::SymbolV(value)
}

/// Returns an interned symbol from the given string.
///
/// (string->symbol string?) -> symbol?
///
/// # Examples
///
/// ```scheme
/// > (string->symbol "abc") ;; => 'abc
/// > (string->symbol "pea pod") ;; => '|pea pod|
/// ```
#[steel_derive::context(name = "string->symbol", arity = "Exact(1)")]
pub fn string_to_symbol(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    let value = match &args[0] {
        SteelVal::StringV(string) => string.clone(),
        val => builtin_stop!(TypeMismatch => "steel->string: expected string, found {}", val),
    };

    let sym = SteelVal::SymbolV(value);

    let mut guard = ctx.thread.compiler.write();
    let interned = guard.constant_map.add_or_get(sym);
    let value = guard.constant_map.get(interned);

    Some(Ok(value))
}

/// Converts an integer into a string.
///
/// Use of this procedure is discouraged in favor of the more powerful and more
/// portable `(number->string)` procedure.
///
/// (int->string int?) -> string?
///
/// # Examples
///
/// ```scheme
/// > (int->string 10) ;; => "10"
/// ```
#[function(name = "int->string")]
pub fn int_to_string(value: isize) -> String {
    format!("{value}")
}

/// Converts a string into an int. Raises an error if the string cannot be converted to an integer.
///
/// Use of this procedure is discouraged in favor of the more powerful and more
/// portable `(string->number)` procedure.
///
/// (string->int string?) -> int?
///
/// # Examples
///
/// ```scheme
/// > (string->int "100") ;; => 10
/// > (string->int "not-an-int") ;; error
/// ```
#[function(name = "string->int")]
pub fn string_to_int(value: &SteelString) -> Result<SteelVal> {
    let parsed_int = value.parse::<isize>();
    match parsed_int {
        Ok(n) => Ok(SteelVal::IntV(n)),
        Err(_) => {
            stop!(TypeMismatch => "could not convert number to integer");
        }
    }
}

/// Converts a string into a list of characters.
///
/// (string->list str [start] [end]) -> (listof char?)
///
/// * str : string?
/// * start : int? = 0
/// * end : int? = (string-length str)
///
/// # Examples
///
/// ```scheme
/// > (string->list "hello") ;; => '(#\h #\e #\l #\l #\o)
/// > (string->list "one two three" 4 7) ;; => '(#\t #\w #\o)
/// ```
#[function(name = "string->list")]
pub fn string_to_list(value: &SteelString, mut rest: RestArgsIter<isize>) -> Result<SteelVal> {
    let i = rest.next().transpose()?;
    let j = rest.next().transpose()?;

    if rest.next().is_some() {
        stop!(ArityMismatch => "string->list expects up to 3 arguments");
    }

    let range = bounds(value.as_str(), i, j, "string->list")?;

    Ok(value[range]
        .chars()
        .map(SteelVal::CharV)
        .collect::<List<_>>()
        .into())
}

/// Creates a new uppercased version of the input string
///
/// (string-upcase string?) -> string?
///
/// # Examples
///
/// ```scheme
/// > (string-upcase "lower") ;; => "LOWER"
/// > (string-upcase "straße") ;; => "STRASSE"
/// ```
#[function(name = "string-upcase", alias = "string->upper")]
pub fn string_upcase(value: &SteelString) -> String {
    value.to_uppercase()
}

/// Creates a new lowercased version of the input string
///
/// (string-downcase string?) -> string?
///
/// # Examples
///
/// ```scheme
/// > (string-downcase "sPonGeBoB tExT") ;; => "spongebob text"
/// > (string-downcase "ὈΔΥΣΣΕΎΣ") ;; => "ὀδυσσεύς"
/// > (string-downcase "STRAẞE") ;; => "straße"
/// ```
#[function(name = "string-downcase", alias = "string->lower")]
pub fn string_downcase(value: &SteelString) -> String {
    value.to_lowercase()
}

/// Applies full unicode case-folding to the input string
///
/// (string-foldcase string?) -> string?
///
/// # Examples
///
/// ```scheme
/// > (string-foldcase "Straße") ;; => "strasse"
/// ```
#[function(name = "string-foldcase")]
pub fn string_foldcase(value: &SteelString) -> String {
    let cm = CaseMapper::new();
    cm.fold_string(value).into_owned()
}

/// Returns a new string with the leading and trailing whitespace removed.
///
/// (trim string?) -> string?
///
/// # Examples
///
/// ```scheme
/// > (trim "   foo     ") ;; => "foo"
/// ```
#[function(name = "trim")]
pub fn trim(value: &SteelString) -> String {
    value.trim().into()
}

/// Returns a new string with the leading whitespace removed.
///
/// (trim string?) -> string?
///
/// # Examples
///
/// ```scheme
/// > (trim "   foo     ") ;; => "foo     "
/// ```
#[function(name = "trim-start")]
pub fn trim_start(value: &SteelString) -> String {
    value.trim_start().into()
}

/// Returns a new string with the trailing whitespace removed.
///
/// (trim string?) -> string?
///
/// # Examples
///
/// ```scheme
/// > (trim "   foo     ") ;; => "   foo"
/// ```
#[function(name = "trim-end")]
pub fn trim_end(value: &SteelString) -> String {
    value.trim_end().into()
}

/// Returns a new string with the given `pat` repeatedly removed from the end
/// of the string
///
/// ```scheme
/// (trim-end-matches string? string?) -> string?
/// ```
///
/// # Examples
/// ```scheme
/// > (trim-end-matches "123foo1bar123123" "123") ;; => "123foo1bar"
/// ```
#[function(name = "trim-end-matches")]
pub fn trim_end_matches(value: &SteelString, pat: &SteelString) -> String {
    value.trim_end_matches(pat.as_str()).into()
}

/// Returns a new string with the given `pat` repeatedly removed from the start
/// of the string
///
/// ```scheme
/// (trim-start-matches string? string?) -> string?
/// ```
///
/// # Examples
/// ```scheme
/// > (trim-start-matches "123foo1bar123123" "123") ;; => "foo1bar123123"
/// ```
#[function(name = "trim-start-matches")]
pub fn trim_start_matches(value: &SteelString, pat: &SteelString) -> String {
    value.trim_start_matches(pat.as_str()).into()
}

/// Returns a list of strings from the original string split on the whitespace
///
/// (split-whitespace string?) -> (listof string?)
///
/// # Examples
///
/// ```scheme
/// (split-whitespace "apples bananas fruits veggies") ;; '("apples" "bananas" "fruits" "veggies")
/// (split-whitespace "one\t \ttwo\nthree") ;; '("one" "two" "three")
/// ```
#[function(name = "split-whitespace")]
pub fn split_whitespace(value: &SteelString) -> SteelVal {
    let split: List<SteelVal> = value
        .split_whitespace()
        .map(|x| SteelVal::StringV(x.into()))
        .collect();
    split.into()
}

/// Splits a string given a separator at most once, yielding
/// a list with at most 2 elements.
///
/// (split-once str pat) -> string?
///
/// * str : string?
/// * pat : string?
///
/// # Examples
/// ```scheme
/// (split-once "foo,bar,baz" ",") ;; => '("foo" "bar,baz")
/// (split-once "foo|bar|" "|") ;; => '("foo" "bar|")
/// (split-once "" "&") ;; => '("")
/// ```
#[function(name = "split-once")]
pub fn split_once(value: &SteelString, pat: &SteelString) -> SteelVal {
    let split: Option<List<SteelVal>> = value
        .split_once(pat.as_str())
        .map(|(x, y)| vec![SteelVal::StringV(x.into()), SteelVal::StringV(y.into())].into());
    split.into()
}

/// Splits a string given a separator pattern into a list of strings.
///
/// (split-many str pat) -> (listof string?)
///
/// * str : string?
/// * pat : string?
///
/// # Examples
/// ```scheme
/// (split-many "foo,bar,baz" ",") ;; => '("foo" "bar" "baz")
/// (split-many "foo|bar|" "|") ;; => '("foo" "bar" "")
/// (split-many "" "&") ;; => '("")
/// ```
#[function(name = "split-many")]
pub fn split_many(value: &SteelString, pat: &SteelString) -> SteelVal {
    let split: List<SteelVal> = value
        .split(pat.as_str())
        .map(|x| SteelVal::StringV(x.into()))
        .collect();
    split.into()
}

/// Checks if the input string starts with a prefix
///
/// (starts-with? input pattern) -> bool?
///
/// * input : string?
/// * pattern: string?
///
/// # Examples
///
/// ```scheme
/// > (starts-with? "foobar" "foo") ;; => #true
/// > (starts-with? "foobar" "bar") ;; => #false
/// ```
#[function(name = "starts-with?")]
pub fn starts_with(value: &SteelString, prefix: &SteelString) -> bool {
    value.starts_with(prefix.as_str())
}

/// Checks if the input string ends with a given suffix
///
/// (ends-with? input pattern) -> bool?
///
/// * input : string?
/// * pattern: string?
///
/// # Examples
///
/// ```scheme
/// > (ends-with? "foobar" "foo") ;; => #false
/// > (ends-with? "foobar" "bar") ;; => #true
/// ```
#[function(name = "ends-with?")]
pub fn ends_with(value: &SteelString, suffix: &SteelString) -> bool {
    value.ends_with(suffix.as_str())
}

/// Get the number of characters in the string.
///
/// (string-length string?) -> int?
///
/// # Examples
///
/// ```scheme
/// > (string-length "apples") ;; => 6
/// > (string-length "αβγ") ;; => 3
/// > (string-length "✅") ;; => 1
/// ```
#[function(name = "string-length")]
pub fn string_length(value: &SteelString) -> usize {
    value.chars().count()
}

/// Get the length of the string in UTF-8 bytes.
///
/// (utf8-length string?) -> int?
///
/// # Examples
///
/// ```scheme
/// > (utf8-length "apples") ;; => 6
/// > (utf8-length "αβγ") ;; => 6
/// > (utf8-length "✅") ;; => 3
/// ```
#[function(name = "utf8-length")]
pub fn utf8_length(value: &SteelString) -> usize {
    value.len()
}

/// Concatenates all of the given strings into one
///
/// (string-append strs...) -> string?
///
/// * strs ... : string?
///
/// # Examples
/// ```scheme
/// > (string-append) ;; => ""
/// > (string-append "foo" "bar") ;; => "foobar"
/// ```
#[function(name = "string-append")]
pub fn string_append(mut rest: RestArgsIter<'_, &SteelString>) -> Result<SteelVal> {
    rest.0
        .try_fold("".to_string(), |accum, next| Ok(accum + next?.as_str()))
        .map(|x| SteelVal::StringV(x.into()))
}

/// Checks if all characters are equal.
///
/// Requires that all inputs are characters, and will otherwise raise an error.
///
/// (char=? char1 char2 ...) -> bool?
///
/// * char1 : char?
/// * char2 : char?
///
/// # Examples
///
/// ```scheme
/// > (char=? #\a #\a) ;; => #t
/// > (char=? #\a #\b) ;; => #f
/// > (char=? #\a #\A) ;; => #f
/// ```
#[function(name = "char=?", constant = true)]
pub fn char_equals(rest: RestArgsIter<char>) -> Result<SteelVal> {
    monotonic!(rest, |ch1: &_, ch2: &_| ch1 == ch2)
}

// TODO:
// Have checks for the type of the arguments each, so we can
// pass a function like: `char`, `char`, and skip all sorts of checks.
//
// Those checks can basically be done inline entirely.
pub fn char_equals_binop(l: SteelVal, r: SteelVal) -> Result<SteelVal> {
    use crate::rvals::FromSteelVal;
    let l = char::from_steelval(&l)?;
    let r = char::from_steelval(&r)?;
    Ok(SteelVal::BoolV(l == r))
}

/// Checks if all characters are equal, in a case-insensitive fashion
/// (i.e. as if char-foldcase was applied to the arguments).
///
/// Requires that all inputs are characters, and will otherwise raise an error.
///
/// (char-ci=? char1 char2 ...) -> bool?
///
/// * char1 : char?
/// * char2 : char?
///
/// # Examples
///
/// ```scheme
/// > (char-ci=? #\s #\S) ;; => #t
/// > (char-ci=? #\ß #\ẞ) ;; => #t
/// > (char-ci=? #\σ #\Σ #\ς) ;; => #t
/// ```
#[function(name = "char-ci=?", constant = true)]
pub fn char_ci_equals(rest: RestArgsIter<char>) -> Result<SteelVal> {
    let cm = CaseMapper::new();
    monotonic!(
        rest.map(|ch| ch.map(|ch| cm.simple_fold(ch))),
        |ch1: &_, ch2: &_| ch1 == ch2
    )
}

macro_rules! impl_char_comparison {
    (-ci, $name:ident, $ext_name:literal, $mode:literal, $op:expr, $(#[$attr:meta])*) => {
        #[doc = concat!("Returns `#t` if the characters are ", $mode ," according to their codepoints,")]
        #[doc = "in a case-insensitive fashion (as if char-foldcase was applied to the arguments)."]
        #[doc = ""]
        #[doc = concat!("(", $ext_name, " char1 char2 ... ) -> bool?")]
        #[doc = "* char1 : char?"]
        #[doc = "* char2 : char?"]
        $(#[$attr])*
        #[function(name = $ext_name, constant = true)]
        pub fn $name(rest: RestArgsIter<&char>) -> Result<SteelVal> {
            let cm = CaseMapper::new();
            monotonic!(rest.map(|ch| ch.map(|ch| cm.simple_fold(*ch))), $op)
        }
    };
    ($name:ident, $ext_name:literal, $mode:literal, $op:expr, $(#[$attr:meta])*) => {
        #[doc = concat!("Returns `#t` if the characters are ", $mode ," according to their codepoints.")]
        #[doc = ""]
        #[doc = concat!("(", $ext_name, " char1 char2 ... ) -> bool?")]
        #[doc = "* char1 : char?"]
        #[doc = "* char2 : char?"]
        $(#[$attr])*
        #[function(name = $ext_name, constant = true)]
        pub fn $name(rest: RestArgsIter<&char>) -> Result<SteelVal> {
            monotonic!(rest, $op)
        }
    };
}

impl_char_comparison!(
    char_less_than,
    "char<?",
    "monotonically increasing",
    |ch1: &_, ch2: &_| ch1 < ch2,
    /// # Examples
    ///
    /// ```scheme
    /// > (char<? #\a #\b) ;; => #t
    /// > (char<? #\a #\b #\c) ;; => #t
    /// > (char<? #\a #\b #\b) ;; => #f
    /// ```
);
impl_char_comparison!(
    -ci,
    char_ci_less_than,
    "char-ci<?",
    "monotonically increasing",
    |ch1: &_, ch2: &_| ch1 < ch2,
    /// # Examples
    ///
    /// ```scheme
    /// > (char-ci<? #\a #\b) ;; => #t
    /// > (char-ci<? #\a #\B) ;; => #t
    /// > (char-ci<? #\a #\B #\c) ;; => #t
    /// > (char-ci<? #\a #\B #\b) ;; => #f
    /// ```
);
impl_char_comparison!(
    char_less_than_equal_to,
    "char<=?",
    "monotonically non-decreasing",
    |ch1: &_, ch2: &_| ch1 <= ch2,
    /// # Examples
    ///
    /// ```scheme
    /// > (char<=? #\a #\b) ;; => #t
    /// > (char<=? #\a #\B) ;; => #f
    /// > (char<=? #\a #\b #\c) ;; => #t
    /// > (char<=? #\a #\b #\b) ;; => #t
    /// ```
);
impl_char_comparison!(
    -ci,
    char_ci_less_than_equal_to,
    "char-ci<=?",
    "monotonically non-decreasing",
    |ch1: &_, ch2: &_| ch1 <= ch2,
    /// # Examples
    ///
    /// ```scheme
    /// > (char-ci<=? #\a #\b) ;; => #t
    /// > (char-ci<=? #\a #\B) ;; => #t
    /// > (char-ci<=? #\a #\B #\c) ;; => #t
    /// > (char-ci<=? #\a #\B #\b) ;; => #t
    /// ```
);
impl_char_comparison!(
    char_greater_than,
    "char>?",
    "monotonically decreasing",
    |ch1: &_, ch2: &_| ch1 > ch2,
    /// # Examples
    ///
    /// ```scheme
    /// > (char>? #\b #\a) ;; => #t
    /// > (char>? #\c #\b #\a) ;; => #t
    /// > (char>? #\c #\b #\b) ;; => #f
    /// ```
);
impl_char_comparison!(
    -ci,
    char_ci_greater_than,
    "char-ci>?",
    "monotonically decreasing",
    |ch1: &_, ch2: &_| ch1 > ch2,
    /// # Examples
    ///
    /// ```scheme
    /// > (char-ci>? #\b #\a) ;; => #t
    /// > (char-ci>? #\B #\a) ;; => #t
    /// > (char-ci>? #\c #\B #\a) ;; => #t
    /// > (char-ci>? #\c #\B #\b) ;; => #f
    /// ```
);
impl_char_comparison!(
    char_greater_than_equal_to,
    "char>=?",
    "monotonically non-increasing",
    |ch1: &_, ch2: &_| ch1 >= ch2,
    /// # Examples
    ///
    /// ```scheme
    /// > (char>=? #\b #\a) ;; => #t
    /// > (char>=? #\c #\b #\a) ;; => #t
    /// > (char>=? #\c #\b #\b) ;; => #t
    /// ```
);
impl_char_comparison!(
    -ci,
    char_ci_greater_than_equal_to,
    "char-ci>=?",
    "monotonically non-increasing",
    |ch1: &_, ch2: &_| ch1 >= ch2,
    /// # Examples
    ///
    /// ```scheme
    /// > (char-ci>? #\b #\a) ;; => #t
    /// > (char-ci>? #\B #\a) ;; => #t
    /// > (char-ci>? #\c #\B #\a) ;; => #t
    /// > (char-ci>? #\c #\B #\b) ;; => #t
    /// ```
);

/// Returns the Unicode codepoint of a given character.
///
/// (char->integer char?) -> integer?
///
/// # Examples
///
/// ```scheme
/// > (char->integer #\a) ;; => 97
/// > (char->integer #\λ) ;; => 955
/// ```
#[function(name = "char->integer")]
pub fn char_to_integer(ch: char) -> u32 {
    ch as u32
}

/// Returns the character corresponding to a given Unicode codepoint.
///
/// (integer->char integer?) -> char?
///
/// # Examples
///
/// ```scheme
/// > (integer->char #x61) ;; => #\a
/// > (integer->char 955) ;; => #\λ
/// ```
#[function(name = "integer->char")]
pub fn integer_to_char(int: u32) -> Result<SteelVal> {
    let Some(ch) = char::from_u32(int) else {
        stop!(ConversionError => "integer {} is out of range for a character", int);
    };

    Ok(ch.into())
}

/// Encodes a string as UTF-8 into a bytevector.
///
/// (string->bytes str [start] [end]) -> bytes?
///
/// * str : string?
/// * start : int? = 0
/// * end : int? = (string-length str)
///
/// # Examples
///
/// ```scheme
/// (string->bytes "Apple") ;; => #u8(#x41 #x70 #x70 #x6C #x65)
/// (string->bytes "αβγ") ;; => #u8(#xCE #xB1 #xCE #xB2 #xCE #xB3)
/// (string->bytes "one two three" 4 7) ;; => #u8(#x74 #x77 #x6F)
/// ```
#[function(name = "string->bytes", alias = "string->utf8")]
pub fn string_to_bytes(value: &SteelString, mut rest: RestArgsIter<isize>) -> Result<SteelVal> {
    let start = rest.next().transpose()?;
    let end = rest.next().transpose()?;

    if rest.next().is_some() {
        stop!(ArityMismatch => "string->bytes expects up to 3 parameters");
    }

    let range = bounds(value.as_str(), start, end, "string->bytes")?;

    let bytes = value.as_bytes()[range].to_owned();

    Ok(SteelVal::ByteVector(SteelByteVector::new(bytes)))
}

/// Returns a vector containing the characters of a given string
///
/// (string->vector s [start] [end]) -> vector?
///
/// * str : string?
/// * start : int? = 0
/// * end : int? = (string-length str)
///
/// # Examples
///
/// ```scheme
/// (string->vector "hello") ;; => '#(#\h #\e #\l #\l #\o)
/// (string->vector "one two three" 4 7) ;; => '#(#\t #\w #\o)
/// ```
#[function(name = "string->vector")]
pub fn string_to_vector(value: &SteelString, mut rest: RestArgsIter<isize>) -> Result<SteelVal> {
    let start = rest.next().transpose()?;
    let end = rest.next().transpose()?;

    if rest.next().is_some() {
        stop!(ArityMismatch => "string->vector expects up to 3 parameters");
    }

    let range = bounds(value.as_str(), start, end, "string->vector")?;

    let chars: Vector<_> = value[range].chars().map(SteelVal::CharV).collect();

    Ok(SteelVal::VectorV(Gc::new(chars).into()))
}

fn bounds(
    s: &str,
    i: Option<isize>,
    j: Option<isize>,
    name: &str,
) -> Result<core::ops::Range<usize>> {
    use core::iter::once;

    let i = i.unwrap_or(0);

    if i < 0 {
        stop!(ContractViolation => "{}: bounds must be non-negative: left: {}", name, i);
    }

    let i = i as usize;

    if i > s.len() {
        stop!(Generic => "{}: index out of bounds: left bound: {}, string length: {}", name, i, s.len());
    }

    if let Some(j) = j {
        if j < 0 {
            stop!(ContractViolation => "{}: bounds must be non-negative: right: {}", name, j);
        }

        if i > (j as usize) {
            stop!(Generic => "{}: left bound must be less than or equal to the right bound: left: {}, right: {}", name, i, j);
        }
    }

    let j = j.map(|j| j as usize);

    let mut char_offsets = s
        .char_indices()
        .map(|(offset, _)| offset)
        .chain(once(s.len()));

    let Some(start) = char_offsets.nth(i) else {
        stop!(Generic => "{}: index out of bounds: left bound: {}", name, i);
    };

    let Some(j) = j else {
        return Ok(start..s.len());
    };

    let mut char_offsets = once(start).chain(char_offsets);

    let Some(end) = char_offsets.nth(j - i) else {
        stop!(Generic => "{}: index out of bounds: right bound: {}", name, j);
    };

    Ok(start..end)
}

/// Returns the upper case version of a character, if defined by Unicode,
/// or the same character otherwise.
///
/// (char-upcase char?) -> char?
///
/// # Examples
///
/// ```scheme
/// > (char-upcase #\d) ;; => #\D
/// > (char-upcase #\U) ;; => #\U
/// > (char-upcase #\ß) ;; => #\ß
/// ```
#[function(name = "char-upcase")]
fn char_upcase(c: char) -> char {
    let cm = CaseMapper::new();
    cm.simple_uppercase(c)
}

/// Returns the lower case version of a character, if defined by Unicode,
/// or the same character otherwise.
///
/// (char-downcase char?) -> char?
///
/// # Examples
///
/// ```scheme
/// > (char-downcase #\U) ;; => #\u
/// > (char-downcase #\d) ;; => #\d
/// > (char-downcase #\ẞ) ;; => #\ß
/// ```
#[function(name = "char-downcase")]
fn char_downcase(c: char) -> char {
    let cm = CaseMapper::new();
    cm.simple_lowercase(c)
}

/// Apply simple unicode case-folding to a char
///
/// (char-foldcase char?) -> char?
///
/// # Examples
///
/// ```scheme
/// > (char-foldcase #\A) ;; => #\a
/// > (char-foldcase #\c) ;; => #\c
/// > (char-foldcase #\ς) ;; => #\σ
/// ```
#[function(name = "char-foldcase")]
fn char_foldcase(c: char) -> char {
    let cm = CaseMapper::new();
    cm.simple_fold(c)
}

/// Returns `#t` if the character is a whitespace character.
///
/// # Example
///
/// ```scheme
/// > (char-whitespace? #\space) ;; => #t
/// > (char-whitespace? #\newline) ;; => #t
/// ; nbsp character
/// > (char-whitespace? #\xA0) ;; => #t
/// > (char-whitespace? #\越) ;; => #f
/// ```
#[function(name = "char-whitespace?")]
fn char_is_whitespace(c: char) -> bool {
    c.is_whitespace()
}

/// Returns `#t` if the character is an ascii decimal digit.
///
/// (char-digit? char?) -> bool?
///
/// # Examples
///
/// ```scheme
/// > (char-digit? #\4) ;; => #t
/// > (char-digit? #\a) ;; => #f
/// > (char-digit? #\٣) ;; => #f
/// > (char-digit? #\①) ;; => #f
/// ```
#[function(name = "char-digit?")]
fn char_is_digit(c: char) -> bool {
    c.is_digit(10)
}

/// Attemps to convert the character into an ascii decimal digit,
/// and returns `#f` on failure.
///
/// (char->number char?) -> (or/c number? bool?)
///
/// # Examples
///
/// ```scheme
/// > (char->number #\4) ;; => 4
/// > (char->number #\a) ;; => #f
/// > (char->number #\٣) ;; => #f
/// ```
#[function(name = "char->number")]
fn char_to_number(c: char) -> Option<u32> {
    c.to_digit(10)
}

/// Joins the given list of strings, with an optional separator.
///
/// (string-join strings [sep]) -> string?
///
/// * strings : (listof string?)
/// * sep : string? = ""
///
/// # Examples
/// ```scheme
/// (string-join '("a" "b" "c")) ;; => "abc"
/// (string-join '("one" "two" "three") ", ") ;; => "one, two, three"
/// ```
#[function(name = "string-join")]
fn string_join(
    strings: SteelList<SteelVal>,
    mut rest: RestArgsIter<'_, &SteelString>,
) -> Result<SteelVal> {
    let mut joined = String::new();

    let len = strings.len();

    let sep = rest.next().transpose()?;

    if rest.next().is_some() {
        todo!()
    }

    for (i, val) in strings.into_iter().enumerate() {
        let SteelVal::StringV(s) = val else {
            stop!(TypeMismatch => "string-join: expected a list of strings");
        };

        joined += s.as_str();

        if i + 1 < len {
            if let Some(sep) = sep.as_ref() {
                joined += sep.as_str();
            }
        }
    }

    Ok(joined.into())
}

/// Searches a string to check if it contains the second argument.
///
/// (string-contains? string? string?) -> bool?
///
/// # Examples
/// ```scheme
/// (string-contains? "hello" "lo") ;;=> #t
/// (string-contains? "hello" "world") ;;=> #f
/// ```
#[function(name = "string-contains?")]
fn string_contains(s: &SteelString, contained: &SteelString) -> bool {
    s.as_str().contains(contained.as_str())
}

#[cfg(test)]
mod string_operation_tests {
    use super::*;
    use crate::rerrs::ErrorKind;

    // TODO combine these 3 macros into one
    macro_rules! apply_tests_arity_too_many {
        ($(($name:expr, $symbol:ident, $func:expr)),* $(,)?) => {
            $(
                #[test]
                pub fn $symbol() {
                    let args = vec![
                        SteelVal::StringV("FOO".into()),
                        SteelVal::StringV("BAR".into()),
                    ];
                    let res = $func(&args);
                    let expected = ErrorKind::ArityMismatch;
                    assert_eq!(res.unwrap_err().kind(), expected);
                }
            )*
        };
    }

    macro_rules! apply_tests_arity_too_few {
        ($(($name:expr, $symbol:ident, $func:expr)),* $(,)?) => {
            $(
                #[test]
                pub fn $symbol() {
                    let args = vec![];
                    let res = $func(&args);
                    let expected = ErrorKind::ArityMismatch;
                    assert_eq!(res.unwrap_err().kind(), expected);
                }
            )*
        };
    }

    macro_rules! apply_tests_bad_arg {
        ($(($name:expr, $symbol:ident, $func:expr)),* $(,)?) => {
            $(
                #[test]
                pub fn $symbol() {
                    let args = vec![SteelVal::NumV(10.0)];
                    let res = $func(&args);
                    let expected = ErrorKind::TypeMismatch;
                    assert_eq!(res.unwrap_err().kind(), expected);
                }
            )*
        };
    }

    apply_tests_arity_too_many! {
        ("string-upcase", string_upper_arity_too_many, steel_string_upcase),
        ("string-downcase", string_lower_arity_too_many, steel_string_downcase),
        ("trim", trim_arity_too_many, steel_trim),
        ("trim-start", trim_start_arity_too_many, steel_trim_start),
        ("trim-end", trim_end_arity_too_many, steel_trim_end),
        ("split-whitespace", split_whitespace_arity_too_many, steel_split_whitespace),
    }

    apply_tests_arity_too_few! {
        ("string-upcase", string_upper_arity_too_few, steel_string_upcase),
        ("string-downcase", string_lower_arity_too_few, steel_string_downcase),
        ("trim", trim_arity_too_few, steel_trim),
        ("trim-start", trim_start_arity_too_few, steel_trim_start),
        ("trim-end", trim_end_arity_too_few, steel_trim_end),
        ("string->list", string_to_list_arity_too_few, steel_string_to_list),
        ("split-whitespace", split_whitespace_arity_too_few, steel_split_whitespace)
    }

    apply_tests_bad_arg! {
        ("string-upcase", string_upper_arity_takes_string, steel_string_upcase),
        ("string-downcase", string_lower_arity_takes_string, steel_string_downcase),
        ("trim", trim_arity_takes_string, steel_trim),
        ("trim-start", trim_start_arity_takes_string, steel_trim_start),
        ("trim-end", trim_end_arity_takes_string, steel_trim_end),
        ("string->list", string_to_list_takes_string, steel_string_to_list),
        ("string->bytes", string_to_bytes_takes_string, steel_string_to_bytes),
        ("string->vector", string_to_vector_takes_string, steel_string_to_vector),
        ("split-whitespace", split_whitespace_arity_takes_string, steel_split_whitespace)
    }

    #[test]
    fn string_append_test_normal() {
        let args = vec![
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("bar".into()),
        ];
        let res = steel_string_append(&args);
        let expected = SteelVal::StringV("foobar".into());
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn string_append_test_takes_string() {
        let args = vec![SteelVal::CharV('a'), SteelVal::CharV('b')];
        let res = steel_string_append(&args);
        let expected = ErrorKind::TypeMismatch;
        assert_eq!(res.unwrap_err().kind(), expected);
    }

    #[test]
    fn string_to_upper_normal() {
        let args = vec![SteelVal::StringV("foobarbaz".into())];
        let res = steel_string_upcase(&args);
        let expected = SteelVal::StringV("FOOBARBAZ".into());
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn string_to_upper_spaces() {
        let args = vec![SteelVal::StringV("foo bar baz qux".into())];
        let res = steel_string_upcase(&args);
        let expected = SteelVal::StringV("FOO BAR BAZ QUX".into());
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn string_to_lower_normal() {
        let args = vec![SteelVal::StringV("FOOBARBAZ".into())];
        let res = steel_string_downcase(&args);
        let expected = SteelVal::StringV("foobarbaz".into());
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn string_to_lower_spaces() {
        let args = vec![SteelVal::StringV("FOO BAR BAZ QUX".into())];
        let res = steel_string_downcase(&args);
        let expected = SteelVal::StringV("foo bar baz qux".into());
        assert_eq!(res.unwrap(), expected);
    }

    // TODO investigate this, assert_eq! fails without converting to string
    #[test]
    fn string_to_list_normal() {
        let args = vec![SteelVal::StringV("foo".into())];
        let res = steel_string_to_list(&args);

        let expected = SteelVal::ListV(
            vec![
                SteelVal::CharV('f'),
                SteelVal::CharV('o'),
                SteelVal::CharV('o'),
            ]
            .into(),
        );

        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn string_to_list_empty() {
        let args = vec![SteelVal::StringV("".into())];
        let res = steel_string_to_list(&args);
        let expected = SteelVal::ListV(List::new());
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn trim_normal_no_changes() {
        let args = vec![SteelVal::StringV("foo".into())];
        let res = steel_trim(&args);
        let expected = SteelVal::StringV("foo".into());
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn trim_normal_trims_both_sides() {
        let args = vec![SteelVal::StringV("      foo  ".into())];
        let res = steel_trim(&args);
        let expected = SteelVal::StringV("foo".into());
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn trim_start_no_changes() {
        let args = vec![SteelVal::StringV("foo".into())];
        let res = steel_trim_start(&args);
        let expected = SteelVal::StringV("foo".into());
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn trim_end_no_changes() {
        let args = vec![SteelVal::StringV("foo".into())];
        let res = steel_trim_end(&args);
        let expected = SteelVal::StringV("foo".into());
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn trim_start_normal_trims_left_side() {
        let args = vec![SteelVal::StringV("      foo  ".into())];
        let res = steel_trim_start(&args);
        let expected = SteelVal::StringV("foo  ".into());
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn trim_end_normal_trims_right_side() {
        let args = vec![SteelVal::StringV("      foo  ".into())];
        let res = steel_trim_end(&args);
        let expected = SteelVal::StringV("      foo".into());
        assert_eq!(res.unwrap(), expected);
    }

    // TODO investigate this one
    #[test]
    fn split_whitespace_no_whitespace() {
        let args = vec![SteelVal::StringV("foo".into())];
        let res = steel_split_whitespace(&args);

        let expected = SteelVal::ListV(vec![SteelVal::StringV("foo".into())].into());

        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn split_whitespace_some_whitespace() {
        let args = vec![SteelVal::StringV("foo bar baz".into())];
        let res = steel_split_whitespace(&args);

        let expected = SteelVal::ListV(
            vec![
                SteelVal::StringV("foo".into()),
                SteelVal::StringV("bar".into()),
                SteelVal::StringV("baz".into()),
            ]
            .into(),
        );
        assert_eq!(res.unwrap(), expected);
    }
}
