use crate::gc::Gc;
use crate::values::lists::List;

use crate::rvals::{RestArgsIter, Result, SteelString, SteelVal};
use crate::steel_vm::builtin::BuiltInModule;
use crate::steel_vm::register_fn::RegisterFn;
use crate::stop;

use num::{BigInt, Num};
use steel_derive::{function, native};

fn char_upcase(c: char) -> char {
    c.to_ascii_uppercase()
}

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
        .register_native_fn_definition(STRING_TO_UPPER_DEFINITION)
        .register_native_fn_definition(STRING_TO_LOWER_DEFINITION)
        .register_native_fn_definition(STRING_LENGTH_DEFINITION)
        .register_native_fn_definition(TRIM_DEFINITION)
        .register_native_fn_definition(TRIM_START_DEFINITION)
        .register_native_fn_definition(TRIM_END_DEFINITION)
        .register_native_fn_definition(SPLIT_WHITESPACE_DEFINITION)
        .register_native_fn_definition(SPLIT_ONCE_DEFINITION)
        .register_native_fn_definition(SPLIT_MANY_DEFINITION)
        .register_native_fn_definition(STRING_TO_INT_DEFINITION)
        .register_native_fn_definition(INT_TO_STRING_DEFINITION)
        .register_native_fn_definition(STRING_TO_SYMBOL_DEFINITION)
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
        .register_fn("char-upcase", char_upcase)
        .register_fn("char-whitespace?", char::is_whitespace)
        .register_fn("char-digit?", |c: char| char::is_digit(c, 10))
        .register_fn("char->number", |c: char| char::to_digit(c, 10))
        .register_native_fn_definition(CHAR_EQUALS_DEFINITION);
    module
}

/// Checks if two characters are equal
///
/// Requires that the two inputs are both characters, and will otherwise
/// raise an error.
#[function(name = "char=?", constant = true)]
pub fn char_equals(left: char, right: char) -> bool {
    left == right
}

fn number_to_string_impl(value: &SteelVal, radix: Option<u32>) -> Result<SteelVal> {
    match value {
        SteelVal::IntV(v) => {
            if let Some(radix) = radix {
                Ok(SteelVal::StringV(
                    radix_fmt::radix(*v, radix as u8).to_string().into(),
                ))
            } else {
                Ok(SteelVal::StringV(v.to_string().into()))
            }
        }
        SteelVal::NumV(n) => Ok(SteelVal::StringV(n.to_string().into())),
        SteelVal::BigNum(n) => Ok(SteelVal::StringV(n.to_string().into())),
        _ => stop!(TypeMismatch => "number->string expects a number type, found: {}", value),
    }
}

/// Converts the given number to a string
#[function(name = "number->string", constant = true)]
pub fn number_to_string(value: &SteelVal, mut rest: RestArgsIter<'_, isize>) -> Result<SteelVal> {
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

    number_to_string_impl(value, radix)
}

fn string_to_number_impl(value: &str, radix: Option<u32>) -> Result<SteelVal> {
    let expr = crate::parser::parser::Parser::parse(value)?;

    if expr.len() != 1 {
        return Ok(SteelVal::BoolV(false));
    }

    let explicit_radix = matches!(
        value.get(0..2),
        Some("#x") | Some("#d") | Some("#o") | Some("#b")
    );

    let implicit_radix = radix.filter(|_| explicit_radix);

    let number = expr.into_iter().next().unwrap();

    let svalue = SteelVal::try_from(number)?;

    match (svalue, implicit_radix) {
        (SteelVal::IntV(_), Some(radix)) => match isize::from_str_radix(value, radix) {
            Ok(parsed) => Ok(SteelVal::IntV(parsed)),
            Err(_) => Ok(SteelVal::BoolV(false)),
        },
        (val @ SteelVal::IntV(_), None) => Ok(val),
        (SteelVal::BigNum(_), Some(radix)) => match BigInt::from_str_radix(value, radix) {
            Ok(parsed) => Ok(SteelVal::BigNum(Gc::new(parsed))),
            Err(_) => Ok(SteelVal::BoolV(false)),
        },

        (val @ SteelVal::BigNum(_), None) => Ok(val),
        (svalue @ SteelVal::NumV(_), _) => Ok(svalue),
        _ => Ok(SteelVal::BoolV(false)),
    }
}

/// Converts the given string to a number, with an optional radix.
/// On failure, it returns `f`
///
/// (string->number digits [radix]) -> (or/c number? boolean?)
///
/// * digits : string?
/// * radix : number?
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
    match string_to_number_impl(value.as_str(), radix) {
        Ok(v) => Ok(v),
        Err(_) => Ok(SteelVal::BoolV(false)),
    }
}

/// Constructs a string from the given characters
#[function(name = "string")]
pub fn string_constructor(rest: RestArgsIter<'_, char>) -> Result<SteelVal> {
    rest.collect::<Result<String>>().map(|x| x.into())
}

/// Compares two strings lexicographically (as in "less-than-or-equal").
#[function(name = "string<=?", constant = true)]
pub fn string_less_than_equal_to(left: &SteelString, right: &SteelString) -> bool {
    left <= right
}

/// Compares two strings lexicographically (as in "less-than-or-equal"),
// in a case insensitive fashion.
#[function(name = "string-ci<=?", constant = true)]
pub fn string_ci_less_than_equal_to(left: &SteelString, right: &SteelString) -> bool {
    left.to_lowercase() <= right.to_lowercase()
}

/// Compares two strings lexicographically (as in "less-than").
#[function(name = "string<?", constant = true)]
pub fn string_less_than(left: &SteelString, right: &SteelString) -> bool {
    left < right
}

/// Compares two strings lexicographically (as in "less-than"),
/// in a case insensitive fashion.
#[function(name = "string-ci<?", constant = true)]
pub fn string_ci_less_than(left: &SteelString, right: &SteelString) -> bool {
    left.to_lowercase() < right.to_lowercase()
}

/// Compares two strings lexicographically (as in "greater-than-or-equal").
#[function(name = "string>=?", constant = true)]
pub fn string_greater_than_equal_to(left: &SteelString, right: &SteelString) -> bool {
    left >= right
}

/// Compares two strings lexicographically (as in "greater-than-or-equal"),
/// in a case insensitive fashion.
#[function(name = "string-ci>=?", constant = true)]
pub fn string_ci_greater_than_equal_to(left: &SteelString, right: &SteelString) -> bool {
    left.to_lowercase() >= right.to_lowercase()
}

/// Compares two strings lexicographically (as in "greater-than").
#[function(name = "string>?", constant = true)]
pub fn string_greater_than(left: &SteelString, right: &SteelString) -> bool {
    left > right
}

/// Compares two strings lexicographically (as in "greater-than"),
/// in a case-insensitive fashion.
#[function(name = "string-ci>?", constant = true)]
pub fn string_ci_greater_than(left: &SteelString, right: &SteelString) -> bool {
    left.to_lowercase() > right.to_lowercase()
}

/// Compares two strings for equality.
#[function(name = "string=?", constant = true)]
pub fn string_equals(left: &SteelString, right: &SteelString) -> bool {
    left == right
}

/// Compares two strings for equality, in a case insensitive fashion.
#[function(name = "string-ci=?", constant = true)]
pub fn string_ci_equals(left: &SteelString, right: &SteelString) -> bool {
    left.to_lowercase() == right.to_lowercase()
}

/// Extracts the nth character out of a given string.
///
/// (string-ref str n)
///
/// * str : string?
/// * n : int?
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
    use std::iter::once;

    if i > value.len() {
        stop!(Generic => "substring: index out of bounds: left bound: {}, string length: {}", i, value.len());
    }

    let j = rest.next().transpose()?.map(|j| j as usize);

    if rest.next().is_some() {
        stop!(ArityMismatch => "substring expects 1 or 2 arguments");
    }

    if let Some(j) = j {
        if i > j {
            stop!(Generic => "substring: left bound must be less than or equal to the right bound: left: {}, right: {}", i, j);
        }
    }

    if value.is_empty() {
        return Ok(SteelVal::StringV("".into()));
    }

    let mut char_offsets = value
        .char_indices()
        .map(|(offset, _)| offset)
        .chain(once(value.len()));

    let Some(start) = char_offsets.nth(i) else {
        stop!(Generic => "substring: index out of bounds: left bound: {}", i);
    };

    let Some(j) = j else {
        return Ok(SteelVal::StringV(value[start..].into()));
    };

    let mut char_offsets = once(start).chain(char_offsets);

    let Some(end) = char_offsets.nth(j - i) else {
        stop!(Generic => "substring: index out of bounds: right bound: {}", j);
    };

    Ok(SteelVal::StringV(value[start..end].into()))
}

/// Creates a string of a given length, filled with an optional character
/// (which defaults to `#\0`).
///
/// (make-string len [char]) -> string?
///
/// * len : int?
/// * char : char? = #\0
#[function(name = "make-string")]
pub fn make_string(k: usize, mut c: RestArgsIter<'_, char>) -> Result<SteelVal> {
    // If the char is there, we want to take it
    let char = c.next();

    // We want the iterator to be exhaused
    if let Some(next) = c.next() {
        stop!(ArityMismatch => format!("make-string expected 1 or 2 arguments, got an additional argument {}", next?))
    }

    let c = char.unwrap_or(Ok('\0'))?;
    Ok((0..k).into_iter().map(|_| c).collect::<String>().into())
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

/// Converts a string into a symbol.
///
/// (string->symbol string?) -> symbol?
///
/// # Examples
///
/// ```scheme
/// > (string->symbol "FooBar") ;; => 'FooBar
/// ```
#[function(name = "string->symbol", constant = true)]
pub fn string_to_symbol(value: SteelString) -> SteelVal {
    SteelVal::SymbolV(value)
}

/// Converts an integer into a string.
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
/// (string->list string?) -> (listof char?)
///
/// # Examples
///
/// ```scheme
/// > (string->list "hello") ;; => '(#\h #\e #\l #\l #\o)
/// ```
#[function(name = "string->list")]
pub fn string_to_list(value: &SteelString) -> SteelVal {
    value
        .chars()
        .map(SteelVal::CharV)
        .collect::<List<_>>()
        .into()
}

/// Creates a new uppercased version of the input string
///
/// (string->upper string?) -> string?
///
/// # Examples
///
/// ```scheme
/// > (string->upper "lower") ;; => "LOWER"
/// ```
#[function(name = "string->upper")]
pub fn string_to_upper(value: &SteelString) -> String {
    value.to_uppercase()
}

/// Creates a new lowercased version of the input string
///
/// (string->lower string?) -> string?
///
/// # Examples
///
/// ```scheme
/// > (string->lower "sPonGeBoB tExT") ;; => "spongebob text"
/// ```
#[function(name = "string->lower")]
pub fn string_to_lower(value: &SteelString) -> String {
    value.to_lowercase()
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
///    input : string?
///    pattern: string?
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

/// Get the length of the given string in UTF-8 bytes.
///
/// (string-length string?) -> int?
///
/// # Examples
///
/// ```scheme
/// > (string-length "apples") ;; => 6
/// > (string-length "✅") ;; => 3
/// > (string-length "🤖") ;; => 4
/// ```
#[function(name = "string-length")]
pub fn string_length(value: &SteelString) -> usize {
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
        ("string-upcase", string_upper_arity_too_many, steel_string_to_upper),
        ("string-lowercase", string_lower_arity_too_many, steel_string_to_lower),
        ("trim", trim_arity_too_many, steel_trim),
        ("trim-start", trim_start_arity_too_many, steel_trim_start),
        ("trim-end", trim_end_arity_too_many, steel_trim_end),
        ("string->list", string_to_list_arity_too_many, steel_string_to_list),
        ("split-whitespace", split_whitespace_arity_too_many, steel_split_whitespace),
    }

    apply_tests_arity_too_few! {
        ("string-upcase", string_upper_arity_too_few, steel_string_to_upper),
        ("string-lowercase", string_lower_arity_too_few, steel_string_to_lower),
        ("trim", trim_arity_too_few, steel_trim),
        ("trim-start", trim_start_arity_too_few, steel_trim_start),
        ("trim-end", trim_end_arity_too_few, steel_trim_end),
        ("string->list", string_to_list_arity_too_few, steel_string_to_list),
        ("split-whitespace", split_whitespace_arity_too_few, steel_split_whitespace)
    }

    apply_tests_bad_arg! {
        ("string-upcase", string_upper_arity_takes_string, steel_string_to_upper),
        ("string-lowercase", string_lower_arity_takes_string, steel_string_to_lower),
        ("trim", trim_arity_takes_string, steel_trim),
        ("trim-start", trim_start_arity_takes_string, steel_trim_start),
        ("trim-end", trim_end_arity_takes_string, steel_trim_end),
        ("string->list", string_to_list_takes_string, steel_string_to_list),
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
        let res = steel_string_to_upper(&args);
        let expected = SteelVal::StringV("FOOBARBAZ".into());
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn string_to_upper_spaces() {
        let args = vec![SteelVal::StringV("foo bar baz qux".into())];
        let res = steel_string_to_upper(&args);
        let expected = SteelVal::StringV("FOO BAR BAZ QUX".into());
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn string_to_lower_normal() {
        let args = vec![SteelVal::StringV("FOOBARBAZ".into())];
        let res = steel_string_to_lower(&args);
        let expected = SteelVal::StringV("foobarbaz".into());
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn string_to_lower_spaces() {
        let args = vec![SteelVal::StringV("FOO BAR BAZ QUX".into())];
        let res = steel_string_to_lower(&args);
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
