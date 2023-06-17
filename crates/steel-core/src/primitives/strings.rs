use im_lists::list::List;

use crate::rvals::{Result, SteelString, SteelVal};
use crate::steel_vm::builtin::BuiltInModule;
use crate::steel_vm::register_fn::RegisterFn;
use crate::stop;

use steel_derive::function;

use super::ControlOperations;

macro_rules! ok_string {
    ($string:expr) => {
        Ok(SteelVal::StringV($string.into()))
    };
}

fn char_upcase(c: char) -> char {
    c.to_ascii_uppercase()
}

pub fn string_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/strings");
    module
        .register_value("string-append", StringOperations::string_append())
        .register_value("to-string", ControlOperations::to_string())
        .register_native_fn_definition(STRING_TO_LIST_DEFINITION)
        .register_native_fn_definition(STRING_TO_UPPER_DEFINITION)
        .register_native_fn_definition(STRING_TO_UPPER_DEFINITION)
        .register_native_fn_definition(STRING_TO_LOWER_DEFINITION)
        .register_native_fn_definition(STRING_LENGTH_DEFINITION)
        .register_native_fn_definition(TRIM_DEFINITION)
        .register_native_fn_definition(TRIM_START_DEFINITION)
        .register_native_fn_definition(TRIM_END_DEFINITION)
        .register_native_fn_definition(SPLIT_WHITESPACE_DEFINITION)
        .register_native_fn_definition(STRING_TO_INT_DEFINITION)
        .register_native_fn_definition(INT_TO_STRING_DEFINITION)
        .register_native_fn_definition(STRING_TO_SYMBOL_DEFINITION)
        .register_native_fn_definition(STARTS_WITH_DEFINITION)
        .register_native_fn_definition(ENDS_WITH_DEFINITION)
        .register_fn("char-upcase", char_upcase);
    module
}

// Just write the documentation for every function, inline - this will make it easier to export the docs!
/// Converts a string into a symbol.
///
/// (string->symbol string?) -> symbol?
///
/// # Examples
///
/// ```scheme
/// > (string->symbol "FooBar") ;; => 'FooBar
/// ```
#[function(name = "string->symbol")]
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

/// Checks if the input string starts with a prefix
///
/// (starts-with? input pattern) -> bool?
///
///    input : string?
///    pattern: string?
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

/// Get the length of the given string
///
/// (string-length string?) -> int?
///
/// # Examples
///
/// ```scheme
/// > (string-length "apples") ;; => 6
/// ```
#[function(name = "string-length")]
pub fn string_length(value: &SteelString) -> usize {
    value.len()
}

// TODO: Undo all of these wrappers, just reference the native functions directly
pub struct StringOperations {}
impl StringOperations {
    pub fn string_append() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() >= 2 {
                let mut arg_iter = args.iter();

                let first_arg = arg_iter.next().unwrap();

                let mut first = if let SteelVal::StringV(first) = first_arg {
                    first.to_string()
                } else {
                    stop!(TypeMismatch => format!("string-append expected a string, found: {first_arg}"))
                };

                for arg in arg_iter {
                    if let SteelVal::StringV(r) = arg {
                        first = first + r;
                    } else {
                        stop!(TypeMismatch => format!("string-append expected a string, found: {first_arg}"))
                    };
                }

                ok_string!(first)
            } else {
                stop!(ArityMismatch => "string-append takes at least two arguments")
            }
        })
    }
}

#[cfg(test)]
mod string_operation_tests {
    use super::*;
    use crate::rerrs::ErrorKind;
    use crate::throw;
    use im_lists::list;

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

    fn apply_function(func: SteelVal, args: Vec<SteelVal>) -> Result<SteelVal> {
        func.func_or_else(throw!(BadSyntax => "string tests"))
            .unwrap()(&args)
    }

    #[test]
    fn string_append_test_normal() {
        let args = vec![
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("bar".into()),
        ];
        let res = apply_function(StringOperations::string_append(), args);
        let expected = SteelVal::StringV("foobar".into());
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn string_append_test_arity_mismatch_too_few() {
        let args = vec![SteelVal::StringV("foo".into())];
        let res = apply_function(StringOperations::string_append(), args);
        let expected = ErrorKind::ArityMismatch;
        assert_eq!(res.unwrap_err().kind(), expected);
    }

    #[test]
    fn string_append_test_takes_string() {
        let args = vec![SteelVal::CharV('a'), SteelVal::CharV('b')];
        let res = apply_function(StringOperations::string_append(), args);
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

        let expected = SteelVal::ListV(list![
            SteelVal::CharV('f'),
            SteelVal::CharV('o'),
            SteelVal::CharV('o')
        ]);

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

        let expected = SteelVal::ListV(list![SteelVal::StringV("foo".into())]);

        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn split_whitespace_some_whitespace() {
        let args = vec![SteelVal::StringV("foo bar baz".into())];
        let res = steel_split_whitespace(&args);

        let expected = SteelVal::ListV(list![
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("bar".into()),
            SteelVal::StringV("baz".into())
        ]);
        assert_eq!(res.unwrap(), expected);
    }
}
