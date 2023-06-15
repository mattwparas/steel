use im_lists::list::List;

use crate::rvals::{Result, SteelString, SteelVal};
use crate::steel_vm::builtin::{Arity, BuiltInModule};
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
        .register_native_fn("string->list", steel_string_to_list, STRING_TO_LIST_ARITY)
        .register_native_fn(
            "string-upcase",
            steel_string_to_upper,
            STRING_TO_UPPER_ARITY,
        )
        .register_native_fn(
            "string-lowercase",
            steel_string_to_lower,
            STRING_TO_LOWER_ARITY,
        )
        .register_native_fn("string-length", steel_string_length, STRING_LENGTH_ARITY)
        .register_native_fn("trim", steel_trim, TRIM_ARITY)
        .register_native_fn("trim-start", steel_trim_start, TRIM_START_ARITY)
        .register_native_fn("trim-end", steel_trim_end, TRIM_END_ARITY)
        .register_native_fn(
            "split-whitespace",
            steel_split_whitespace,
            SPLIT_WHITESPACE_ARITY,
        )
        .register_native_fn("string->int", steel_string_to_int, STRING_TO_INT_ARITY)
        .register_native_fn("int->string", steel_int_to_string, INT_TO_STRING_ARITY)
        .register_native_fn(
            "string->symbol",
            steel_string_to_symbol,
            STRING_TO_SYMBOL_ARITY,
        )
        .register_native_fn("starts-with?", steel_starts_with, STARTS_WITH_ARITY)
        .register_native_fn("ends-with?", steel_ends_with, ENDS_WITH_ARITY)
        .register_fn("char-upcase", char_upcase);
    module
}

#[function(name = "string->symbol")]
pub fn string_to_symbol(value: SteelString) -> SteelVal {
    SteelVal::SymbolV(value)
}

#[function(name = "int->string")]
pub fn int_to_string(value: isize) -> String {
    format!("{value}")
}

#[function(name = "string->int")]
pub fn string_to_int(value: SteelString) -> Result<SteelVal> {
    let parsed_int = value.parse::<isize>();
    match parsed_int {
        Ok(n) => Ok(SteelVal::IntV(n)),
        Err(_) => {
            stop!(TypeMismatch => "could not convert number to integer");
        }
    }
}

#[function(name = "string->list")]
pub fn string_to_list(value: SteelString) -> SteelVal {
    value
        .chars()
        .map(SteelVal::CharV)
        .collect::<List<_>>()
        .into()
}

#[function(name = "string->upper")]
pub fn string_to_upper(value: SteelString) -> String {
    value.to_uppercase()
}

#[function(name = "string->lower")]
pub fn string_to_lower(value: SteelString) -> String {
    value.to_lowercase()
}

#[function(name = "trim")]
pub fn trim(value: SteelString) -> String {
    value.trim().into()
}

#[function(name = "trim-start")]
pub fn trim_start(value: SteelString) -> String {
    value.trim_start().into()
}

#[function(name = "trim-end")]
pub fn trim_end(value: SteelString) -> String {
    value.trim_end().into()
}

#[function(name = "split-whitespace")]
pub fn split_whitespace(value: SteelString) -> SteelVal {
    let split: List<SteelVal> = value
        .split_whitespace()
        .map(|x| SteelVal::StringV(x.into()))
        .collect();
    split.into()
}

#[function(name = "starts-with?")]
pub fn starts_with(value: SteelString, prefix: SteelString) -> bool {
    value.starts_with(prefix.as_str())
}

#[function(name = "ends-with?")]
pub fn ends_with(value: SteelString, suffix: SteelString) -> bool {
    value.ends_with(suffix.as_str())
}

#[function(name = "string-length")]
pub fn string_length(value: SteelString) -> usize {
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

    // pub fn string_to_symbol() -> SteelVal {
    //     SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
    //         if args.len() == 1 {
    //             if let SteelVal::StringV(s) = &args[0] {
    //                 Ok(SteelVal::SymbolV(s.clone()))
    //             } else {
    //                 stop!(TypeMismatch => "string->symbol expected a string")
    //             }
    //         } else {
    //             stop!(ArityMismatch => "string->symbol takes one argument")
    //         }
    //     })
    // }

    // pub fn int_to_string() -> SteelVal {
    //     SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
    //         if args.len() == 1 {
    //             if let SteelVal::IntV(s) = &args[0] {
    //                 Ok(SteelVal::StringV(format!("{s}").into()))
    //             } else {
    //                 stop!(TypeMismatch => "string->int expected a string")
    //             }
    //         } else {
    //             stop!(ArityMismatch => "string->int takes one argument")
    //         }
    //     })
    // }

    // pub fn string_to_int() -> SteelVal {
    //     SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
    //         if args.len() == 1 {
    //             if let SteelVal::StringV(s) = &args[0] {
    //                 let parsed_int = s.parse::<isize>();
    //                 match parsed_int {
    //                     Ok(n) => Ok(SteelVal::IntV(n)),
    //                     Err(_) => {
    //                         stop!(TypeMismatch => "could not convert number to integer");
    //                     }
    //                 }
    //             } else {
    //                 stop!(TypeMismatch => "string->int expected a string")
    //             }
    //         } else {
    //             stop!(ArityMismatch => "string->int takes one argument")
    //         }
    //     })
    // }

    // pub fn string_to_list() -> SteelVal {
    //     SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
    //         if args.len() == 1 {
    //             if let SteelVal::StringV(s) = &args[0] {
    //                 Ok(s.chars().map(SteelVal::CharV).collect::<List<_>>().into())
    //             } else {
    //                 stop!(TypeMismatch => "string->list expected a string")
    //             }
    //         } else {
    //             stop!(ArityMismatch => "string->list takes one argument")
    //         }
    //     })
    // }

    // pub fn string_to_upper() -> SteelVal {
    //     SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
    //         if args.len() == 1 {
    //             if let SteelVal::StringV(s) = &args[0] {
    //                 let upper = s.to_uppercase();
    //                 ok_string!(upper)
    //             // Ok(Gc::new(SteelVal::StringV(upper)))
    //             } else {
    //                 stop!(TypeMismatch => "string-upcase expected a string")
    //             }
    //         } else {
    //             stop!(ArityMismatch => "string-upcase takes one argument")
    //         }
    //     })
    // }

    // pub fn string_to_lower() -> SteelVal {
    //     SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
    //         if args.len() == 1 {
    //             if let SteelVal::StringV(s) = &args[0] {
    //                 let lower = s.to_lowercase();
    //                 ok_string!(lower)
    //             // Ok(Gc::new(SteelVal::StringV(lower)))
    //             } else {
    //                 stop!(TypeMismatch => "string-lowercase expected a string")
    //             }
    //         } else {
    //             stop!(ArityMismatch => "string-lowercase takes one argument")
    //         }
    //     })
    // }

    // pub fn trim() -> SteelVal {
    //     SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
    //         if args.len() == 1 {
    //             if let SteelVal::StringV(s) = &args[0] {
    //                 let trimmed = s.trim();
    //                 ok_string!(trimmed.to_string())
    //             // Ok(Gc::new(SteelVal::StringV(trimmed.to_string())))
    //             } else {
    //                 stop!(TypeMismatch => "trim expected a string")
    //             }
    //         } else {
    //             stop!(ArityMismatch => "trim takes one argument")
    //         }
    //     })
    // }

    // pub fn trim_start() -> SteelVal {
    //     SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
    //         if args.len() == 1 {
    //             if let SteelVal::StringV(s) = &args[0] {
    //                 let trimmed = s.trim_start();
    //                 ok_string!(trimmed.to_string())
    //             // Ok(Gc::new(SteelVal::StringV(trimmed.to_string())))
    //             } else {
    //                 stop!(TypeMismatch => "trim-start expected a string")
    //             }
    //         } else {
    //             stop!(ArityMismatch => "trim-start takes one argument")
    //         }
    //     })
    // }

    // pub fn trim_end() -> SteelVal {
    //     SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
    //         if args.len() == 1 {
    //             if let SteelVal::StringV(s) = &args[0] {
    //                 let trimmed = s.trim_end();
    //                 ok_string!(trimmed.to_string())
    //             // Ok(Gc::new(SteelVal::StringV(trimmed.to_string())))
    //             } else {
    //                 stop!(TypeMismatch => "trim-end expected a string")
    //             }
    //         } else {
    //             stop!(ArityMismatch => "trim-end takes one argument")
    //         }
    //     })
    // }

    // pub fn split_whitespace() -> SteelVal {
    //     SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
    //         if args.len() == 1 {
    //             if let SteelVal::StringV(s) = &args[0] {
    //                 let split: List<SteelVal> = s
    //                     .split_whitespace()
    //                     .map(|x| SteelVal::StringV(x.into()))
    //                     .collect();
    //                 Ok(split.into())
    //             } else {
    //                 stop!(TypeMismatch => "split-whitespace expected a string")
    //             }
    //         } else {
    //             stop!(ArityMismatch => "split-whitespace takes one argument")
    //         }
    //     })
    // }

    // pub fn string_length() -> SteelVal {
    //     SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
    //         if args.len() == 1 {
    //             if let SteelVal::StringV(s) = &args[0] {
    //                 Ok(SteelVal::IntV(s.len() as isize))
    //             } else {
    //                 stop!(TypeMismatch => "string-length expected a string")
    //             }
    //         } else {
    //             stop!(ArityMismatch => "string-length takes one argument")
    //         }
    //     })
    // }

    // pub fn starts_with() -> SteelVal {
    //     SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
    //         if args.len() == 2 {
    //             match (&args[0], &args[1]) {
    //                 (SteelVal::StringV(s), SteelVal::StringV(p)) => {
    //                     Ok(SteelVal::BoolV(s.starts_with(p.as_ref())))
    //                 }
    //                 _ => {
    //                     stop!(ArityMismatch => "starts-with? takes two arguments")
    //                 }
    //             }
    //         } else {
    //             stop!(ArityMismatch => "starts-with? takes two arguments")
    //         }
    //     })
    // }

    // pub fn ends_with() -> SteelVal {
    //     SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
    //         if args.len() == 2 {
    //             match (&args[0], &args[1]) {
    //                 (SteelVal::StringV(s), SteelVal::StringV(p)) => {
    //                     Ok(SteelVal::BoolV(s.ends_with(p.as_ref())))
    //                 }
    //                 _ => {
    //                     stop!(ArityMismatch => "ends-with? takes two arguments")
    //                 }
    //             }
    //         } else {
    //             stop!(ArityMismatch => "ends-with? takes two arguments")
    //         }
    //     })
    // }
}

#[cfg(test)]
mod string_operation_tests {
    use super::*;
    // use crate::gc::Gc;
    use crate::rerrs::ErrorKind;
    // use crate::rvals::ConsCell;
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
