// use crate::env::{FALSE, TRUE};
use crate::rerrs::SteelErr;
// use crate::rvals::SteelVal::*;
use crate::gc::Gc;
use crate::rvals::{Result, SteelVal};
use crate::stop;

use crate::primitives::lists::ListOperations;

macro_rules! ok_string {
    ($string:expr) => {
        Ok(Gc::new(SteelVal::StringV($string)))
    };
}

// macro_rules! extract_types_from_vec {

// }

// make this macro to generate functions with some sanity
// ideally we want something like this
/*

new_function_fixed_arity!{
    "string-append",
    arity => 2,
    (arg1: String, arg2: String, arg3: String),

}

(Structs => {$($type:ty),* $(,)?} Functions => {$($binding:expr => $func:ident),* $(,)?}) => {
        {
            let mut interpreter = SteelInterpreter::new();
            $ (
                interpreter.insert_bindings(<$type>::generate_bindings());
            ) *

            $ (
                interpreter.insert_binding($binding, SteelVal::FuncV($func));
            ) *

            interpreter
        }
    };

*/

// min! will calculate the minimum of any number of arguments
// macro_rules! min {
//     // base case
//     ($x:expr) => ($x);
//     // `$x` followed by at least one `$y,`
//     ($x:expr, $($y:expr),+) => (
//         // call min! on the tail `$y`
//         std::cmp::min($x, min!($($y),+))
//     )
// }

// macro_rules! result_tuple {
//     (($($arg:tt),+)) => {
//         (
//             $ (
//                 $arg?
//             ), *
//         )
//     }
// }

// macro_rules! type_and_arity_check {
//     ($name:expr, $arity:expr, $args:expr, ($($type:ident => $inner_type:ty), * $(,)?)) => {{

//         if $args.len() != $arity {
//             stop!(TypeMismatch => format!("{} expected takes two arguments, found {}", $args.len(), $arity))
//         }
//         let args_iter = $args.into_iter();
//         let result = (
//             $ (
//                 {
//                     let val: $inner_type = if let Some(SteelVal::$type(inner_value)) = args_iter.next().map(|x| x.as_ref()) {
//                         inner_value
//                     } else {
//                         stop!(TypeMismatch => format!("{} type mismatch", $name))
//                     };
//                     val
//                 }

//             ) ,*
//         );

//         Ok(result)
//     }}
// }

pub struct StringOperations {}
impl StringOperations {
    pub fn string_append() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() == 2 {
                if let (SteelVal::StringV(l), SteelVal::StringV(r)) =
                    (&args[0].as_ref(), &args[1].as_ref())
                {
                    let new_string = l.clone() + &r.clone();
                    ok_string!(new_string)
                // Ok(Gc::new(SteelVal::StringV(new_string)))
                } else {
                    stop!(TypeMismatch => "string-append expected two strings")
                }
            } else {
                stop!(ArityMismatch => "string-append takes two arguments")
            }
        })
    }

    // pub fn test_test() -> SteelVal {
    //     embedded_function! {
    //         fn _test_function(test_test_test: &str, arg1: StringV, arg2: StringV) {
    //             ok_string!(arg1.to_string())
    //         }
    //     }
    // }

    // pub fn test_test() -> SteelVal {
    //     SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
    //         let (arg1, arg2, arg3): (&String, &String, &String) = type_and_arity_check!(
    //             "test-test",
    //             3,
    //             args,
    //             (StringV => &String, StringV => &String, StringV => &String)
    //         )?;

    //         ok_string!(arg1.to_string())
    //     })
    // }

    pub fn string_to_int() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() == 1 {
                if let SteelVal::StringV(s) = &args[0].as_ref() {
                    let parsed_int = s.parse::<isize>();
                    match parsed_int {
                        Ok(n) => Ok(Gc::new(SteelVal::IntV(n))),
                        Err(_) => {
                            stop!(TypeMismatch => "could not convert number to integer");
                        }
                    }
                } else {
                    stop!(TypeMismatch => "string->int expected a string")
                }
            } else {
                stop!(ArityMismatch => "string->int takes one argument")
            }
        })
    }

    pub fn string_to_list() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() == 1 {
                if let SteelVal::StringV(s) = &args[0].as_ref() {
                    let chars: Vec<Gc<SteelVal>> =
                        s.chars().map(|x| Gc::new(SteelVal::CharV(x))).collect();
                    ListOperations::built_in_list_func()(&chars)
                } else {
                    stop!(TypeMismatch => "string->list expected a string")
                }
            } else {
                stop!(ArityMismatch => "string->list takes one argument")
            }
        })
    }

    pub fn string_to_upper() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() == 1 {
                if let SteelVal::StringV(s) = &args[0].as_ref() {
                    let upper = s.to_uppercase();
                    ok_string!(upper)
                // Ok(Gc::new(SteelVal::StringV(upper)))
                } else {
                    stop!(TypeMismatch => "string-upcase expected a string")
                }
            } else {
                stop!(ArityMismatch => "string-upcase takes one argument")
            }
        })
    }

    pub fn string_to_lower() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() == 1 {
                if let SteelVal::StringV(s) = &args[0].as_ref() {
                    let lower = s.to_lowercase();
                    ok_string!(lower.to_string())
                // Ok(Gc::new(SteelVal::StringV(lower)))
                } else {
                    stop!(TypeMismatch => "string-lowercase expected a string")
                }
            } else {
                stop!(ArityMismatch => "string-lowercase takes one argument")
            }
        })
    }

    pub fn trim() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() == 1 {
                if let SteelVal::StringV(s) = &args[0].as_ref() {
                    let trimmed = s.trim();
                    ok_string!(trimmed.to_string())
                // Ok(Gc::new(SteelVal::StringV(trimmed.to_string())))
                } else {
                    stop!(TypeMismatch => "trim expected a string")
                }
            } else {
                stop!(ArityMismatch => "trim takes one argument")
            }
        })
    }

    pub fn trim_start() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() == 1 {
                if let SteelVal::StringV(s) = &args[0].as_ref() {
                    let trimmed = s.trim_start();
                    ok_string!(trimmed.to_string())
                // Ok(Gc::new(SteelVal::StringV(trimmed.to_string())))
                } else {
                    stop!(TypeMismatch => "trim-start expected a string")
                }
            } else {
                stop!(ArityMismatch => "trim-start takes one argument")
            }
        })
    }

    pub fn trim_end() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() == 1 {
                if let SteelVal::StringV(s) = &args[0].as_ref() {
                    let trimmed = s.trim_end();
                    ok_string!(trimmed.to_string())
                // Ok(Gc::new(SteelVal::StringV(trimmed.to_string())))
                } else {
                    stop!(TypeMismatch => "trim-end expected a string")
                }
            } else {
                stop!(ArityMismatch => "trim-end takes one argument")
            }
        })
    }

    pub fn split_whitespace() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() == 1 {
                if let SteelVal::StringV(s) = &args[0].as_ref() {
                    let split: Vec<Gc<SteelVal>> = s
                        .split_whitespace()
                        .map(|x| Gc::new(SteelVal::StringV(x.to_string())))
                        .collect();
                    ListOperations::built_in_list_func()(&split)
                } else {
                    stop!(TypeMismatch => "split-whitespace expected a string")
                }
            } else {
                stop!(ArityMismatch => "split-whitespace takes one argument")
            }
        })
    }
}

#[cfg(test)]
mod string_operation_tests {
    use super::*;
    use crate::throw;
    use im_rc::Vector;

    // TODO combine these 3 macros into one
    macro_rules! apply_tests_arity_too_many {
        ($(($name:expr, $symbol:ident, $func:expr)),* $(,)?) => {
            $(
                #[test]
                pub fn $symbol() {
                    let args = vec![
                        SteelVal::StringV("FOO".to_string()),
                        SteelVal::StringV("BAR".to_string()),
                    ];
                    let res = apply_function($func.clone(), args);
                    let expected = SteelErr::ArityMismatch(format!("{} takes one argument", $name), None);
                    assert_eq!(res.unwrap_err(), expected);
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
                    let res = apply_function($func.clone(), args);
                    let expected = SteelErr::ArityMismatch(format!("{} takes one argument", $name), None);
                    assert_eq!(res.unwrap_err(), expected);
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
                    let res = apply_function($func.clone(), args);
                    let expected = SteelErr::TypeMismatch(format!("{} expected a string", $name), None);
                    assert_eq!(res.unwrap_err(), expected);
                }
            )*
        };
    }

    apply_tests_arity_too_many! {
        ("string-upcase", string_upper_arity_too_many, StringOperations::string_to_upper()),
        ("string-lowercase", string_lower_arity_too_many, StringOperations::string_to_lower()),
        ("trim", trim_arity_too_many, StringOperations::trim()),
        ("trim-start", trim_start_arity_too_many, StringOperations::trim_start()),
        ("trim-end", trim_end_arity_too_many, StringOperations::trim_end()),
        ("string->list", string_to_list_arity_too_many, StringOperations::string_to_list()),
        ("split-whitespace", split_whitespace_arity_too_many, StringOperations::split_whitespace()),
    }

    apply_tests_arity_too_few! {
        ("string-upcase", string_upper_arity_too_few, StringOperations::string_to_upper()),
        ("string-lowercase", string_lower_arity_too_few, StringOperations::string_to_lower()),
        ("trim", trim_arity_too_few, StringOperations::trim()),
        ("trim-start", trim_start_arity_too_few, StringOperations::trim_start()),
        ("trim-end", trim_end_arity_too_few, StringOperations::trim_end()),
        ("string->list", string_to_list_arity_too_few, StringOperations::string_to_list()),
        ("split-whitespace", split_whitespace_arity_too_few, StringOperations::split_whitespace())
    }

    apply_tests_bad_arg! {
        ("string-upcase", string_upper_arity_takes_string, StringOperations::string_to_upper()),
        ("string-lowercase", string_lower_arity_takes_string, StringOperations::string_to_lower()),
        ("trim", trim_arity_takes_string, StringOperations::trim()),
        ("trim-start", trim_start_arity_takes_string, StringOperations::trim_start()),
        ("trim-end", trim_end_arity_takes_string, StringOperations::trim_end()),
        ("string->list", string_to_list_takes_string, StringOperations::string_to_list()),
        ("split-whitespace", split_whitespace_arity_takes_string, StringOperations::split_whitespace())
    }

    fn apply_function(func: SteelVal, args: Vec<SteelVal>) -> Result<Gc<SteelVal>> {
        let args: Vec<Gc<SteelVal>> = args.into_iter().map(|x| Gc::new(x)).collect();
        func.func_or_else(throw!(BadSyntax => "string tests"))
            .unwrap()(&args)
    }

    #[test]
    fn string_append_test_normal() {
        let args = vec![
            SteelVal::StringV("foo".to_string()),
            SteelVal::StringV("bar".to_string()),
        ];
        let res = apply_function(StringOperations::string_append(), args);
        let expected = Gc::new(SteelVal::StringV("foobar".to_string()));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn string_append_test_arity_mismatch_too_few() {
        let args = vec![SteelVal::StringV("foo".to_string())];
        let res = apply_function(StringOperations::string_append(), args);
        let expected =
            SteelErr::ArityMismatch("string-append takes two arguments".to_string(), None);
        assert_eq!(res.unwrap_err(), expected);
    }

    #[test]
    fn string_append_test_arity_mismatch_too_many() {
        let args = vec![
            SteelVal::StringV("foo".to_string()),
            SteelVal::StringV("bar".to_string()),
            SteelVal::StringV("baz".to_string()),
        ];
        let res = apply_function(StringOperations::string_append(), args);
        let expected =
            SteelErr::ArityMismatch("string-append takes two arguments".to_string(), None);
        assert_eq!(res.unwrap_err(), expected);
    }

    #[test]
    fn string_append_test_takes_string() {
        let args = vec![SteelVal::CharV('a'), SteelVal::CharV('b')];
        let res = apply_function(StringOperations::string_append(), args);
        let expected =
            SteelErr::TypeMismatch("string-append expected two strings".to_string(), None);
        assert_eq!(res.unwrap_err(), expected);
    }

    #[test]
    fn string_to_upper_normal() {
        let args = vec![SteelVal::StringV("foobarbaz".to_string())];
        let res = apply_function(StringOperations::string_to_upper(), args);
        let expected = Gc::new(SteelVal::StringV("FOOBARBAZ".to_string()));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn string_to_upper_spaces() {
        let args = vec![SteelVal::StringV("foo bar baz qux".to_string())];
        let res = apply_function(StringOperations::string_to_upper(), args);
        let expected = Gc::new(SteelVal::StringV("FOO BAR BAZ QUX".to_string()));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn string_to_lower_normal() {
        let args = vec![SteelVal::StringV("FOOBARBAZ".to_string())];
        let res = apply_function(StringOperations::string_to_lower(), args);
        let expected = Gc::new(SteelVal::StringV("foobarbaz".to_string()));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn string_to_lower_spaces() {
        let args = vec![SteelVal::StringV("FOO BAR BAZ QUX".to_string())];
        let res = apply_function(StringOperations::string_to_lower(), args);
        let expected = Gc::new(SteelVal::StringV("foo bar baz qux".to_string()));
        assert_eq!(res.unwrap(), expected);
    }

    // TODO investigate this, assert_eq! fails without converting to string
    #[test]
    fn string_to_list_normal() {
        let args = vec![SteelVal::StringV("foo".to_string())];
        let res = apply_function(StringOperations::string_to_list(), args);
        let expected = Gc::new(SteelVal::Pair(
            Gc::new(SteelVal::CharV('f')),
            Some(Gc::new(SteelVal::Pair(
                Gc::new(SteelVal::CharV('o')),
                Some(Gc::new(SteelVal::CharV('o'))),
            ))),
        ));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn string_to_list_empty() {
        let args = vec![SteelVal::StringV("".to_string())];
        let res = apply_function(StringOperations::string_to_list(), args);
        let expected = Gc::new(SteelVal::VectorV(Vector::new()));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn trim_normal_no_changes() {
        let args = vec![SteelVal::StringV("foo".to_string())];
        let res = apply_function(StringOperations::trim(), args);
        let expected = Gc::new(SteelVal::StringV("foo".to_string()));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn trim_normal_trims_both_sides() {
        let args = vec![SteelVal::StringV("      foo  ".to_string())];
        let res = apply_function(StringOperations::trim(), args);
        let expected = Gc::new(SteelVal::StringV("foo".to_string()));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn trim_start_no_changes() {
        let args = vec![SteelVal::StringV("foo".to_string())];
        let res = apply_function(StringOperations::trim_start(), args);
        let expected = Gc::new(SteelVal::StringV("foo".to_string()));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn trim_end_no_changes() {
        let args = vec![SteelVal::StringV("foo".to_string())];
        let res = apply_function(StringOperations::trim_end(), args);
        let expected = Gc::new(SteelVal::StringV("foo".to_string()));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn trim_start_normal_trims_left_side() {
        let args = vec![SteelVal::StringV("      foo  ".to_string())];
        let res = apply_function(StringOperations::trim_start(), args);
        let expected = Gc::new(SteelVal::StringV("foo  ".to_string()));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn trim_end_normal_trims_right_side() {
        let args = vec![SteelVal::StringV("      foo  ".to_string())];
        let res = apply_function(StringOperations::trim_end(), args);
        let expected = Gc::new(SteelVal::StringV("      foo".to_string()));
        assert_eq!(res.unwrap(), expected);
    }

    // TODO investigate this one
    #[test]
    fn split_whitespace_no_whitespace() {
        let args = vec![SteelVal::StringV("foo".to_string())];
        let res = apply_function(StringOperations::split_whitespace(), args);
        let expected = Gc::new(SteelVal::Pair(
            Gc::new(SteelVal::StringV("foo".to_string())),
            None,
        ));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn split_whitespace_some_whitespace() {
        let args = vec![SteelVal::StringV("foo bar baz".to_string())];
        let res = apply_function(StringOperations::split_whitespace(), args);
        let expected = Gc::new(SteelVal::Pair(
            Gc::new(SteelVal::StringV("foo".to_string())),
            Some(Gc::new(SteelVal::Pair(
                Gc::new(SteelVal::StringV("bar".to_string())),
                Some(Gc::new(SteelVal::StringV("baz".to_string()))),
            ))),
        ));
        assert_eq!(res.unwrap(), expected);
    }
}
