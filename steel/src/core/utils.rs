// Generates a macro that expands to code snippet to do arity checks
// example usage:
// arity_check!(length, args, 1);
// Expands to:
// if args.len() != 1 {
//     stop!(ArityMismatch => format!("length expected only one argument, found {}", args.len()))
// }
// assert!(args.len() == 1);

#[macro_use]
macro_rules! arity_check_generator {
    ($($arity:tt),*) => {
        macro_rules! arity_check {
            ($name:tt, $args:expr, 1) => {
                if $args.len() != 1 {
                    stop!(ArityMismatch => format!(stringify!($name expected only one argument, found {}), $args.len()))
                }
                assert!($args.len() == 1);
            };

            $ (
                ($name:tt, $args:expr, $arity) => {
                    if $args.len() != $arity {
                        stop!(ArityMismatch => format!(stringify!($name expected two arguments, found {}), $args.len()))
                    }
                    assert!($args.len() == $arity);
                };
            ) *
        }
    }
}

arity_check_generator!(0, 1, 2, 3, 4, 5, 6, 7, 8);

pub(crate) use arity_check;

// Declares a const for a function that takes an immutable slice to the arguments
// e.g.
//      declare_const_ref_functions! { LENGTH => length }
// expands into
//      const LENGTH: SteelVal = SteelVal::FuncV(length)
#[macro_use]
macro_rules! declare_const_ref_functions {
    ($($name:tt => $func_name:tt),* $(,)? ) => {
        $ (
            pub(crate) const $name: SteelVal = SteelVal::FuncV($func_name);
        ) *
    }
}

pub(crate) use declare_const_ref_functions;

// Declares a const for a function that takes an immutable slice to the arguments
// e.g.
//      declare_const_mut_ref_functions! { CONS => cons }
// expands into
//      const LENGTH: SteelVal = SteelVal::MutFunc(length)
#[macro_use]
macro_rules! declare_const_mut_ref_functions {
    ($($name:tt => $func_name:tt),* $(,)? ) => {
        $ (
            pub(crate) const $name: SteelVal = SteelVal::MutFunc($func_name);
        ) *
    }
}

pub(crate) use declare_const_mut_ref_functions;
