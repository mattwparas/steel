// Generates a macro that expands to code snippet to do arity checks
// example usage:
// arity_check!(length, args, 1);
// Expands to:
// if args.len() != 1 {
//     stop!(ArityMismatch => format!("length expected only one argument, found {}", args.len()))
// }
// assert!(args.len() == 1);

// Only compile these helper macros when either the standard library is available
// or when higher-level modules that depend on them are enabled. This avoids
// unused-macro warnings in minimal `no_std` builds.
#[cfg(any(
    feature = "std",
    feature = "no_std_primitives",
    feature = "no_std_values"
))]
#[cfg(any(
    feature = "std",
    feature = "no_std_primitives",
    feature = "no_std_values"
))]
use alloc::format;

#[cfg(any(
    feature = "std",
    feature = "no_std_primitives",
    feature = "no_std_values"
))]
pub(crate) fn arity_mismatch_message(name: &'static str, expected: usize, found: usize) -> String {
    match expected {
        1 => format!("{name} expected only one argument, found {found}"),
        other => format!("{name} expected {other} arguments, found {found}"),
    }
}

#[cfg(any(
    feature = "std",
    feature = "no_std_primitives",
    feature = "no_std_values"
))]
macro_rules! arity_check_generator {
    ($($arity:tt),*) => {
        macro_rules! arity_check {
            ($name:tt, $args:expr, 1) => {
                if $args.len() != 1 {
                    stop!(ArityMismatch => crate::core::utils::arity_mismatch_message(stringify!($name), 1, $args.len()))
                }
                assert!($args.len() == 1);
            };

            $ (
                ($name:tt, $args:expr, $arity) => {
                    if $args.len() != $arity {
                        stop!(ArityMismatch => crate::core::utils::arity_mismatch_message(stringify!($name), $arity, $args.len()))
                    }
                    assert!($args.len() == $arity);
                };
            ) *
        }
    }
}

#[cfg(any(
    feature = "std",
    feature = "no_std_primitives",
    feature = "no_std_values"
))]
arity_check_generator!(0, 1, 2, 3, 4, 5, 6, 7, 8);

#[cfg(any(
    feature = "std",
    feature = "no_std_primitives",
    feature = "no_std_values"
))]
pub(crate) use arity_check;

// Declares a const for a function that takes an immutable slice to the arguments
// e.g.
//      declare_const_ref_functions! { LENGTH => length }
// expands into
//      const LENGTH: SteelVal = SteelVal::FuncV(length)
#[cfg(any(
    feature = "std",
    feature = "no_std_primitives",
    feature = "no_std_values"
))]
macro_rules! declare_const_ref_functions {
    ($($name:tt => $func_name:tt),* $(,)? ) => {
        $ (
            pub(crate) const $name: SteelVal = SteelVal::FuncV($func_name);
        ) *
    }
}

#[cfg(any(
    feature = "std",
    feature = "no_std_primitives",
    feature = "no_std_values"
))]
pub(crate) use declare_const_ref_functions;

// Declares a const for a function that takes an immutable slice to the arguments
// e.g.
//      declare_const_mut_ref_functions! { CONS => cons }
// expands into
//      const LENGTH: SteelVal = SteelVal::MutFunc(length)
#[cfg(any(
    feature = "std",
    feature = "no_std_primitives",
    feature = "no_std_values"
))]
macro_rules! declare_const_mut_ref_functions {
    ($($name:tt => $func_name:tt),* $(,)? ) => {
        $ (
            pub(crate) const $name: SteelVal = SteelVal::MutFunc($func_name);
        ) *
    }
}

#[cfg(any(
    feature = "std",
    feature = "no_std_primitives",
    feature = "no_std_values"
))]
pub(crate) use declare_const_mut_ref_functions;

// pub(crate) trait Boxed {
//     fn boxed(self) -> Box<Self>;
//     fn refcounted(self) -> Rc<Self>;
//     fn rc_refcell(self) -> Rc<RefCell<Self>>;
// }

// impl<T> Boxed for T {
//     #[inline(always)]
//     fn boxed(self) -> Box<T> {
//         Box::new(self)
//     }

//     #[inline(always)]
//     fn refcounted(self) -> Rc<T> {
//         Rc::new(self)
//     }

//     #[inline(always)]
//     fn rc_refcell(self) -> Rc<RefCell<T>> {
//         Rc::new(RefCell::new(self))
//     }
// }
