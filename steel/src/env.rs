// use crate::rvals::Result;
// #[macro_use]
use crate::{
    gc::Gc,
    primitives::{
        ListOperations, NumOperations, StringOperations, SymbolOperations, VectorOperations,
    },
    rerrs::{ErrorKind, SteelErr},
    rvals::{Result, SteelVal},
    throw,
};

// use std::{
//     cell::RefCell,
//     collections::BTreeMap,
//     rc::Weak,
//     sync::atomic::{AtomicUsize, Ordering},
// };

// TODO
pub const fn _new_void() -> SteelVal {
    // VOID.with(Gc::clone)
    SteelVal::Void
}

// TODO
pub const fn _new_true() -> SteelVal {
    // TRUE.with(Gc::clone)
    SteelVal::BoolV(true)
}

// TODO
pub const fn _new_false() -> SteelVal {
    SteelVal::BoolV(false)
    // FALSE.with(Gc::clone)
}

// pub static ENV_ID: AtomicUsize = AtomicUsize::new(0);

#[macro_use]
macro_rules! ensure_tonicity {
    ($check_fn:expr) => {{
        |args: &[SteelVal]| -> Result<SteelVal> {
            let mut args_iter = args.iter();
            let first = args_iter.next().ok_or(SteelErr::new(
                ErrorKind::ArityMismatch,
                "expected at least one argument".to_string(),
            ))?;
            fn f<'a>(prev: &SteelVal, mut xs: impl Iterator<Item = &'a SteelVal>) -> bool {
                match xs.next() {
                    Some(x) => $check_fn(prev, x) && f(x, xs),
                    None => true,
                }
            }
            let res = f(&first, args_iter);
            Ok(SteelVal::BoolV(res))
        }
    }};
}

// #[macro_use]
// macro_rules! ensure_tonicity_pointer_equality {
//     ($check_fn:expr) => {{
//         |args: &[SteelVal]| -> Result<SteelVal> {
//             // let args_iter: Vec<Rc<SteelVal>> = args.into_iter();
//             let mut args_iter = args.into_iter();
//             let first = args_iter.next().ok_or(SteelErr::ArityMismatch(
//                 "expected at least one argument".to_string(),
//                 None,
//             ))?;
//             fn f<'a>(prev: &SteelVal, mut xs: impl Iterator<Item = &'a SteelVal>) -> bool {
//                 match xs.next() {
//                     Some(x) => $check_fn(prev, x) && f(x, xs),
//                     None => true,
//                 }
//             };
//             let res = f(&first, args_iter);
//             Ok(SteelVal::BoolV(res))
//         }
//     }};
// }

#[macro_use]
macro_rules! gen_pred {
    ($variant:ident) => {{
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if let Some(first) = args.first() {
                if let SteelVal::$variant(..) = first {
                    return Ok(SteelVal::BoolV(true));
                }
            }
            Ok(SteelVal::BoolV(false))
        })
    }};

    ($variant1:ident, $variant2:ident) => {{
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if let Some(first) = args.first() {
                match first {
                    SteelVal::$variant1(..) | SteelVal::$variant2(..) => {
                        return Ok(SteelVal::BoolV(true));
                    }
                    _ => {}
                }
            }
            Ok(SteelVal::BoolV(false))
        })
    }};
}

#[derive(Debug)]
pub struct Env {
    pub(crate) bindings_vec: Vec<SteelVal>,
}

pub trait MacroEnv {
    fn validate_identifier(&self, name: &str) -> bool;
}

impl Env {
    /// Make a new `Env` from
    /// another parent `Env`.
    pub fn new() -> Self {
        Env {
            bindings_vec: Vec::new(),
        }
    }

    pub fn extract(&self, idx: usize) -> Option<SteelVal> {
        self.bindings_vec.get(idx).cloned()
    }

    /// top level global env has no parent
    pub fn root() -> Self {
        Env {
            bindings_vec: Vec::new(),
        }
    }

    /// Search starting from the current environment
    /// for `idx`, looking through the parent chain in order.
    ///
    /// if found, return that value
    ///
    /// Otherwise, error with `FreeIdentifier`
    // #[inline]
    pub fn repl_lookup_idx(&self, idx: usize) -> Result<SteelVal> {
        Ok(self.bindings_vec[idx].clone())
    }

    #[inline]
    pub fn repl_define_idx(&mut self, idx: usize, val: SteelVal) {
        // self.bindings_map.insert(idx, val);
        // unimplemented!()
        if idx < self.bindings_vec.len() {
            self.bindings_vec[idx] = val;
        } else {
            // println!("Index: {}, length: {}", idx, self.bindings_vec.len());
            self.bindings_vec.push(val);
            assert_eq!(self.bindings_vec.len() - 1, idx);
        }
    }

    pub fn repl_set_idx(&mut self, idx: usize, val: SteelVal) -> Result<SteelVal> {
        let output = self.bindings_vec[idx].clone();
        self.bindings_vec[idx] = val;
        Ok(output)
    }

    #[inline]
    pub fn add_root_value(&mut self, idx: usize, val: SteelVal) {
        // self.bindings_map.insert(idx, val);
        self.repl_define_idx(idx, val);
    }

    // bit of a quality of life hack
    pub fn constant_env_to_hashmap() -> SteelVal {
        let mut hm = crate::im_rc::HashMap::new();
        for (name, val) in Env::constant_core() {
            hm.insert(SteelVal::SymbolV(name.into()), val);
        }
        SteelVal::HashMapV(Gc::new(hm))
    }

    // TODO actually determine which functions are const
    pub fn constant_core() -> Vec<(&'static str, SteelVal)> {
        vec![
            ("+", NumOperations::adder()),
            ("i+", NumOperations::integer_add()),
            ("f+", NumOperations::float_add()),
            ("*", NumOperations::multiply()),
            ("/", NumOperations::divide()),
            ("-", NumOperations::subtract()),
            // ("list", ListOperations::list()),
            ("car", ListOperations::car()),
            ("cdr", ListOperations::cdr()),
            ("first", ListOperations::car()),
            ("rest", ListOperations::cdr()),
            // ("cons", ListOperations::cons()),
            // ("append", ListOperations::append()),
            // ("push-back", ListOperations::push_back()),
            ("range", ListOperations::range()),
            // ("list->vector", ListOperations::list_to_vec()),
            ("null?", VectorOperations::list_vec_null()),
            ("int?", gen_pred!(IntV)),
            ("float?", gen_pred!(NumV)),
            ("number?", gen_pred!(NumV, IntV)),
            ("string?", gen_pred!(StringV)),
            ("symbol?", gen_pred!(SymbolV)),
            ("vector?", gen_pred!(VectorV)),
            ("list?", gen_pred!(Pair)),
            ("integer?", gen_pred!(IntV)),
            ("boolean?", gen_pred!(BoolV)),
            ("function?", gen_pred!(Closure, FuncV)),
            ("=", SteelVal::FuncV(ensure_tonicity!(|a, b| a == b))),
            ("equal?", SteelVal::FuncV(ensure_tonicity!(|a, b| a == b))),
            // (
            //     "eq?",
            //     SteelVal::FuncV(ensure_tonicity_pointer_equality!(|a, b| Gc::ptr_eq(a, b))),
            // ),
            (">", SteelVal::FuncV(ensure_tonicity!(|a, b| a > b))),
            (">=", SteelVal::FuncV(ensure_tonicity!(|a, b| a >= b))),
            ("<", SteelVal::FuncV(ensure_tonicity!(|a, b| a < b))),
            ("<=", SteelVal::FuncV(ensure_tonicity!(|a, b| a <= b))),
            ("string-append", StringOperations::string_append()),
            ("string->list", StringOperations::string_to_list()),
            ("string-upcase", StringOperations::string_to_upper()),
            ("string-lowercase", StringOperations::string_to_lower()),
            ("trim", StringOperations::trim()),
            ("trim-start", StringOperations::trim_start()),
            ("trim-end", StringOperations::trim_end()),
            ("split-whitespace", StringOperations::split_whitespace()),
            ("void", SteelVal::Void),
            ("list->string", ListOperations::list_to_string()),
            ("concat-symbols", SymbolOperations::concat_symbols()),
            ("symbol->string", SymbolOperations::symbol_to_string()),
            ("string->int", StringOperations::string_to_int()),
            ("even?", NumOperations::even()),
            ("odd?", NumOperations::odd()),
        ]
    }
}

#[cfg(test)]
mod env_tests {
    use super::*;
    use crate::stop;
    fn _unwrap_single_float(exp: &SteelVal) -> Result<f64> {
        match exp {
            SteelVal::NumV(num) => Ok(*num),
            _ => stop!(TypeMismatch => "expected a number"),
        }
    }
}
