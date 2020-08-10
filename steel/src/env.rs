// use crate::rvals::Result;
// #[macro_use]
use crate::primitives::ControlOperations;
use crate::primitives::FsFunctions;
use crate::primitives::IoFunctions;
use crate::primitives::ListOperations;
use crate::primitives::NumOperations;
use crate::primitives::PortOperations;
use crate::primitives::StringOperations;
use crate::primitives::SymbolOperations;
use crate::primitives::VectorOperations;

// use crate::primitives::{Adder, Divider, Multiplier, SteelFunctor, Subtractor};
use crate::rerrs::SteelErr;
use crate::rvals::{Result, SteelVal};
use crate::stop;

use std::cell::RefCell;
use std::collections::HashMap;

use std::rc::Rc;
use std::rc::Weak;
// use std::convert::AsRef;
// use std::borrow::BorrowMut;
use crate::compiler::AST;

use crate::vm::SymbolMap;

// use std::mem;

thread_local! {
    pub static VOID: Rc<SteelVal> = Rc::new(SteelVal::Void);
    pub static TRUE: Rc<SteelVal> = Rc::new(SteelVal::BoolV(true));
    pub static FALSE: Rc<SteelVal> = Rc::new(SteelVal::BoolV(false));
}

#[macro_use]
macro_rules! ensure_tonicity {
    ($check_fn:expr) => {{
        |args: &[Rc<SteelVal>]| -> Result<Rc<SteelVal>> {
            let args_iter: Vec<SteelVal> = args.into_iter().map(|x| (**x).clone()).collect();
            let mut args_iter = args_iter.iter();
            let first = args_iter.next().ok_or(SteelErr::ArityMismatch(
                "expected at least one argument".to_string(),
                None,
            ))?;
            fn f<'a>(prev: &SteelVal, mut xs: impl Iterator<Item = &'a SteelVal>) -> bool {
                match xs.next() {
                    Some(x) => $check_fn(prev, x) && f(x, xs),
                    None => true,
                }
            };
            let res = f(&first, args_iter);
            if res {
                Ok(TRUE.with(|f| Rc::clone(f)))
            } else {
                Ok(FALSE.with(|f| Rc::clone(f)))
            }
        }
    }};
}

#[macro_use]
macro_rules! ensure_tonicity_pointer_equality {
    ($check_fn:expr) => {{
        |args: &[Rc<SteelVal>]| -> Result<Rc<SteelVal>> {
            // let args_iter: Vec<Rc<SteelVal>> = args.into_iter();
            let mut args_iter = args.into_iter();
            let first = args_iter.next().ok_or(SteelErr::ArityMismatch(
                "expected at least one argument".to_string(),
                None,
            ))?;
            fn f<'a>(prev: &Rc<SteelVal>, mut xs: impl Iterator<Item = &'a Rc<SteelVal>>) -> bool {
                match xs.next() {
                    Some(x) => $check_fn(prev, x) && f(x, xs),
                    None => true,
                }
            };
            let res = f(&first, args_iter);
            if res {
                Ok(TRUE.with(|f| Rc::clone(f)))
            } else {
                Ok(FALSE.with(|f| Rc::clone(f)))
            }
        }
    }};
}

#[macro_use]
macro_rules! gen_pred {
    ($variant:ident) => {{
        SteelVal::FuncV(|args: &[Rc<SteelVal>]| -> Result<Rc<SteelVal>> {
            if let Some(first) = args.first() {
                if let SteelVal::$variant(..) = first.as_ref() {
                    return Ok(TRUE.with(|f| Rc::clone(f)));
                }
            }
            Ok(FALSE.with(|f| Rc::clone(f)))
        })
    }};

    ($variant1:ident, $variant2:ident) => {{
        SteelVal::FuncV(|args: &[Rc<SteelVal>]| -> Result<Rc<SteelVal>> {
            if let Some(first) = args.first() {
                match first.as_ref() {
                    SteelVal::$variant1(..) | SteelVal::$variant2(..) => {
                        return Ok(TRUE.with(|f| Rc::clone(f)));
                    }
                    _ => {}
                }
            }
            Ok(FALSE.with(|f| Rc::clone(f)))
        })
    }};
}

pub type RcRefCell<T> = Rc<RefCell<T>>;
pub fn new_rc_ref_cell<T>(x: T) -> RcRefCell<T> {
    Rc::new(RefCell::new(x))
}

pub struct Env {
    bindings: HashMap<String, Rc<SteelVal>>,
    bindings_vec: Vec<Rc<SteelVal>>,
    offset: usize,
    parent: Option<Rc<RefCell<Env>>>,
    sub_expression: Option<Weak<RefCell<Env>>>,
    heap: Vec<Rc<RefCell<Env>>>,
    is_binding_context: bool,
    is_binding_offset: bool,
    module: Vec<AST>,
    is_module_context: bool,
    ndefs: usize,
}

impl Drop for Env {
    fn drop(&mut self) {
        self.heap.clear();
        self.clear_bindings();
        self.heap.clear();
        self.module.clear();
        self.bindings_vec.clear();
    }
}

pub trait MacroEnv {
    fn validate_identifier(&self, name: &str) -> bool;
}

// Don't love this one, but for now it'll suffice
// TODO
impl MacroEnv for Rc<RefCell<Env>> {
    fn validate_identifier(&self, name: &str) -> bool {
        self.borrow().lookup(name).is_ok()
    }
}

impl MacroEnv for &Rc<RefCell<Env>> {
    fn validate_identifier(&self, name: &str) -> bool {
        self.borrow().lookup(name).is_ok()
    }
}

impl Env {
    /// Make a new `Env` from
    /// another parent `Env`.
    pub fn new(parent: &Rc<RefCell<Self>>, offset: usize) -> Self {
        Env {
            bindings: HashMap::new(),
            bindings_vec: Vec::new(),
            offset,
            parent: Some(Rc::clone(&parent)),
            sub_expression: None,
            heap: Vec::new(),
            is_binding_context: false,
            is_binding_offset: false,
            module: Vec::new(),
            is_module_context: false,
            ndefs: 0,
        }
    }

    pub fn add_module(&mut self, new_mod: AST) {
        self.module.push(new_mod)
    }

    pub fn len(&self) -> usize {
        self.bindings_vec.len()
    }

    pub fn local_offset(&self) -> usize {
        self.offset
    }

    pub fn ndefs(&self) -> usize {
        self.ndefs
    }

    pub fn parent_ndefs(&self) -> usize {
        if let Some(p) = &self.parent {
            p.borrow().ndefs()
        } else if let Some(p) = &self.sub_expression {
            p.upgrade().unwrap().borrow().ndefs()
        } else {
            0
        }
    }

    pub fn offset(&self) -> usize {
        // let parent_offset =

        let parent_offset = if let Some(p) = &self.parent {
            println!("Getting here!");
            p.borrow().local_offset()
        } else if let Some(p) = &self.sub_expression {
            // println!("---------Inside this one----------");
            p.upgrade().unwrap().borrow().offset()
        } else {
            println!("else case");
            0
        };

        println!("Parent offset: {}", parent_offset);

        self.offset + parent_offset
    }

    // pub fn parent_ndef(&self) -> usize {
    //     if let Some(p) = &self.parent {
    //         p.borrow().ndef_body()
    //     }
    // }

    pub fn new_subexpression(sub_expression: Weak<RefCell<Self>>, offset: usize) -> Self {
        Env {
            bindings: HashMap::new(),
            bindings_vec: Vec::new(),
            offset,
            parent: None,
            sub_expression: Some(sub_expression),
            heap: Vec::new(),
            is_binding_context: false,
            is_binding_offset: false,
            module: Vec::new(),
            is_module_context: false,
            ndefs: 0,
        }
    }

    pub fn set_ndefs(&mut self, ndefs: usize) {
        self.ndefs = ndefs
    }

    pub fn is_binding_context(&self) -> bool {
        self.is_binding_context
    }

    pub fn set_binding_context(&mut self, b: bool) {
        self.is_binding_context = b;
    }

    pub fn is_binding_offset(&self) -> bool {
        self.is_binding_offset
    }

    pub fn set_binding_offset(&mut self, b: bool) {
        self.is_binding_offset = b;
    }

    pub fn is_module_context(&self) -> bool {
        self.is_module_context
    }

    pub fn set_module_context(&mut self, b: bool) {
        self.is_module_context = b;
    }

    pub fn is_root(&self) -> bool {
        self.parent.is_none() && self.sub_expression.is_none()
    }

    /// top level global env has no parent
    pub fn root() -> Self {
        Env {
            bindings: HashMap::new(),
            bindings_vec: Vec::new(),
            offset: 0,
            parent: None,
            sub_expression: None,
            heap: Vec::new(),
            is_binding_context: false,
            is_binding_offset: false,
            module: Vec::new(),
            is_module_context: false,
            ndefs: 0,
        }
    }

    pub fn heap(&self) -> &[Rc<RefCell<Env>>] {
        &self.heap
    }

    pub fn parent(&self) -> &Option<Rc<RefCell<Env>>> {
        &self.parent
    }

    pub fn add_to_heap(&mut self, val: Rc<RefCell<Env>>) {
        self.heap.push(val);
    }

    pub fn get_modules(&self) -> &[AST] {
        &self.module
    }

    // pub fn sub_expression(&self) -> &Option<Rc<RefCell<Env>>> {
    //     &self.sub_expression
    // }

    pub fn is_one_layer_down(&self) -> bool {
        self.parent.is_some()
    }

    pub fn clear_bindings(&mut self) {
        self.bindings.clear();
    }

    pub fn print_bindings(&self) {
        println!("{:#?}", self.bindings);
    }

    pub fn string_bindings_vec(&self) -> String {
        format!("{:?}", self.bindings_vec)
    }

    pub fn bindings(&self) -> &HashMap<String, Rc<SteelVal>> {
        &self.bindings
    }

    /// Within the current environment, bind
    /// identifier `key` to `val`
    pub fn define(&mut self, key: String, val: Rc<SteelVal>) {
        // println!("Defining: {} with val: {}", key, (*val).clone());
        // println!("{:?}", self.bindings.keys());
        self.bindings.insert(key, val);
    }

    pub fn define_idx(&mut self, idx: usize, val: Rc<SteelVal>) {
        // println!("Defining value: {:?} at idx: {}", val, idx);
        // println!("Current bindings: {:?}", self.bindings_vec);

        if idx < self.bindings_vec.len() {
            // let e = mem::replace(&mut self.bindings_vec[idx], val);
            // println!("Strong count: {}", Rc::strong_count(&e));
            // std::mem::drop(e);
            // println!("Strong count: {}", Rc::strong_count(&e));
            self.bindings_vec[idx] = val;
        } else if idx == self.bindings_vec.len() {
            self.bindings_vec.push(val);
        } else {
            panic!("Out of bounds define statement");
        }
        // println!("{:?}", self.bindings_vec);
        // self.offset += 1;
    }

    pub fn reserve_defs(&mut self, ndefs: usize) {
        for _ in 0..ndefs {
            self.bindings_vec.push(VOID.with(|f| Rc::clone(f)));
        }
    }

    pub fn try_define(&mut self, key: &str, val: Rc<SteelVal>) {
        if let Some(v) = self.bindings.get_mut(key) {
            *v = val;
        } else {
            self.bindings.insert(key.to_string(), val);
        }

        // if self.bindings.contains_key(key) {
        //     self.bindings.get_mut(k)
        // } else {

        // }
    }

    /// Within the current environment,
    /// bind identifiers `keys` to `vals`
    /// throws arity mismatch if they don't have the same length
    pub fn define_all(&mut self, keys: &[String], vals: Vec<Rc<SteelVal>>) -> Result<()> {
        let expected_len = keys.len();
        let actual_len = vals.len();
        if expected_len != actual_len {
            let e = format!(
                "function expected {} params, got {}",
                expected_len, actual_len
            );
            stop!(ArityMismatch => e);
        }
        // let iter = keys.iter().map(String::as_ref).zip(vals.into_iter());
        let iter = keys.iter().cloned().zip(vals.into_iter());
        self.define_zipped(iter);
        Ok(())
    }

    pub fn define_zipped(&mut self, zipped: impl Iterator<Item = (String, Rc<SteelVal>)>) {
        zipped.for_each(|(param, arg)| self.define(param, arg))
    }

    /// Search starting from the current environment
    /// for `key`, looking through the parent chain in order.
    ///
    /// If found, update binding for
    /// `key` with `val` and return old value.
    ///
    /// Otherwise, error with `FreeIdentifier`
    pub fn set(&mut self, key: String, val: Rc<SteelVal>) -> Result<Rc<SteelVal>> {
        // println!("GETTING INSIDE SET!");

        if self.bindings.contains_key(&key) {
            // println!("Inside here...");

            self.bindings
                .insert(key.clone(), val)
                .ok_or_else(|| SteelErr::FreeIdentifier(key.to_string(), None))
        } else {
            if self.parent.is_some() {
                match &self.parent {
                    Some(par) => par.borrow_mut().set(key, val),
                    None => {
                        stop!(FreeIdentifier => key.to_string()); // Err(SteelErr::FreeIdentifier(name.to_string())),
                    }
                }
            } else {
                match &self.sub_expression {
                    Some(par) => match par.upgrade() {
                        Some(x) => x.borrow_mut().set(key, val),
                        None => {
                            stop!(Generic => "Parent subexpression was dropped looking for {}", key.to_string())
                        }
                    },
                    None => {
                        // println!("Somehow getting here!");
                        stop!(FreeIdentifier => key.to_string())
                    }
                }
            }
        }
    }

    /// Search starting from the current environment
    /// for `key`, looking through the parent chain in order.
    ///
    /// If found, remove the binding and return the value
    ///
    /// Otherwise, error with `FreeIdentifier`
    pub fn remove(&mut self, key: &str) -> Result<Rc<SteelVal>> {
        if self.bindings.contains_key(key) {
            self.bindings
                .remove(key)
                .ok_or_else(|| SteelErr::FreeIdentifier(key.to_string(), None))
        } else {
            match &self.parent {
                Some(par) => par.borrow_mut().remove(key),
                None => stop!(FreeIdentifier => key), // Err(SteelErr::FreeIdentifier(key.to_string())),
            }
        }
    }

    pub fn lookup_idx(&self, idx: usize) -> Result<Rc<SteelVal>> {
        let offset = self.offset;
        // println!("Looking up {}, with offset: {}", idx, offset);
        // println!("{:?}", self.bindings_vec);

        // if offset != 0 {
        //     offset = offset - 1;
        // }

        // if self.sub_expression.is_some() {
        //     offset = offset + 1;
        // }

        if idx >= offset {
            let lookup = idx - offset;

            // let lookup = if self.sub_expression.is_some() {
            //     (self.bindings_vec.len() - 1) - (idx - offset + 1)
            // } else {
            //     idx - offset
            // };

            // let updated_offset = if offset != 0 { offset + 1 } else { offset };

            if let Some(v) = self.bindings_vec.get(lookup) {
                // println!("Found {:?}", v);
                return Ok(Rc::clone(v));
            } else {
                stop!(FreeIdentifier => "blagh");
            }
        // }
        // if let Some(v) = self.bindings_vec.get(idx - self.offset) {
        //     Ok(Rc::clone(v))
        } else {
            // half assed module approach
            if !self.module.is_empty() {
                for module in &self.module {
                    let res = module.lookup_idx(idx);
                    if res.is_ok() {
                        return res;
                    }
                }
            }

            if self.parent.is_some() {
                match &self.parent {
                    Some(par) => par.borrow().lookup_idx(idx),
                    None => {
                        stop!(FreeIdentifier => idx); // Err(SteelErr::FreeIdentifier(name.to_string())),
                    }
                }
            } else {
                match &self.sub_expression {
                    Some(par) => match par.upgrade() {
                        Some(x) => x.borrow().lookup_idx(idx),
                        None => {
                            stop!(Generic => "Parent subexpression was dropped looking for {}", idx)
                        }
                    },
                    None => stop!(FreeIdentifier => idx),
                }
            }
        }
    }

    /// Search starting from the current environment
    /// for `key`, looking through the parent chain in order.
    ///
    /// if found, return that value
    ///
    /// Otherwise, error with `FreeIdentifier`
    pub fn lookup(&self, name: &str) -> Result<Rc<SteelVal>> {
        // println!("Looking up: {}", name);

        // println!("{:?}", self.bindings.keys());
        if self.bindings.contains_key(name) {
            // value needs to be cloned because
            // user needs to be able to own a persistent value
            // from Cell that may be modified later
            Ok(Rc::clone(&self.bindings[name]))
        } else {
            // half assed module approach
            if !self.module.is_empty() {
                for module in &self.module {
                    let res = module.lookup(name);
                    if res.is_ok() {
                        return res;
                    }
                }
            }

            if self.parent.is_some() {
                match &self.parent {
                    Some(par) => par.borrow().lookup(name),
                    None => {
                        stop!(FreeIdentifier => name); // Err(SteelErr::FreeIdentifier(name.to_string())),
                    }
                }
            } else {
                match &self.sub_expression {
                    Some(par) => match par.upgrade() {
                        Some(x) => x.borrow().lookup(name),
                        None => {
                            stop!(Generic => "Parent subexpression was dropped looking for {}", name)
                        }
                    },
                    None => stop!(FreeIdentifier => name),
                }
            }
        }
    }
    /// default environment contains bindings for
    /// implementations of constants and things like
    /// `car`, `cdr`, `+`
    pub fn default_env() -> Env {
        let mut env = Env::root();
        env.define_zipped(
            Env::default_bindings()
                .into_iter()
                .map(|x| (x.0.to_string(), Rc::new(x.1))),
        );

        for (idx, val) in Env::default_bindings().into_iter().enumerate() {
            env.define_idx(idx, Rc::new(val.1));
        }

        // for (idx, val) in Env::default_bindings().iter().enumerate() {
        //     env.define_idx(val)
        // }

        env
    }

    pub fn default_symbol_map() -> SymbolMap {
        let mut sm = SymbolMap::new();
        for val in Env::default_bindings() {
            sm.add(val.0);
        }
        sm
    }

    pub fn add_rooted_value(&mut self, sm: &mut SymbolMap, val: (&'static str, SteelVal)) {
        sm.add(val.0);
        self.bindings_vec.push(Rc::new(val.1));
    }

    // pub fn add_rooted_values()

    pub fn define_zipped_rooted(
        &mut self,
        sm: &mut SymbolMap,
        zipped: impl Iterator<Item = (String, SteelVal)>,
    ) {
        zipped.for_each(|(param, arg)| {
            sm.add(param.as_str());
            self.bindings_vec.push(Rc::new(arg));
        });
    }

    // pub fn gen_rooted_sym_table() -> SymbolMap {
    //     let mut env = Env::root();
    // }

    fn default_bindings() -> Vec<(&'static str, SteelVal)> {
        vec![
            // ("+", SteelVal::FuncV(Adder::new_func())),
            ("+", NumOperations::adder()),
            ("i+", NumOperations::integer_add()),
            ("f+", NumOperations::float_add()),
            // ("*", SteelVal::FuncV(Multiplier::new_func())),
            ("*", NumOperations::multiply()),
            // ("/", SteelVal::FuncV(Divider::new_func())),
            ("/", NumOperations::divide()),
            // ("-", SteelVal::FuncV(Subtractor::new_func())),
            ("-", NumOperations::subtract()),
            ("list", ListOperations::list()),
            ("car", ListOperations::car()),
            ("cdr", ListOperations::cdr()),
            ("first", ListOperations::car()),
            ("rest", ListOperations::cdr()),
            ("head", ListOperations::car()),
            ("tail", ListOperations::cdr()),
            ("cons", ListOperations::cons()),
            ("append", ListOperations::append()),
            ("push-back", ListOperations::push_back()),
            ("reverse", ListOperations::reverse()),
            ("range", ListOperations::range()),
            ("list->vector", ListOperations::list_to_vec()),
            ("vector", VectorOperations::vec_construct()),
            ("push-front", VectorOperations::vec_cons()),
            ("pop-front", VectorOperations::vec_car()),
            ("vec-rest", VectorOperations::vec_cdr()),
            ("null?", VectorOperations::list_vec_null()),
            ("push", VectorOperations::vec_push()),
            ("range-vec", VectorOperations::vec_range()),
            ("number?", gen_pred!(NumV, IntV)),
            ("string?", gen_pred!(StringV)),
            ("symbol?", gen_pred!(SymbolV)),
            ("vector?", gen_pred!(VectorV)),
            ("list?", gen_pred!(Pair)),
            ("=", SteelVal::FuncV(ensure_tonicity!(|a, b| a == b))),
            ("equal?", SteelVal::FuncV(ensure_tonicity!(|a, b| a == b))),
            (
                "eq?",
                SteelVal::FuncV(ensure_tonicity_pointer_equality!(|a, b| Rc::ptr_eq(a, b))),
            ),
            (">", SteelVal::FuncV(ensure_tonicity!(|a, b| a > b))),
            (">=", SteelVal::FuncV(ensure_tonicity!(|a, b| a >= b))),
            ("<", SteelVal::FuncV(ensure_tonicity!(|a, b| a < b))),
            ("<=", SteelVal::FuncV(ensure_tonicity!(|a, b| a <= b))),
            ("display", IoFunctions::display()),
            ("newline", IoFunctions::newline()),
            ("read-to-string", IoFunctions::read_to_string()),
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
            ("open-input-file", PortOperations::open_input_file()),
            ("read-port-to-string", PortOperations::read_port_to_string()),
            ("read-line-from-port", PortOperations::read_line_to_string()),
            ("concat-symbols", SymbolOperations::concat_symbols()),
            ("error!", ControlOperations::error()),
            ("symbol->string", SymbolOperations::symbol_to_string()),
            ("random-int", NumOperations::random_int()),
            ("string->int", StringOperations::string_to_int()),
            // ("flatten", ListOperations::flatten()),
            ("even?", NumOperations::even()),
            ("odd?", NumOperations::odd()),
            ("is-dir?", FsFunctions::is_dir()),
            ("is-file?", FsFunctions::is_file()),
            ("read-dir", FsFunctions::read_dir()),
            ("path-exists?", FsFunctions::path_exists()),
            ("file-name", FsFunctions::file_name()),
            ("current-directory", FsFunctions::current_dir()),
        ]
    }
}

#[cfg(test)]
mod env_tests {
    use super::*;
    fn unwrap_single_float(exp: &SteelVal) -> Result<f64> {
        match exp {
            SteelVal::NumV(num) => Ok(*num),
            _ => stop!(TypeMismatch => "expected a number"),
        }
    }
    #[test]
    fn env_basic() {
        // default_env <- c1 <- c2
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        assert!(default_env.borrow().lookup("+").is_ok());
        let c1 = Rc::new(RefCell::new(Env::new(&default_env, 0)));
        c1.borrow_mut()
            .define("x".to_owned(), Rc::new(SteelVal::NumV(1.0)));
        let c2 = Rc::new(RefCell::new(Env::new(&c1, 0)));
        c2.borrow_mut()
            .define("y".to_owned(), Rc::new(SteelVal::NumV(2.0)));
        assert!(default_env.borrow_mut().lookup("+").is_ok());
        assert!(c2.borrow_mut().lookup("+").is_ok());
        assert_eq!(
            unwrap_single_float(&c2.borrow_mut().lookup("y").unwrap()).unwrap(),
            2.0
        );
        assert_eq!(
            unwrap_single_float(&c2.borrow_mut().lookup("x").unwrap()).unwrap(),
            1.0
        );
        assert!(c2.borrow_mut().lookup("z").is_err());
    }
}
