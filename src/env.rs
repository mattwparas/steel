use crate::rerrs::RucketErr;
use crate::rvals::RucketVal;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::result;

pub type RcRefCell<T> = Rc<RefCell<T>>;
pub fn new_rc_ref_cell<T>(x: T) -> RcRefCell<T> {
    Rc::new(RefCell::new(x))
}

#[derive(Clone)]
pub struct Env {
    bindings: HashMap<String, RucketVal>,
    parent: EnvRef,
}

// TODO
// Think about functional data structures to avoid cloning here
impl Env {
    // TODO
    pub fn new(parent: EnvRef) -> Env {
        Env {
            bindings: HashMap::new(),
            parent: parent,
        }
    }

    // pub fn add_env(&'a self) -> Env<'a> {
    //     Env::new(Some(self))
    // }

    // TODO
    // pub fn lookup(&self, var: &str) -> result::Result<RucketVal, RucketErr> {
    //     let mut p = Some(self);
    //     while let Some(par) = p {
    //         if let Some(b) = par.bindings.get(var) {
    //             return Ok(b.clone());
    //         } else {
    //             p = par.parent;
    //         }
    //     }
    //     Err(RucketErr::FreeIdentifier(var.to_string()))
    // }

    pub fn insert_binding(&mut self, var: String, val: RucketVal) {
        self.bindings.insert(var, val);
    }

    // pub fn new(parent: EnvRef) -> Env {
    //     Env {
    //         parent,
    //         values: HashMap::new(),
    //     }
    // }

    pub fn with_values(parent: EnvRef, bindings: HashMap<String, RucketVal>) -> Env {
        Env { parent, bindings }
    }

    /// Converts `Env` into a `EnvRef`.
    /// This function moves `Env` into a `RefCell`.
    /// If you need another pointer to newly created EnvRef,
    /// use `EnvRef::clone_ref()` which only copies the pointer,
    /// not the environment itself.
    pub fn into_ref(self) -> EnvRef {
        EnvRef::new(self)
    }

    // TODO make this not bad
    pub fn lookup(&self, name: &str) -> Result<RucketVal, RucketErr> {
        if self.bindings.contains_key(name) {
            Ok(self.bindings[name].clone())
        } else if self.parent.is_some() {
            self.parent.lookup(name)
        } else {
            Err(RucketErr::FreeIdentifier(name.to_string()))
        }
    }

    // pub fn with_ref<F, T>(&self, name: &str, mut f: F) -> Result<RucketVal, RucketErr>
    // where
    //     F: FnMut(&RucketVal) -> Result<RucketVal, RucketErr>,
    // {
    //     if self.bindings.contains_key(name) {
    //         let sexpr = &self.bindings[name];
    //         f(sexpr)
    //     } else if self.parent.is_some() {
    //         self.parent.with_ref(name, f)
    //     } else {
    //         Err(RucketErr::FreeIdentifier(name.to_string()))
    //         // bail!(UnboundVar => name)
    //     }
    // }

    // pub fn with_mut_ref<F, T>(&mut self, name: &str, mut f: F) -> Result<RucketVal, RucketErr>
    // where
    //     F: FnMut(&mut RucketVal) -> Result<RucketVal, RucketErr>,
    // {
    //     if self.bindings.contains_key(name) {
    //         let sexpr = self.bindings.get_mut(name).unwrap();
    //         f(sexpr)
    //     } else if self.parent.is_some() {
    //         self.parent.with_mut_ref(name, f)
    //     } else {
    //         Err(RucketErr::FreeIdentifier(name.to_string()))
    //         // bail!(UnboundVar => name)
    //     }
    // }

    pub fn define(&mut self, key: String, val: RucketVal) {
        self.bindings.insert(key, val);
    }

    pub fn set(&mut self, key: String, val: RucketVal) -> Result<RucketVal, RucketErr> {
        if self.bindings.contains_key(&key) {
            self.bindings
                .insert(key.clone(), val)
                .ok_or_else(|| RucketErr::FreeIdentifier(key.to_string()))
        } else if self.parent.is_some() {
            self.parent.set(key, val)
        } else {
            Err(RucketErr::FreeIdentifier(key))
            // bail!(UnboundVar => key)
        }
    }

    pub fn remove(&mut self, key: &str) -> Result<RucketVal, RucketErr> {
        if self.bindings.contains_key(key) {
            self.bindings
                .remove(key)
                .ok_or_else(|| RucketErr::FreeIdentifier(key.to_string()))
        } else if self.parent.is_some() {
            self.parent.remove(key)
        } else {
            Err(RucketErr::FreeIdentifier(key.to_string()))
            // bail!(UnboundVar => key)
        }
    }

    pub fn pack(&mut self, keys: &[String], vals: Vec<RucketVal>) {
        for (i, arg) in vals.into_iter().enumerate() {
            self.bindings.insert(keys[i].clone(), arg);
        }
    }
}

#[derive(Clone)]
pub struct EnvRef(RcRefCell<Option<Env>>);

impl EnvRef {
    // pub fn new() -> EnvRef {
    //     EnvRef {
    //         env: Env::new(None),
    //     }
    // }

    // pub fn update_env(&mut self) {
    //     self.env = Env::new(Some(&self.env));
    // }

    // pub fn lookup(&self, var: &str) -> result::Result<RucketVal, RucketErr> {
    //     self.env.lookup(var)
    // }

    // pub fn insert_binding(&mut self, var: String, val: RucketVal) {
    //     self.env.insert_binding(var, val);
    // }

    // pub fn default_env_ref() -> EnvRef {
    //     EnvRef { env: default_env() }
    // }

    /// A null environment.
    /// Used as parent environment of global environment.
    pub fn null() -> EnvRef {
        EnvRef(new_rc_ref_cell(None))
    }

    pub fn new(env: Env) -> EnvRef {
        EnvRef(new_rc_ref_cell(Some(env)))
    }

    pub fn is_some(&self) -> bool {
        self.0.borrow().as_ref().is_some()
    }

    pub fn clone_ref(&self) -> EnvRef {
        EnvRef(Rc::clone(&self.0))
    }

    pub fn lookup(&self, name: &str) -> Result<RucketVal, RucketErr> {
        self.0
            .borrow()
            .as_ref()
            .ok_or_else(|| RucketErr::EnvironmentNotFound)?
            .lookup(name)
    }

    /// Use this function to get a real reference to what is inside the Environment,
    /// not a copy of it. Useful for Ports particularly.
    /// It's impossible to return a reference to something inside a RefCell.
    /// (Actually it's quite possible trough std::cell::Ref but not in this
    /// particular case) So we need this extra functions.
    // pub fn with_ref<F, T>(&self, name: &str, f: F) -> Result<RucketVal, RucketErr>
    // where
    //     F: FnMut(&RucketVal) -> Result<RucketVal, RucketErr>,
    // {
    //     self.0
    //         .borrow()
    //         .as_ref()
    //         .ok_or_else(|| RucketErr::EnvironmentNotFound)?
    //         .with_ref(name, f)
    // }

    // pub fn with_mut_ref<F, T>(&self, name: &str, f: F) -> Result<RucketVal, RucketErr>
    // where
    //     F: FnMut(&mut RucketVal) -> Result<RucketVal, RucketErr>,
    // {
    //     self.0
    //         .borrow_mut()
    //         .as_mut()
    //         .ok_or_else(|| RucketErr::EnvironmentNotFound)?
    //         .with_mut_ref(name, f)
    // }

    pub fn define(&self, key: String, val: RucketVal) {
        self.0
            .borrow_mut()
            .as_mut()
            .expect("Can't find environment")
            .define(key, val);
    }

    pub fn set(&self, key: String, val: RucketVal) -> Result<RucketVal, RucketErr> {
        self.0
            .borrow_mut()
            .as_mut()
            .ok_or_else(|| RucketErr::EnvironmentNotFound)?
            .set(key, val)
    }

    pub fn remove(&self, key: &str) -> Result<RucketVal, RucketErr> {
        self.0
            .borrow_mut()
            .as_mut()
            .ok_or_else(|| RucketErr::EnvironmentNotFound)?
            .remove(key)
    }
}

pub fn default_env() -> Env {
    let mut data: HashMap<String, RucketVal> = HashMap::new();
    data.insert(
        "+".to_string(),
        RucketVal::FuncV(
            |args: &[RucketVal]| -> result::Result<RucketVal, RucketErr> {
                let sum = unwrap_list_of_floats(args)?
                    .iter()
                    .fold(0.0, |sum, a| sum + a);

                Ok(RucketVal::NumV(sum))
            },
        ),
    );

    data.insert(
        "-".to_string(),
        RucketVal::FuncV(
            |args: &[RucketVal]| -> result::Result<RucketVal, RucketErr> {
                let floats = unwrap_list_of_floats(args)?;
                let first = *floats.first().ok_or(RucketErr::ArityMismatch(
                    "expected at least one number".to_string(),
                ))?;
                let sum_of_rest = floats[1..].iter().fold(0.0, |sum, a| sum + a);

                Ok(RucketVal::NumV(first - sum_of_rest))
            },
        ),
    );

    Env {
        bindings: data,
        parent: EnvRef::null(),
    }
}

fn unwrap_list_of_floats(args: &[RucketVal]) -> result::Result<Vec<f64>, RucketErr> {
    args.iter().map(|x| unwrap_single_float(x)).collect()
}

fn unwrap_single_float(exp: &RucketVal) -> result::Result<f64, RucketErr> {
    match exp {
        RucketVal::NumV(num) => Ok(*num),
        _ => Err(RucketErr::ExpectedNumber("expected a number".to_string())),
    }
}
