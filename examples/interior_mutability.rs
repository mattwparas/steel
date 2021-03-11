use std::{
    cell::RefCell,
    rc::Rc,
    sync::{Arc, Mutex},
};

use steel_vm::engine::Engine;
use steel_vm::register_fn::RegisterFn;

use once_cell::sync::Lazy;
use steel_derive::Steel; // 1.3.1

// Since Steel is a functional language, we can perform mutation by using the interior mutability pattern
// We expose an Rc<RefCell<T>> (or, in a multi thread environment an Arc<Mutex<T>>) and expose
// functions to interact with it
#[derive(Clone, Debug, Steel, PartialEq)]
pub struct RcRefCellWrapper(Rc<RefCell<usize>>);

pub fn new_rc_ref_cell(val: usize) -> RcRefCellWrapper {
    RcRefCellWrapper(Rc::new(RefCell::new(val)))
}

pub fn rc_refcell_increment(value: RcRefCellWrapper) {
    *value.0.borrow_mut() += 1;
}

#[derive(Clone, Debug, Steel)]
pub struct MutexWrapper(Arc<Mutex<usize>>);

pub fn new_mutex_wrapper(val: usize) -> MutexWrapper {
    MutexWrapper(Arc::new(Mutex::new(val)))
}

pub fn mutex_increment(value: MutexWrapper) {
    *value.0.lock().unwrap() += 1;
}

// Optionally, you can also provide functions which hold references to static global
// variables
static ARRAY: Lazy<Mutex<Vec<u8>>> = Lazy::new(|| Mutex::new(vec![]));

fn push_global_vector() {
    ARRAY.lock().unwrap().push(1);
    println!("{:?}", ARRAY.lock().unwrap());
}

pub fn main() {
    let mut vm = Engine::new();

    vm.register_type::<RcRefCellWrapper>("RcRefCellWrapper?")
        .register_type::<MutexWrapper>("MutexWrapper?")
        .register_fn("new-rc-refcell", new_rc_ref_cell)
        .register_fn("rc-refcell-inc", rc_refcell_increment)
        .register_fn("new-mutex-wrapper", new_mutex_wrapper)
        .register_fn("mutex-inc", mutex_increment)
        .register_fn("push-global-vector", push_global_vector);

    vm.run(
        r#"
        (define rc (new-rc-refcell 0))
        (define mutex (new-mutex-wrapper 0))

        ;; Increment 5 times
        (rc-refcell-inc rc)
        (rc-refcell-inc rc)
        (rc-refcell-inc rc)
        (rc-refcell-inc rc)
        (rc-refcell-inc rc)

        ;; Increment 3 times
        (mutex-inc mutex)
        (mutex-inc mutex)
        (mutex-inc mutex)

        ;; Will print out twice with a growing vector
        (push-global-vector)
        (push-global-vector)
    "#,
    )
    .unwrap();

    let rc = vm.extract::<RcRefCellWrapper>("rc").unwrap();
    let mutex: MutexWrapper = vm.extract("mutex").unwrap();

    println!("RcRefCellWrapper: {:?}", rc);
    println!("MutexWrapper: {:?}", mutex);
}
