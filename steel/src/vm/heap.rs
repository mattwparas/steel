use crate::env::Env;
use crate::gc::{Gc, OBJECT_COUNT};
use crate::rvals::SteelVal;
use std::cell::RefCell;
use std::rc::Rc;
// use std::collections

pub struct Heap(Vec<Rc<RefCell<Env>>>);

impl Heap {
    pub fn new() -> Self {
        Heap(Vec::new())
    }

    pub fn add(&mut self, val: Rc<RefCell<Env>>) {
        if !val.borrow().is_root() {
            self.0.push(val)
        }
    }

    pub fn clear(&mut self) {
        self.0.clear()
    }

    pub fn inspect_heap(&self) {
        println!("heap length: {}", self.0.len());
        let mut hp: Vec<String> = self
            .0
            .iter()
            .map(|x| x.borrow().string_bindings_vec())
            .collect();
        println!("{:?}", hp);

        println!("Length of the heap: {}", hp.len());

        hp.sort();
        hp.dedup();

        println!("Length of the heap after removing duplicates: {}", hp.len());
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn append(&mut self, other: &mut Self) {
        self.0.append(&mut other.0)
    }

    // make iterative instead of recursive
    // don't allow a node to be visited twice
    pub fn gather(&mut self, leaf: &Rc<RefCell<Env>>) {
        self.add(Rc::clone(leaf));

        if leaf.borrow().is_root() {
            return;
        }

        // {
        //     leaf.borrow_mut().set_reachable(true);
        // }

        let mut env = Rc::clone(leaf);
        // let mut heap = vec![Rc::clone(leaf)];

        while let Some(parent_env) = Rc::clone(&env).borrow().sub_expression() {
            let upgraded_env = parent_env.upgrade().unwrap();
            if !upgraded_env.borrow().is_root() {
                let e = Rc::clone(&upgraded_env);

                // See if this works...
                for (_, value) in e.borrow().bindings_map() {
                    if let SteelVal::Closure(bytecode_lambda) = value.as_ref() {
                        let p_env = bytecode_lambda.sub_expression_env().upgrade().unwrap();

                        let reachable = p_env.borrow().is_reachable();

                        if !reachable {
                            self.gather(&p_env);
                        }
                        // trace_envs(&p_env, new_heap);
                        // heap.append(&mut found_envs);
                    }
                }

                env = upgraded_env;
            } else {
                break;
            }
        }
    }

    pub fn mark(&self) {
        &self
            .0
            .iter()
            .for_each(|x| x.borrow_mut().set_reachable(true));
    }

    pub fn sweep(&mut self) {
        &self.0.retain(|x| x.borrow().is_reachable());
    }

    pub fn mark_and_sweep(&mut self) {
        self.mark();
        self.sweep();
    }

    pub fn collect_mark_and_sweep(&mut self, leaf: &Rc<RefCell<Env>>) {
        self.gather(leaf);
        self.mark_and_sweep();
    }

    pub fn gather_and_mark(leaf: &Rc<RefCell<Env>>) {
        let mut heap = Self::new();
        heap.gather(leaf);
        heap.mark();
    }

    pub fn gather_and_mark_2(leaf1: &Rc<RefCell<Env>>, leaf2: &Rc<RefCell<Env>>) {
        let mut heap = Self::new();
        heap.gather(leaf1);
        heap.gather(leaf2);
        heap.mark();
    }

    pub fn gather_mark_and_sweep(&mut self, leaf: &Rc<RefCell<Env>>) {
        Self::gather_and_mark(leaf);
        self.sweep()
    }

    pub fn gather_mark_and_sweep_2(&mut self, leaf1: &Rc<RefCell<Env>>, leaf2: &Rc<RefCell<Env>>) {
        Self::gather_and_mark_2(leaf1, leaf2);
        self.sweep();
    }

    pub fn gather_from_slice(&mut self, args: &[Gc<SteelVal>]) {
        for arg in args {
            if let SteelVal::Closure(closure) = arg.as_ref() {
                let parent_env = closure.sub_expression_env().upgrade().unwrap();
                self.gather(&parent_env)
            }
        }
    }
}
