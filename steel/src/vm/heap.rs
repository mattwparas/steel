use crate::env::Env;
use crate::gc::{Gc, OBJECT_COUNT};
use crate::rvals::SteelVal;
use std::cell::RefCell;
use std::rc::Rc;
use std::rc::Weak;

use std::collections::HashMap;

pub struct Heap {
    heap: Vec<Rc<RefCell<Env>>>,
    root: Option<Weak<RefCell<Env>>>,
    limit: usize,
    max_double: usize,
    current_double: usize,
}

impl Default for Heap {
    fn default() -> Self {
        Heap {
            heap: Vec::new(),
            root: None,
            limit: 5000,
            max_double: 4,
            current_double: 0,
        }
    }
}

impl Heap {
    pub fn new() -> Self {
        Heap {
            heap: Vec::new(),
            root: None,
            limit: 5000,
            max_double: 4,
            current_double: 0,
        }
    }

    pub fn plant_root(&mut self, root_ref: Weak<RefCell<Env>>) {
        self.root.replace(root_ref);
    }

    pub fn add(&mut self, val: Rc<RefCell<Env>>) {
        if !val.borrow().is_root() {
            self.heap.push(val)
        }
    }

    pub fn clear(&mut self) {
        self.heap.clear()
    }

    fn root(&self) -> &Option<Weak<RefCell<Env>>> {
        &self.root
    }

    fn walk(&mut self, node: &Weak<RefCell<Env>>) {
        // let reachable = p_env.borrow().is_reachable();
        if let Some(upgraded) = node.upgrade() {
            let reachable = upgraded.borrow().is_reachable();
            if !reachable {
                self.add(Rc::clone(&upgraded));
                println!("Adding an env by walking from the root");
                for child in upgraded.borrow().children() {
                    self.walk(child)
                }
            }
        }
    }

    // Should be infallible
    pub fn gather_and_mark_from_global_root(&mut self) {
        let mut heap = Heap::new();

        if let Some(root) = self.root() {
            for child in root.upgrade().unwrap().borrow().children() {
                heap.walk(child)
            }
        }

        heap.mark();
    }

    pub fn inspect_heap(&self) {
        println!("heap length: {}", self.heap.len());
        self.heap.iter().for_each(|x| {
            println!(
                "{}, {}, {}",
                x.borrow().is_reachable(),
                Rc::weak_count(x),
                Rc::strong_count(x)
            )
        });
        let hp: Vec<String> = self
            .heap
            .iter()
            .map(|x| x.borrow().string_bindings_vec())
            .collect();
        println!("{:?}", hp);

        // println!("Length of the heap: {}", hp.len());

        // hp.sort();
        // hp.dedup();

        // println!("Length of the heap after removing duplicates: {}", hp.len());
    }

    // pub fn find_rogue_references(&self) {
    //     let mut heap = Heap::new();
    //     for env in &self.heap {
    //         for (_, value) in env.borrow().bindings_map() {
    //             heap.walk_down_closure(value);
    //         }
    //     }
    //     println!("##################");
    //     heap.inspect_heap();
    //     heap.mark();
    // }

    pub fn len(&self) -> usize {
        self.heap.len()
    }

    pub fn is_empty(&self) -> bool {
        self.heap.is_empty()
    }

    pub fn reset_limit(&mut self) {
        self.limit = 5000;
    }

    pub fn append(&mut self, other: &mut Self) {
        self.heap.append(&mut other.heap)
    }

    pub fn collect_garbage(&mut self) {
        // heap.inspect_heap();
        // TODO should GC here but for some reason... not working...
        // heap.gather_mark_and_sweep_2(&global_env, &inner_env);
        // heap.drop_large_refs();

        // heap.inspect_heap();

        if self.len() > self.limit {
            println!("Heap length before mark and sweep: {}", self.len());
            println!("Active Object Count: {:?}", OBJECT_COUNT);
            // self.inspect_heap();
            self.profile_heap();
            self.drop_large_refs();
            if self.current_double < self.max_double {
                self.limit *= 2;
                self.current_double += 1;
            }
            self.profile_heap();
            // self.inspect_heap();
            println!("Heap length after mark and sweep: {}", self.len());
            println!("Active Object Count: {:?}", OBJECT_COUNT);
            println!("Limit doubled to: {}", self.limit);
        }
    }

    pub fn profile_heap(&self) {
        let mut hm = HashMap::new();
        for val in &self.heap {
            let key = (Rc::weak_count(val), Rc::strong_count(val));
            // hm.insert()
            let count = hm.entry(key).or_insert(0);
            *count += 1;
        }
        println!("{:?}", hm);
    }

    pub fn drop_large_refs(&mut self) {
        // let mut new_vec = Vec::new();

        // self.heap.reverse();

        self.heap
            .retain(|x| Rc::weak_count(x) > 1 && Rc::strong_count(x) == 1);

        // let mut count = self.heap.len();

        // loop {
        //     self.heap
        //         .retain(|x| Rc::weak_count(x) > 1 && Rc::strong_count(x) == 1);
        //     let new_length = self.heap.len();

        //     if new_length == count {
        //         break;
        //     } else {
        //         count = new_length;
        //     }
        // }
    }

    // #[inline]
    fn match_closure(&mut self, val: &Gc<SteelVal>) {
        match val.as_ref() {
            SteelVal::Closure(bytecode_lambda) => {
                let p_env = bytecode_lambda.sub_expression_env().upgrade().unwrap();

                let reachable = p_env.borrow().is_reachable();

                // println!("FOUND A CLOSURE!");

                if !reachable {
                    // println!(
                    //     "Was it reachable? No, but was it root?: {}",
                    //     p_env.borrow().is_root()
                    // );
                    self.gather(&p_env);
                }
            }
            SteelVal::Pair(_, _) => {
                // println!("Getting here!");
                SteelVal::iter(Gc::clone(val)).for_each(|x| self.match_closure(&x))
            }
            SteelVal::VectorV(v) => v.iter().for_each(|x| self.match_closure(x)),
            _ => {} // SteelVal::Closure
        }
    }

    // fn walk_down_closure(&mut self, val: &Gc<SteelVal>) {
    //     match val.as_ref() {
    //         SteelVal::Closure(bytecode_lambda) => {
    //             let p_env = bytecode_lambda.sub_expression_env().upgrade().unwrap();

    //             let reachable = p_env.borrow().is_reachable();

    //             println!("FOUND A CLOSURE!");

    //             if !reachable {
    //                 println!(
    //                     "Was it reachable? No, but was it root?: {}",
    //                     p_env.borrow().is_root()
    //                 );
    //                 self.walk(&Rc::downgrade(&p_env));
    //             }
    //         }
    //         SteelVal::Pair(_, _) => {
    //             println!("Getting here!");
    //             SteelVal::iter(Gc::clone(val)).for_each(|x| self.match_closure(&x))
    //         }
    //         SteelVal::VectorV(v) => v.iter().for_each(|x| self.match_closure(x)),
    //         _ => {} // SteelVal::Closure
    //     }
    // }

    // make iterative instead of recursive
    // don't allow a node to be visited twice

    // TODO
    // make sure to mark roots to gather into heap at various points
    // I think the entire gather stage leaves it empty because it has no args / the env is empty
    // and for some reason this means that the proper envs are not being reached from the leaves
    // I honestly have no idea but this might get us some of the way there
    pub fn gather(&mut self, leaf: &Rc<RefCell<Env>>) {
        self.add(Rc::clone(leaf));

        if leaf.borrow().is_root() {
            return;
        }

        let mut env = Rc::clone(leaf);
        // let mut heap = vec![Rc::clone(leaf)];

        while let Some(parent_env) = Rc::clone(&env).borrow().sub_expression() {
            let upgraded_env = parent_env.upgrade().unwrap();
            if !upgraded_env.borrow().is_root() {
                let e = Rc::clone(&upgraded_env);
                self.add(Rc::clone(&e));
                // See if this works...
                for value in e.borrow().bindings_map().values() {
                    self.match_closure(value);
                }

                env = upgraded_env;
            } else {
                break;
            }
        }
    }

    pub fn mark(&self) {
        self.heap
            .iter()
            .for_each(|x| x.borrow_mut().set_reachable(true));
    }

    pub fn sweep(&mut self) {
        &self
            .heap
            .retain(|x| x.borrow().is_reachable() || Rc::weak_count(x) > 1);
        // .retain(|x| x.borrow().is_reachable());
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
        // self.add(Rc::clone(leaf1));
        // self.add(Rc::clone(leaf2));
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
