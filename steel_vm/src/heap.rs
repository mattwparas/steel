use std::cell::RefCell;
use std::rc::Rc;
use std::rc::Weak;
use steel::{
    env::Env,
    gc::{Gc, OBJECT_COUNT},
    rvals::SteelVal,
};

use std::collections::HashMap;
pub(crate) static HEAP_LIMIT: usize = 100;

use log::debug;

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
            limit: HEAP_LIMIT,
            max_double: 2,
            current_double: 0,
        }
    }
}

impl Drop for Heap {
    fn drop(&mut self) {
        self.heap.clear();
    }
}

impl Heap {
    pub fn new() -> Self {
        Heap {
            heap: Vec::new(),
            root: None,
            limit: HEAP_LIMIT,
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
        debug!("Freeing the heap with length: {}", self.len());
        self.heap.clear()
    }

    fn root(&self) -> &Option<Weak<RefCell<Env>>> {
        &self.root
    }

    pub fn limit(&self) -> usize {
        self.limit
    }

    // fn walk(&mut self, node: &Weak<RefCell<Env>>) {
    //     // let reachable = p_env.borrow().is_reachable();
    //     if let Some(upgraded) = node.upgrade() {
    //         let reachable = upgraded.borrow().is_reachable();
    //         if !reachable {
    //             self.add(Rc::clone(&upgraded));
    //             println!("Adding an env by walking from the root");
    //             for child in upgraded.borrow().children() {
    //                 self.walk(child)
    //             }
    //         }
    //     }
    // }

    // // Should be infallible
    // pub fn gather_and_mark_from_global_root(&mut self) {
    //     let mut heap = Heap::new();

    //     if let Some(root) = self.root() {
    //         for child in root.upgrade().unwrap().borrow().children() {
    //             heap.walk(child)
    //         }
    //     }

    //     println!(
    //         "//////////// heap length before global root traversal: {}",
    //         self.len()
    //     );

    //     heap.mark();
    //     heap.clear();
    //     // heap.sweep();
    //     self.sweep();

    //     println!(
    //         "//////////// heap length after global root traversal: {}",
    //         self.len()
    //     );
    // }

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
    }

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
        // println!("Calling garbage collection");

        if self.len() > self.limit {
            // std::thread::sleep(std::time::Duration::new(3, 0));
            debug!(
                "Before mark and sweep - Heap-length: {}, Active-Object-Count: {:?}",
                self.len(),
                OBJECT_COUNT
            );
            self.profile_heap();
            self.drop_large_refs();
            if self.current_double < self.max_double {
                self.limit *= 2;
                self.current_double += 1;
            } else {
                // std::thread::sleep(std::time::Duration::new(3, 0));
                // println!("******************************************************");
                // println!("******************* RESET ****************************");
                // println!("******************************************************");
                self.limit = HEAP_LIMIT;
                self.current_double = 0;
                // std::thread::sleep(std::time::Duration::new(3, 0));
            }
            self.profile_heap();

            debug!(
                "After mark and sweep - Heap-length: {}, Active-Object-Count: {:?}",
                self.len(),
                OBJECT_COUNT
            );

            debug!("Heap limit set to: {}", self.limit);
            // std::thread::sleep(std::time::Duration::new(3, 0));
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

        debug!("Heap profile: {:?}", hm);
    }

    pub fn drop_large_refs(&mut self) {
        debug!("Dropping envs with a weak count of 0");

        // self.heap
        //     .retain(|x| Rc::weak_count(x) > 1 && Rc::strong_count(x) > 1);

        // self.heap
        //     .retain(|x| (Rc::weak_count(x) + x.borrow().weak_count()) > 1);
        self.heap.retain(|x| Rc::weak_count(x) > 0);
        // Drop the heap size back down to conserve memory
        self.heap.shrink_to_fit();

        // self.heap.retain(|x| Rc::strong_count(x) > 1);
    }

    // #[inline]
    fn match_closure(&mut self, val: &SteelVal) {
        match val {
            SteelVal::Closure(bytecode_lambda) => {
                let p_env = bytecode_lambda.sub_expression_env().upgrade().unwrap();

                let reachable = p_env.borrow().is_reachable();

                if !reachable {
                    self.gather(&p_env);
                }
            }
            SteelVal::Pair(_) => {
                // println!("Getting here!");
                SteelVal::iter(val.clone()).for_each(|x| self.match_closure(&x))
            }
            SteelVal::VectorV(v) => v.iter().for_each(|x| self.match_closure(x)),
            _ => {} // SteelVal::Closure
        }
    }

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
        // std::thread::sleep(std::time::Duration::new(5, 0));
        // println!(
        //     "env currently at sweep: {:?}",
        //     self.heap
        //         .iter()
        //         .map(|x| (
        //             x.borrow().is_reachable(),
        //             Rc::weak_count(x),
        //             x.borrow().weak_count()
        //         ))
        //         .collect::<Vec<_>>()
        // );
        // std::thread::sleep(std::time::Duration::new(5, 0));

        &self
            .heap
            .retain(|x| x.borrow().is_reachable() || Rc::weak_count(x) > 0);
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
        heap.clear();
    }

    pub fn gather_mark_and_sweep(&mut self, leaf: &Rc<RefCell<Env>>) {
        Self::gather_and_mark(leaf);
        self.sweep()
    }

    pub fn gather_mark_and_sweep_2(&mut self, leaf1: &Rc<RefCell<Env>>, leaf2: &Rc<RefCell<Env>>) {
        // println!(
        //     "!!!!!!!!!!! ############# gather, mark and sweep ############### !!!!!!!!!!!!!!!"
        // );
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
