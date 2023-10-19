use std::collections::{HashSet, VecDeque};

use num::BigInt;

use crate::steel_vm::{builtin::get_function_name, engine::Engine};

use super::*;

thread_local! {
    // Use this to print values, in lieu of a bespoke printer
    static PRINTING_KERNEL: RefCell<Engine> = {

        let mut engine = Engine::new_printer();

        engine.run(include_str!("../scheme/print.scm")).unwrap();

        RefCell::new(engine)
    };
}

pub fn install_printer() {
    PRINTING_KERNEL.with(|x| {
        x.borrow().globals();
    });
}

#[steel_derive::function(name = "print-in-engine")]
pub fn print_in_engine(value: SteelVal) {
    PRINTING_KERNEL
        .with(|x| {
            x.borrow_mut()
                .call_function_by_name_with_args("print", vec![value])
        })
        .unwrap();
}

#[derive(Default)]
// Keep track of any reference counted values that are visited, in a pointer
pub(super) struct CycleDetector {
    // Keep a mapping of the pointer -> gensym
    visited: std::collections::HashSet<usize>,

    // Recording things that have already been seen
    cycles: std::collections::HashMap<usize, usize>,

    // Values captured in cycles
    values: Vec<SteelVal>,
}

impl CycleDetector {
    pub(super) fn detect_and_display_cycles(val: &SteelVal, f: &mut fmt::Formatter) -> fmt::Result {
        let mut detector = CycleDetector::default();

        detector.visit(val);

        detector.start_format(val, f)
    }

    fn start_format(self, val: &SteelVal, f: &mut fmt::Formatter) -> fmt::Result {
        // println!("STARTING FORMAT");

        for node in &self.values {
            let id = match &node {
                SteelVal::CustomStruct(c) => {
                    // println!("FOUND CUSTOM STRUCT");

                    let ptr_addr = c.as_ptr() as usize;
                    self.cycles.get(&ptr_addr).unwrap()
                }
                SteelVal::HeapAllocated(b) => {
                    // println!("FOUND HEAP ALLOCATED");

                    // let ptr_addr = b.as_ptr_usize();

                    // self.cycles.get(&ptr_addr).unwrap()

                    // if let Some(value) = self.cycles.get(&ptr_addr) {
                    // value
                    // } else {
                    // Get the object that THIS points to
                    let ptr_addr = b.get().as_ptr_usize().unwrap();
                    self.cycles.get(&ptr_addr).unwrap()
                    // }
                }
                SteelVal::ListV(l) => {
                    let ptr_addr = l.as_ptr_usize();

                    self.cycles.get(&ptr_addr).unwrap()
                }
                _ => {
                    unreachable!()
                }
            };

            write!(f, "#{id}=")?;
            self.top_level_format_with_cycles(node, f)?;
            writeln!(f)?;
        }

        if !self.values.contains(val) {
            self.format_with_cycles(val, f)?;
        }

        Ok(())
    }

    fn top_level_format_with_cycles(&self, val: &SteelVal, f: &mut fmt::Formatter) -> fmt::Result {
        match val {
            BoolV(b) => write!(f, "#{b}"),
            NumV(x) => write!(f, "{x:?}"),
            IntV(x) => write!(f, "{x}"),
            StringV(s) => write!(f, "{s:?}"),
            BigNum(b) => write!(f, "{}", b.as_ref()),
            CharV(c) => write!(f, "#\\{c}"),
            FuncV(func) => {
                if let Some(name) = get_function_name(*func) {
                    write!(f, "#<function:{}>", name.name)
                } else {
                    write!(f, "#<function>")
                }
            }
            Void => write!(f, "#<void>"),
            SymbolV(s) => write!(f, "{s}"),
            VectorV(lst) => {
                let mut iter = lst.iter();
                write!(f, "'#(")?;
                if let Some(last) = iter.next_back() {
                    for item in iter {
                        self.format_with_cycles(item, f)?;
                        write!(f, " ")?;
                    }
                    self.format_with_cycles(last, f)?;
                }
                write!(f, ")")
            }
            Custom(x) => write!(f, "#<{}>", x.borrow().display()?),
            CustomStruct(s) => {
                let guard = s;

                {
                    if guard
                        .get(&SteelVal::SymbolV(SteelString::from("#:transparent")))
                        .and_then(|x| x.as_bool())
                        .unwrap_or_default()
                    {
                        write!(f, "({}", guard.name)?;

                        for i in guard.fields.iter() {
                            write!(f, " ")?;
                            self.format_with_cycles(i, f)?;
                        }

                        write!(f, ")")
                    } else {
                        write!(f, "({})", guard.name)
                    }
                }
            }

            PortV(_) => write!(f, "#<port>"),
            Closure(_) => write!(f, "#<bytecode-closure>"),
            HashMapV(hm) => write!(f, "#<hashmap {:#?}>", hm.as_ref()),
            IterV(_) => write!(f, "#<iterator>"),
            HashSetV(hs) => write!(f, "#<hashset {:?}>", hs.0),
            FutureFunc(_) => write!(f, "#<future-func>"),
            FutureV(_) => write!(f, "#<future>"),
            StreamV(_) => write!(f, "#<stream>"),
            BoxedFunction(b) => {
                if let Some(name) = b.name() {
                    write!(f, "#<function:{}>", name)
                } else {
                    write!(f, "#<function>")
                }
            }
            ContinuationFunction(c) => write!(f, "#<continuation: {:?}>", c.stack),
            // #[cfg(feature = "jit")]
            // CompiledFunction(_) => write!(f, "#<compiled-function>"),
            ListV(l) => {
                write!(f, "(")?;

                let mut iter = l.iter().peekable();

                while let Some(item) = iter.next() {
                    self.format_with_cycles(item, f)?;
                    if iter.peek().is_some() {
                        write!(f, " ")?
                    }
                }

                write!(f, ")")
            }
            MutFunc(_) => write!(f, "#<function>"),
            BuiltIn(_) => write!(f, "#<function>"),
            ReducerV(_) => write!(f, "#<reducer>"),
            MutableVector(v) => write!(f, "{:?}", v.get()),
            SyntaxObject(s) => {
                if let Some(raw) = &s.raw {
                    write!(f, "#<syntax:{:?} {:?}>", s.span, raw)
                } else {
                    write!(f, "#<syntax:{:?} {:?}>", s.span, s.syntax)
                }
            }
            BoxedIterator(_) => write!(f, "#<iterator>"),
            Boxed(b) => write!(f, "'#&{}", b.borrow()),
            Reference(x) => write!(f, "{}", x.format()?),
            HeapAllocated(b) => write!(f, "'#&{}", b.get()),
        }
    }

    fn format_with_cycles(&self, val: &SteelVal, f: &mut fmt::Formatter) -> fmt::Result {
        match val {
            BoolV(b) => write!(f, "#{b}"),
            NumV(x) => write!(f, "{x:?}"),
            IntV(x) => write!(f, "{x}"),
            StringV(s) => write!(f, "{s:?}"),
            CharV(c) => write!(f, "#\\{c}"),
            FuncV(func) => {
                if let Some(name) = get_function_name(*func) {
                    write!(f, "#<function:{}>", name.name)
                } else {
                    write!(f, "#<function>")
                }
            }
            Void => write!(f, "#<void>"),
            SymbolV(s) => write!(f, "{s}"),
            VectorV(lst) => {
                let mut iter = lst.iter();
                write!(f, "(")?;
                if let Some(last) = iter.next_back() {
                    for item in iter {
                        self.format_with_cycles(item, f)?;
                        write!(f, " ")?;
                    }
                    self.format_with_cycles(last, f)?;
                }
                write!(f, ")")
            }
            Custom(x) => write!(f, "{}", x.borrow().display()?),
            CustomStruct(s) => {
                if let Some(id) = self.cycles.get(&(s.as_ptr() as usize)) {
                    write!(f, "#{id}#")
                } else {
                    let guard = s;

                    {
                        if s.get(&SteelVal::SymbolV(SteelString::from("#:transparent")))
                            .and_then(|x| x.as_bool())
                            .unwrap_or_default()
                        {
                            write!(f, "({}", guard.name)?;

                            for i in guard.fields.iter() {
                                write!(f, " ")?;
                                self.format_with_cycles(i, f)?;
                            }

                            write!(f, ")")
                        } else {
                            write!(f, "({})", guard.name)
                        }
                    }
                }
            }

            PortV(_) => write!(f, "#<port>"),
            Closure(_) => write!(f, "#<bytecode-closure>"),
            HashMapV(hm) => write!(f, "#<hashmap {:#?}>", hm.as_ref()),
            IterV(_) => write!(f, "#<iterator>"),
            HashSetV(hs) => write!(f, "#<hashset {:?}>", hs.0),
            FutureFunc(_) => write!(f, "#<future-func>"),
            FutureV(_) => write!(f, "#<future>"),
            // Promise(_) => write!(f, "#<promise>"),
            StreamV(_) => write!(f, "#<stream>"),
            BoxedFunction(b) => {
                if let Some(name) = b.name() {
                    write!(f, "#<function:{}>", name)
                } else {
                    write!(f, "#<function>")
                }
            }
            ContinuationFunction(c) => write!(f, "#<continuation: {:?}>", c.stack),
            // #[cfg(feature = "jit")]
            // CompiledFunction(_) => write!(f, "#<compiled-function>"),
            ListV(l) => {
                write!(f, "(")?;

                let mut iter = l.iter().peekable();

                while let Some(item) = iter.next() {
                    self.format_with_cycles(item, f)?;
                    if iter.peek().is_some() {
                        write!(f, " ")?
                    }
                }

                // for item in l.iter().pe

                // for item in l {
                //     display_helper(item, f)?;
                //     write!(f, " ")?;
                // }
                write!(f, ")")
            }
            // write!(f, "#<list {:?}>", l),
            MutFunc(_) => write!(f, "#<function>"),
            BuiltIn(_) => write!(f, "#<function>"),
            ReducerV(_) => write!(f, "#<reducer>"),
            MutableVector(v) => write!(f, "{:?}", v.get()),
            SyntaxObject(s) => {
                if let Some(raw) = &s.raw {
                    write!(f, "#<syntax:{:?} {:?}>", s.span, raw)
                } else {
                    write!(f, "#<syntax:{:?} {:?}>", s.span, s.syntax)
                }
            }
            BoxedIterator(_) => write!(f, "#<iterator>"),
            Boxed(b) => write!(f, "'#&{}", b.borrow()),
            Reference(x) => write!(f, "{}", x.format()?),
            BigNum(b) => write!(f, "{}", b.as_ref()),
            HeapAllocated(b) => {
                if let Some(id) = b.get().as_ptr_usize().and_then(|x| self.cycles.get(&x)) {
                    write!(f, "#{id}#")
                } else {
                    write!(f, "'#&{}", b.get())
                }
            }

            SteelVal::Custom(_) => todo!(),
            SteelVal::SyntaxObject(_) => todo!(),
        }
    }

    fn add(&mut self, val: usize, steelval: &SteelVal) -> bool {
        if self.visited.contains(&val) {
            let id = self.cycles.len();

            // If we've already seen this, its fine, we can just move on
            if let std::collections::hash_map::Entry::Vacant(e) = self.cycles.entry(val) {
                e.insert(id);
                // Keep track of the actual values that are being captured
                self.values.push(steelval.clone());
            } else {
                return true;
            }

            return true;
        }

        self.visited.insert(val);
        false
    }

    // TODO: Complete the rest of this visitor
    fn visit(&mut self, val: &SteelVal) {
        match val {
            SteelVal::CustomStruct(s) => {
                if !self.add(s.as_ptr() as usize, val) {
                    for val in s.fields.iter() {
                        self.visit(val);
                    }
                }
            }
            SteelVal::HeapAllocated(b) => {
                if !self.add(b.as_ptr_usize(), val) {
                    // if let Some(obj_ptr) = b.get().as_ptr_usize() {
                    // if !self.add(obj_ptr, val) {
                    self.visit(&b.get())
                    // }
                    // }
                }

                // if let Some(obj_ptr) = b.get().as_ptr_usize() {
                //     if !self.add(obj_ptr, val) {
                //         self.visit(&b.get())
                //     }
                // }
            }
            SteelVal::ListV(l) => {
                if !self.add(l.as_ptr_usize(), val) {
                    for val in l {
                        self.visit(val);
                    }
                }
            }
            SteelVal::HashMapV(h) => {
                for (key, val) in h.iter() {
                    self.visit(key);
                    self.visit(val);
                }
            }
            _ => {}
        }
    }
}

fn replace_with_void(value: &mut SteelVal) -> SteelVal {
    std::mem::replace(value, SteelVal::Void)
}

impl SteelVal {
    fn make_void(&mut self) -> SteelVal {
        std::mem::replace(self, SteelVal::Void)
    }
}

#[cfg(not(feature = "without-drop-protection"))]
pub(crate) mod drop_impls {

    use super::*;

    thread_local! {
        pub static DROP_BUFFER: RefCell<VecDeque<SteelVal>> = RefCell::new(VecDeque::with_capacity(128));
        pub static FORMAT_BUFFER: RefCell<VecDeque<SteelVal>> = RefCell::new(VecDeque::with_capacity(128));
    }

    impl Drop for SteelVector {
        fn drop(&mut self) {
            if self.0.is_empty() {
                return;
            }

            if let Some(inner) = self.0.get_mut() {
                DROP_BUFFER
                    .try_with(|drop_buffer| {
                        if let Ok(mut drop_buffer) = drop_buffer.try_borrow_mut() {
                            for value in std::mem::take(inner) {
                                drop_buffer.push_back(value);
                            }

                            IterativeDropHandler::bfs(&mut drop_buffer);
                        }
                    })
                    .ok();
            }
        }
    }

    impl Drop for SteelHashMap {
        fn drop(&mut self) {
            if self.0.is_empty() {
                return;
            }

            if let Some(inner) = self.0.get_mut() {
                DROP_BUFFER
                    .try_with(|drop_buffer| {
                        if let Ok(mut drop_buffer) = drop_buffer.try_borrow_mut() {
                            for (key, value) in std::mem::take(inner) {
                                drop_buffer.push_back(key);
                                drop_buffer.push_back(value);
                            }

                            IterativeDropHandler::bfs(&mut drop_buffer);
                        }
                    })
                    .ok();
            }
        }
    }

    impl Drop for UserDefinedStruct {
        fn drop(&mut self) {
            if self.fields.is_empty() {
                return;
            }

            if DROP_BUFFER
                .try_with(|drop_buffer| {
                    if let Ok(mut drop_buffer) = drop_buffer.try_borrow_mut() {
                        // for value in std::mem::take(&mut self.fields) {
                        //     drop_buffer.push_back(value);
                        // }

                        drop_buffer.extend(Vec::from(std::mem::take(&mut self.fields)));

                        IterativeDropHandler::bfs(&mut drop_buffer);
                    }
                })
                .is_err()
            {
                let mut buffer = Vec::from(std::mem::take(&mut self.fields)).into();

                IterativeDropHandler::bfs(&mut buffer);
            }
        }
    }

    impl Drop for LazyStream {
        fn drop(&mut self) {
            if self.initial_value == SteelVal::Void && self.stream_thunk == SteelVal::Void {
                return;
            }

            DROP_BUFFER
                .try_with(|drop_buffer| {
                    if let Ok(mut drop_buffer) = drop_buffer.try_borrow_mut() {
                        drop_buffer.push_back(self.initial_value.make_void());
                        drop_buffer.push_back(self.stream_thunk.make_void());

                        IterativeDropHandler::bfs(&mut drop_buffer);
                    }
                })
                .ok();
        }
    }

    impl Drop for ByteCodeLambda {
        fn drop(&mut self) {
            if self.captures.is_empty() {
                return;
            }

            DROP_BUFFER
                .try_with(|drop_buffer| {
                    if let Ok(mut drop_buffer) = drop_buffer.try_borrow_mut() {
                        for value in std::mem::take(&mut self.captures) {
                            drop_buffer.push_back(value);
                        }

                        IterativeDropHandler::bfs(&mut drop_buffer);
                    }
                })
                .ok();
        }
    }
}

pub struct IterativeDropHandler<'a> {
    drop_buffer: &'a mut VecDeque<SteelVal>,
}

impl<'a> IterativeDropHandler<'a> {
    pub fn bfs(drop_buffer: &'a mut VecDeque<SteelVal>) {
        IterativeDropHandler { drop_buffer }.visit();
    }
}

impl<'a> BreadthFirstSearchSteelValVisitor for IterativeDropHandler<'a> {
    type Output = ();

    fn default_output(&mut self) -> Self::Output {
        ()
    }

    fn pop_front(&mut self) -> Option<SteelVal> {
        self.drop_buffer.pop_front()
    }

    fn push_back(&mut self, value: SteelVal) {
        self.drop_buffer.push_back(value)
    }

    fn visit_bool(&mut self, _boolean: bool) {}
    fn visit_float(&mut self, _float: f64) {}
    fn visit_int(&mut self, _int: isize) {}
    fn visit_char(&mut self, _c: char) {}
    fn visit_void(&mut self) {}
    fn visit_string(&mut self, _string: SteelString) {}
    fn visit_function_pointer(&mut self, _ptr: FunctionSignature) {}
    fn visit_symbol(&mut self, _symbol: SteelString) {}
    fn visit_port(&mut self, _port: Gc<SteelPort>) {}
    fn visit_future(&mut self, _future: Gc<FutureResult>) {}
    fn visit_mutable_function(&mut self, _function: MutFunctionSignature) {}
    fn visit_bignum(&mut self, _bignum: Gc<BigInt>) {}
    fn visit_future_function(&mut self, _function: BoxedAsyncFunctionSignature) {}
    fn visit_builtin_function(&mut self, _function: BuiltInSignature) {}
    fn visit_boxed_function(&mut self, _function: Rc<BoxedDynFunction>) {}

    fn visit_closure(&mut self, closure: Gc<ByteCodeLambda>) {
        if let Ok(mut inner) = closure.try_unwrap() {
            for value in std::mem::take(&mut inner.captures) {
                self.push_back(value);
            }
        }
    }

    fn visit_immutable_vector(&mut self, mut vector: SteelVector) {
        if let Some(inner) = vector.0.get_mut() {
            for value in std::mem::take(inner) {
                self.push_back(value);
            }
        }
    }

    fn visit_custom_type(&mut self, custom_type: Gc<RefCell<Box<dyn CustomType>>>) {
        if let Ok(inner) = custom_type.try_unwrap() {
            let mut inner = inner.into_inner();

            // let this decide if we're doing anything with this custom type
            inner.drop_mut(self);
        }
    }

    fn visit_hash_map(&mut self, mut hashmap: SteelHashMap) {
        if let Some(inner) = hashmap.0.get_mut() {
            for (key, value) in std::mem::take(inner) {
                self.push_back(key);
                self.push_back(value);
            }
        }
    }

    fn visit_hash_set(&mut self, mut hashset: SteelHashSet) {
        if let Some(inner) = hashset.0.get_mut() {
            for key in std::mem::take(inner) {
                self.push_back(key);
            }
        }
    }

    fn visit_steel_struct(&mut self, steel_struct: Gc<UserDefinedStruct>) {
        if let Ok(mut inner) = steel_struct.try_unwrap() {
            for value in Vec::from(std::mem::take(&mut inner.fields)) {
                self.push_back(value);
            }
        }
    }

    fn visit_transducer(&mut self, transducer: Gc<Transducer>) {
        if let Ok(inner) = transducer.try_unwrap() {
            for transducer in inner.ops {
                match transducer {
                    crate::values::transducers::Transducers::Map(m) => self.push_back(m),
                    crate::values::transducers::Transducers::Filter(v) => self.push_back(v),
                    crate::values::transducers::Transducers::Take(t) => self.push_back(t),
                    crate::values::transducers::Transducers::Drop(d) => self.push_back(d),
                    crate::values::transducers::Transducers::FlatMap(fm) => self.push_back(fm),
                    crate::values::transducers::Transducers::Flatten => {}
                    crate::values::transducers::Transducers::Window(w) => self.push_back(w),
                    crate::values::transducers::Transducers::TakeWhile(tw) => self.push_back(tw),
                    crate::values::transducers::Transducers::DropWhile(dw) => self.push_back(dw),
                    crate::values::transducers::Transducers::Extend(e) => self.push_back(e),
                    crate::values::transducers::Transducers::Cycle => {}
                    crate::values::transducers::Transducers::Enumerating => {}
                    crate::values::transducers::Transducers::Zipping(z) => self.push_back(z),
                    crate::values::transducers::Transducers::Interleaving(i) => self.push_back(i),
                }
            }
        }
    }

    fn visit_reducer(&mut self, reducer: Gc<Reducer>) {
        if let Ok(inner) = reducer.try_unwrap() {
            match inner {
                Reducer::ForEach(f) => self.push_back(f),
                Reducer::Generic(rf) => {
                    self.push_back(rf.initial_value);
                    self.push_back(rf.function);
                }
                _ => {}
            }
        }
    }

    fn visit_stream(&mut self, stream: Gc<LazyStream>) {
        if let Ok(mut inner) = stream.try_unwrap() {
            self.push_back(replace_with_void(&mut inner.initial_value));
            self.push_back(replace_with_void(&mut inner.stream_thunk));
        }
    }

    // Walk the whole thing! This includes the stack and all the stack frames
    fn visit_continuation(&mut self, continuation: Gc<Continuation>) {
        if let Ok(mut inner) = continuation.try_unwrap() {
            for value in std::mem::take(&mut inner.stack) {
                self.push_back(value);
            }

            if let Some(inner) = inner.current_frame.function.get_mut() {
                for value in std::mem::take(&mut inner.captures) {
                    self.push_back(value);
                }
            }

            for mut frame in std::mem::take(&mut inner.stack_frames) {
                if let Some(inner) = frame.function.get_mut() {
                    for value in std::mem::take(&mut inner.captures) {
                        self.push_back(value);
                    }
                }
            }
        }
    }

    fn visit_list(&mut self, list: List<SteelVal>) {
        if list.strong_count() == 1 {
            for value in list {
                self.push_back(value);
            }
        }
    }

    // TODO: When this gets replaced with heap storage, then we can do this more
    // effectively!
    fn visit_mutable_vector(&mut self, vector: HeapRef<Vec<SteelVal>>) {
        // if let Ok(inner) = vector.try_unwrap() {
        //     let inner = inner.into_inner();

        //     for value in inner {
        //         self.push_back(value);
        //     }
        // }
    }

    // TODO: Once the root is added back to this, bring it back
    fn visit_boxed_iterator(&mut self, _iterator: Gc<RefCell<BuiltInDataStructureIterator>>) {}

    fn visit_syntax_object(&mut self, syntax_object: Gc<Syntax>) {
        if let Ok(inner) = syntax_object.try_unwrap() {
            if let Some(raw) = inner.raw {
                self.push_back(raw);
            }

            self.push_back(inner.syntax);
        }
    }

    fn visit_boxed_value(&mut self, boxed_value: Gc<RefCell<SteelVal>>) {
        if let Ok(inner) = boxed_value.try_unwrap() {
            self.push_back(inner.into_inner());
        }
    }

    fn visit_reference_value(&mut self, reference: Rc<OpaqueReference<'static>>) {
        if let Ok(mut inner) = Rc::try_unwrap(reference) {
            inner.drop_mut(self);
        }
    }

    fn visit_heap_allocated(&mut self, heap_ref: HeapRef<SteelVal>) -> Self::Output {
        todo!()
    }
}

// (&self, val: &SteelVal, f: &mut fmt::Formatter)

// Keep track of any reference counted values that are visited, in a pointer
pub(super) struct CycleDetectorBFS<'a> {
    // Keep a mapping of the pointer -> gensym
    visited: std::collections::HashSet<usize>,

    // Recording things that have already been seen
    cycles: std::collections::HashMap<usize, usize>,

    // Values captured in cycles
    values: Vec<SteelVal>,

    formatter: &'a mut fmt::Formatter<'a>,

    queue: &'a mut VecDeque<SteelVal>,
}

impl<'a> BreadthFirstSearchSteelValVisitor for CycleDetectorBFS<'a> {
    type Output = fmt::Result;

    fn default_output(&mut self) -> Self::Output {
        Ok(())
    }

    fn pop_front(&mut self) -> Option<SteelVal> {
        self.queue.pop_front()
    }

    fn push_back(&mut self, value: SteelVal) {
        self.queue.push_back(value)
    }

    fn visit_closure(&mut self, _: Gc<ByteCodeLambda>) -> Self::Output {
        write!(self.formatter, "#<bytecode-closure>")
    }

    fn visit_bool(&mut self, boolean: bool) -> Self::Output {
        write!(self.formatter, "#{boolean}")
    }

    fn visit_float(&mut self, float: f64) -> Self::Output {
        write!(self.formatter, "{:?}", float)
    }

    fn visit_int(&mut self, int: isize) -> Self::Output {
        write!(self.formatter, "{}", int)
    }

    fn visit_char(&mut self, c: char) -> Self::Output {
        write!(self.formatter, "#\\{c}")
    }

    fn visit_immutable_vector(&mut self, vector: SteelVector) -> Self::Output {
        todo!()
    }

    fn visit_void(&mut self) -> Self::Output {
        write!(self.formatter, "#<void>")
    }

    fn visit_string(&mut self, string: SteelString) -> Self::Output {
        write!(self.formatter, "{:?}", string)
    }

    fn visit_function_pointer(&mut self, ptr: FunctionSignature) -> Self::Output {
        if let Some(name) = get_function_name(ptr) {
            write!(self.formatter, "#<function:{}>", name.name)
        } else {
            write!(self.formatter, "#<function>")
        }
    }

    fn visit_symbol(&mut self, symbol: SteelString) -> Self::Output {
        write!(self.formatter, "{}", symbol)
    }

    fn visit_custom_type(&mut self, custom_type: Gc<RefCell<Box<dyn CustomType>>>) -> Self::Output {
        todo!()
    }

    fn visit_hash_map(&mut self, hashmap: SteelHashMap) -> Self::Output {
        todo!()
    }

    fn visit_hash_set(&mut self, hashset: SteelHashSet) -> Self::Output {
        todo!()
    }

    fn visit_steel_struct(&mut self, steel_struct: Gc<UserDefinedStruct>) -> Self::Output {
        todo!()
    }

    fn visit_port(&mut self, port: Gc<SteelPort>) -> Self::Output {
        todo!()
    }

    fn visit_transducer(&mut self, transducer: Gc<Transducer>) -> Self::Output {
        todo!()
    }

    fn visit_reducer(&mut self, reducer: Gc<Reducer>) -> Self::Output {
        todo!()
    }

    fn visit_future_function(&mut self, function: BoxedAsyncFunctionSignature) -> Self::Output {
        todo!()
    }

    fn visit_future(&mut self, future: Gc<FutureResult>) -> Self::Output {
        todo!()
    }

    fn visit_stream(&mut self, stream: Gc<LazyStream>) -> Self::Output {
        todo!()
    }

    fn visit_boxed_function(&mut self, function: Rc<BoxedDynFunction>) -> Self::Output {
        todo!()
    }

    fn visit_continuation(&mut self, continuation: Gc<Continuation>) -> Self::Output {
        todo!()
    }

    fn visit_list(&mut self, list: List<SteelVal>) -> Self::Output {
        todo!()
    }

    fn visit_mutable_function(&mut self, function: MutFunctionSignature) -> Self::Output {
        todo!()
    }

    fn visit_mutable_vector(&mut self, vector: HeapRef<Vec<SteelVal>>) -> Self::Output {
        todo!()
    }

    fn visit_builtin_function(&mut self, function: BuiltInSignature) -> Self::Output {
        todo!()
    }

    fn visit_boxed_iterator(
        &mut self,
        iterator: Gc<RefCell<BuiltInDataStructureIterator>>,
    ) -> Self::Output {
        todo!()
    }

    fn visit_syntax_object(&mut self, syntax_object: Gc<Syntax>) -> Self::Output {
        todo!()
    }

    fn visit_boxed_value(&mut self, boxed_value: Gc<RefCell<SteelVal>>) -> Self::Output {
        todo!()
    }

    fn visit_reference_value(&mut self, reference: Rc<OpaqueReference<'static>>) -> Self::Output {
        todo!()
    }

    fn visit_bignum(&mut self, bignum: Gc<BigInt>) -> Self::Output {
        todo!()
    }

    fn visit_heap_allocated(&mut self, heap_ref: HeapRef<SteelVal>) -> Self::Output {
        todo!()
    }
}

pub trait BreadthFirstSearchSteelValVisitor {
    type Output;

    fn default_output(&mut self) -> Self::Output;

    fn pop_front(&mut self) -> Option<SteelVal>;

    fn push_back(&mut self, value: SteelVal);

    fn visit(&mut self) -> Self::Output {
        let mut ret = self.default_output();

        while let Some(value) = self.pop_front() {
            ret = match value {
                Closure(c) => self.visit_closure(c),
                BoolV(b) => self.visit_bool(b),
                NumV(n) => self.visit_float(n),
                IntV(i) => self.visit_int(i),
                CharV(c) => self.visit_char(c),
                VectorV(v) => self.visit_immutable_vector(v),
                Void => self.visit_void(),
                StringV(s) => self.visit_string(s),
                FuncV(f) => self.visit_function_pointer(f),
                SymbolV(s) => self.visit_symbol(s),
                SteelVal::Custom(c) => self.visit_custom_type(c),
                HashMapV(h) => self.visit_hash_map(h),
                HashSetV(s) => self.visit_hash_set(s),
                CustomStruct(c) => self.visit_steel_struct(c),
                PortV(p) => self.visit_port(p),
                IterV(t) => self.visit_transducer(t),
                ReducerV(r) => self.visit_reducer(r),
                FutureFunc(f) => self.visit_future_function(f),
                FutureV(f) => self.visit_future(f),
                StreamV(s) => self.visit_stream(s),
                BoxedFunction(b) => self.visit_boxed_function(b),
                ContinuationFunction(c) => self.visit_continuation(c),
                ListV(l) => self.visit_list(l),
                MutFunc(m) => self.visit_mutable_function(m),
                BuiltIn(b) => self.visit_builtin_function(b),
                MutableVector(b) => self.visit_mutable_vector(b),
                BoxedIterator(b) => self.visit_boxed_iterator(b),
                SteelVal::SyntaxObject(s) => self.visit_syntax_object(s),
                Boxed(b) => self.visit_boxed_value(b),
                Reference(r) => self.visit_reference_value(r),
                BigNum(b) => self.visit_bignum(b),
                HeapAllocated(b) => self.visit_heap_allocated(b),
            };
        }

        ret
    }

    fn visit_closure(&mut self, closure: Gc<ByteCodeLambda>) -> Self::Output;
    fn visit_bool(&mut self, boolean: bool) -> Self::Output;
    fn visit_float(&mut self, float: f64) -> Self::Output;
    fn visit_int(&mut self, int: isize) -> Self::Output;
    fn visit_char(&mut self, c: char) -> Self::Output;
    fn visit_immutable_vector(&mut self, vector: SteelVector) -> Self::Output;
    fn visit_void(&mut self) -> Self::Output;
    fn visit_string(&mut self, string: SteelString) -> Self::Output;
    fn visit_function_pointer(&mut self, ptr: FunctionSignature) -> Self::Output;
    fn visit_symbol(&mut self, symbol: SteelString) -> Self::Output;
    fn visit_custom_type(&mut self, custom_type: Gc<RefCell<Box<dyn CustomType>>>) -> Self::Output;
    fn visit_hash_map(&mut self, hashmap: SteelHashMap) -> Self::Output;
    fn visit_hash_set(&mut self, hashset: SteelHashSet) -> Self::Output;
    fn visit_steel_struct(&mut self, steel_struct: Gc<UserDefinedStruct>) -> Self::Output;
    fn visit_port(&mut self, port: Gc<SteelPort>) -> Self::Output;
    fn visit_transducer(&mut self, transducer: Gc<Transducer>) -> Self::Output;
    fn visit_reducer(&mut self, reducer: Gc<Reducer>) -> Self::Output;
    fn visit_future_function(&mut self, function: BoxedAsyncFunctionSignature) -> Self::Output;
    fn visit_future(&mut self, future: Gc<FutureResult>) -> Self::Output;
    fn visit_stream(&mut self, stream: Gc<LazyStream>) -> Self::Output;
    fn visit_boxed_function(&mut self, function: Rc<BoxedDynFunction>) -> Self::Output;
    fn visit_continuation(&mut self, continuation: Gc<Continuation>) -> Self::Output;
    fn visit_list(&mut self, list: List<SteelVal>) -> Self::Output;
    fn visit_mutable_function(&mut self, function: MutFunctionSignature) -> Self::Output;
    fn visit_mutable_vector(&mut self, vector: HeapRef<Vec<SteelVal>>) -> Self::Output;
    fn visit_builtin_function(&mut self, function: BuiltInSignature) -> Self::Output;
    fn visit_boxed_iterator(
        &mut self,
        iterator: Gc<RefCell<BuiltInDataStructureIterator>>,
    ) -> Self::Output;
    fn visit_syntax_object(&mut self, syntax_object: Gc<Syntax>) -> Self::Output;
    fn visit_boxed_value(&mut self, boxed_value: Gc<RefCell<SteelVal>>) -> Self::Output;
    fn visit_reference_value(&mut self, reference: Rc<OpaqueReference<'static>>) -> Self::Output;
    fn visit_bignum(&mut self, bignum: Gc<BigInt>) -> Self::Output;
    fn visit_heap_allocated(&mut self, heap_ref: HeapRef<SteelVal>) -> Self::Output;
}

struct RecursiveEqualityHandler<'a> {
    left: EqualityVisitor<'a>,
    right: EqualityVisitor<'a>,
}

impl<'a> RecursiveEqualityHandler<'a> {
    pub fn compare_equality(left: SteelVal, right: SteelVal) {}

    fn visit(&mut self) -> bool {
        while let (Some(left), Some(right)) = (self.left.pop_front(), self.right.pop_front()) {
            match (left, right) {
                (Closure(l), Closure(r)) => {
                    if l != r {
                        return false;
                    }

                    self.left.visit_closure(l)
                }
                (BoolV(l), BoolV(r)) => {
                    if l != r {
                        return false;
                    }
                }
                (NumV(l), NumV(r)) => {
                    if l != r {
                        return false;
                    }
                }
                (IntV(l), IntV(r)) => {
                    if l != r {
                        return false;
                    }
                }
                (CharV(l), CharV(r)) => {
                    if l != r {
                        return false;
                    }
                }
                (VectorV(l), VectorV(r)) => self.left.visit_immutable_vector(l),
                (Void, Void) => self.left.visit_void(),
                (StringV(l), StringV(r)) => self.left.visit_string(l),
                (FuncV(l), FuncV(r)) => self.left.visit_function_pointer(l),
                (SymbolV(l), SymbolV(r)) => self.left.visit_symbol(l),
                (SteelVal::Custom(l), SteelVal::Custom(r)) => {
                    if l.borrow().inner_type_id() != r.borrow().inner_type_id() {
                        return false;
                    }

                    // Go down to the next level
                    self.left.visit_custom_type(l);
                    self.right.visit_custom_type(r);
                }
                // HashMapV(h) => self.visit_hash_map(h),
                // HashSetV(s) => self.visit_hash_set(s),
                // CustomStruct(c) => self.visit_steel_struct(c),
                // PortV(p) => self.visit_port(p),
                // IterV(t) => self.visit_transducer(t),
                // ReducerV(r) => self.visit_reducer(r),
                // FutureFunc(f) => self.visit_future_function(f),
                // FutureV(f) => self.visit_future(f),
                // StreamV(s) => self.visit_stream(s),
                // BoxedFunction(b) => self.visit_boxed_function(b),
                // ContinuationFunction(c) => self.visit_continuation(c),
                // ListV(l) => self.visit_list(l),
                // MutFunc(m) => self.visit_mutable_function(m),
                // BuiltIn(b) => self.visit_builtin_function(b),
                // MutableVector(b) => self.visit_mutable_vector(b),
                // BoxedIterator(b) => self.visit_boxed_iterator(b),
                // SteelVal::SyntaxObject(s) => self.visit_syntax_object(s),
                // Boxed(b) => self.visit_boxed_value(b),
                // Reference(r) => self.visit_reference_value(r),
                // BigNum(b) => self.visit_bignum(b),
                // HeapAllocated(b) => self.visit_heap_allocated(b),
                (_, _) => {
                    return false;
                }
            }
        }

        false
    }
}

struct EqualityVisitor<'a> {
    // TODO: Keep a cycle collector around as well?
    queue: &'a mut VecDeque<SteelVal>,

    // Mark each node that we've visited, if we encounter any mutable objects
    // on the way, then we'll start using the visited set. But we'll optimistically
    // assume that there are no mutable objects, and we won't start using this
    // until we absolutely have to.
    visited: &'a mut fxhash::FxHashSet<usize>,
}

impl<'a> BreadthFirstSearchSteelValVisitor for EqualityVisitor<'a> {
    type Output = ();

    fn default_output(&mut self) -> Self::Output {}

    fn pop_front(&mut self) -> Option<SteelVal> {
        todo!()
    }

    fn push_back(&mut self, value: SteelVal) {
        todo!()
    }

    fn visit_closure(&mut self, closure: Gc<ByteCodeLambda>) -> Self::Output {
        todo!()
    }

    // Leaf nodes, we don't need to do anything here
    fn visit_bool(&mut self, boolean: bool) -> Self::Output {}
    fn visit_float(&mut self, float: f64) -> Self::Output {}
    fn visit_int(&mut self, int: isize) -> Self::Output {}
    fn visit_char(&mut self, c: char) -> Self::Output {}
    fn visit_void(&mut self) -> Self::Output {}
    fn visit_string(&mut self, string: SteelString) -> Self::Output {}
    fn visit_function_pointer(&mut self, ptr: FunctionSignature) -> Self::Output {}
    fn visit_symbol(&mut self, symbol: SteelString) -> Self::Output {}
    fn visit_port(&mut self, port: Gc<SteelPort>) -> Self::Output {}
    fn visit_boxed_function(&mut self, function: Rc<BoxedDynFunction>) -> Self::Output {}
    fn visit_mutable_function(&mut self, function: MutFunctionSignature) -> Self::Output {}
    fn visit_builtin_function(&mut self, function: BuiltInSignature) -> Self::Output {}

    fn visit_immutable_vector(&mut self, vector: SteelVector) -> Self::Output {
        todo!()
    }

    fn visit_custom_type(&mut self, custom_type: Gc<RefCell<Box<dyn CustomType>>>) -> Self::Output {
        todo!()
    }

    fn visit_hash_map(&mut self, hashmap: SteelHashMap) -> Self::Output {
        todo!()
    }

    fn visit_hash_set(&mut self, hashset: SteelHashSet) -> Self::Output {
        todo!()
    }

    fn visit_steel_struct(&mut self, steel_struct: Gc<UserDefinedStruct>) -> Self::Output {
        todo!()
    }

    fn visit_transducer(&mut self, transducer: Gc<Transducer>) -> Self::Output {
        todo!()
    }

    fn visit_reducer(&mut self, reducer: Gc<Reducer>) -> Self::Output {
        todo!()
    }

    fn visit_future_function(&mut self, function: BoxedAsyncFunctionSignature) -> Self::Output {
        todo!()
    }

    fn visit_future(&mut self, future: Gc<FutureResult>) -> Self::Output {
        todo!()
    }

    fn visit_stream(&mut self, stream: Gc<LazyStream>) -> Self::Output {
        todo!()
    }

    fn visit_continuation(&mut self, continuation: Gc<Continuation>) -> Self::Output {
        todo!()
    }

    fn visit_list(&mut self, list: List<SteelVal>) -> Self::Output {
        todo!()
    }

    fn visit_mutable_vector(&mut self, vector: HeapRef<Vec<SteelVal>>) -> Self::Output {
        todo!()
    }

    fn visit_boxed_iterator(
        &mut self,
        iterator: Gc<RefCell<BuiltInDataStructureIterator>>,
    ) -> Self::Output {
        todo!()
    }

    fn visit_syntax_object(&mut self, syntax_object: Gc<Syntax>) -> Self::Output {
        todo!()
    }

    fn visit_boxed_value(&mut self, boxed_value: Gc<RefCell<SteelVal>>) -> Self::Output {
        todo!()
    }

    fn visit_reference_value(&mut self, reference: Rc<OpaqueReference<'static>>) -> Self::Output {
        todo!()
    }

    fn visit_bignum(&mut self, bignum: Gc<BigInt>) -> Self::Output {}

    fn visit_heap_allocated(&mut self, heap_ref: HeapRef<SteelVal>) -> Self::Output {
        todo!()
    }
}
