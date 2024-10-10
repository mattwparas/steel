use crate::gc::shared::{MutableContainer, ShareableMut};
use crate::steel_vm::{builtin::get_function_name, vm::Continuation, vm::ContinuationMark};
use crate::values::lists::Pair;
use num::BigInt;
use std::{cell::Cell, collections::VecDeque};

use super::*;

#[derive(Default)]
// Keep track of any reference counted values that are visited, in a pointer
pub(super) struct CycleDetector {
    // Recording things that have already been seen
    cycles: fxhash::FxHashMap<usize, usize>,

    // Values captured in cycles
    values: Vec<SteelVal>,

    depth: usize,
}

/// Specifies how to format for the `format_with_cycles` function.
#[derive(PartialEq)]
enum FormatType {
    Normal,
    TopLevel,
}

impl CycleDetector {
    pub(super) fn detect_and_display_cycles(val: &SteelVal, f: &mut fmt::Formatter) -> fmt::Result {
        // Consider using one shared queue here
        let mut queue = Vec::new();

        let mut bfs_detector = CycleCollector {
            visited: fxhash::FxHashSet::default(),
            cycles: fxhash::FxHashMap::default(),
            values: Vec::new(),
            queue: &mut queue,
            found_mutable: false,
        };

        bfs_detector.push_back(val.clone());

        bfs_detector.visit();

        CycleDetector {
            cycles: bfs_detector.cycles,
            values: bfs_detector.values,
            depth: 0,
        }
        .start_format(val, f)
    }

    fn start_format(mut self, val: &SteelVal, f: &mut fmt::Formatter) -> fmt::Result {
        for node in std::mem::take(&mut self.values) {
            let id = match &node {
                SteelVal::CustomStruct(c) => {
                    let ptr_addr = c.as_ptr() as usize;
                    self.cycles.get(&ptr_addr).unwrap()
                }
                SteelVal::HeapAllocated(b) => {
                    // Get the object that THIS points to
                    let ptr_addr = b.get().as_ptr_usize().unwrap();
                    self.cycles.get(&ptr_addr).unwrap()
                }
                SteelVal::ListV(l) => {
                    let ptr_addr = l.as_ptr_usize();

                    self.cycles.get(&ptr_addr).unwrap()
                }
                SteelVal::VectorV(l) => {
                    let ptr_addr = l.0.as_ptr() as usize;

                    self.cycles.get(&ptr_addr).unwrap()
                }
                SteelVal::HashMapV(l) => {
                    let ptr_addr = l.0.as_ptr() as usize;

                    self.cycles.get(&ptr_addr).unwrap()
                }
                SteelVal::HashSetV(l) => {
                    let ptr_addr = l.0.as_ptr() as usize;

                    self.cycles.get(&ptr_addr).unwrap()
                }
                SteelVal::Custom(l) => {
                    let ptr_addr = l.as_ptr() as usize;

                    self.cycles.get(&ptr_addr).unwrap()
                }
                SteelVal::Boxed(b) => {
                    let ptr_addr = b.as_ptr() as usize;

                    self.cycles.get(&ptr_addr).unwrap()
                }
                SteelVal::SyntaxObject(s) => {
                    let ptr_addr = s.as_ptr() as usize;

                    self.cycles.get(&ptr_addr).unwrap()
                }
                SteelVal::MutableVector(v) => {
                    let ptr_addr = v.as_ptr_usize();

                    self.cycles.get(&ptr_addr).unwrap()
                }
                _ => {
                    unreachable!()
                }
            };

            write!(f, "#{id}=")?;
            self.format_with_cycles(&node, f, FormatType::TopLevel)?;
            writeln!(f)?;
        }

        if !self.values.contains(val) {
            self.format_with_cycles(val, f, FormatType::Normal)?;
        }

        Ok(())
    }

    fn format_with_cycles(
        &mut self,
        val: &SteelVal,
        f: &mut fmt::Formatter,
        format_type: FormatType,
    ) -> fmt::Result {
        self.depth += 1;

        if self.depth > 128 {
            return write!(f, "...");
        }

        let res = match val {
            BoolV(b) => write!(f, "#{b}"),
            NumV(x) => write!(f, "{}", RealLiteral::Float(*x)),
            IntV(x) => write!(f, "{x}"),
            BigNum(b) => write!(f, "{}", b.as_ref()),
            Rational(x) => write!(f, "{n}/{d}", n = x.numer(), d = x.denom()),
            BigRational(x) => write!(f, "{n}/{d}", n = x.numer(), d = x.denom()),
            Complex(x) => write!(f, "{}", x.as_ref()),
            StringV(s) => write!(f, "{s:?}"),
            ByteVector(b) => {
                write!(f, "#u8(")?;

                let bytes = b.vec.read();
                let mut iter = bytes.iter();

                if let Some(last) = iter.next_back() {
                    for byte in iter {
                        write!(f, "#x{byte:02X} ")?;
                    }

                    write!(f, "#x{last:02X}")?;
                }

                write!(f, ")")
            }
            CharV(c) => {
                if c.is_ascii_control() {
                    write!(f, "{}", c)
                } else {
                    write!(f, "#\\{c}")
                }
            }
            Pair(p) => {
                write!(f, "(")?;
                self.format_with_cycles(&p.car, f, FormatType::Normal)?;
                write!(f, " . ")?;
                self.format_with_cycles(&p.cdr, f, FormatType::Normal)?;
                write!(f, ")")
            }
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
                write!(f, "#(")?;

                if let Some(last) = iter.next_back() {
                    for item in iter {
                        self.format_with_cycles(item, f, FormatType::Normal)?;
                        write!(f, " ")?;
                    }
                    self.format_with_cycles(last, f, FormatType::Normal)?;
                }
                write!(f, ")")
            }
            // TODO: Somehow getting an already borrowed error here on 208
            Custom(x) => match format_type {
                FormatType::Normal => write!(
                    f,
                    "{}",
                    x.try_read()
                        .map(|x| x.display())
                        .unwrap_or(Ok(format!("#<{:p}>", x)))?
                ),
                FormatType::TopLevel => write!(f, "#<{}>", x.read().display()?),
            },
            CustomStruct(s) => match format_type {
                FormatType::Normal => {
                    if let Some(id) = self.cycles.get(&(s.as_ptr() as usize)) {
                        write!(f, "#{id}#")
                    } else {
                        let guard = s;

                        {
                            if s.get(&SteelVal::SymbolV(SteelString::from("#:transparent")))
                                .and_then(|x| x.as_bool())
                                .unwrap_or_default()
                            {
                                write!(f, "({}", guard.name())?;

                                for i in guard.fields.iter() {
                                    write!(f, " ")?;
                                    self.format_with_cycles(i, f, FormatType::Normal)?;
                                }

                                write!(f, ")")
                            } else {
                                write!(f, "({})", guard.name())
                            }
                        }
                    }
                }
                FormatType::TopLevel => {
                    let guard = s;
                    {
                        if guard
                            .get(&SteelVal::SymbolV(SteelString::from("#:transparent")))
                            .and_then(|x| x.as_bool())
                            .unwrap_or_default()
                        {
                            write!(f, "({}", guard.name())?;

                            for i in guard.fields.iter() {
                                write!(f, " ")?;
                                self.format_with_cycles(i, f, FormatType::Normal)?;
                            }

                            write!(f, ")")
                        } else {
                            write!(f, "({})", guard.name())
                        }
                    }
                }
            },
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
            ContinuationFunction(_) => write!(f, "#<continuation>"),
            // #[cfg(feature = "jit")]
            // CompiledFunction(_) => write!(f, "#<compiled-function>"),
            ListV(l) => {
                write!(f, "(")?;

                let mut iter = l.iter().peekable();

                while let Some(item) = iter.next() {
                    self.format_with_cycles(item, f, FormatType::Normal)?;
                    if iter.peek().is_some() {
                        write!(f, " ")?
                    }
                }
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
            Boxed(b) => write!(f, "'#&{}", b.read()),
            Reference(x) => write!(f, "{}", x.format()?),
            HeapAllocated(b) => {
                let maybe_id = b.get().as_ptr_usize().and_then(|x| self.cycles.get(&x));
                match (maybe_id, format_type) {
                    (Some(id), FormatType::Normal) => {
                        write!(f, "#{id}#")
                    }
                    _ => {
                        write!(f, "'#&{}", b.get())
                    }
                }
            }
        };

        self.depth -= 1;

        res
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

pub(crate) struct SteelCycleCollector {
    cycles: fxhash::FxHashMap<usize, usize>,
    values: List<SteelVal>,
}

impl Custom for SteelCycleCollector {}

impl SteelCycleCollector {
    pub fn from_root(value: SteelVal) -> Self {
        let mut queue = Vec::new();

        let mut collector = CycleCollector {
            visited: fxhash::FxHashSet::default(),
            cycles: fxhash::FxHashMap::default(),
            values: Vec::new(),
            queue: &mut queue,
            found_mutable: false,
        };

        collector.push_back(value);

        collector.visit();

        if collector.found_mutable {
            SteelCycleCollector {
                cycles: collector.cycles,
                values: collector.values.into(),
            }
        } else {
            SteelCycleCollector {
                cycles: fxhash::FxHashMap::default(),
                values: List::new(),
            }
        }
    }

    // Get the value
    pub fn get(&self, node: SteelVal) -> Option<usize> {
        match node {
            SteelVal::CustomStruct(c) => {
                let ptr_addr = c.as_ptr() as usize;
                self.cycles.get(&ptr_addr)
            }
            SteelVal::HeapAllocated(b) => {
                // Get the object that THIS points to
                let ptr_addr = b.get().as_ptr_usize().unwrap();
                self.cycles.get(&ptr_addr)
            }
            SteelVal::ListV(l) => {
                let ptr_addr = l.as_ptr_usize();

                self.cycles.get(&ptr_addr)
            }
            SteelVal::VectorV(l) => {
                let ptr_addr = l.0.as_ptr() as usize;

                self.cycles.get(&ptr_addr)
            }
            SteelVal::HashMapV(l) => {
                let ptr_addr = l.0.as_ptr() as usize;

                self.cycles.get(&ptr_addr)
            }
            SteelVal::HashSetV(l) => {
                let ptr_addr = l.0.as_ptr() as usize;

                self.cycles.get(&ptr_addr)
            }
            SteelVal::Custom(l) => {
                let ptr_addr = l.as_ptr() as usize;

                self.cycles.get(&ptr_addr)
            }
            SteelVal::Boxed(b) => {
                let ptr_addr = b.as_ptr() as usize;

                self.cycles.get(&ptr_addr)
            }
            SteelVal::SyntaxObject(s) => {
                let ptr_addr = s.as_ptr() as usize;

                self.cycles.get(&ptr_addr)
            }
            _ => None,
        }
        .copied()
    }

    pub fn values(&self) -> List<SteelVal> {
        self.values.clone()
    }
}

struct CycleCollector<'a> {
    // Keep a mapping of the pointer -> gensym
    visited: fxhash::FxHashSet<usize>,

    // Recording things that have already been seen
    cycles: fxhash::FxHashMap<usize, usize>,

    // Values captured in cycles
    values: Vec<SteelVal>,

    // Queue of items to check
    queue: &'a mut Vec<SteelVal>,

    // Whether we found something mutable - if we haven't, then a cycle
    // isn't even possible
    found_mutable: bool,
}

impl<'a> CycleCollector<'a> {
    fn add(&mut self, val: usize, steelval: &SteelVal) -> bool {
        if !self.found_mutable {
            false;
        }

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
}

impl<'a> BreadthFirstSearchSteelValVisitor for CycleCollector<'a> {
    type Output = ();

    fn default_output(&mut self) -> Self::Output {}

    fn pop_front(&mut self) -> Option<SteelVal> {
        self.queue.pop()
    }

    fn push_back(&mut self, value: SteelVal) {
        self.queue.push(value)
    }

    fn visit_bytevector(&mut self, _bytevector: SteelByteVector) -> Self::Output {}
    fn visit_closure(&mut self, _closure: Gc<ByteCodeLambda>) -> Self::Output {}
    fn visit_bool(&mut self, _boolean: bool) -> Self::Output {}
    fn visit_float(&mut self, _float: f64) -> Self::Output {}
    fn visit_int(&mut self, _int: isize) -> Self::Output {}
    fn visit_rational(&mut self, _: Rational32) -> Self::Output {}
    fn visit_bigrational(&mut self, _: Gc<BigRational>) -> Self::Output {}
    fn visit_bignum(&mut self, _: Gc<BigInt>) -> Self::Output {}
    fn visit_complex(&mut self, _: Gc<SteelComplex>) -> Self::Output {}
    fn visit_char(&mut self, _c: char) -> Self::Output {}

    fn visit_immutable_vector(&mut self, vector: SteelVector) -> Self::Output {
        if !self.add(
            vector.0.as_ptr() as usize,
            &SteelVal::VectorV(vector.clone()),
        ) {
            for value in vector.0.iter() {
                self.push_back(value.clone());
            }
        }
    }

    fn visit_void(&mut self) -> Self::Output {}
    fn visit_string(&mut self, _string: SteelString) -> Self::Output {}
    fn visit_function_pointer(&mut self, _ptr: FunctionSignature) -> Self::Output {}
    fn visit_symbol(&mut self, _symbol: SteelString) -> Self::Output {}

    // If we have cycles here, it is game over - we probably don't want to be
    // able to render to these easily?
    fn visit_custom_type(&mut self, _custom_type: GcMut<Box<dyn CustomType>>) -> Self::Output {}

    fn visit_hash_map(&mut self, hashmap: SteelHashMap) -> Self::Output {
        if !self.add(
            hashmap.0.as_ptr() as usize,
            &SteelVal::HashMapV(hashmap.clone()),
        ) {
            for (key, value) in hashmap.0.iter() {
                self.push_back(key.clone());
                self.push_back(value.clone());
            }
        }
    }

    fn visit_hash_set(&mut self, hashset: SteelHashSet) -> Self::Output {
        if !self.add(
            hashset.0.as_ptr() as usize,
            &SteelVal::HashSetV(hashset.clone()),
        ) {
            for key in hashset.0.iter() {
                self.push_back(key.clone())
            }
        }
    }

    fn visit_steel_struct(&mut self, steel_struct: Gc<UserDefinedStruct>) -> Self::Output {
        if !self.add(
            steel_struct.as_ptr() as usize,
            &SteelVal::CustomStruct(steel_struct.clone()),
        ) {
            for value in steel_struct.fields.iter() {
                self.push_back(value.clone())
            }
        }
    }

    fn visit_port(&mut self, _port: SteelPort) -> Self::Output {}
    fn visit_transducer(&mut self, _transducer: Gc<Transducer>) -> Self::Output {}
    fn visit_reducer(&mut self, _reducer: Gc<Reducer>) -> Self::Output {}
    fn visit_future_function(&mut self, _function: BoxedAsyncFunctionSignature) -> Self::Output {}
    fn visit_future(&mut self, _future: Gc<FutureResult>) -> Self::Output {}
    fn visit_stream(&mut self, _stream: Gc<LazyStream>) -> Self::Output {}
    fn visit_boxed_function(&mut self, _function: Gc<BoxedDynFunction>) -> Self::Output {}
    fn visit_continuation(&mut self, _continuation: Continuation) -> Self::Output {}

    fn visit_list(&mut self, list: List<SteelVal>) -> Self::Output {
        if !self.add(list.as_ptr_usize(), &SteelVal::ListV(list.clone())) {
            for value in list {
                self.push_back(value);
            }
        }
    }

    fn visit_mutable_function(&mut self, _function: MutFunctionSignature) -> Self::Output {}

    // TODO: Figure out the mutable vector first
    fn visit_mutable_vector(&mut self, vector: HeapRef<Vec<SteelVal>>) -> Self::Output {
        self.found_mutable = true;

        if !self.add(
            vector.as_ptr_usize(),
            &SteelVal::MutableVector(vector.clone()),
        ) {
            for value in vector.get().iter() {
                self.push_back(value.clone());
            }
        }
    }

    fn visit_builtin_function(&mut self, _function: BuiltInSignature) -> Self::Output {}

    fn visit_boxed_iterator(&mut self, _iterator: GcMut<OpaqueIterator>) -> Self::Output {}

    fn visit_syntax_object(&mut self, syntax_object: Gc<Syntax>) -> Self::Output {
        if !self.add(
            syntax_object.as_ptr() as usize,
            &SteelVal::SyntaxObject(syntax_object.clone()),
        ) {
            if let Some(raw) = syntax_object.raw.clone() {
                self.push_back(raw);
            }

            self.push_back(syntax_object.syntax.clone());
        }
    }

    fn visit_boxed_value(&mut self, boxed_value: GcMut<SteelVal>) -> Self::Output {
        if !self.add(
            boxed_value.as_ptr() as usize,
            &SteelVal::Boxed(boxed_value.clone()),
        ) {
            self.push_back(boxed_value.read().clone());
        }
    }

    fn visit_reference_value(&mut self, _reference: Gc<OpaqueReference<'static>>) -> Self::Output {}

    fn visit_heap_allocated(&mut self, heap_ref: HeapRef<SteelVal>) -> Self::Output {
        self.found_mutable = true;

        if !self.add(
            heap_ref.as_ptr_usize(),
            &SteelVal::HeapAllocated(heap_ref.clone()),
        ) {
            self.push_back(heap_ref.get());
        }
    }

    // TODO: Revisit this!
    fn visit_pair(&mut self, pair: Gc<Pair>) -> Self::Output {
        self.push_back(pair.car());
        self.push_back(pair.cdr());
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

                        drop_buffer.extend(
                            self.fields.drain(..), // std::mem::replace(&mut self.fields, Recycle::noop()).into_iter(),
                        );

                        IterativeDropHandler::bfs(&mut drop_buffer);
                    }
                })
                .is_err()
            {
                let mut buffer = self.fields.drain(..).collect();

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

    // impl Drop for ByteCodeLambda {
    //     fn drop(&mut self) {
    //         if self.captures.is_empty() {
    //             return;
    //         }

    //         DROP_BUFFER
    //             .try_with(|drop_buffer| {
    //                 if let Ok(mut drop_buffer) = drop_buffer.try_borrow_mut() {
    //                     for value in std::mem::take(&mut self.captures) {
    //                         drop_buffer.push_back(value);
    //                     }

    //                     IterativeDropHandler::bfs(&mut drop_buffer);
    //                 }
    //             })
    //             .ok();
    //     }
    // }
}

pub struct IterativeDropHandler<'a> {
    drop_buffer: &'a mut VecDeque<SteelVal>,
}

impl<'a> IterativeDropHandler<'a> {
    pub fn bfs(drop_buffer: &'a mut VecDeque<SteelVal>) {
        // println!("Current depth: {}", DEPTH.with(|x| x.get()));

        // DEPTH.with(|x| x.set(x.get() + 1));
        IterativeDropHandler { drop_buffer }.visit();
        // DEPTH.with(|x| x.set(x.get() - 1));
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
        match &value {
            SteelVal::BoolV(_)
            | SteelVal::NumV(_)
            | SteelVal::IntV(_)
            | SteelVal::CharV(_)
            | SteelVal::Void
            | SteelVal::StringV(_)
            | SteelVal::FuncV(_)
            | SteelVal::SymbolV(_)
            | SteelVal::FutureFunc(_)
            | SteelVal::FutureV(_)
            | SteelVal::BoxedFunction(_)
            | SteelVal::MutFunc(_)
            | SteelVal::BuiltIn(_)
            | SteelVal::ByteVector(_)
            | SteelVal::BigNum(_) => return,
            _ => {
                self.drop_buffer.push_back(value);
            }
        }
    }

    fn visit_bytevector(&mut self, _bytevector: SteelByteVector) -> Self::Output {}
    fn visit_bool(&mut self, _boolean: bool) {}
    fn visit_float(&mut self, _float: f64) {}
    fn visit_int(&mut self, _int: isize) {}
    fn visit_rational(&mut self, _: Rational32) {}
    fn visit_bigrational(&mut self, _: Gc<BigRational>) {}
    fn visit_char(&mut self, _c: char) {}
    fn visit_void(&mut self) {}
    fn visit_string(&mut self, _string: SteelString) {}
    fn visit_function_pointer(&mut self, _ptr: FunctionSignature) {}
    fn visit_symbol(&mut self, _symbol: SteelString) {}
    fn visit_port(&mut self, _port: SteelPort) {}
    fn visit_future(&mut self, _future: Gc<FutureResult>) {}
    fn visit_mutable_function(&mut self, _function: MutFunctionSignature) {}
    fn visit_complex(&mut self, _: Gc<SteelComplex>) {}
    fn visit_bignum(&mut self, _bignum: Gc<BigInt>) {}
    fn visit_future_function(&mut self, _function: BoxedAsyncFunctionSignature) {}
    fn visit_builtin_function(&mut self, _function: BuiltInSignature) {}
    fn visit_boxed_function(&mut self, _function: Gc<BoxedDynFunction>) {}

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

    fn visit_custom_type(&mut self, custom_type: GcMut<Box<dyn CustomType>>) {
        if let Ok(inner) = custom_type.try_unwrap() {
            let mut inner = inner.consume();

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
            for value in inner.fields.drain(..) {
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
    fn visit_continuation(&mut self, continuation: Continuation) {
        if let Ok(inner) = crate::gc::Shared::try_unwrap(continuation.inner).map(|x| x.consume()) {
            match inner {
                ContinuationMark::Closed(mut inner) => {
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

                ContinuationMark::Open(mut inner) => {
                    for value in inner.current_stack_values {
                        self.push_back(value);
                    }

                    if let Some(inner) = inner.current_frame.function.get_mut() {
                        for value in std::mem::take(&mut inner.captures) {
                            self.push_back(value);
                        }
                    }
                }
            }
        }
    }

    fn visit_list(&mut self, list: List<SteelVal>) {
        // println!("VISITING LIST: {}", list.strong_count());
        // println!("list: {:?}", list);

        if list.strong_count() == 1 {
            for value in list.draining_iterator() {
                // println!(
                // "PUSHING BACK VALUE - queue size: {}",
                // self.drop_buffer.len()
                // );

                // println!("enqueueing: {}", value);

                self.push_back(value);
            }
        }

        // if list.strong_count() == 1 {
        //     for value in list {
        //         self.push_back(value);
        //     }
        // }
    }

    // TODO: When this gets replaced with heap storage, then we can do this more
    // effectively!
    fn visit_mutable_vector(&mut self, _vector: HeapRef<Vec<SteelVal>>) {}

    // TODO: Once the root is added back to this, bring it back
    fn visit_boxed_iterator(&mut self, iterator: GcMut<OpaqueIterator>) {
        if let Ok(inner) = iterator.try_unwrap() {
            self.push_back(inner.consume().root)
        }
    }

    fn visit_syntax_object(&mut self, syntax_object: Gc<Syntax>) {
        if let Ok(inner) = syntax_object.try_unwrap() {
            if let Some(raw) = inner.raw {
                self.push_back(raw);
            }

            self.push_back(inner.syntax);
        }
    }

    fn visit_boxed_value(&mut self, boxed_value: GcMut<SteelVal>) {
        if let Ok(inner) = boxed_value.try_unwrap() {
            self.push_back(inner.consume());
        }
    }

    fn visit_reference_value(&mut self, reference: Gc<OpaqueReference<'static>>) {
        if let Ok(mut inner) = Gc::try_unwrap(reference) {
            inner.drop_mut(self);
        }
    }

    fn visit_heap_allocated(&mut self, _heap_ref: HeapRef<SteelVal>) -> Self::Output {}

    fn visit(&mut self) -> Self::Output {
        let mut ret = self.default_output();

        while let Some(value) = self.pop_front() {
            ret = match value {
                Closure(c) => self.visit_closure(c),
                BoolV(b) => self.visit_bool(b),
                NumV(n) => self.visit_float(n),
                IntV(i) => self.visit_int(i),
                Rational(x) => self.visit_rational(x),
                BigRational(x) => self.visit_bigrational(x),
                BigNum(b) => self.visit_bignum(b),
                Complex(x) => self.visit_complex(x),
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
                HeapAllocated(b) => self.visit_heap_allocated(b),
                Pair(p) => self.visit_pair(p),
                ByteVector(b) => self.visit_bytevector(b),
            };
        }

        // println!("--- finished draining drop queue ----");

        ret
    }

    fn visit_pair(&mut self, pair: Gc<Pair>) -> Self::Output {
        if let Ok(inner) = Gc::try_unwrap(pair) {
            self.push_back(inner.car);
            self.push_back(inner.cdr);
        }
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
                Rational(x) => self.visit_rational(x),
                BigRational(x) => self.visit_bigrational(x),
                BigNum(b) => self.visit_bignum(b),
                Complex(x) => self.visit_complex(x),
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
                HeapAllocated(b) => self.visit_heap_allocated(b),
                Pair(p) => self.visit_pair(p),
                ByteVector(b) => self.visit_bytevector(b),
            };
        }

        ret
    }

    fn visit_closure(&mut self, closure: Gc<ByteCodeLambda>) -> Self::Output;
    fn visit_bool(&mut self, _: bool) -> Self::Output;
    fn visit_float(&mut self, _: f64) -> Self::Output;
    fn visit_int(&mut self, _: isize) -> Self::Output;
    fn visit_rational(&mut self, _: Rational32) -> Self::Output;
    fn visit_bigrational(&mut self, _: Gc<BigRational>) -> Self::Output;
    fn visit_bignum(&mut self, _: Gc<BigInt>) -> Self::Output;
    fn visit_complex(&mut self, _: Gc<SteelComplex>) -> Self::Output;
    fn visit_char(&mut self, _: char) -> Self::Output;
    fn visit_immutable_vector(&mut self, vector: SteelVector) -> Self::Output;
    fn visit_void(&mut self) -> Self::Output;
    fn visit_string(&mut self, string: SteelString) -> Self::Output;
    fn visit_function_pointer(&mut self, ptr: FunctionSignature) -> Self::Output;
    fn visit_symbol(&mut self, symbol: SteelString) -> Self::Output;
    fn visit_custom_type(&mut self, custom_type: GcMut<Box<dyn CustomType>>) -> Self::Output;
    fn visit_hash_map(&mut self, hashmap: SteelHashMap) -> Self::Output;
    fn visit_hash_set(&mut self, hashset: SteelHashSet) -> Self::Output;
    fn visit_steel_struct(&mut self, steel_struct: Gc<UserDefinedStruct>) -> Self::Output;
    fn visit_port(&mut self, port: SteelPort) -> Self::Output;
    fn visit_transducer(&mut self, transducer: Gc<Transducer>) -> Self::Output;
    fn visit_reducer(&mut self, reducer: Gc<Reducer>) -> Self::Output;
    fn visit_future_function(&mut self, function: BoxedAsyncFunctionSignature) -> Self::Output;
    fn visit_future(&mut self, future: Gc<FutureResult>) -> Self::Output;
    fn visit_stream(&mut self, stream: Gc<LazyStream>) -> Self::Output;
    fn visit_boxed_function(&mut self, function: Gc<BoxedDynFunction>) -> Self::Output;
    fn visit_continuation(&mut self, continuation: Continuation) -> Self::Output;
    fn visit_list(&mut self, list: List<SteelVal>) -> Self::Output;
    fn visit_mutable_function(&mut self, function: MutFunctionSignature) -> Self::Output;
    fn visit_mutable_vector(&mut self, vector: HeapRef<Vec<SteelVal>>) -> Self::Output;
    fn visit_builtin_function(&mut self, function: BuiltInSignature) -> Self::Output;
    fn visit_boxed_iterator(&mut self, iterator: GcMut<OpaqueIterator>) -> Self::Output;
    fn visit_syntax_object(&mut self, syntax_object: Gc<Syntax>) -> Self::Output;
    fn visit_boxed_value(&mut self, boxed_value: GcMut<SteelVal>) -> Self::Output;
    fn visit_reference_value(&mut self, reference: Gc<OpaqueReference<'static>>) -> Self::Output;
    fn visit_heap_allocated(&mut self, heap_ref: HeapRef<SteelVal>) -> Self::Output;
    fn visit_pair(&mut self, pair: Gc<Pair>) -> Self::Output;
    fn visit_bytevector(&mut self, bytevector: SteelByteVector) -> Self::Output;
}

pub trait BreadthFirstSearchSteelValReferenceVisitor<'a> {
    type Output;

    fn default_output(&mut self) -> Self::Output;

    fn pop_front(&mut self) -> Option<&'a SteelVal>;

    fn push_back(&mut self, value: &'a SteelVal);

    fn visit(&mut self) -> Self::Output {
        let mut ret = self.default_output();

        while let Some(value) = self.pop_front() {
            ret = match value {
                Closure(c) => self.visit_closure(c),
                BoolV(b) => self.visit_bool(*b),
                NumV(n) => self.visit_float(*n),
                IntV(i) => self.visit_int(*i),
                Rational(x) => self.visit_rational(*x),
                BigRational(x) => self.visit_bigrational(x),
                Complex(_) => unimplemented!(),
                CharV(c) => self.visit_char(*c),
                VectorV(v) => self.visit_immutable_vector(v),
                Void => self.visit_void(),
                StringV(s) => self.visit_string(s),
                FuncV(f) => self.visit_function_pointer(*f),
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
                Pair(p) => self.visit_pair(p),
                ByteVector(b) => self.visit_bytevector(b),
            };
        }

        ret
    }

    fn visit_bytevector(&mut self, _: &'a SteelByteVector) -> Self::Output;
    fn visit_closure(&mut self, _: &'a Gc<ByteCodeLambda>) -> Self::Output;
    fn visit_bool(&mut self, _: bool) -> Self::Output;
    fn visit_float(&mut self, _: f64) -> Self::Output;
    fn visit_int(&mut self, _: isize) -> Self::Output;
    fn visit_rational(&mut self, _: Rational32) -> Self::Output;
    fn visit_bigrational(&mut self, _: &'a Gc<BigRational>) -> Self::Output;
    fn visit_bignum(&mut self, _: &'a Gc<BigInt>) -> Self::Output;
    fn visit_char(&mut self, _: char) -> Self::Output;
    fn visit_immutable_vector(&mut self, vector: &'a SteelVector) -> Self::Output;
    fn visit_void(&mut self) -> Self::Output;
    fn visit_string(&mut self, string: &'a SteelString) -> Self::Output;
    fn visit_function_pointer(&mut self, ptr: FunctionSignature) -> Self::Output;
    fn visit_symbol(&mut self, symbol: &'a SteelString) -> Self::Output;
    fn visit_custom_type(&mut self, custom_type: &'a GcMut<Box<dyn CustomType>>) -> Self::Output;
    fn visit_hash_map(&mut self, hashmap: &'a SteelHashMap) -> Self::Output;
    fn visit_hash_set(&mut self, hashset: &'a SteelHashSet) -> Self::Output;
    fn visit_steel_struct(&mut self, steel_struct: &'a Gc<UserDefinedStruct>) -> Self::Output;
    fn visit_port(&mut self, port: &'a SteelPort) -> Self::Output;
    fn visit_transducer(&mut self, transducer: &'a Gc<Transducer>) -> Self::Output;
    fn visit_reducer(&mut self, reducer: &'a Gc<Reducer>) -> Self::Output;
    fn visit_future_function(&mut self, function: &'a BoxedAsyncFunctionSignature) -> Self::Output;
    fn visit_future(&mut self, future: &'a Gc<FutureResult>) -> Self::Output;
    fn visit_stream(&mut self, stream: &'a Gc<LazyStream>) -> Self::Output;
    fn visit_boxed_function(&mut self, function: &'a Gc<BoxedDynFunction>) -> Self::Output;
    fn visit_continuation(&mut self, continuation: &'a Continuation) -> Self::Output;
    fn visit_list(&mut self, list: &'a List<SteelVal>) -> Self::Output;
    fn visit_mutable_function(&mut self, function: &'a MutFunctionSignature) -> Self::Output;
    fn visit_mutable_vector(&mut self, vector: &'a HeapRef<Vec<SteelVal>>) -> Self::Output;
    fn visit_builtin_function(&mut self, function: &'a BuiltInSignature) -> Self::Output;
    fn visit_boxed_iterator(&mut self, iterator: &'a GcMut<OpaqueIterator>) -> Self::Output;
    fn visit_syntax_object(&mut self, syntax_object: &'a Gc<Syntax>) -> Self::Output;
    fn visit_boxed_value(&mut self, boxed_value: &'a GcMut<SteelVal>) -> Self::Output;
    fn visit_reference_value(
        &mut self,
        reference: &'a Gc<OpaqueReference<'static>>,
    ) -> Self::Output;
    fn visit_heap_allocated(&mut self, heap_ref: &'a HeapRef<SteelVal>) -> Self::Output;
    fn visit_pair(&mut self, pair: &'a Gc<Pair>) -> Self::Output;
}

thread_local! {
    static LEFT_QUEUE: RefCell<Vec<SteelVal>> = RefCell::new(Vec::with_capacity(128));
    static RIGHT_QUEUE: RefCell<Vec<SteelVal>> = RefCell::new(Vec::with_capacity(128));
    static VISITED_SET: RefCell<fxhash::FxHashSet<usize>> = RefCell::new(fxhash::FxHashSet::default());
    static EQ_DEPTH: Cell<usize> = Cell::new(0);
}

fn increment_eq_depth() {
    #[cfg(feature = "sandbox")]
    EQ_DEPTH.with(|x| x.set(x.get() + 1));
}

fn decrement_eq_depth() {
    #[cfg(feature = "sandbox")]
    EQ_DEPTH.with(|x| x.set(x.get() - 1));
}

fn reset_eq_depth() {
    #[cfg(feature = "sandbox")]
    EQ_DEPTH.with(|x| x.set(0));
}

fn eq_depth() -> usize {
    #[cfg(feature = "sandbox")]
    return EQ_DEPTH.with(|x| x.get());

    #[cfg(not(feature = "sandbox"))]
    0
}

struct RecursiveEqualityHandler<'a> {
    left: EqualityVisitor<'a>,
    right: EqualityVisitor<'a>,
    visited: &'a mut fxhash::FxHashSet<usize>,
    // found_mutable_object: bool,
}

impl<'a> RecursiveEqualityHandler<'a> {
    pub fn compare_equality(&mut self, left: SteelVal, right: SteelVal) -> bool {
        self.left.push_back(left);
        self.right.push_back(right);

        self.visit()
    }

    fn should_visit(&mut self, value: usize) -> bool {
        // if !self.found_mutable_object {
        // return true;
        // }

        if self.visited.insert(value) {
            return true;
        }

        return false;
    }

    fn visit(&mut self) -> bool {
        loop {
            let (left, right) = match (self.left.pop_front(), self.right.pop_front()) {
                (Some(l), Some(r)) => (l, r),
                (None, None) => return true,
                _ => return false,
            };

            // println!("{} - {}", left, right);

            // println!(
            //     "Queue size: {:?}",
            //     self.left.queue.len(),
            //     // self.right.queue.len()
            // );

            match (left, right) {
                (ListV(l), ListV(r)) => {
                    // If we've reached the same object, we're good
                    if l.ptr_eq(&r) || l.storage_ptr_eq(&r) {
                        continue;
                    }

                    if self.should_visit(l.elements_as_ptr_usize())
                        && self.should_visit(r.elements_as_ptr_usize())
                    {
                        if l.len() != r.len() {
                            return false;
                        }

                        for (lvalue, rvalue) in l.iter().zip(r.iter()) {
                            // TODO: @Matt - need to do optimistic checks here so we don't
                            // visit things we don't need to - basically a "check left" function
                            match (lvalue, rvalue) {
                                (SteelVal::ListV(llist), SteelVal::ListV(rlist))
                                    if (llist.is_empty() && rlist.is_empty())
                                        || llist.ptr_eq(&rlist)
                                        || llist.storage_ptr_eq(&rlist) =>
                                {
                                    continue;
                                }
                                // (SteelVal::ListV(llist), SteelVal::ListV(rlist)) if llist.len() == 1 && rlist.len() == 1 {

                                // }
                                (a, b) => {
                                    // println!("Pushing back: {}", a);

                                    self.left.push_back(a.clone());
                                    self.right.push_back(b.clone());
                                }
                            }
                        }
                    }

                    continue;
                }
                // If we run into issues with any stack overflow
                // check here first.
                (Pair(l), Pair(r)) => {
                    if Gc::ptr_eq(&l, &r) {
                        continue;
                    }

                    self.left.push_back(l.car());
                    self.right.push_back(r.car());

                    self.left.push_back(l.cdr());
                    self.right.push_back(r.cdr());
                }
                (BoolV(l), BoolV(r)) => {
                    if l != r {
                        return false;
                    }

                    continue;
                }
                (NumV(l), NumV(r)) => {
                    if l != r {
                        return false;
                    }

                    continue;
                }
                (IntV(l), IntV(r)) => {
                    if l != r {
                        return false;
                    }

                    continue;
                }
                (CharV(l), CharV(r)) => {
                    if l != r {
                        return false;
                    }

                    continue;
                }
                (VectorV(l), VectorV(r)) => {
                    if l.len() != r.len() {
                        return false;
                    }

                    // If these point to the same object, break early
                    if Gc::ptr_eq(&l.0, &r.0) {
                        continue;
                    }

                    // Should we visit these?
                    if self.should_visit(l.0.as_ptr() as usize)
                        && self.should_visit(r.0.as_ptr() as usize)
                    {
                        self.left.visit_immutable_vector(l);
                        self.right.visit_immutable_vector(r);
                    } else {
                        return false;
                    }

                    continue;
                }
                (Void, Void) => {
                    continue;
                }
                (StringV(l), StringV(r)) => {
                    if l != r {
                        return false;
                    }
                    continue;
                }
                (FuncV(l), FuncV(r)) => {
                    if l as usize != r as usize {
                        return false;
                    }
                    continue;
                }
                (MutFunc(l), MutFunc(r)) => {
                    if l as usize != r as usize {
                        return false;
                    }
                    continue;
                }
                (SymbolV(l), SymbolV(r)) => {
                    if l != r {
                        return false;
                    }
                    continue;
                }
                (SteelVal::Custom(l), SteelVal::Custom(r)) => {
                    if l.read().inner_type_id() != r.read().inner_type_id() {
                        return false;
                    }

                    if l.read().check_equality_hint(r.read().as_ref()) {
                        // Go down to the next level
                        self.left.visit_custom_type(l);
                        self.right.visit_custom_type(r);
                        continue;
                    } else {
                        return false;
                    }
                }

                (SteelVal::Custom(l), other) => {
                    if l.read().check_equality_hint_general(&other) {
                        self.left.visit_custom_type(l);

                        match other {
                            Closure(x) => self.right.visit_closure(x),
                            VectorV(v) => self.right.visit_immutable_vector(v),
                            SteelVal::Custom(r) => self.right.visit_custom_type(r),
                            HashMapV(r) => self.right.visit_hash_map(r),
                            HashSetV(r) => self.right.visit_hash_set(r),
                            CustomStruct(r) => self.right.visit_steel_struct(r),
                            PortV(r) => self.right.visit_port(r),
                            IterV(r) => self.right.visit_transducer(r),
                            ReducerV(r) => self.right.visit_reducer(r),
                            ContinuationFunction(r) => self.right.visit_continuation(r),
                            ListV(r) => self.right.visit_list(r),
                            SteelVal::Pair(r) => self.right.visit_pair(r),
                            MutableVector(r) => self.right.visit_mutable_vector(r),
                            BoxedIterator(r) => self.right.visit_boxed_iterator(r),
                            SteelVal::SyntaxObject(r) => self.right.visit_syntax_object(r),
                            Boxed(r) => self.right.visit_boxed_value(r),
                            HeapAllocated(r) => self.right.visit_heap_allocated(r),
                            _ => {}
                        }

                        continue;
                    } else {
                        return false;
                    }
                }

                (other, SteelVal::Custom(r)) => {
                    if r.read().check_equality_hint_general(&other) {
                        match other {
                            Closure(x) => self.left.visit_closure(x),
                            VectorV(v) => self.left.visit_immutable_vector(v),
                            SteelVal::Custom(r) => self.left.visit_custom_type(r),
                            HashMapV(r) => self.left.visit_hash_map(r),
                            HashSetV(r) => self.left.visit_hash_set(r),
                            CustomStruct(r) => self.left.visit_steel_struct(r),
                            PortV(r) => self.left.visit_port(r),
                            IterV(r) => self.left.visit_transducer(r),
                            ReducerV(r) => self.left.visit_reducer(r),
                            ContinuationFunction(r) => self.left.visit_continuation(r),
                            ListV(r) => self.left.visit_list(r),
                            SteelVal::Pair(r) => self.left.visit_pair(r),
                            MutableVector(r) => self.left.visit_mutable_vector(r),
                            BoxedIterator(r) => self.left.visit_boxed_iterator(r),
                            SteelVal::SyntaxObject(r) => self.left.visit_syntax_object(r),
                            Boxed(r) => self.left.visit_boxed_value(r),
                            HeapAllocated(r) => self.left.visit_heap_allocated(r),
                            _ => {}
                        }

                        self.right.visit_custom_type(r);

                        continue;
                    } else {
                        return false;
                    }
                }

                (SteelVal::HashMapV(l), SteelVal::HashMapV(r)) => {
                    if Gc::ptr_eq(&l.0, &r.0) {
                        continue;
                    }

                    if self.should_visit(l.0.as_ptr() as usize)
                        && self.should_visit(r.0.as_ptr() as usize)
                    {
                        if l.len() != r.len() {
                            return false;
                        }

                        // TODO: Implicitly here we are assuming that this key was even hashable
                        // to begin with, since it ended up in the right spot, and didn't blow
                        // the stack on a recursive structure.
                        //
                        // This still does not handle the pathological edge case of something like
                        // (hash (hash (hash ...) value) value)
                        //
                        // In this case, we'll get a stack overflow, when trying to compare equality
                        // with these maps if they're sufficiently deep.
                        //
                        // The issue is that if the two maps are equivalent, we need to check the
                        // existence of each key in the left map with each key in the right map.
                        // Doing so invokes an equality check, where we'll then invoke this logic
                        // again. We could solve this by disallowing hashmaps as keys - then
                        // we would not the same issue where putting a hashmap into the map
                        // causes the equality checks to go off the rails.

                        if eq_depth() > 512 {
                            log::error!("Aborting eq checks before the stack overflows");

                            return false;
                        }

                        for (key, value) in l.0.iter() {
                            if let Some(right_value) = r.0.get(key) {
                                self.left.push_back(value.clone());
                                self.right.push_back(right_value.clone());
                            } else {
                                // We know that these are not equal, because the
                                // key in the left map does not exist in the right
                                return false;
                            }
                        }
                    }

                    continue;
                }
                (HashSetV(l), HashSetV(r)) => {
                    if Gc::ptr_eq(&l.0, &r.0) {
                        continue;
                    }

                    if self.should_visit(l.0.as_ptr() as usize)
                        && self.should_visit(r.0.as_ptr() as usize)
                    {
                        if l.len() != r.len() {
                            return false;
                        }
                        if eq_depth() > 512 {
                            log::error!("Aborting eq checks before the stack overflows");

                            return false;
                        }

                        for key in l.0.iter() {
                            if !l.0.contains(key) {
                                return false;
                            }
                        }
                    }

                    continue;
                }
                (CustomStruct(l), CustomStruct(r)) => {
                    // If these are the same object, just continue
                    if Gc::ptr_eq(&l, &r) {
                        continue;
                    }

                    if self.should_visit(l.as_ptr() as usize)
                        && self.should_visit(r.as_ptr() as usize)
                    {
                        // Check the top level equality indicators to make sure
                        // that these two types are the same
                        if !(l.type_descriptor == r.type_descriptor && l.name() == r.name()) {
                            return false;
                        }

                        self.left.visit_steel_struct(l);
                        self.right.visit_steel_struct(r);
                    }

                    continue;
                }
                // (PortV(_), PortV(_)) => {
                // return
                // }
                (IterV(l), IterV(r)) => {
                    self.left.visit_transducer(l);
                    self.right.visit_transducer(r);

                    continue;
                }
                (ReducerV(l), ReducerV(r)) => {
                    self.left.visit_reducer(l);
                    self.right.visit_reducer(r);

                    continue;
                }
                // FutureV(f) => self.visit_future(f),
                (ContinuationFunction(l), ContinuationFunction(r)) => {
                    if !Continuation::ptr_eq(&l, &r) {
                        return false;
                    }

                    continue;
                }
                // MutFunc(m) => self.visit_mutable_function(m),
                (BuiltIn(l), BuiltIn(r)) => {
                    if l as usize != r as usize {
                        return false;
                    }
                    continue;
                }
                // MutableVector(b) => self.visit_mutable_vector(b),
                // BoxedIterator(b) => self.visit_boxed_iterator(b),
                // SteelVal::SyntaxObject(s) => self.visit_syntax_object(s),
                // Boxed(b) => self.visit_boxed_value(b),
                // Reference(r) => self.visit_reference_value(r),
                (BigNum(l), BigNum(r)) => {
                    if l != r {
                        return false;
                    }
                    continue;
                }
                (SyntaxObject(l), SyntaxObject(r)) => {
                    if Gc::ptr_eq(&l, &r) {
                        continue;
                    }

                    self.left.visit_syntax_object(l);
                    self.right.visit_syntax_object(r);

                    continue;
                }
                (HeapAllocated(l), HeapAllocated(r)) => {
                    if HeapRef::ptr_eq(&l, &r) {
                        continue;
                    }

                    self.left.visit_heap_allocated(l);
                    self.right.visit_heap_allocated(r);

                    continue;
                }

                (MutableVector(l), MutableVector(r)) => {
                    if HeapRef::ptr_eq(&l, &r) {
                        continue;
                    }

                    self.left.visit_mutable_vector(l);
                    self.right.visit_mutable_vector(r);

                    continue;
                }
                (Closure(l), Closure(r)) => {
                    if l != r {
                        return false;
                    }

                    self.left.visit_closure(l);
                    self.right.visit_closure(r);

                    continue;
                }
                (_, _) => {
                    return false;
                }
            }

            // unreachable!();
        }
    }
}

// TODO: This _needs_ to use references. Or otherwise we'll thrash stuff on drop
pub struct EqualityVisitor<'a> {
    // Mark each node that we've visited, if we encounter any mutable objects
    // on the way, then we'll start using the visited set. But we'll optimistically
    // assume that there are no mutable objects, and we won't start using this
    // until we absolutely have to.
    // found_mutable_object: bool,
    queue: &'a mut Vec<SteelVal>,
}

impl<'a> BreadthFirstSearchSteelValVisitor for EqualityVisitor<'a> {
    type Output = ();

    fn default_output(&mut self) -> Self::Output {}

    fn pop_front(&mut self) -> Option<SteelVal> {
        self.queue.pop()
    }

    fn push_back(&mut self, value: SteelVal) {
        self.queue.push(value)
    }

    fn visit_closure(&mut self, _closure: Gc<ByteCodeLambda>) -> Self::Output {}

    // Leaf nodes, we don't need to do anything here
    fn visit_bytevector(&mut self, _bytevector: SteelByteVector) -> Self::Output {}
    fn visit_bool(&mut self, _boolean: bool) -> Self::Output {}
    fn visit_float(&mut self, _float: f64) -> Self::Output {}
    fn visit_int(&mut self, _int: isize) -> Self::Output {}
    fn visit_rational(&mut self, _: Rational32) -> Self::Output {}
    fn visit_bigrational(&mut self, _: Gc<BigRational>) -> Self::Output {}
    fn visit_bignum(&mut self, _: Gc<BigInt>) -> Self::Output {}
    fn visit_complex(&mut self, _: Gc<SteelComplex>) -> Self::Output {}
    fn visit_char(&mut self, _c: char) -> Self::Output {}
    fn visit_void(&mut self) -> Self::Output {}
    fn visit_string(&mut self, _string: SteelString) -> Self::Output {}
    fn visit_function_pointer(&mut self, _ptr: FunctionSignature) -> Self::Output {}
    fn visit_symbol(&mut self, _symbol: SteelString) -> Self::Output {}
    fn visit_port(&mut self, _port: SteelPort) -> Self::Output {}
    fn visit_boxed_function(&mut self, _function: Gc<BoxedDynFunction>) -> Self::Output {}
    fn visit_mutable_function(&mut self, _function: MutFunctionSignature) -> Self::Output {}
    fn visit_builtin_function(&mut self, _function: BuiltInSignature) -> Self::Output {}

    //
    fn visit_immutable_vector(&mut self, vector: SteelVector) -> Self::Output {
        // If we've found the mutable object, mark that this has been visited. Only
        // if self.should_visit(vector.0.as_ptr() as usize) {
        for value in vector.iter() {
            self.push_back(value.clone());
        }
        // }
    }

    // SHOULD SET MUTABLE HERE
    fn visit_custom_type(&mut self, custom_type: GcMut<Box<dyn CustomType>>) -> Self::Output {
        custom_type.read().visit_children_for_equality(self);
    }

    fn visit_hash_map(&mut self, _hashmap: SteelHashMap) -> Self::Output {
        // TODO: See comment above
    }

    fn visit_hash_set(&mut self, _hashset: SteelHashSet) -> Self::Output {
        // TODO: See comment above
    }

    fn visit_steel_struct(&mut self, steel_struct: Gc<UserDefinedStruct>) -> Self::Output {
        // if self.should_visit(steel_struct.as_ptr() as usize) {
        for value in steel_struct.fields.iter() {
            self.push_back(value.clone());
        }
        // }
    }

    fn visit_transducer(&mut self, transducer: Gc<Transducer>) -> Self::Output {
        for transducer in transducer.ops.iter() {
            match transducer.clone() {
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

    fn visit_reducer(&mut self, reducer: Gc<Reducer>) -> Self::Output {
        match reducer.as_ref().clone() {
            Reducer::ForEach(f) => self.push_back(f),
            Reducer::Generic(rf) => {
                self.push_back(rf.initial_value);
                self.push_back(rf.function);
            }
            _ => {}
        }
    }

    fn visit_future_function(&mut self, _function: BoxedAsyncFunctionSignature) -> Self::Output {}
    fn visit_future(&mut self, _future: Gc<FutureResult>) -> Self::Output {}

    fn visit_stream(&mut self, _stream: Gc<LazyStream>) -> Self::Output {}

    fn visit_continuation(&mut self, _continuation: Continuation) -> Self::Output {}

    fn visit_list(&mut self, list: List<SteelVal>) -> Self::Output {
        for value in list.iter() {
            self.push_back(value.clone());
        }
    }

    fn visit_mutable_vector(&mut self, vector: HeapRef<Vec<SteelVal>>) -> Self::Output {
        for value in vector.get().iter() {
            self.push_back(value.clone());
        }
    }

    fn visit_boxed_iterator(&mut self, _iterator: GcMut<OpaqueIterator>) -> Self::Output {}

    fn visit_syntax_object(&mut self, syntax_object: Gc<Syntax>) -> Self::Output {
        if let Some(raw) = syntax_object.raw.clone() {
            self.push_back(raw);
        }

        self.push_back(syntax_object.syntax.clone());
    }

    fn visit_boxed_value(&mut self, boxed_value: GcMut<SteelVal>) -> Self::Output {
        self.push_back(boxed_value.read().clone());
    }

    fn visit_reference_value(&mut self, _reference: Gc<OpaqueReference<'static>>) -> Self::Output {}

    // Should set mutable here
    fn visit_heap_allocated(&mut self, heap_ref: HeapRef<SteelVal>) -> Self::Output {
        // self.found_mutable_object = true;

        // if self.should_visit(heap_ref.as_ptr_usize()) {
        self.push_back(heap_ref.get());
        // }
    }

    fn visit_pair(&mut self, pair: Gc<Pair>) -> Self::Output {
        self.push_back(pair.car());
        self.push_back(pair.cdr());
    }
}

impl PartialEq for SteelVal {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Void, Void) => true,
            (BoolV(l), BoolV(r)) => l == r,
            (IntV(l), IntV(r)) => l == r,
            (NumV(l), NumV(r)) => l == r,
            (Rational(l), Rational(r)) => l == r,
            (BigRational(l), BigRational(r)) => l == r,
            (BigNum(l), BigNum(r)) => l == r,
            (Complex(l), Complex(r)) => l == r,
            (StringV(l), StringV(r)) => l == r,
            (SymbolV(l), SymbolV(r)) => l == r,
            (CharV(l), CharV(r)) => l == r,
            (FuncV(l), FuncV(r)) => *l as usize == *r as usize,
            (ByteVector(l), ByteVector(r)) => l == r,
            // (VectorV(l), VectorV(r)) => l == r,
            // (ListV(l), ListV(r)) => l == r,
            // (HashSetV(l), HashSetV(r)) => l == r,
            // (HashMapV(l), HashMapV(r)) => l == r,
            // (Closure(l), Closure(r)) => l == r,
            // (IterV(l), IterV(r)) => l == r,
            // (ListV(l), ListV(r)) => l == r,
            // (CustomStruct(l), CustomStruct(r)) => l == r,
            // (Custom(l), Custom(r)) => Gc::ptr_eq(l, r),
            // (HeapAllocated(l), HeapAllocated(r)) => l.get() == r.get(),
            (left, right) => {
                LEFT_QUEUE.with(|left_queue| {
                    RIGHT_QUEUE.with(|right_queue| {
                        VISITED_SET.with(|visited_set| {
                            match (
                                left_queue.try_borrow_mut(),
                                right_queue.try_borrow_mut(),
                                visited_set.try_borrow_mut(),
                            ) {
                                (Ok(mut left_queue), Ok(mut right_queue), Ok(mut visited_set)) => {
                                    let mut equality_handler = RecursiveEqualityHandler {
                                        left: EqualityVisitor {
                                            queue: &mut left_queue,
                                        },
                                        right: EqualityVisitor {
                                            queue: &mut right_queue,
                                        },
                                        visited: &mut visited_set,
                                    };

                                    let res = equality_handler
                                        .compare_equality(left.clone(), right.clone());

                                    reset_eq_depth();

                                    // Clean up!
                                    equality_handler.left.queue.clear();
                                    equality_handler.right.queue.clear();
                                    equality_handler.visited.clear();

                                    res
                                }
                                _ => {
                                    let mut left_queue = Vec::new();
                                    let mut right_queue = Vec::new();

                                    let mut visited_set = fxhash::FxHashSet::default();

                                    increment_eq_depth();

                                    let mut equality_handler = RecursiveEqualityHandler {
                                        left: EqualityVisitor {
                                            queue: &mut left_queue,
                                        },
                                        right: EqualityVisitor {
                                            queue: &mut right_queue,
                                        },
                                        visited: &mut visited_set,
                                    };

                                    let res = equality_handler
                                        .compare_equality(left.clone(), right.clone());

                                    decrement_eq_depth();

                                    res
                                }
                            }
                        })
                    })
                })
            }
        }
    }
}
