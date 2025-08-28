use std::cell::Cell;

use im_lists::{
    handler::{DefaultDropHandler, DropHandler},
    shared::PointerFamily,
};

use crate::{
    gc::Gc,
    rvals::{FromSteelVal, IntoSteelVal},
    SteelVal,
};

// TODO:
// Builtin immutable pairs
#[derive(Clone, Hash)]
pub struct Pair {
    pub(crate) car: SteelVal,
    pub(crate) cdr: SteelVal,
}

impl Pair {
    pub fn cons(car: SteelVal, cdr: SteelVal) -> Self {
        Pair { car, cdr }
    }

    pub fn car(&self) -> SteelVal {
        self.car.clone()
    }

    pub fn cdr(&self) -> SteelVal {
        self.cdr.clone()
    }

    pub fn car_ref(&self) -> &SteelVal {
        &self.car
    }

    pub fn cdr_ref(&self) -> &SteelVal {
        &self.cdr
    }
}

impl From<Pair> for SteelVal {
    fn from(pair: Pair) -> Self {
        SteelVal::Pair(Gc::new(pair))
    }
}

impl std::fmt::Debug for Pair {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} . {})", &self.car, &self.cdr)
    }
}

#[cfg(feature = "without-drop-protection")]
type DropHandlerChoice = im_lists::handler::DefaultDropHandler;
#[cfg(not(feature = "without-drop-protection"))]
type DropHandlerChoice = list_drop_handler::ListDropHandler;

thread_local! {
    pub static DEPTH: Cell<usize> = const { Cell::new(0) };
}

pub struct GcPointerType;

impl PointerFamily for GcPointerType {
    type Pointer<T: 'static> = Gc<T>;

    fn new<T: 'static>(value: T) -> Self::Pointer<T> {
        Gc::new(value)
    }

    fn strong_count<T: 'static>(this: &Self::Pointer<T>) -> usize {
        Gc::strong_count(this)
    }

    fn try_unwrap<T: 'static>(this: Self::Pointer<T>) -> Option<T> {
        Gc::try_unwrap(this).ok()
    }

    fn get_mut<T: 'static>(this: &mut Self::Pointer<T>) -> Option<&mut T> {
        Gc::get_mut(this)
    }

    fn ptr_eq<T: 'static>(this: &Self::Pointer<T>, other: &Self::Pointer<T>) -> bool {
        Gc::ptr_eq(this, other)
    }

    fn make_mut<T: Clone + 'static>(ptr: &mut Self::Pointer<T>) -> &mut T {
        Gc::make_mut(ptr)
    }

    fn clone<T: 'static>(ptr: &Self::Pointer<T>) -> Self::Pointer<T> {
        Gc::clone(ptr)
    }

    fn as_ptr<T: 'static>(this: &Self::Pointer<T>) -> *const T {
        Gc::as_ptr(this)
    }

    fn into_raw<T: 'static>(this: Self::Pointer<T>) -> *const T {
        Gc::into_raw(this)
    }

    unsafe fn from_raw<T: 'static>(this: *const T) -> Self::Pointer<T> {
        Gc::from_raw(this)
    }
}

#[cfg(not(feature = "without-drop-protection"))]
mod list_drop_handler {

    use std::collections::VecDeque;

    use super::*;

    pub struct ListDropHandler;

    use crate::rvals::cycles::{drop_impls::DROP_BUFFER, IterativeDropHandler};

    impl DropHandler<im_lists::list::GenericList<SteelVal, PointerType, 4, 2, Self>>
        for ListDropHandler
    {
        #[inline(always)]
        fn drop_handler(obj: &mut im_lists::list::GenericList<SteelVal, PointerType, 4, 2, Self>) {
            if obj.is_empty() {
                return;
            }

            if obj.strong_count() == 1 {
                if DROP_BUFFER
                    .try_with(|drop_buffer| {
                        if let Ok(mut drop_buffer) = drop_buffer.try_borrow_mut() {
                            let taken = std::mem::take(obj);

                            for value in taken.draining_iterator() {
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
                                    | SteelVal::BigNum(_)
                                    | SteelVal::MutableVector(_) => continue,
                                    SteelVal::ListV(l) => {
                                        // println!("Value: {}", l.strong_count());
                                        if l.strong_count() == 1 {
                                            drop_buffer.push_back(value);
                                        }
                                    }
                                    _ => {
                                        drop_buffer.push_back(value);
                                    }
                                }
                            }

                            // println!("{:?}", now.elapsed());

                            IterativeDropHandler::bfs(&mut drop_buffer);
                        } else {
                            let mut drop_buffer = VecDeque::new();

                            for value in std::mem::take(obj).draining_iterator() {
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
                                    | SteelVal::BigNum(_)
                                    | SteelVal::MutableVector(_) => continue,

                                    SteelVal::ListV(l) => {
                                        if l.strong_count() == 1 {
                                            drop_buffer.push_back(value);
                                        }
                                    }

                                    _ => {
                                        drop_buffer.push_back(value);
                                    }
                                }
                            }

                            IterativeDropHandler::bfs(&mut drop_buffer);
                        }
                    })
                    .is_err()
                {
                    let mut drop_buffer = VecDeque::new();
                    for value in std::mem::take(obj).draining_iterator() {
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
                            | SteelVal::BigNum(_)
                            | SteelVal::MutableVector(_) => continue,
                            SteelVal::ListV(l) => {
                                if l.strong_count() == 1 {
                                    drop_buffer.push_back(value);
                                }
                            }
                            _ => {
                                drop_buffer.push_back(value);
                            }
                        }
                    }

                    IterativeDropHandler::bfs(&mut drop_buffer);
                }
            }
        }
    }
}

type PointerType = GcPointerType;

pub type SteelList<T> = im_lists::list::GenericList<T, PointerType, 4, 2, DefaultDropHandler>;

pub type List<T> = im_lists::list::GenericList<T, PointerType, 4, 2, DropHandlerChoice>;

pub(crate) type CellPointer<T> = im_lists::list::RawCell<T, PointerType, 4, 2, DropHandlerChoice>;

pub type ConsumingIterator<T> =
    im_lists::list::ConsumingIter<T, PointerType, 4, 2, DropHandlerChoice>;

impl<T: FromSteelVal + Clone, D: im_lists::handler::DropHandler<Self>> FromSteelVal
    for im_lists::list::GenericList<T, PointerType, 4, 2, D>
{
    fn from_steelval(val: &SteelVal) -> crate::rvals::Result<Self> {
        if let SteelVal::ListV(l) = val {
            l.iter().map(T::from_steelval).collect()
        } else {
            stop!(TypeMismatch => "Unable to convert SteelVal to List, found: {}", val);
        }
    }
}

impl<T: IntoSteelVal + Clone, D: im_lists::handler::DropHandler<Self>> IntoSteelVal
    for im_lists::list::GenericList<T, PointerType, 4, 2, D>
{
    fn into_steelval(self) -> crate::rvals::Result<SteelVal> {
        self.into_iter()
            .map(|x| x.into_steelval())
            .collect::<crate::rvals::Result<List<_>>>()
            .map(SteelVal::ListV)
    }
}
