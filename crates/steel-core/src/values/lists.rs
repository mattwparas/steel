use std::cell::Cell;

use im_lists::handler::{DefaultDropHandler, DropHandler};

use crate::{
    rvals::{FromSteelVal, IntoSteelVal},
    SteelVal,
};

// TODO:
// Builtin immutable pairs
#[derive(Clone)]
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
    pub static DEPTH: Cell<usize> = Cell::new(0);
}

#[cfg(not(feature = "without-drop-protection"))]
mod list_drop_handler {

    use std::collections::VecDeque;

    use super::*;

    pub struct ListDropHandler;

    use crate::rvals::cycles::{drop_impls::DROP_BUFFER, IterativeDropHandler};

    impl DropHandler<im_lists::list::GenericList<SteelVal, im_lists::shared::RcPointer, 4, 2, Self>>
        for ListDropHandler
    {
        fn drop_handler(
            obj: &mut im_lists::list::GenericList<
                SteelVal,
                im_lists::shared::RcPointer,
                4,
                2,
                Self,
            >,
        ) {
            if obj.strong_count() == 1 {
                if obj.is_empty() {
                    return;
                }

                if DROP_BUFFER
                    .try_with(|drop_buffer| {
                        if let Ok(mut drop_buffer) = drop_buffer.try_borrow_mut() {
                            // Optimistically check what these values are. If they're
                            // primitives, then we can just skip pushing them back
                            // entirely.
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
                                    | SteelVal::BigNum(_) => continue,
                                    _ => {
                                        drop_buffer.push_back(value);
                                    }
                                }
                            }

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
                                    | SteelVal::BigNum(_) => continue,
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
                            | SteelVal::BigNum(_) => continue,
                            _ => {
                                drop_buffer.push_back(value);
                            }
                        }
                    }

                    IterativeDropHandler::bfs(&mut drop_buffer);
                }
            }

            // DEPTH.with(|x| x.set(x.get() - 1));
        }
    }
}

pub type SteelList<T> =
    im_lists::list::GenericList<T, im_lists::shared::RcPointer, 4, 2, DefaultDropHandler>;

// TODO: Change this to just be something like `SteelList`
pub type List<T> =
    im_lists::list::GenericList<T, im_lists::shared::RcPointer, 4, 2, DropHandlerChoice>;

pub type ConsumingIterator<T> =
    im_lists::list::ConsumingIter<T, im_lists::shared::RcPointer, 4, 2, DropHandlerChoice>;

impl<T: FromSteelVal + Clone, D: im_lists::handler::DropHandler<Self>> FromSteelVal
    for im_lists::list::GenericList<T, im_lists::shared::RcPointer, 4, 2, D>
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
    for im_lists::list::GenericList<T, im_lists::shared::RcPointer, 4, 2, D>
{
    fn into_steelval(self) -> crate::rvals::Result<SteelVal> {
        self.into_iter()
            .map(|x| x.into_steelval())
            .collect::<crate::rvals::Result<List<_>>>()
            .map(SteelVal::ListV)
    }
}
