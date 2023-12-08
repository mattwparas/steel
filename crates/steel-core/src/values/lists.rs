use std::cell::Cell;

use im_lists::handler::DropHandler;

use crate::SteelVal;

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

    impl
        DropHandler<
            im_lists::list::GenericList<SteelVal, im_lists::shared::RcPointer, 256, 1, Self>,
        > for ListDropHandler
    {
        fn drop_handler(
            obj: &mut im_lists::list::GenericList<
                SteelVal,
                im_lists::shared::RcPointer,
                256,
                1,
                Self,
            >,
        ) {
            // println!("CALLING DROP HANDLER: {}", obj.strong_count());
            // DEPTH.with(|x| x.set(x.get() + 1));
            // println!("Current depth: {}", DEPTH.with(|x| x.get()));

            if obj.strong_count() == 1 {
                if obj.len() == 0 {
                    // println!("Early returning");
                    // DEPTH.with(|x| x.set(x.get() - 1));
                    return;
                }

                // println!("Doing stuff...");

                if DROP_BUFFER
                    .try_with(|drop_buffer| {
                        if let Ok(mut drop_buffer) = drop_buffer.try_borrow_mut() {
                            for value in std::mem::take(obj).draining_iterator() {
                                drop_buffer.push_back(value);
                            }

                            IterativeDropHandler::bfs(&mut drop_buffer);
                        } else {
                            let mut drop_buffer = VecDeque::new();

                            for value in std::mem::take(obj) {
                                drop_buffer.push_back(value);
                            }

                            IterativeDropHandler::bfs(&mut drop_buffer);
                        }
                    })
                    .is_err()
                {
                    let mut drop_buffer = VecDeque::new();
                    for value in std::mem::take(obj).draining_iterator() {
                        drop_buffer.push_back(value);
                    }

                    IterativeDropHandler::bfs(&mut drop_buffer);
                }
            }

            // DEPTH.with(|x| x.set(x.get() - 1));
        }
    }
}

// TODO: Change this to just be something like `SteelList`
pub type List<T> =
    im_lists::list::GenericList<T, im_lists::shared::RcPointer, 256, 1, DropHandlerChoice>;

pub type ConsumingIterator<T> =
    im_lists::list::ConsumingIter<T, im_lists::shared::RcPointer, 256, 1, DropHandlerChoice>;
