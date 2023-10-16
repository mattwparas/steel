use im_lists::handler::DropHandler;

use crate::{
    rvals::cycles::{self, IterativeDropHandler},
    SteelVal,
};

pub struct ListDropHandler;

impl DropHandler<im_lists::list::GenericList<SteelVal, im_lists::shared::RcPointer, 256, 1, Self>>
    for ListDropHandler
{
    fn drop_handler(
        obj: &mut im_lists::list::GenericList<SteelVal, im_lists::shared::RcPointer, 256, 1, Self>,
    ) {
        if obj.strong_count() == 1 {
            cycles::DROP_BUFFER
                .try_with(|drop_buffer| {
                    if let Ok(mut drop_buffer) = drop_buffer.try_borrow_mut() {
                        for value in std::mem::take(obj).draining_iterator() {
                            drop_buffer.push_back(value);
                        }

                        IterativeDropHandler::bfs(&mut drop_buffer);
                    }
                })
                .ok();
        }
    }
}

// TODO: Change this to just be something like `SteelList`
pub type List<T> =
    im_lists::list::GenericList<T, im_lists::shared::RcPointer, 256, 1, ListDropHandler>;

pub type ConsumingIterator<T> = im_lists::list::ConsumingIter<
    T,
    im_lists::shared::RcPointer,
    256,
    1,
    crate::values::lists::ListDropHandler,
>;
