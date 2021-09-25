use std::{cell::RefCell, cmp::Ordering, rc::Weak};

use crate::SteelVal;

// Upvalues themselves need to be stored on the heap
// Consider a separate section for them on the heap, or wrap them in a wrapper
// before allocating on the heap
#[derive(Clone, Debug)]
pub struct UpValue {
    // Either points to a stack location, or the value
    pub(crate) location: Location,
    // The next upvalue in the sequence
    pub(crate) next: Option<Weak<RefCell<UpValue>>>,
    // Reachable
    pub(crate) reachable: bool,
}

impl PartialEq for UpValue {
    fn eq(&self, other: &Self) -> bool {
        self.location == other.location
    }
}

impl PartialOrd for UpValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (&self.location, &other.location) {
            (Location::Stack(l), Location::Stack(r)) => Some(l.cmp(r)),
            _ => panic!("Cannot compare two values on the heap"),
        }
    }
}

impl UpValue {
    // Given a reference to the stack, either get the value from the stack index
    // Or snag the steelval stored inside the upvalue
    pub(crate) fn get_value(&self, stack: &[SteelVal]) -> SteelVal {
        // println!("Getting value from: {:?}", self.location);
        // println!("Stack: {:?}", stack);
        match self.location {
            Location::Stack(idx) => stack[idx].clone(),
            Location::Closed(ref v) => v.clone(),
        }
    }

    pub(crate) fn try_move_value(&mut self, stack: &mut [SteelVal]) -> SteelVal {
        match self.location {
            Location::Stack(idx) => std::mem::replace(&mut stack[idx], SteelVal::Void),
            Location::Closed(ref v) => v.clone(),
        }
    }

    pub(crate) fn is_reachable(&self) -> bool {
        self.reachable
    }

    // Given a reference to the stack, either get the value from the stack index
    // Or snag the steelval stored inside the upvalue
    pub(crate) fn mutate_value(&mut self, stack: &mut [SteelVal], value: SteelVal) -> SteelVal {
        match self.location {
            Location::Stack(idx) => {
                let old = stack[idx].clone();
                stack[idx] = value;
                old
            }
            Location::Closed(ref v) => {
                let old = v.clone();
                self.location = Location::Closed(value);
                old
            }
        }
    }

    pub(crate) fn get_value_if_closed(&self) -> Option<&SteelVal> {
        if let Location::Closed(ref v) = self.location {
            Some(v)
        } else {
            None
        }
    }

    pub(crate) fn set_value(&mut self, val: SteelVal) {
        self.location = Location::Closed(val);
    }

    pub(crate) fn mark_reachable(&mut self) {
        self.reachable = true;
    }

    pub(crate) fn reset(&mut self) {
        self.reachable = false;
    }

    pub(crate) fn is_open(&self) -> bool {
        matches!(self.location, Location::Stack(_))
    }

    pub(crate) fn index(&self) -> Option<usize> {
        if let Location::Stack(idx) = &self.location {
            Some(*idx)
        } else {
            None
        }
    }

    pub(crate) fn new(stack_index: usize, next: Option<Weak<RefCell<UpValue>>>) -> Self {
        UpValue {
            location: Location::Stack(stack_index),
            next,
            reachable: false,
        }
    }

    pub(crate) fn set_next(&mut self, next: Weak<RefCell<UpValue>>) {
        self.next = Some(next);
    }
}

// Either points to a stack index, or it points to a SteelVal directly
// When performing an OPCODE::GET_UPVALUE, index into the array in the current
// function being executed in the stack frame, and pull it in
#[derive(Clone, PartialEq, Debug)]
pub(crate) enum Location {
    Stack(usize),
    Closed(SteelVal),
}
