use crate::gc::Gc;
use crate::jit::code_gen::JIT;
use crate::jit::value::{decode, to_encoded_double};
use crate::steel_vm::stack::StackFrame;
use crate::SteelVal;

#[derive(Clone, Copy)]
pub enum Sig {
    NoArgs = 0,
    One,
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
    Ten,
}

impl Sig {
    pub fn from_usize(arity: usize) -> Self {
        use Sig::*;
        match arity {
            0 => NoArgs,
            1 => One,
            2 => Two,
            3 => Three,
            4 => Four,
            5 => Five,
            6 => Six,
            7 => Seven,
            8 => Eight,
            9 => Nine,
            10 => Ten,
            _ => panic!("Unable to construct function pointer with more than 10 arguments"),
        }
    }
}

#[derive(Clone)]
pub struct JitFunctionPointer {
    signature: Sig,
    fn_ptr: *const u8,
}

impl JitFunctionPointer {
    pub(crate) fn new(signature: Sig, fn_ptr: *const u8) -> Self {
        JitFunctionPointer { signature, fn_ptr }
    }

    pub(crate) fn arity(&self) -> usize {
        (self.signature as u8) as usize
    }
}

macro_rules! pop_values_and_call {
    ($func:expr, $stack:expr => $($param:ident),* => $($reference:ident),*) => {
        {

            $(
                let $param = Gc::new($stack.pop().expect("Empty stack"));
            )*

            let output = decode($func($(
                to_encoded_double(&$reference)
            ,)*));

            JIT::free();

            return output;
        }
    };
}

impl JitFunctionPointer {
    pub fn call_func(&self, stack: &mut StackFrame) -> SteelVal {
        unsafe { self.call_func_unsafe(stack) }
    }

    unsafe fn call_func_unsafe(&self, stack: &mut StackFrame) -> SteelVal {
        // Get at the internal function pointer that this points to
        // We'll need to transform it into an actual function pointer later
        let fn_ptr = self.fn_ptr;

        match self.signature {
            // If it has no args, it can't construct anything
            // Should just be an integer
            Sig::NoArgs => {
                let func: fn() -> isize = std::mem::transmute(fn_ptr);
                let output = func();
                SteelVal::IntV(output)
            }
            Sig::One => {
                let func: fn(f64) -> f64 = std::mem::transmute(fn_ptr);
                pop_values_and_call!(func, stack => first => first);
            }
            Sig::Two => {
                let func: fn(f64, f64) -> f64 = std::mem::transmute(fn_ptr);

                pop_values_and_call!(func, stack => first, second => second, first);
            }
            Sig::Three => {
                let func: fn(f64, f64, f64) -> f64 = std::mem::transmute(fn_ptr);
                pop_values_and_call!(func, stack => first, second, third => third, second, first);
            }
            Sig::Four => {
                let func: fn(f64, f64, f64, f64) -> f64 = std::mem::transmute(fn_ptr);
                pop_values_and_call!(func, stack => first, second, third, fourth => fourth, third, second, first);
            }
            Sig::Five => {
                let func: fn(f64, f64, f64, f64, f64) -> f64 = std::mem::transmute(fn_ptr);
                pop_values_and_call!(func, stack => first, second, third, fourth, fifth => fifth, fourth, third, second, first);
            }
            Sig::Six => {
                let func: fn(f64, f64, f64, f64, f64, f64) -> f64 = std::mem::transmute(fn_ptr);
                pop_values_and_call!(func, stack => first, second, third, fourth, fifth, sixth => sixth, fifth, fourth, third, second, first);
            }
            Sig::Seven => {
                let func: fn(f64, f64, f64, f64, f64, f64, f64) -> f64 =
                    std::mem::transmute(fn_ptr);
                pop_values_and_call!(func, stack => first, second, third, fourth, fifth, sixth, seventh => seventh, sixth, fifth, fourth, third, second, first);
            }
            Sig::Eight => {
                let func: fn(f64, f64, f64, f64, f64, f64, f64, f64) -> f64 =
                    std::mem::transmute(fn_ptr);
                pop_values_and_call!(func, stack => first, second, third, fourth, fifth, sixth, seventh, eigth => eigth, seventh, sixth, fifth, fourth, third, second, first);
            }
            Sig::Nine => todo!(),
            Sig::Ten => todo!(),
        }
    }
}
