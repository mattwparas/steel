use crate::gc::Gc;
use crate::jit::code_gen::JIT;
use crate::jit::value::{decode, to_encoded_double};
use crate::steel_vm::stack::StackFrame;
use crate::SteelVal;

// If the argument is native, use it on its own
// I should know based on the type signature and return value, whether inputs need to be treated as native
// or if they need to be boxed back up again
pub enum ArgType {
    Int,
    Float,
    Bool,
    Steel,
}

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

pub struct JitFunctionPointer {
    signature: Sig,
    fn_ptr: *const u8,
}

impl JitFunctionPointer {
    pub(crate) fn new(signature: Sig, fn_ptr: *const u8) -> Self {
        JitFunctionPointer { signature, fn_ptr }
    }
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
                let input = stack.pop().expect("Empty stack!");
                let coerced = Gc::new(input);
                let func: fn(isize) -> isize = std::mem::transmute(fn_ptr);
                let output = func(coerced.as_ptr() as isize);
                let coerced_back = if let Some(inner) = (output as *const SteelVal).as_ref() {
                    inner.clone()
                } else {
                    panic!("Illegal value returned from JIT")
                };

                JIT::free();
                return coerced_back;
            }
            Sig::Two => {
                let func: fn(f64, f64) -> f64 = std::mem::transmute(fn_ptr);

                let second = stack.pop().expect("Empty stack!");
                let first = stack.pop().expect("Empty stack!");

                let coerced_second = Gc::new(second);
                let coerced_first = Gc::new(first);

                let output = func(
                    to_encoded_double(&coerced_first),
                    to_encoded_double(&coerced_second),
                );

                let coerced_back = decode(output);

                // let coerced_back = if let Some(inner) = (output as *const SteelVal).as_ref() {
                //     inner.clone()
                // } else {
                //     panic!("Illegal value returned from JIT")
                // };

                JIT::free();
                return coerced_back;
            }
            Sig::Three => {
                // let func: fn(isize, isize, isize) -> isize = std::mem::transmute(fn_ptr);
                unimplemented!()
            }
            Sig::Four => todo!(),
            Sig::Five => todo!(),
            Sig::Six => todo!(),
            Sig::Seven => todo!(),
            Sig::Eight => todo!(),
            Sig::Nine => todo!(),
            Sig::Ten => todo!(),
        }
    }
}
