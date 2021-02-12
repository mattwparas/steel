use crate::gc::Gc;
use crate::rerrs::SteelErr;
use crate::rvals::{Result, SteelVal};
use crate::stop;

// use crate::rvals::Transducer;
// use crate::rvals::Transducers;

use crate::contracts::*;

pub struct ContractOperations {}

impl ContractOperations {
    pub fn make_flat_contract() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() != 2 {
                stop!(ArityMismatch => "make/c requires 2 argments, the contract and the name")
            }

            let function = Gc::clone(&args[0]);
            let name = Gc::clone(&args[1]);

            if let SteelVal::SymbolV(s) = name.as_ref() {
                FlatContract::new_from_steelval(function, s.to_string())
            } else {
                stop!(TypeMismatch => "make-flat/c requires a symbol for the name in the second position")
            }
        })
    }

    pub fn make_function_contract() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if let Some((last, elements)) = args.split_last() {
                let last = Gc::clone(last);
                FunctionContract::new_from_steelval(elements, last)
            } else {
                stop!(ArityMismatch => "function contract missing range position")
            }
        })
    }

    pub fn bind_contract_to_function() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() != 2 {
                stop!(ArityMismatch => "bind/c requires 2 arguments, a contract and a function")
            }

            let contract = Gc::clone(&args[0]);
            let function = Gc::clone(&args[1]);

            ContractedFunction::new_from_steelvals(contract, function)
        })
    }
}
