use crate::contracts::*;
use crate::rerrs::SteelErr;
use crate::rvals::{Result, SteelVal};
use crate::stop;

pub struct ContractOperations {}

impl ContractOperations {
    pub fn make_c() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.is_empty() {
                stop!(ArityMismatch => "make/c given no arguments");
            }

            let contract = args[0].clone();
            if contract.is_contract() {
                return Ok(contract);
            }

            if args.len() == 2 {
                let function = args[0].clone();
                let name = args[1].clone();

                if function.is_function() {
                    return FlatContract::new_from_steelval(function, name.to_string());

                    // if let SteelVal::SymbolV(s) = name.as_ref() {
                    //     return FlatContract::new_from_steelval(function, s.to_string());
                    // } else {
                    //     stop!(TypeMismatch => "make/c attempted to make a flat contract, expected a symbol for the name in the second position");
                    // }
                }
            }

            if let Some((last, elements)) = args.split_last() {
                let last = last.clone();
                FunctionContract::new_from_steelval(elements, last)
            } else {
                stop!(ArityMismatch => "function contract missing range position")
            }
        })
    }

    pub fn make_flat_contract() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 2 {
                stop!(ArityMismatch => "make/c requires 2 argments, the contract and the name")
            }

            let function = args[0].clone();
            let name = args[1].clone();

            if let SteelVal::SymbolV(s) = name {
                FlatContract::new_from_steelval(function, s.to_string())
            } else {
                stop!(TypeMismatch => "make-flat/c requires a symbol for the name in the second position")
            }
        })
    }

    pub fn make_function_contract() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if let Some((last, elements)) = args.split_last() {
                let last = last.clone();
                FunctionContract::new_from_steelval(elements, last)
            } else {
                stop!(ArityMismatch => "function contract missing range position")
            }
        })
    }

    pub fn bind_contract_to_function() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() < 2 || args.len() > 4 {
                stop!(ArityMismatch => "bind/c requires 2 arguments, a contract and a function")
            }

            let contract = args[0].clone();
            let function = args[1].clone();

            let name = args.get(2).map(|x| x.clone());

            ContractedFunction::new_from_steelvals(contract, function, name)
        })
    }
}
