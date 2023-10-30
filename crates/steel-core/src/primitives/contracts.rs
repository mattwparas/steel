// use crate::values::contracts::*;
// use crate::{builtin_stop, stop};
// use crate::{
//     rvals::{Result, SteelVal},
//     steel_vm::vm::VmCore,
// };

// pub const MAKE_C: SteelVal = SteelVal::FuncV(make_c);
// pub const MAKE_DEPENDENT_CONTRACT: SteelVal = SteelVal::FuncV(make_dependent_contract);
// pub const MAKE_FLAT_CONTRACT: SteelVal = SteelVal::FuncV(make_flat_contract);
// pub const MAKE_FUNCTION_CONTRACT: SteelVal = SteelVal::FuncV(make_function_contract);
// pub const BIND_CONTRACT_TO_FUNCTION: SteelVal = SteelVal::BuiltIn(bind_contract_to_function);

// pub fn make_c(args: &[SteelVal]) -> Result<SteelVal> {
//     if args.is_empty() {
//         stop!(ArityMismatch => "make/c given no arguments");
//     }

//     let contract = args[0].clone();
//     if contract.is_contract() {
//         return Ok(contract);
//     }

//     if args.len() == 2 {
//         let function = args[0].clone();
//         let name = args[1].clone();

//         if function.is_function() {
//             return FlatContract::new_from_steelval(function, name.to_string());
//         }
//     }

//     if let Some((last, elements)) = args.split_last() {
//         let last = last.clone();
//         FunctionContract::new_from_steelval(elements, last)
//     } else {
//         stop!(ArityMismatch => "function contract missing range position")
//     }
// }

// pub fn make_dependent_contract(args: &[SteelVal]) -> Result<SteelVal> {
//     if let Some((last, elements)) = args.split_last() {
//         let last = last.clone();
//         DependentContract::new_from_steelvals(elements, last)
//     } else {
//         stop!(ArityMismatch => "function contract missing range position")
//     }
// }

// pub fn make_flat_contract(args: &[SteelVal]) -> Result<SteelVal> {
//     if args.len() != 2 {
//         stop!(ArityMismatch => "make/c requires 2 argments, the contract and the name")
//     }

//     let function = args[0].clone();
//     let name = args[1].clone();

//     if let SteelVal::SymbolV(s) = name {
//         FlatContract::new_from_steelval(function, s.to_string())
//     } else {
//         stop!(TypeMismatch => "make-flat/c requires a symbol for the name in the second position")
//     }
// }

// pub fn make_function_contract(args: &[SteelVal]) -> Result<SteelVal> {
//     if let Some((last, elements)) = args.split_last() {
//         let last = last.clone();
//         FunctionContract::new_from_steelval(elements, last)
//     } else {
//         stop!(ArityMismatch => "function contract missing range position")
//     }
// }

// pub fn bind_contract_to_function(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
//     if args.len() < 2 || args.len() > 4 {
//         builtin_stop!(ArityMismatch => "bind/c requires 2 arguments, a contract and a function")
//     }

//     let contract = args[0].clone();
//     let function = args[1].clone();

//     if !ctx.thread.runtime_options.contracts_on {
//         return Some(Ok(function));
//     }

//     let name = args.get(2).cloned();

//     Some(ContractedFunction::new_from_steelvals(
//         contract, function, name,
//     ))
// }
