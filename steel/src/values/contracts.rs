use crate::gc::Gc;
use crate::rerrs::{ErrorKind, SteelErr};
use crate::rvals::{Result, SteelVal};
use itertools::Itertools;
use std::fmt;

/// Flat contracts are simply predicates to apply to a value. These can be immediately applied
/// at attachment to a value.
#[derive(Clone, PartialEq)]
pub struct FlatContract {
    /// Steel Function of any kind
    predicate: SteelVal,
    /// Name of the function for blaming purposes
    pub(crate) name: String,
}

impl fmt::Display for FlatContract {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl FlatContract {
    #[inline(always)]
    pub fn new_from_steelval(predicate: SteelVal, name: String) -> Result<SteelVal> {
        if predicate.is_contract() {
            Ok(predicate)
        } else if predicate.is_function() {
            Ok(FlatContract::new(predicate, name).into())
        } else {
            stop!(TypeMismatch => format!("flat contracts require a function argument, found {}", predicate.to_string()));
        }
    }

    pub fn new(predicate: SteelVal, name: String) -> Self {
        FlatContract { predicate, name }
    }

    pub fn predicate(&self) -> &SteelVal {
        &self.predicate
    }
}

impl From<FlatContract> for SteelVal {
    fn from(val: FlatContract) -> SteelVal {
        SteelVal::Contract(Gc::new(ContractType::Flat(val)))
    }
}

/// Struct for function contracts. Contains all of the necessary information
/// for contract evaluation and blaming, including the pre and post conditions, the contract
/// attachment location, and the parent contract from which this contract was derived (if any)
#[derive(Clone, PartialEq)]
pub struct FunctionContract {
    /// List of pre conditions, required to be list of ContractType
    pre_conditions: Box<[Gc<ContractType>]>,
    /// Post condition, required to be a contract type
    post_condition: Gc<ContractType>,
    /// Location/Name of contract attachment
    pub(crate) contract_attachment_location: Option<String>,
    /// Stack of function contracts to also abide by, checked at application
    parent: Option<Gc<FunctionContract>>,
}

impl fmt::Display for FunctionContract {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "(-> {} {})",
            self.pre_conditions.iter().map(|x| x.to_string()).join(" "),
            self.post_condition.to_string()
        )
    }
}

impl FunctionContract {
    #[inline(always)]
    pub fn new_from_steelval(
        pre_conditions: &[SteelVal],
        post_condition: SteelVal,
    ) -> Result<SteelVal> {
        let pre_conditions = pre_conditions
            .iter()
            .map(|x| {
                if let SteelVal::Contract(c) = x {
                    Ok(c.clone()) // TODO find out how to remove this clone
                } else {
                    stop!(TypeMismatch => "Function contract domain requires a list of contracts")
                }
            })
            .collect::<Result<Box<_>>>()?;

        let post_condition = if let SteelVal::Contract(c) = post_condition {
            c
        } else {
            stop!(TypeMismatch => "function contract range expected a contract")
        };

        Ok(FunctionContract::new(pre_conditions, post_condition, None, None).into())
    }

    pub fn set_parent(&mut self, p: Gc<FunctionContract>) {
        self.parent = Some(p);
    }

    pub fn parent(&self) -> Option<Gc<FunctionContract>> {
        (&self.parent).as_ref().map(Gc::clone)
    }

    pub fn set_attachment_location(&mut self, loc: Option<String>) {
        self.contract_attachment_location = loc
    }

    pub fn new(
        pre_conditions: Box<[Gc<ContractType>]>,
        post_condition: Gc<ContractType>,
        contract_attachment_location: Option<String>,
        parent: Option<Gc<FunctionContract>>,
    ) -> Self {
        FunctionContract {
            pre_conditions,
            post_condition,
            contract_attachment_location,
            parent,
        }
    }

    pub fn arity(&self) -> usize {
        self.pre_conditions.len()
    }

    pub fn pre_conditions(&self) -> &[Gc<ContractType>] {
        &self.pre_conditions
    }

    pub fn post_condition(&self) -> &Gc<ContractType> {
        &self.post_condition
    }
}

impl From<FunctionContract> for SteelVal {
    fn from(val: FunctionContract) -> SteelVal {
        SteelVal::Contract(Gc::new(ContractType::Function(val)))
    }
}

/// The contract type. `Flat` contracts apply to reified values (non functions)
/// `Function` contracts apply to exactly that - functions.
#[derive(Clone, PartialEq)]
pub enum ContractType {
    Flat(FlatContract),
    Function(FunctionContract),
}

impl fmt::Display for ContractType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Flat(flat) => write!(f, "{}", flat),
            Self::Function(fc) => write!(f, "{}", fc),
        }
    }
}

/// Represents a Steel function wrapped with a contract
/// Contains the contract, the function, and the name of the contract (for blaming)
#[derive(Clone)]
pub struct ContractedFunction {
    pub contract: FunctionContract,
    pub function: SteelVal,
    pub name: Option<String>,
}

impl PartialEq for ContractedFunction {
    fn eq(&self, other: &Self) -> bool {
        self.contract == other.contract
    }
}

impl ContractedFunction {
    pub fn new(contract: FunctionContract, function: SteelVal, name: Option<String>) -> Self {
        ContractedFunction {
            contract,
            function,
            name,
        }
    }

    pub fn arity(&self) -> Option<usize> {
        if let SteelVal::Closure(func) = &self.function {
            Some(func.arity())
        } else {
            None
        }
    }

    #[inline(always)]
    pub fn new_from_steelvals(
        contract: SteelVal,
        function: SteelVal,
        name: Option<SteelVal>,
    ) -> Result<SteelVal> {
        let name = match name {
            Some(SteelVal::SymbolV(s)) => Some(s.unwrap()),
            Some(_) => stop!(TypeMismatch => "bind/c expected a symbol in the first position"),
            None => None,
        };

        let contract = if let SteelVal::Contract(fc) = contract {
            if let ContractType::Function(fc) = fc.as_ref() {
                fc.clone()
            } else {
                stop!(TypeMismatch => "bind/c requires a function contract")
            }
        } else {
            stop!(TypeMismatch => "bind/c requires a function contract")
        };

        if !function.is_function() {
            stop!(TypeMismatch => "bind/c requires a function");
        }

        // let function = if let SteelVal::Closure(b) = function {
        //     b.clone()
        // } else {
        //     stop!(TypeMismatch => "bind/c requires a bytecode function, not a primitive")
        // };

        // Check the arity only if we have it
        if let SteelVal::Closure(function) = &function {
            if contract.arity() != function.arity() {
                stop!(TypeMismatch => format!("contract did not match function arity: function has arity: {}, contract has arity: {}", function.arity(), contract.arity()));
            }
        }

        Ok(ContractedFunction::new(contract, function, name).into())
    }
}

impl From<ContractedFunction> for SteelVal {
    fn from(val: ContractedFunction) -> Self {
        SteelVal::ContractedFunction(Gc::new(val))
    }
}
