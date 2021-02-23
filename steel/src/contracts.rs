use crate::rerrs::SteelErr;
use crate::rvals::{Result, SteelVal};
use crate::{gc::Gc, rvals::ByteCodeLambda};
use itertools::Itertools;
use std::fmt;

#[derive(Clone, PartialEq)]
pub struct FlatContract {
    // function of any kind
    predicate: Gc<SteelVal>,
    // name of the function for blaming purposes
    pub name: String,
}

impl fmt::Display for FlatContract {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl FlatContract {
    #[inline(always)]
    pub fn new_from_steelval(predicate: Gc<SteelVal>, name: String) -> Result<Gc<SteelVal>> {
        if predicate.is_contract() {
            Ok(predicate)
        } else if predicate.is_function() {
            Ok(Gc::new(FlatContract::new(predicate, name).into()))
        } else {
            stop!(TypeMismatch => format!("flat contracts require a function argument, found {}", predicate.to_string()));
        }
    }

    pub fn new(predicate: Gc<SteelVal>, name: String) -> Self {
        FlatContract { predicate, name }
    }

    pub fn predicate(&self) -> &Gc<SteelVal> {
        &self.predicate
    }
}

impl From<FlatContract> for SteelVal {
    fn from(val: FlatContract) -> SteelVal {
        SteelVal::Contract(ContractType::Flat(val))
    }
}

#[derive(Clone, PartialEq)]
pub struct FunctionContract {
    // List of pre conditions, required to be list of ContractType
    pre_conditions: Box<[ContractType]>,
    // Post condition, required to be a contract type
    post_condition: Gc<ContractType>,
    // Location/Name of contract attachment
    pub contract_attachment_location: Option<String>,
    // Stack of function contracts to also abide by, checked at application
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
        pre_conditions: &[Gc<SteelVal>],
        post_condition: Gc<SteelVal>,
    ) -> Result<Gc<SteelVal>> {
        let pre_conditions = pre_conditions
            .iter()
            .map(|x| {
                if let SteelVal::Contract(c) = x.as_ref() {
                    Ok(c.clone()) // TODO find out how to remove this clone
                } else {
                    stop!(TypeMismatch => "Function contract domain requires a list of contracts")
                }
            })
            .collect::<Result<Box<_>>>()?;

        let post_condition = if let SteelVal::Contract(c) = post_condition.as_ref() {
            c.clone()
        } else {
            stop!(TypeMismatch => "function contract range expected a contract")
        };

        Ok(Gc::new(
            FunctionContract::new(pre_conditions, Gc::new(post_condition), None, None).into(),
        ))
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
        pre_conditions: Box<[ContractType]>,
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

    pub fn pre_conditions(&self) -> &[ContractType] {
        &self.pre_conditions
    }

    pub fn post_condition(&self) -> &Gc<ContractType> {
        &self.post_condition
    }
}

impl From<FunctionContract> for SteelVal {
    fn from(val: FunctionContract) -> SteelVal {
        SteelVal::Contract(ContractType::Function(val))
    }
}

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

#[derive(Clone)]
pub struct ContractedFunction {
    pub contract: FunctionContract,
    pub function: ByteCodeLambda,
    pub name: Option<String>,
}

impl PartialEq for ContractedFunction {
    fn eq(&self, other: &Self) -> bool {
        self.contract == other.contract
    }
}

impl ContractedFunction {
    pub fn new(contract: FunctionContract, function: ByteCodeLambda, name: Option<String>) -> Self {
        ContractedFunction {
            contract,
            function,
            name,
        }
    }

    pub fn arity(&self) -> usize {
        self.function.arity()
    }

    #[inline(always)]
    pub fn new_from_steelvals(
        contract: Gc<SteelVal>,
        function: Gc<SteelVal>,
        name: Option<Gc<SteelVal>>,
    ) -> Result<Gc<SteelVal>> {
        let name = match name.as_ref().map(|x| x.as_ref()) {
            Some(SteelVal::SymbolV(s)) => Some(s.unwrap()),
            Some(_) => stop!(TypeMismatch => "bind/c expected a symbol in the first position"),
            None => None,
        };

        let contract = if let SteelVal::Contract(ContractType::Function(fc)) = contract.as_ref() {
            fc.clone()
        } else {
            stop!(TypeMismatch => "bind/c requires a function contract")
        };

        let function = if let SteelVal::Closure(b) = function.as_ref() {
            b.clone()
        } else {
            stop!(TypeMismatch => "bind/c requires a bytecode function, not a primitive")
        };

        if contract.arity() != function.arity() {
            stop!(TypeMismatch => "contract did not match function arity");
        }

        Ok(Gc::new(
            ContractedFunction::new(contract, function, name).into(),
        ))
    }
}

impl From<ContractedFunction> for SteelVal {
    fn from(val: ContractedFunction) -> Self {
        SteelVal::ContractedFunction(Box::new(val))
    }
}
