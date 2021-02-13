use crate::rerrs::SteelErr;
use crate::rvals::{Result, SteelVal};
use crate::{gc::Gc, rvals::ByteCodeLambda};

#[derive(Clone, PartialEq)]
pub struct FlatContract {
    // function of any kind
    predicate: Gc<SteelVal>,
    // name of the function for blaming purposes
    pub name: String,
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
            FunctionContract::new(pre_conditions, Gc::new(post_condition)).into(),
        ))
    }

    pub fn new(pre_conditions: Box<[ContractType]>, post_condition: Gc<ContractType>) -> Self {
        FunctionContract {
            pre_conditions,
            post_condition,
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

#[derive(Clone)]
pub struct ContractedFunction {
    pub contract: FunctionContract,
    pub function: ByteCodeLambda,
}

impl ContractedFunction {
    pub fn new(contract: FunctionContract, function: ByteCodeLambda) -> Self {
        ContractedFunction { contract, function }
    }

    #[inline(always)]
    pub fn new_from_steelvals(
        contract: Gc<SteelVal>,
        function: Gc<SteelVal>,
    ) -> Result<Gc<SteelVal>> {
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

        Ok(Gc::new(ContractedFunction::new(contract, function).into()))
    }
}

impl From<ContractedFunction> for SteelVal {
    fn from(val: ContractedFunction) -> Self {
        SteelVal::ContractedFunction(val)
    }
}
