use crate::gc::Gc;
use crate::rvals::{Result, SteelVal};
// use itertools::Itertools;
use std::collections::HashMap;
use std::fmt;

use crate::parser::ast::IteratorExtensions;

use super::functions::ByteCodeLambda;

/// Flat contracts are simply predicates to apply to a value. These can be immediately applied
/// at attachment to a value.
#[derive(Clone, PartialEq)]
pub struct FlatContract {
    /// Steel Function of any kind
    pub(crate) predicate: SteelVal,
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
            stop!(TypeMismatch => format!("flat contracts require a function argument, found {predicate}"));
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

// (x) (>= x) -- contains a vector of the arguments and then the contract
#[derive(Clone, PartialEq)]
pub(crate) struct DependentPair {
    pub(crate) argument_name: String,
    pub(crate) arguments: Vec<String>,
    pub(crate) thunk: Gc<ByteCodeLambda>,
    pub(crate) thunk_name: String,
}

impl DependentPair {
    fn new(
        argument_name: String,
        arguments: Vec<String>,
        thunk: Gc<ByteCodeLambda>,
        thunk_name: String,
    ) -> Self {
        DependentPair {
            argument_name,
            arguments,
            thunk,
            thunk_name,
        }
    }
}

impl fmt::Display for DependentPair {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "[{} ({}) {}]",
            self.argument_name,
            self.arguments.iter().join(" "),
            self.thunk_name
        )
    }
}

// The arg positions -> which argument maps to which index in the input
// the pre-conditions - array of dependent pairs
// post-condition - dependent pair
// the rest is the same
#[derive(Clone, PartialEq)]
pub struct DependentContract {
    pub(crate) arg_positions: HashMap<String, usize>,
    pub(crate) pre_conditions: Box<[DependentPair]>,
    pub(crate) post_condition: DependentPair,
    pub(crate) contract_attachment_location: Option<String>,
    parent: Option<Gc<FunctionKind>>,
}

fn parse_list(lst: SteelVal) -> Result<(String, Vec<String>, Gc<ByteCodeLambda>, String)> {
    if let SteelVal::ListV(l) = lst {
        let mut iter = l.into_iter();

        let ident = iter
            .next()
            .ok_or_else(throw!(ArityMismatch => "make-dependent-function/c expected a symbol in the first position"))?
            .symbol_or_else(throw!(TypeMismatch => "make-dependent-function/c expected a symbol in the first position"))?
            .to_string();

        let raw_arguments = iter.next().ok_or_else(throw!(ArityMismatch => "make-dependent-function/c expected a list in the second position"))?;

        let arguments = match &raw_arguments {
            SteelVal::ListV(l) => {
                l.iter()
                .map(|x| x.clone_symbol_or_else(throw!(TypeMismatch => "make-dependent-function/c expected a symbol in the list of arguments")))
                .collect::<Result<Vec<_>>>()
            }
            _ => stop!(TypeMismatch => format!("make-dependent-function/c expected a list of symbols, found: {raw_arguments}")),
        }?;

        let contract = iter
            .next()
            .ok_or_else(throw!(ArityMismatch => "make-dependent-function/c expected a contract in the third position"))?
            .closure_or_else(throw!(TypeMismatch => "make-dependent-function/c expected a contract in the third position"))?;

        let thunk_name = iter
            .next()
            .ok_or_else(throw!(ArityMismatch => "make-dependent-function/c expected a name in the fourth position"))?
            .to_string();

        if iter.next().is_some() {
            stop!(ArityMismatch => "make-dependent-function/c condition expects 4 arguments, found (at least) 5");
        }

        Ok((ident, arguments, contract, thunk_name))
    } else {
        stop!(TypeMismatch => "make-dependent-function/c expects a list");
    }
}

impl DependentContract {
    pub(crate) fn new_from_steelvals(
        pre_condition_lists: &[SteelVal],
        post_condition_list: SteelVal,
    ) -> Result<SteelVal> {
        let mut arg_positions = HashMap::new();
        let mut pre_conditions = Vec::new();

        for (index, pre_condition) in pre_condition_lists.iter().enumerate() {
            let (ident, arguments, contract, thunk_name) = parse_list(pre_condition.clone())?;

            // Insert the index of the name in order
            arg_positions.insert(ident.clone(), index);

            if index == 0 && !arguments.is_empty() {
                stop!(ArityMismatch => "dependent contract depends on values not present!");
            }

            if index != 0 {
                for argument in &arguments {
                    if !arg_positions.contains_key(argument) {
                        stop!(Generic => "dependent contract must depend on variable in scope!");
                    }
                }
            }

            let pre_condition = DependentPair::new(ident, arguments, contract, thunk_name);
            pre_conditions.push(pre_condition);
        }

        let post_condition = {
            let (ident, arguments, contract, thunk_name) = parse_list(post_condition_list)?;

            for argument in &arguments {
                if !arg_positions.contains_key(argument) {
                    stop!(Generic => "dependent contract result (range) condition must depend on one of arguments (domain)");
                }
            }

            DependentPair::new(ident, arguments, contract, thunk_name)
        };

        let pre_conditions = pre_conditions.into_boxed_slice();
        let dep_contract = DependentContract {
            arg_positions,
            pre_conditions,
            post_condition,
            contract_attachment_location: None,
            parent: None,
        };

        Ok(SteelVal::Contract(Gc::new(ContractType::Function(
            FunctionKind::Dependent(dep_contract),
        ))))
    }
}

impl Contract for DependentContract {
    fn arity(&self) -> usize {
        self.pre_conditions.len()
    }

    fn set_parent(&mut self, p: Gc<FunctionKind>) {
        self.parent = Some(p);
    }

    fn parent(&self) -> Option<Gc<FunctionKind>> {
        self.parent.as_ref().map(Gc::clone)
    }

    fn set_attachment_location(&mut self, loc: Option<String>) {
        self.contract_attachment_location = loc
    }
}

pub trait Contract {
    fn set_parent(&mut self, p: Gc<FunctionKind>);
    fn parent(&self) -> Option<Gc<FunctionKind>>;
    fn set_attachment_location(&mut self, loc: Option<String>);
    fn arity(&self) -> usize;
}

impl fmt::Display for DependentContract {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "(->i ({}) {})",
            self.pre_conditions.iter().map(|x| x.to_string()).join(" "),
            self.post_condition
        )
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
    parent: Option<Gc<FunctionKind>>,
}

impl Contract for FunctionContract {
    fn arity(&self) -> usize {
        self.pre_conditions.len()
    }

    fn set_parent(&mut self, p: Gc<FunctionKind>) {
        self.parent = Some(p);
    }

    fn parent(&self) -> Option<Gc<FunctionKind>> {
        self.parent.as_ref().map(Gc::clone)
    }

    fn set_attachment_location(&mut self, loc: Option<String>) {
        self.contract_attachment_location = loc
    }
}

impl fmt::Display for FunctionContract {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "(-> {} {})",
            self.pre_conditions.iter().map(|x| x.to_string()).join(" "),
            *self.post_condition
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
            stop!(TypeMismatch => "function contract range expected a contract, found: {}", post_condition)
        };

        Ok(FunctionContract::new(pre_conditions, post_condition, None, None).into())
    }

    pub fn pre_conditions(&self) -> &[Gc<ContractType>] {
        &self.pre_conditions
    }

    pub fn post_condition(&self) -> &Gc<ContractType> {
        &self.post_condition
    }

    pub fn new(
        pre_conditions: Box<[Gc<ContractType>]>,
        post_condition: Gc<ContractType>,
        contract_attachment_location: Option<String>,
        parent: Option<Gc<FunctionKind>>,
    ) -> Self {
        FunctionContract {
            pre_conditions,
            post_condition,
            contract_attachment_location,
            parent,
        }
    }
}

impl From<FunctionContract> for SteelVal {
    fn from(val: FunctionContract) -> SteelVal {
        SteelVal::Contract(Gc::new(ContractType::Function(FunctionKind::Basic(val))))
    }
}

/// The contract type. `Flat` contracts apply to reified values (non functions)
/// `Function` contracts apply to exactly that - functions.
#[derive(Clone, PartialEq)]
pub enum ContractType {
    Flat(FlatContract),
    Function(FunctionKind),
}

#[derive(Clone, PartialEq)]
pub enum FunctionKind {
    Basic(FunctionContract),
    Dependent(DependentContract),
}

impl Contract for FunctionKind {
    fn arity(&self) -> usize {
        match self {
            Self::Basic(fc) => fc.arity(),
            Self::Dependent(dc) => dc.arity(),
        }
    }

    fn set_parent(&mut self, p: Gc<FunctionKind>) {
        match self {
            Self::Basic(fc) => fc.set_parent(p),
            Self::Dependent(dc) => dc.set_parent(p),
        }
    }

    fn parent(&self) -> Option<Gc<FunctionKind>> {
        match self {
            Self::Basic(fc) => fc.parent(),
            Self::Dependent(dc) => dc.parent(),
        }
    }

    fn set_attachment_location(&mut self, loc: Option<String>) {
        match self {
            Self::Basic(fc) => fc.set_attachment_location(loc),
            Self::Dependent(dc) => dc.set_attachment_location(loc),
        }
    }
}

impl fmt::Display for FunctionKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Basic(fc) => write!(f, "{fc}"),
            Self::Dependent(dc) => write!(f, "{dc}"),
        }
    }
}

impl fmt::Display for ContractType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Flat(flat) => write!(f, "{flat}"),
            Self::Function(fc) => write!(f, "{fc}"),
        }
    }
}

/// Represents a Steel function wrapped with a contract
/// Contains the contract, the function, and the name of the contract (for blaming)
#[derive(Clone)]
pub struct ContractedFunction {
    pub contract: FunctionKind,
    pub function: SteelVal,
    pub name: Option<String>,
}

impl PartialEq for ContractedFunction {
    fn eq(&self, other: &Self) -> bool {
        self.contract == other.contract
    }
}

impl ContractedFunction {
    pub fn new(contract: FunctionKind, function: SteelVal, name: Option<String>) -> Self {
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
            Some(SteelVal::SymbolV(s)) => Some(s.to_string()),
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
