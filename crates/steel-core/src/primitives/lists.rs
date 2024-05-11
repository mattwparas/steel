use crate::{
    gc::Gc,
    steel_vm::{
        builtin::{Arity, BuiltInFunctionType, BuiltInModule, NativeFunctionDefinition},
        vm::{apply, VmContext, APPLY_DOC},
    },
    values::lists::Pair,
};
use crate::{
    rvals::{IntoSteelVal, Result, SteelVal},
    steel_vm::vm::VmCore,
};
use crate::{stop, throw};

use crate::values::lists::List;

use crate::core::utils::{
    arity_check, declare_const_mut_ref_functions, declare_const_ref_functions,
};

declare_const_ref_functions! {
    LIST_TO_STRING => steel_list_to_string,
}

declare_const_mut_ref_functions! {
    PUSH_BACK => push_back,
}

pub struct UnRecoverableResult(Result<SteelVal>);

impl IntoSteelVal for UnRecoverableResult {
    #[inline(always)]
    fn into_steelval(self) -> Result<SteelVal> {
        self.into()
    }
}

impl From<UnRecoverableResult> for Result<SteelVal> {
    fn from(value: UnRecoverableResult) -> Self {
        value.0
    }
}

impl From<Result<SteelVal>> for UnRecoverableResult {
    fn from(value: Result<SteelVal>) -> Self {
        UnRecoverableResult(value)
    }
}

/// Lists in Steel have an interface that matches those of classic schemes or lisps.
/// At face value, they appear to be implemented as cons cells - however, under the hood
/// they are actually implemented as unrolled linked lists.
///
/// This means that for most practical purposes, interaction with lists is the same.
/// That being said, there are no improper lists, meaning, pairs are actually just lists of two elements.
///
/// Indexing into a list also takes O(n/64) - which means you'll get constant time indexing on small lists.
///
/// ```scheme
/// (list 10 20 30 40) ;; => '(10 20 30 40)
/// ```
#[steel_derive::define_module(name = "steel/lists")]
pub fn list_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/lists");

    module
        .register_native_fn_definition(NEW_DEFINITION)
        .register_native_fn_definition(CONS_DEFINITION)
        .register_native_fn_definition(RANGE_DEFINITION)
        .register_native_fn_definition(LENGTH_DEFINITION)
        .register_native_fn_definition(LAST_DEFINITION)
        .register_native_fn_definition(IS_EMPTY_DEFINITION)
        .register_native_fn_definition(FIRST_DEFINITION)
        .register_native_fn_definition(CAR_DEFINITION)
        .register_native_fn_definition(CDR_DEFINITION)
        .register_native_fn_definition(REST_DEFINITION)
        .register_native_fn_definition(APPEND_DEFINITION)
        .register_native_fn_definition(REVERSE_DEFINITION)
        .register_native_fn_definition(LIST_REF_DEFINITION)
        .register_native_fn_definition(TRY_LIST_REF_DEFINITION)
        .register_value("list->string", crate::primitives::lists::LIST_TO_STRING)
        .register_value("push-back", crate::primitives::lists::PUSH_BACK)
        .register_native_fn_definition(PAIR_DEFINITION)
        .register_native_fn_definition(NativeFunctionDefinition {
            name: "apply",
            func: BuiltInFunctionType::Context(apply),
            arity: Arity::Exact(2),
            doc: Some(APPLY_DOC),
            is_const: true,
            signature: None,
        })
        .register_value("transduce", crate::steel_vm::transducers::TRANSDUCE)
        .register_native_fn_definition(SECOND_DEFINITION)
        .register_native_fn_definition(THIRD_DEFINITION)
        .register_native_fn_definition(TAKE_DEFINITION)
        .register_native_fn_definition(LIST_TAIL_DEFINITION)
        .register_native_fn_definition(CDR_IS_NULL_DEFINITION);

    module
}

/// Get the second element of the list. Raises an error if the list does not have an element in the second position.
///
/// (second l) -> any/c
///
/// * l : list?
///
/// # Examples
///
/// ```scheme
/// > (second '(1 2 3)) ;; => 2
/// > (second '())
/// error[E11]: Generic
///         ┌─ :1:2
///         │
///         1 │ (second '())
///         │  ^^^^^^ second: index out of bounds - list did not have an element in the second position: []
#[steel_derive::function(name = "second", constant = true)]
pub fn second(list: &List<SteelVal>) -> Result<SteelVal> {
    list.get(1).cloned().ok_or_else(throw!(Generic => "second: index out of bounds - list did not have an element in the second position: {:?}", list))
}

/// Get the third element of the list. Raises an error if the list does not have an element in the third position.
///
/// (third l) -> any/c
///
/// * l : list?
///
/// # Examples
/// ```scheme
/// > (third '(1 2 3)) ;; => 3
/// > (third '())
/// error[E11]: Generic
///        ┌─ :1:2
///        │
///        1 │ (third '())
///        │  ^^^^^^ third: index out of bounds - list did not have an element in the second position: []
/// ```
#[steel_derive::function(name = "third", constant = true)]
pub(crate) fn third(list: &List<SteelVal>) -> Result<SteelVal> {
    list.get(2).cloned().ok_or_else(throw!(Generic => "third: Index out of bounds - list did not have an element in the second position: {:?}", list))
}

fn _test_map(ctx: &mut VmCore, args: &[SteelVal]) -> Result<SteelVal> {
    arity_check!(test_map, args, 2);

    let mut arg_iter = args.iter();
    let arg1 = arg_iter.next().unwrap();
    let arg2 = arg_iter.next().unwrap();

    if let SteelVal::ListV(l) = arg2 {
        if arg1.is_function() {
            // unimplemented!()

            Ok(SteelVal::ListV(
                l.into_iter()
                    .map(|x| ctx.call_function_one_arg(arg1, x.clone()))
                    .collect::<Result<_>>()?,
            ))

            // ctx.call_function_one_arg_or_else(function, arg)
        } else {
            stop!(TypeMismatch => "test-map expected a function")
        }
    } else {
        stop!(TypeMismatch => "test-map expects a list")
    }
}

#[steel_derive::function(name = "list-tail")]
pub fn list_tail(list_or_pair: &SteelVal, pos: usize) -> Result<SteelVal> {
    match list_or_pair {
        SteelVal::ListV(l) => l
            .tail(pos)
            .ok_or_else(throw!(Generic => format!("list-tail expects at least {} 
                    elements in the list, found: {}", pos, l.len())))
            .map(SteelVal::ListV),
        SteelVal::Pair(p) => {
            let mut value = p.cdr();
            let mut count = 1;

            while count < pos {
                count += 1;
                value = value
                    .pair()
                    .map(|x| x.cdr())
                    .ok_or_else(throw!(Generic => format!("list-tail: index reached a 
                        non-pair: index: {} in {}", count, list_or_pair)))?;
            }

            Ok(value)
        }

        _ if pos == 0 => Ok(list_or_pair.clone()),

        _ => {
            stop!(TypeMismatch => format!("list-tail expects either a list or a pair, found: {}", list_or_pair))
        }
    }
}

/// Returns a newly allocated list containing the vs as its elements.
///
/// (list v ...) -> list?
///
/// * v : any/c
///
/// # Examples
///
/// ```scheme
/// > (list 1 2 3 4 5) ;; => '(1 2 3 4 5)
/// > (list (list 1 2) (list 3 4)) ;; => '((1 2) (3 4))
/// ```
#[steel_derive::native(name = "list", arity = "AtLeast(0)")]
pub fn new(args: &[SteelVal]) -> Result<SteelVal> {
    Ok(SteelVal::ListV(args.iter().cloned().collect()))
}

/// Checks if the list is empty
///
/// (empty? lst) -> bool?
///
/// * lst: list?
///
/// # Examples
///
/// ```scheme
/// > (empty? (list 1 2 3 4 5)) ;; => #false
/// > (empty? '()) ;; => #true
/// ```
#[steel_derive::function(name = "empty?")]
pub fn is_empty(list: &SteelVal) -> bool {
    list.list().map(|x| x.is_empty()).unwrap_or_default()
}

/// Checks if the given value can be treated as a pair.
/// Note - there are no improper lists in steel, so any list with at least one element
/// is considered a pair.
///
/// (pair? any/c) -> bool?
///
/// # Examples
///
/// ```scheme
/// > (pair? '(10 20)) ;; => #true
/// > (pair? '(10)) ;; => #true
/// > (pair? '()) ;; => #false
/// ```
#[steel_derive::function(name = "pair?")]
fn pair(list: &SteelVal) -> bool {
    match list {
        SteelVal::ListV(l) => l.iter().next().is_some(),
        SteelVal::Pair(_) => true,
        _ => false,
    }
}

/// Returns a newly allocated list whose first element is `a` and second element is `d`.
///
/// (cons a d) -> list?
///
/// * a : any/c
/// * d : any/c
///
/// # Examples
/// ```scheme
/// > (cons 1 2) ;; => '(1 . 2)
/// > (cons 1 '()) ;; => '(1)
/// ```
#[steel_derive::function(name = "cons", arity = "Exact(2)")]
pub fn cons(arg: &mut SteelVal, arg2: &mut SteelVal) -> Result<SteelVal> {
    match (std::mem::replace(arg, SteelVal::Void), arg2) {
        (left, SteelVal::ListV(right)) => {
            right.cons_mut(left);

            // Consider moving in a default value instead of cloning?
            Ok(SteelVal::ListV(right.clone()))
        }
        // Silly, but this then gives us a special "pair" that is different
        // from a real bonafide list
        (left, right) => Ok(SteelVal::Pair(Gc::new(Pair::cons(left, right.clone())))),
    }
}

#[macro_export]
/// `panic!()` in debug builds, optimization hint in release.
macro_rules! debug_unreachable {
    () => {
        if cfg!(debug_assertions) {
            std::hint::unreachable_unchecked()
        } else {
            unreachable!();
        }
    };
    ($e:expr) => {
        if cfg!(debug_assertions) {
            std::hint::unreachable_unchecked()
        } else {
            unreachable!($e);
        }
    };
}

// Warning: here be dragons
// This is really just for use cases where we absolutely, 100% know, that within the body of a function,
// it is _not possible_ for the value to be anything but a list. Eliding these checks in hot loops
// can prove to be beneficial.

// pub(crate) fn unsafe_cons(args: &mut [SteelVal]) -> Result<SteelVal> {
//     match (args[0].clone(), &mut args[1]) {
//         (left, SteelVal::ListV(right)) => {
//             right.cons_mut(left);
//             // Consider moving in a default value instead of cloning?
//             Ok(SteelVal::ListV(right.clone()))
//         }
//         _ => unsafe { debug_unreachable!() },
//     }
// }

/// Returns a newly allocated list of the elements in the range (n, m]
///
/// (range n m) -> (listof int?)
///
/// * n : int?
/// * m : int?
///
/// ```scheme
/// > (range 0 10) ;; => '(0 1 2 3 4 5 6 7 8 9)
/// ```
#[steel_derive::function(name = "range")]
fn range(lower: isize, upper: isize) -> Result<SteelVal> {
    if lower < 0 {
        stop!(Generic => "range expects a positive integer");
    }

    if upper < 0 {
        stop!(Generic => "range expects a positive integer");
    }

    Ok(SteelVal::ListV(
        (lower as usize..upper as usize)
            .into_iter()
            .map(|x| SteelVal::IntV(x as isize))
            .collect(),
    ))
}

/// Returns the length of the list.
///
/// (length l) -> int?
///
/// * l : list?
///
/// # Examples
///
/// ```scheme
/// > (length (list 10 20 30)) ;; => 3
/// ```
#[steel_derive::function(name = "length")]
fn length(list: &List<SteelVal>) -> usize {
    list.len()
}

/// Returns a list that has the same elements as `lst`, but in reverse order.
/// This function takes time proportional to the length of `lst`.
///
/// (reverse lst) -> list?
///
/// * l : list?
///
/// # Examples
/// ```scheme
/// > (reverse (list 1 2 3 4)) ;; '(4 3 2 1)
/// ```
#[steel_derive::function(name = "reverse", constant = true)]
fn reverse(arg: &mut SteelVal) -> Result<SteelVal> {
    if let SteelVal::ListV(l) = std::mem::replace(arg, SteelVal::Void) {
        Ok(SteelVal::ListV(l.reverse()))
    } else {
        stop!(TypeMismatch => "reverse expects a list")
    }
}

/// Returns the last element in the list. Takes time proportional to the length of the list.
///
/// (last l) -> any/c
///
/// * l : list?
///
/// # Examples
/// ```scheme
/// > (list (list 1 2 3 4)) ;; => 4
/// ```
#[steel_derive::function(name = "last", constant = true)]
pub fn last(list: &List<SteelVal>) -> Result<SteelVal> {
    list.last()
        .cloned()
        .ok_or_else(throw!(Generic => "last resulted in an error - empty list"))
}

/// Returns the first element of the list l.
///
/// (first l) -> any/c
///
/// * l : list?
///
/// # Examples
///
/// ```scheme
/// > (first '(1 2)) ;; => 1
/// > (first (cons 2 3)) ;; => 2
/// ```
#[steel_derive::function(name = "first", constant = true)]
fn first(list: &List<SteelVal>) -> Result<SteelVal> {
    list.car()
        .ok_or_else(throw!(Generic => "first resulted in an error - empty list"))
}

/// Returns the first element of the list l.
///
/// (car l) -> any/c
///
/// * l : list?
///
/// # Examples
///
/// ```scheme
/// > (car '(1 2)) ;; => 1
/// > (car (cons 2 3)) ;; => 2
/// ```
#[steel_derive::function(name = "car", constant = true)]
pub(crate) fn car(list: &SteelVal) -> Result<SteelVal> {
    match list {
        SteelVal::ListV(l) => l
            .car()
            .ok_or_else(throw!(Generic => "car resulted in an error - empty list")),

        SteelVal::Pair(p) => Ok(p.car()),

        _ => stop!(TypeMismatch => "car expected a list or pair, found: {}", list),
    }
}

// Optimistic check to see if the rest is null before making an allocation
#[steel_derive::native(name = "cdr-null?", constant = true, arity = "Exact(1)")]
fn cdr_is_null(args: &[SteelVal]) -> Result<SteelVal> {
    arity_check!(cdr_is_null, args, 1);

    match &args[0] {
        SteelVal::ListV(l) => {
            if l.is_empty() {
                stop!(Generic => "cdr-null? expects a non empty list");
            }
            Ok(SteelVal::BoolV(!l.cdr_exists()))
        }
        other => {
            stop!(TypeMismatch => "cdr-null? expects a list, found: {}", other);
        }
    }
}

/// Returns the rest of the list. Will raise an error if the list is empty.
///
/// (cdr l) -> list?
///
/// * l : list?
///
/// # Examples
/// ```scheme
/// > (cdr (list 10 20 30)) ;; => '(20 30)
/// > (cdr (list 10)) ;; => '()
/// > (cdr '())
/// error[E11]: Generic
///    ┌─ :1:2
///    │
///    1 │ (cdr '())
///    │  ^^^ cdr expects a non empty list
/// ```
#[steel_derive::function(name = "cdr", constant = true)]
pub(crate) fn cdr(arg: &mut SteelVal) -> Result<SteelVal> {
    match std::mem::replace(arg, SteelVal::Void) {
        SteelVal::ListV(mut l) => {
            if l.is_empty() {
                stop!(Generic => "cdr expects a non empty list");
            }

            match l.rest_mut() {
                Some(_) => Ok(SteelVal::ListV(l)),
                None => Ok(SteelVal::ListV(l)),
            }
        }

        SteelVal::Pair(p) => Ok(p.cdr()),
        arg => {
            stop!(TypeMismatch => format!("cdr expects a list, found: {}", arg))
        }
    }
}

/// Returns the rest of the list. Will raise an error if the list is empty.
///
/// (rest l) -> list?
///
/// * l : list?
///
/// # Examples
/// ```scheme
/// > (rest (list 10 20 30)) ;; => '(20 30)
/// > (rest (list 10)) ;; => '()
/// > (rest (list 10))
/// error[E11]: Generic
///    ┌─ :1:2
///    │
///    1 │ (rest '())
///    │  ^^^^ rest expects a non empty list
/// ```
#[steel_derive::function(name = "rest", constant = true, arity = "Exact(1)")]
fn rest(arg: &mut SteelVal) -> Result<SteelVal> {
    if let SteelVal::ListV(mut l) = std::mem::replace(arg, SteelVal::Void) {
        if l.is_empty() {
            stop!(Generic => "rest expects a non empty list");
        }

        match l.rest_mut() {
            Some(l) => Ok(SteelVal::ListV(l.clone())),
            None => Ok(SteelVal::ListV(l.clone())),
        }
    } else {
        stop!(TypeMismatch => "rest expects a list")
    }
}

/// Returns the first n elements of the list l as a new list.
///
/// (take l n) -> list?
///
/// * l : list?
/// * n : (and/c positive? int?)
///
/// # Examples
///
/// ```scheme
/// > (take '(1 2 3 4) 2) ;; => '(0 1)
/// > (take (range 0 10) 4) ;; => '(0 1 2 3)
/// ```
#[steel_derive::function(name = "take", constant = true)]
fn take(list: &List<SteelVal>, n: isize) -> Result<SteelVal> {
    if n < 0 {
        stop!(Generic => "take expects a positive integer")
    } else {
        Ok(SteelVal::ListV(list.take(n as usize)))
    }
}

/// Appends the given lists together. If provided with no lists, will return the empty list.
///
/// (append lst ...)
///
/// lst : list?
///
/// # Examples
/// ```scheme
/// > (append (list 1 2) (list 3 4)) ;; => '(1 2 3 4)
/// > (append) ;; => '()
/// ```
#[steel_derive::native_mut(name = "append", constant = true, arity = "AtLeast(0)")]
fn append(args: &mut [SteelVal]) -> Result<SteelVal> {
    if let Some((first, rest)) = args.split_first_mut() {
        let initial = if let SteelVal::ListV(ref mut l) = first {
            l
        } else {
            stop!(TypeMismatch => "append expects a list, found: {}", &args[0]);
        };

        for value in rest {
            if let SteelVal::ListV(r) = value {
                initial.append_mut(r.clone());
            } else {
                stop!(TypeMismatch => "append expects a list, found: {}", value);
            }
        }

        return Ok(SteelVal::ListV(initial.clone()));
    } else {
        return Ok(SteelVal::ListV(List::new()));
    }
}

#[steel_derive::function(name = "try-list-ref", constant = true)]
pub fn try_list_ref(list: &List<SteelVal>, index: isize) -> Result<SteelVal> {
    if index < 0 {
        stop!(Generic => "list-ref expects a positive integer")
    } else if let Some(l) = list.get(index as usize) {
        Ok(l.clone())
    } else {
        Ok(SteelVal::BoolV(false))
    }
}

/// Returns the value located at the given index. Will raise an error if you try to index out of bounds.
///
/// Note: Runs in time proportional to the length of the list, however lists in Steel are implemented in such a fashion that the
/// time complexity is O(n/64). Meaning, for small lists this can be constant.
///
/// (list-ref lst index) -> list?
///
/// * lst : list?
/// * index : (and/c int? positive?)
///
/// # Examples
/// ```scheme
/// > (list-ref (list 1 2 3 4) 2) ;; => 3
/// > (list-ref (range 0 100) 42) ;; => 42"
/// > (list-ref (list 1 2 3 4) 10)
/// error[E11]: Generic
///   ┌─ :1:2
///   │
/// 1 │ (list-ref (list 1 2 3 4) 10)
///   │  ^^^^^^^^ out of bounds index in list-ref - list length: 4, index: 10
/// ```
#[steel_derive::function(name = "list-ref", constant = true)]
pub fn list_ref(list: &List<SteelVal>, index: isize) -> Result<SteelVal> {
    if index < 0 {
        stop!(Generic => "list-ref expects a positive integer, found: {}", index);
    }

    list.get(index as usize)
        .cloned()
        .ok_or_else(throw!(Generic => format!("out of bounds index in list-ref - list length: {}, index: {}", list.len(), index)))
}

#[steel_derive::function(name = "list->string", constant = true)]
fn list_to_string(list: &List<SteelVal>) -> Result<SteelVal> {
    list.iter()
        .map(|x| {
            x.char_or_else(throw!(TypeMismatch => "list->string expected a list of characters"))
        })
        .collect::<Result<String>>()
        .map(|x| x.into())
        .map(SteelVal::StringV)
}

// TODO this could be broken using &mut
// need to think about it more
fn push_back(args: &mut [SteelVal]) -> Result<SteelVal> {
    arity_check!(push_back, args, 2);

    let arg = args[1].clone();

    if let SteelVal::ListV(l) = &mut args[0] {
        l.push_back(arg);

        Ok(args[0].clone())
    } else {
        stop!(TypeMismatch => "push-back expects a list")
    }
}

impl From<List<SteelVal>> for SteelVal {
    fn from(l: List<SteelVal>) -> Self {
        SteelVal::ListV(l)
    }
}

#[cfg(test)]
mod list_operation_tests {

    use super::*;
    use crate::rerrs::ErrorKind;

    #[test]
    fn cons_test_normal_input() {
        let mut args = [SteelVal::IntV(1), SteelVal::IntV(2)];
        let res = steel_cons(&mut args);

        let expected = SteelVal::Pair(Gc::new(Pair::cons(SteelVal::IntV(1), SteelVal::IntV(2))));

        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn cons_single_input() {
        let mut args = [SteelVal::IntV(1)];
        let res = steel_cons(&mut args);
        let expected = ErrorKind::ArityMismatch;
        assert_eq!(res.unwrap_err().kind(), expected);
    }

    #[test]
    fn cons_no_input() {
        let res = steel_cons(&mut []);
        let expected = ErrorKind::ArityMismatch;
        assert_eq!(res.unwrap_err().kind(), expected);
    }

    #[test]
    fn cons_with_empty_list() {
        let mut args = [SteelVal::IntV(1), SteelVal::ListV(List::new())];
        let res = steel_cons(&mut args);

        let expected = crate::list![1i32];

        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn cons_with_non_empty_vector() {
        let mut args = [
            SteelVal::IntV(1),
            SteelVal::ListV(vec![SteelVal::IntV(2)].into()),
        ];
        let res = steel_cons(&mut args);
        let expected = crate::list![1i32, 2i32];
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn car_normal_input() {
        let args = [crate::list![1i32, 2i32]];
        let res = steel_car(&args);
        let expected = SteelVal::IntV(1);
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn car_bad_input() {
        let args = [SteelVal::IntV(1)];
        let res = steel_car(&args);
        let expected = ErrorKind::TypeMismatch;
        assert_eq!(res.unwrap_err().kind(), expected);
    }

    #[test]
    fn car_too_many_args() {
        let args = [SteelVal::IntV(1), SteelVal::IntV(2)];
        let res = steel_car(&args);
        let expected = ErrorKind::ArityMismatch;
        assert_eq!(res.unwrap_err().kind(), expected);
    }

    #[test]
    fn cdr_normal_input_2_elements() {
        let mut args = [crate::list![1i32, 2i32]];
        let res = steel_cdr(&mut args);
        let expected = crate::list![2i32];
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn cdr_normal_input_3_elements() {
        let mut args = [crate::list![1i32, 2i32, 3i32]];
        let res = steel_cdr(&mut args);
        let expected = crate::list![2i32, 3i32];
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn cdr_bad_input() {
        let mut args = [SteelVal::IntV(1)];
        let res = steel_cdr(&mut args);
        let expected = ErrorKind::TypeMismatch;
        assert_eq!(res.unwrap_err().kind(), expected);
    }

    #[test]
    fn cdr_too_many_args() {
        let mut args = [SteelVal::NumV(1.0), SteelVal::NumV(2.0)];
        let res = steel_cdr(&mut args);
        let expected = ErrorKind::ArityMismatch;
        assert_eq!(res.unwrap_err().kind(), expected);
    }

    #[test]
    fn cdr_single_element_list() {
        let mut args = [SteelVal::ListV(vec![SteelVal::NumV(1.0)].into())];
        let res = steel_cdr(&mut args);
        let expected = SteelVal::ListV(List::new());
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn range_tests_arity_too_few() {
        let args = [SteelVal::IntV(1)];
        let res = steel_range(&args);
        let expected = ErrorKind::ArityMismatch;
        assert_eq!(res.unwrap_err().kind(), expected);
    }

    #[test]
    fn range_test_arity_too_many() {
        let args = [
            SteelVal::NumV(1.0),
            SteelVal::NumV(2.0),
            SteelVal::NumV(3.0),
        ];
        let res = steel_range(&args);
        let expected = ErrorKind::ArityMismatch;
        assert_eq!(res.unwrap_err().kind(), expected);
    }

    #[test]
    fn range_test_normal_input() {
        let args = [SteelVal::IntV(0), SteelVal::IntV(3)];
        let res = steel_range(&args);
        let expected =
            SteelVal::ListV(vec![SteelVal::IntV(0), SteelVal::IntV(1), SteelVal::IntV(2)].into());
        assert_eq!(res.unwrap(), expected);
    }
}
