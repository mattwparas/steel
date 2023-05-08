use crate::steel_vm::{
    builtin::{DocTemplate, MarkdownDoc},
    vm::VmContext,
};
use crate::{
    rvals::{IntoSteelVal, Result, SteelVal},
    steel_vm::vm::VmCore,
};
use crate::{stop, throw};
use im_lists::{list, list::List};

use crate::core::utils::{
    arity_check, declare_const_mut_ref_functions, declare_const_ref_functions,
};

declare_const_ref_functions! {
    LIST => new,
    LENGTH => length,
    // NEW => new,
    REVERSE => reverse,
    LAST => last,
    // TAKE => take,
    LIST_REF => list_ref,
    TRY_LIST_REF => try_list_ref,
    RANGE => range,
    IS_EMPTY => is_empty,
    CAR => car,
    LIST_TO_STRING => list_to_string,
    // FIRST => car,
    PAIR => pair,
}

declare_const_mut_ref_functions! {
    CONS => cons,
    // FIRST => first,
    REST => rest,
    CDR => cdr,
    APPEND => append,
    PUSH_BACK => push_back,
}

// pub(crate) const TEST_MAP: SteelVal = SteelVal::BuiltIn(test_map);

// TODO replace all usages with const
// const LENGTH: SteelVal = SteelVal::FuncV(length);
// const NEW: SteelVal = SteelVal::FuncV(new);

pub(crate) struct UnRecoverableResult(Result<SteelVal>);

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

pub(crate) const LIST_MODULE_DOC: MarkdownDoc<'static> = MarkdownDoc(
    r#"

# steel/lists
    
Lists in Steel have an interface that matches those of classic schemes or lisps. At face value, they appear to be implemented as cons cells - however, under the hood they are actually implemented as unrolled linked lists.

This means that for most practical purposes, interaction with lists is the same. That being said, there are no improper lists, meaning, pairs are actually just lists of two elements.

Indexing into a list also takes O(n/64) - which means you'll get constant time indexing on small lists.

```scheme
(list 10 20 30 40) ;; => '(10 20 30 40)
```

"#,
);

pub(crate) const SECOND_DOC: DocTemplate<'static> = DocTemplate {
    signature: "(second l) -> any/c",
    params: &["l : list?"],
    description: r#"Get the second element of the list. Raises an error if the list does not have an element in the second position."#,
    examples: &[
        ("λ > (second '(1 2 3))", "=> 2"),
        ("λ > (second '())", "error[E11]: Generic
        ┌─ :1:2
        │
        1 │ (second '())
        │  ^^^^^^ second: index out of bounds - list did not have an element in the second position: []
    "),
    ],
};

pub(crate) fn second(list: &List<SteelVal>) -> UnRecoverableResult {
    list.get(1).cloned().ok_or_else(throw!(Generic => "second: index out of bounds - list did not have an element in the second position: {:?}", list)).into()
}

pub(crate) const THIRD_DOC: DocTemplate<'static> = DocTemplate {
    signature: "(third l) -> any/c",
    params: &["l : list?"],
    description: r#"Get the third element of the list. Raises an error if the list does not have an element in the second position."#,
    examples: &[
        ("λ > (third '(1 2 3))", "=> 3"),
        ("λ > (third '())", "error[E11]: Generic
        ┌─ :1:2
        │
        1 │ (third '())
        │  ^^^^^^ third: index out of bounds - list did not have an element in the second position: []
    "),
    ],
};

pub(crate) fn third(list: &List<SteelVal>) -> UnRecoverableResult {
    list.get(2).cloned().ok_or_else(throw!(Generic => "third: Index out of bounds - list did not have an element in the second position: {:?}", list)).into()
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

pub(crate) const LIST_DOC: DocTemplate<'static> = DocTemplate {
    signature: "(list v ...) -> list?",
    params: &["v: any/c"],
    description: "Returns a newly allocated list containing the vs as its elements.",
    examples: &[
        ("> (list 1 2 3 4 5)", "'(1 2 3 4)"),
        ("> (list (list 1 2) (list 3 4))", "'((1 2) (3 4))"),
    ],
};

pub fn new(args: &[SteelVal]) -> Result<SteelVal> {
    Ok(SteelVal::ListV(args.iter().cloned().collect()))
}

pub(crate) const IS_EMPTY_DOC: DocTemplate<'static> = DocTemplate {
    signature: "(empty? lst) -> bool?",
    params: &["lst: list?"],
    description: "Checks if the list is empty",
    examples: &[
        ("> (empty? (list 1 2 3 4 5))", "#false"),
        ("> (empty? '())", "#true"),
    ],
};

fn is_empty(args: &[SteelVal]) -> Result<SteelVal> {
    arity_check!(is_empty, args, 1);

    if let SteelVal::ListV(l) = &args[0] {
        Ok(l.is_empty().into())
    } else {
        stop!(TypeMismatch => "empty? expects a list")
    }
}

fn pair(args: &[SteelVal]) -> Result<SteelVal> {
    arity_check!(pair, args, 1);

    if let SteelVal::ListV(l) = &args[0] {
        Ok(l.iter().next().is_some().into())
    } else {
        Ok(SteelVal::BoolV(false))
    }
}

pub(crate) const CONS_DOC: DocTemplate<'static> = DocTemplate {
    signature: "(cons a d) -> list?",
    params: &["a : any/c", "d : any/c"],
    description: r#"Returns a newly allocated list whose first element is a and second element is d.
Note: In steel, there are only proper lists. Pairs do not exist directly. "#,
    examples: &[("> (cons 1 2)", "'(1 2)"), ("> (cons 1 '())", "'(1)")],
};

fn cons(args: &mut [SteelVal]) -> Result<SteelVal> {
    if args.len() != 2 {
        stop!(ArityMismatch => "cons takes only two arguments")
    }
    match (args[0].clone(), &mut args[1]) {
        (left, SteelVal::ListV(right)) => {
            right.cons_mut(left);
            Ok(SteelVal::ListV(right.clone()))
        }
        (left, right) => Ok(SteelVal::ListV(list![left, right.clone()])),
    }

    // match (&args[1].clone(), &mut args[0]) {
    //     (right, SteelVal::ListV(left)) => {
    //         println!("Strong count: {:?}", left.strong_count());
    //         right.cons_mut(left);
    //         Ok(SteelVal::ListV(right.clone()))
    //     }
    //     (right, left) => Ok(SteelVal::ListV(list![left, right])),
    // }
}

pub(crate) const RANGE_DOC: DocTemplate<'static> = DocTemplate {
    signature: "(range n m) -> (listof int?)",
    params: &["n : int?", "m : int?"],
    description: r#"Returns a newly allocated list of the elements in the range (n, m]."#,
    examples: &[("λ > (range 0 10)", "=> '(0 1 2 3 4 5 6 7 8 9)")],
};

fn range(args: &[SteelVal]) -> Result<SteelVal> {
    arity_check!(new_range, args, 2);

    if let (SteelVal::IntV(lower), SteelVal::IntV(upper)) = (&args[0], &args[1]) {
        if *lower < 0 {
            stop!(Generic => "range expects a positive integer");
        }

        if *upper < 0 {
            stop!(Generic => "range expects a positive integer");
        }

        Ok(SteelVal::ListV(
            (*lower as usize..*upper as usize)
                .into_iter()
                .map(|x| SteelVal::IntV(x as isize))
                .collect(),
        ))
    } else {
        stop!(ArityMismatch => "range takes two integers")
    }
}

pub(crate) const LENGTH_DOC: DocTemplate<'static> = DocTemplate {
    signature: "(length l) -> int?",
    params: &["l : list?"],
    description: r#"Returns the length of the list."#,
    examples: &[
        ("λ > (length (list 1 2 3 4 5))", "=> 5"),
        ("λ > (length (range 0 10))", "=> 10"),
    ],
};

fn length(args: &[SteelVal]) -> Result<SteelVal> {
    arity_check!(length, args, 1);

    if let SteelVal::ListV(l) = &args[0] {
        Ok(l.len().into())
    } else {
        stop!(TypeMismatch => "length expects a list, found: {:?}", &args[0])
    }
}

pub(crate) const REVERSE_DOC: DocTemplate<'static> = DocTemplate {
    signature: "(reverse l) -> list?",
    params: &["l : list?"],
    description: r#"Returns a list that has the same elements as lst, but in reverse order.
This function takes time proportional to the length of lst."#,
    examples: &[("> (reverse (list 1 2 3 4))", "=> '(4 3 2 1)")],
};

fn reverse(args: &[SteelVal]) -> Result<SteelVal> {
    arity_check!(reverse, args, 1);

    if let SteelVal::ListV(l) = &args[0] {
        Ok(SteelVal::ListV(l.clone().reverse()))
    } else {
        stop!(TypeMismatch => "reverse expects a list")
    }
}

pub(crate) const LAST_DOC: DocTemplate<'static> = DocTemplate {
    signature: "(last l) -> any/c",
    params: &["l : list?"],
    description: r#"Returns the last element in the list.
Takes time proportional to the length of the list."#,
    examples: &[("> (last (list 1 2 3 4))", "=> 4")],
};

pub fn last(args: &[SteelVal]) -> Result<SteelVal> {
    arity_check!(last, args, 1);

    if let SteelVal::ListV(l) = &args[0] {
        l.last()
            .cloned()
            .ok_or_else(throw!(Generic => "last resulted in an error - empty list"))
    } else {
        stop!(TypeMismatch => "last expects a list")
    }
}

// fn first(args: &mut [SteelVal]) -> Result<SteelVal> {
//     arity_check!(first, args, 1);

//     if let SteelVal::ListV(l) = &mut args[0] {
//         l.pop_front()
//             .ok_or_else(throw!(Generic => "first resulted in an error - empty list"))
//     } else {
//         stop!(TypeMismatch => "first expects a list")
//     }
// }

pub(crate) const CAR_DOC: DocTemplate<'static> = DocTemplate {
    signature: "(car l) -> any/c",
    params: &["l : list?"],
    description: r#"Returns the first element of the list l."#,
    examples: &[("> (car '(1 2))", "=> 1"), ("> (car (cons 2 3))", "=> 2")],
};

pub(crate) const FIRST_DOC: DocTemplate<'static> = DocTemplate {
    signature: "(first l) -> any/c",
    params: &["l : list?"],
    description: r#"Returns the first element of the list l."#,
    examples: &[
        ("> (first '(1 2))", "=> 1"),
        ("> (first (cons 2 3))", "=> 2"),
    ],
};

fn car(args: &[SteelVal]) -> Result<SteelVal> {
    arity_check!(car, args, 1);
    if let SteelVal::ListV(l) = &args[0] {
        l.car()
            .ok_or_else(throw!(Generic => "first resulted in an error - empty list"))
    } else {
        stop!(TypeMismatch => format!("first expects a list, found: {:?}", &args[0]))
    }
}

pub(crate) const CDR_DOC: DocTemplate<'static> = DocTemplate {
    signature: "(cdr l) -> list?",
    params: &["l : list?"],
    description: r#"Returns the rest of the list. Will raise an error if the list is empty."#,
    examples: &[
        ("λ > (cdr (list 10 20 30))", "=> '(20 30)"),
        ("λ > (cdr (list 10))", "=> '()"),
        (
            "λ > (cdr '())",
            r#"error[E11]: Generic
    ┌─ :1:2
    │
    1 │ (cdr '())
    │  ^^^ cdr expects a non empty list
"#,
        ),
    ],
};

fn cdr(args: &mut [SteelVal]) -> Result<SteelVal> {
    arity_check!(rest, args, 1);

    if let SteelVal::ListV(l) = &mut args[0] {
        if l.is_empty() {
            stop!(Generic => "cdr expects a non empty list");
        }

        match l.rest_mut() {
            Some(l) => Ok(SteelVal::ListV(l.clone())),
            None => Ok(SteelVal::ListV(l.clone())),
        }
    } else {
        stop!(TypeMismatch => format!("cdr expects a list, found: {}", &args[0]))
    }
}

pub(crate) const REST_DOC: DocTemplate<'static> = DocTemplate {
    signature: "(rest l) -> list?",
    params: &["l : list?"],
    description: r#"Returns the rest of the list. Will raise an error if the list is empty."#,
    examples: &[
        ("λ > (rest (list 10 20 30))", "=> '(20 30)"),
        ("λ > (rest (list 10))", "=> '()"),
        (
            "λ > (rest '())",
            r#"error[E11]: Generic
    ┌─ :1:2
    │
    1 │ (rest '())
    │  ^^^^ rest expects a non empty list
"#,
        ),
    ],
};

fn rest(args: &mut [SteelVal]) -> Result<SteelVal> {
    arity_check!(rest, args, 1);

    if let SteelVal::ListV(l) = &mut args[0] {
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

pub fn take(args: &[SteelVal]) -> Result<SteelVal> {
    arity_check!(take, args, 2);

    if let (SteelVal::ListV(l), SteelVal::IntV(n)) = (&args[0], &args[1]) {
        if *n < 0 {
            stop!(Generic => "take expects a positive integer")
        } else {
            Ok(SteelVal::ListV(l.take(*n as usize)))
        }
    } else {
        stop!(TypeMismatch => "take expects a list in the first position, and a positive integer in the second position")
    }
}

pub(crate) const APPEND_DOC: DocTemplate<'static> = DocTemplate {
    signature: "(append l r) -> list?",
    params: &["l : list?", "r : list?"],
    description: r#"Returns the rest of the list. Will raise an error if the list is empty."#,
    examples: &[("λ > (append (list 1 2) (list 3 4))", "=> '(1 2 3 4)")],
};

fn append(args: &mut [SteelVal]) -> Result<SteelVal> {
    arity_check!(append, args, 2);

    if let (SteelVal::ListV(r), SteelVal::ListV(l)) = (args[1].clone(), &mut args[0]) {
        l.append_mut(r);
        Ok(SteelVal::ListV(l.clone()))
    } else {
        stop!(TypeMismatch => "append expects two lists, found: {:?} and {:?}", &args[0], &args[1]);
    }
}

pub fn try_list_ref(args: &[SteelVal]) -> Result<SteelVal> {
    arity_check!(try_list_ref, args, 2);

    // todo!()
    if let (SteelVal::ListV(lst), SteelVal::IntV(n)) = (&args[0], &args[1]) {
        if *n < 0 {
            stop!(Generic => "list-ref expects a positive integer")
        } else if let Some(l) = lst.get(*n as usize) {
            Ok(l.clone())
        } else {
            Ok(SteelVal::BoolV(false))
        }
    } else {
        stop!(TypeMismatch => format!("try-list-ref expects a list and an integer, found {} and {}", &args[0], &args[1]))
    }
}

pub(crate) const LIST_REF_DOC: DocTemplate<'static> = DocTemplate {
    signature: "(list-ref lst index) -> list?",
    params: &["lst : list?", "index : (and/c int? positive?)"],
    description: r#"Returns the value located at the given index. Will raise an error if you try to index out of bounds.

Note: Runs in time proportional to the length of the list, however lists in Steel are implemented in such a fashion that the
time complexity is O(n/64). Meaning, for small lists this can be constant."#,
    examples: &[
        ("λ > (list-ref (list 1 2 3 4) 2)", "=> 3"),
        ("λ > (list-ref (range 0 100) 42)", "=> 42"),
        (
            "λ > (list-ref (list 1 2 3 4) 10)",
            r#"error[E11]: Generic
  ┌─ :1:2
  │
1 │ (list-ref (list 1 2 3 4) 10)
  │  ^^^^^^^^ out of bounds index in list-ref - list length: 4, index: 10"#,
        ),
    ],
};

pub fn list_ref(args: &[SteelVal]) -> Result<SteelVal> {
    arity_check!(list_ref, args, 2);

    // todo!()
    if let (SteelVal::ListV(lst), SteelVal::IntV(n)) = (&args[0], &args[1]) {
        if *n < 0 {
            stop!(Generic => "list-ref expects a positive integer")
        } else {
            lst.get(*n as usize)
                .cloned()
                .ok_or_else(throw!(Generic => format!("out of bounds index in list-ref - list length: {}, index: {}", lst.len(), n)))
        }
    } else {
        stop!(TypeMismatch => format!("list-ref expects a list and an integer, found {} and {}", &args[0], &args[1]))
    }
}

fn list_to_string(args: &[SteelVal]) -> Result<SteelVal> {
    arity_check!(list_to_string, args, 1);

    if let SteelVal::ListV(l) = &args[0] {
        let collected_string = l
            .iter()
            .map(|x| {
                x.char_or_else(throw!(TypeMismatch => "list->string expected a list of characters"))
            })
            .collect::<Result<String>>()?;

        Ok(SteelVal::StringV(collected_string.into()))
    } else {
        stop!(TypeMismatch => "first expects a list")
    }
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
        let res = cons(&mut args);
        let expected = SteelVal::ListV(list![SteelVal::IntV(1), SteelVal::IntV(2)]);

        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn cons_single_input() {
        let mut args = [SteelVal::IntV(1)];
        let res = cons(&mut args);
        let expected = ErrorKind::ArityMismatch;
        assert_eq!(res.unwrap_err().kind(), expected);
    }

    #[test]
    fn cons_no_input() {
        let res = cons(&mut []);
        let expected = ErrorKind::ArityMismatch;
        assert_eq!(res.unwrap_err().kind(), expected);
    }

    #[test]
    fn cons_with_empty_list() {
        let mut args = [SteelVal::IntV(1), SteelVal::ListV(List::new())];
        let res = cons(&mut args);
        // let expected = SteelVal::ListV(list![SteelVal::IntV(1)]);

        let expected = crate::list![1i32];

        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn cons_with_non_empty_vector() {
        let mut args = [SteelVal::IntV(1), SteelVal::ListV(list![SteelVal::IntV(2)])];
        let res = cons(&mut args);
        let expected = crate::list![1i32, 2i32];
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn car_normal_input() {
        let args = [crate::list![1i32, 2i32]];
        let res = car(&args);
        let expected = SteelVal::IntV(1);
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn car_bad_input() {
        let args = [SteelVal::IntV(1)];
        let res = car(&args);
        let expected = ErrorKind::TypeMismatch;
        assert_eq!(res.unwrap_err().kind(), expected);
    }

    #[test]
    fn car_too_many_args() {
        let args = [SteelVal::IntV(1), SteelVal::IntV(2)];
        let res = car(&args);
        let expected = ErrorKind::ArityMismatch;
        assert_eq!(res.unwrap_err().kind(), expected);
    }

    #[test]
    fn cdr_normal_input_2_elements() {
        let mut args = [crate::list![1i32, 2i32]];
        let res = cdr(&mut args);
        let expected = crate::list![2i32];
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn cdr_normal_input_3_elements() {
        let mut args = [crate::list![1i32, 2i32, 3i32]];
        let res = cdr(&mut args);
        let expected = crate::list![2i32, 3i32];
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn cdr_bad_input() {
        let mut args = [SteelVal::IntV(1)];
        let res = cdr(&mut args);
        let expected = ErrorKind::TypeMismatch;
        assert_eq!(res.unwrap_err().kind(), expected);
    }

    #[test]
    fn cdr_too_many_args() {
        let mut args = [SteelVal::NumV(1.0), SteelVal::NumV(2.0)];
        let res = cdr(&mut args);
        let expected = ErrorKind::ArityMismatch;
        assert_eq!(res.unwrap_err().kind(), expected);
    }

    #[test]
    fn cdr_single_element_list() {
        let mut args = [SteelVal::ListV(list![SteelVal::NumV(1.0)])];
        let res = cdr(&mut args);
        let expected = SteelVal::ListV(List::new());
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn range_tests_arity_too_few() {
        let args = [SteelVal::IntV(1)];
        let res = range(&args);
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
        let res = range(&args);
        let expected = ErrorKind::ArityMismatch;
        assert_eq!(res.unwrap_err().kind(), expected);
    }

    #[test]
    fn range_test_normal_input() {
        let args = [SteelVal::IntV(0), SteelVal::IntV(3)];
        let res = range(&args);
        let expected = SteelVal::ListV(list![
            SteelVal::IntV(0),
            SteelVal::IntV(1),
            SteelVal::IntV(2)
        ]);
        assert_eq!(res.unwrap(), expected);
    }
}
