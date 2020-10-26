use crate::env::{FALSE, TRUE, VOID};
use crate::gc::Gc;
use crate::parser::Expr;
use crate::rerrs::SteelErr;
use crate::rvals::{Result, SteelVal};
use crate::stop;
use crate::throw;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub enum StructFunctionType {
    Constructor,
    Getter(usize),
    Setter(usize),
    Predicate(String),
}

#[derive(Clone, Debug, PartialEq)]
pub struct SteelStruct {
    name: Rc<str>,
    fields: Vec<Gc<SteelVal>>,
    function_purpose: StructFunctionType,
}

// Housekeeping (just in case there are cyclical references)
impl Drop for SteelStruct {
    fn drop(&mut self) {
        self.fields.clear();
    }
}

impl SteelStruct {
    pub fn new(
        name: Rc<str>,
        fields: Vec<Gc<SteelVal>>,
        function_purpose: StructFunctionType,
    ) -> Self {
        SteelStruct {
            name,
            fields,
            function_purpose,
        }
    }

    // This will blow up the stack with a sufficiently large recursive struct
    pub fn pretty_print(&self) -> String {
        format!("{}: {:#?}", self.name, self.fields)
    }
}

impl SteelStruct {
    pub fn generate_from_tokens(list_of_tokens: &[Expr]) -> Result<Vec<(String, SteelVal)>> {
        let (name, list_of_tokens) = list_of_tokens.split_first().ok_or_else(
            throw!(ArityMismatch => "struct definition requires a name and a list of field names"),
        )?;

        let name = name.atom_identifier_or_else(throw!(TypeMismatch => "struct definition expected an identifier as the first argument"))?;

        if list_of_tokens.len() != 1 {
            stop!(ArityMismatch => "Struct definition requires list of field names")
        }
        let field_names = list_of_tokens[0].vector_val_or_else(
            throw!(ArityMismatch => "struct requires list of identifiers for the field names"),
        )?;

        let field_names_as_strs: Vec<&str> = field_names
            .iter()
            .map(|x| {
                x.atom_identifier_or_else(throw!(TypeMismatch => "struct expected identifiers"))
            })
            .collect::<Result<_>>()?;

        // collect the functions
        // for each field there are going to be 2 functions
        // add 2 for the constructor and the predicate
        let mut funcs = Vec::with_capacity(field_names_as_strs.len() * 2 + 2);
        // generate constructor
        let cons = constructor(name, field_names_as_strs.len());
        // generate predicate
        funcs.push((format!("{}?", name), predicate(&name)));
        // generate getters and setters
        for (idx, field) in field_names_as_strs.into_iter().enumerate() {
            funcs.push((format!("{}-{}", name, field), getter(&name, idx)));
            funcs.push((format!("set-{}-{}!", name, field), setter(&name, idx)));
        }
        funcs.push((name.to_string(), cons));
        Ok(funcs)
    }
}

// initialize hashmap to be field_names -> void
// just do arity check before inserting to make sure things check out
// that way field names as a vec are no longer necessary
fn constructor(name: &str, len: usize) -> SteelVal {
    let factory: SteelStruct = SteelStruct::new(
        Rc::from(name),
        vec![VOID.with(|f| Gc::clone(f)); len],
        StructFunctionType::Constructor,
    );

    SteelVal::StructClosureV(
        factory,
        |args: Vec<Gc<SteelVal>>, factory: &SteelStruct| -> Result<Gc<SteelVal>> {
            if args.len() != factory.fields.len() {
                let error_message = format!(
                    "{} expected {} arguments, found {}",
                    factory.name.clone(),
                    args.len(),
                    factory.fields.len()
                );
                stop!(ArityMismatch => error_message);
            }
            let mut new_struct = factory.clone();

            for (idx, arg) in args.into_iter().enumerate() {
                let key = new_struct
                    .fields
                    .get_mut(idx)
                    .ok_or_else(throw!(TypeMismatch => "Couldn't find that field in the struct"))?;
                *key = arg;
            }

            Ok(Gc::new(SteelVal::StructV(new_struct)))
        },
    )
}

fn predicate(name: &str) -> SteelVal {
    let factory = SteelStruct::new(
        Rc::from(name),
        Vec::new(),
        StructFunctionType::Predicate(name.to_string()),
    );
    SteelVal::StructClosureV(
        factory,
        |args: Vec<Gc<SteelVal>>, factory: &SteelStruct| -> Result<Gc<SteelVal>> {
            if args.len() != 1 {
                let error_message = format!(
                    "{}? expected one argument, found {}",
                    factory.name,
                    args.len()
                );
                stop!(ArityMismatch => error_message);
            }

            match args[0].as_ref() {
                SteelVal::StructV(my_struct) => {
                    if let StructFunctionType::Predicate(name_huh) = &factory.function_purpose {
                        if my_struct.name.as_ref() == name_huh {
                            Ok(TRUE.with(|f| Gc::clone(f)))
                        } else {
                            Ok(FALSE.with(|f| Gc::clone(f)))
                        }
                    } else {
                        stop!(TypeMismatch => "something went wrong with struct predicate")
                    }
                }
                _ => Ok(FALSE.with(|f| Gc::clone(f))),
            }
        },
    )
}

fn getter(name: &str, idx: usize) -> SteelVal {
    let factory = SteelStruct::new(Rc::from(name), Vec::new(), StructFunctionType::Getter(idx));
    SteelVal::StructClosureV(
        factory,
        |args: Vec<Gc<SteelVal>>, factory: &SteelStruct| -> Result<Gc<SteelVal>> {
            if args.len() != 1 {
                let error_message = format!(
                    "{} getter expected one argument, found {}",
                    factory.name,
                    args.len()
                );
                stop!(ArityMismatch => error_message);
            }

            let my_struct = args[0].struct_or_else(throw!(TypeMismatch => "expected struct"))?;

            if let StructFunctionType::Getter(idx) = &factory.function_purpose {
                if let Some(ret_val) = my_struct.fields.get(*idx) {
                    Ok(Gc::clone(ret_val))
                } else {
                    stop!(TypeMismatch => "Couldn't find that field in the struct")
                }
            } else {
                stop!(TypeMismatch => "something went wrong with struct getter")
            }
        },
    )
}

fn setter(name: &str, idx: usize) -> SteelVal {
    let factory = SteelStruct::new(Rc::from(name), Vec::new(), StructFunctionType::Setter(idx));
    SteelVal::StructClosureV(
        factory,
        |args: Vec<Gc<SteelVal>>, factory: &SteelStruct| -> Result<Gc<SteelVal>> {
            if args.len() != 2 {
                let error_message = format!(
                    "{} setter expected two arguments, found {}",
                    factory.name,
                    args.len()
                );
                stop!(ArityMismatch => error_message);
            }

            let my_struct = args[0].struct_or_else(throw!(TypeMismatch => "expected struct"))?;
            let value = Gc::clone(&args[1]);

            if let StructFunctionType::Setter(idx) = &factory.function_purpose {
                let mut new_struct = my_struct.clone();
                let key = new_struct
                    .fields
                    .get_mut(*idx)
                    .ok_or_else(throw!(TypeMismatch => "Couldn't find that field in the struct"))?;
                *key = value;
                Ok(Gc::new(SteelVal::StructV(new_struct)))
            } else {
                stop!(TypeMismatch => "something went wrong with struct setter")
            }
        },
    )
}

#[cfg(test)]
mod struct_tests {

    use super::*;

    fn apply_function(func: SteelVal, args: Vec<SteelVal>) -> Result<Gc<SteelVal>> {
        let args: Vec<Gc<SteelVal>> = args.into_iter().map(|x| Gc::new(x)).collect();
        let (factory, func) = func
            .struct_func_or_else(throw!(BadSyntax => "string tests"))
            .unwrap();

        func(args, factory)
    }

    #[test]
    fn constructor_normal() {
        let args = vec![SteelVal::IntV(1), SteelVal::IntV(2)];
        let res = apply_function(constructor("Promise", 2), args);
        let expected = Gc::new(SteelVal::StructV(SteelStruct {
            name: Rc::from("Promise"),
            fields: vec![Gc::new(SteelVal::IntV(1)), Gc::new(SteelVal::IntV(2))],
            function_purpose: StructFunctionType::Constructor,
        }));
        assert_eq!(res.unwrap(), expected)
    }

    #[test]
    fn setter_position_0() {
        let args = vec![
            SteelVal::StructV(SteelStruct {
                name: Rc::from("Promise"),
                fields: vec![Gc::new(SteelVal::IntV(1)), Gc::new(SteelVal::IntV(2))],
                function_purpose: StructFunctionType::Constructor,
            }),
            SteelVal::IntV(100),
        ];

        let res = apply_function(setter("Promise", 0), args);
        let expected = Gc::new(SteelVal::StructV(SteelStruct {
            name: Rc::from("Promise"),
            fields: vec![Gc::new(SteelVal::IntV(100)), Gc::new(SteelVal::IntV(2))],
            function_purpose: StructFunctionType::Constructor,
        }));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn setter_position_1() {
        let args = vec![
            SteelVal::StructV(SteelStruct {
                name: Rc::from("Promise"),
                fields: vec![Gc::new(SteelVal::IntV(1)), Gc::new(SteelVal::IntV(2))],
                function_purpose: StructFunctionType::Constructor,
            }),
            SteelVal::IntV(100),
        ];

        let res = apply_function(setter("Promise", 1), args);
        let expected = Gc::new(SteelVal::StructV(SteelStruct {
            name: Rc::from("Promise"),
            fields: vec![Gc::new(SteelVal::IntV(1)), Gc::new(SteelVal::IntV(100))],
            function_purpose: StructFunctionType::Constructor,
        }));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn getter_position_0() {
        let args = vec![SteelVal::StructV(SteelStruct {
            name: Rc::from("Promise"),
            fields: vec![Gc::new(SteelVal::IntV(1)), Gc::new(SteelVal::IntV(2))],
            function_purpose: StructFunctionType::Constructor,
        })];

        let res = apply_function(getter("Promise", 0), args);
        let expected = Gc::new(SteelVal::IntV(1));
        assert_eq!(res.unwrap(), expected);
    }
}

#[cfg(test)]
mod struct_integration_tests {
    use crate::test_util::assert_script;

    #[test]
    fn test_trie_sort() {
        let script = r#"
        (struct trie (char children end-word? word-up-to))

        ;; Rename functions for the sake of compatibility
        (define empty (list))
        (define empty-trie (trie void empty #f empty))

        ;; Throw in a mediocre flatten definition
        (define (flatten lst)
        (cond ((null? lst) empty)
                ((list? lst)
                (append (flatten (car lst)) (flatten (cdr lst))))
                (else (list lst))))

        ;; contract: (listof char?) (listof tries?) integer? -> (listof trie?)
        (define (create-children char-list lst prefix-chars)
        (cond [(= (length char-list) 1)
                (handle-last-letter char-list lst prefix-chars)]
                [else ;; you are in the middle of the word
                (handle-intern-letter char-list lst prefix-chars)]))

        ;; contract: (listof char?) (listof trie?) integer? -> (listof trie?)
        (define (handle-last-letter char-list lst prefix-chars)
        (define char (first char-list))
        ; (define next-prefix (append prefix-chars (list char)))
        (define next-prefix (push-back prefix-chars char))
        (cond [(empty? lst) ;; children are empty, return list of empty children
                (list (trie char empty #t next-prefix))]
                [(< char (trie-char (first lst))) ;; less than, put it to the left
                (cons (trie char empty #t next-prefix) lst)]
                [(= char (trie-char (first lst))) ;; equal, step down a level
                (cons (trie char (trie-children (first lst)) #t next-prefix) (rest lst))]
                [else ;; move to the right
                (cons (first lst)
                    (create-children char-list (rest lst) prefix-chars))]))

        ;; contract: (listof char?) (listof trie?) integer? -> (listof trie?)
        (define (handle-intern-letter char-list lst prefix-chars)
        (define char (first char-list))
        ; (define next-prefix (append prefix-chars (list char)))
        (define next-prefix (push-back prefix-chars char))
        (cond [(empty? lst) ;; no children, pop off front and step down
                (list (trie char (create-children
                                (rest char-list) empty next-prefix) #f next-prefix))]
                [(< char (trie-char (first lst))) ;; place where it is, pop off front and go
                (cons (trie char (create-children
                                (rest char-list) empty next-prefix) #f next-prefix) lst)]
                [(= char (trie-char (first lst))) ;; equal, step down
                (cons (trie char (create-children (rest char-list) (trie-children (first lst)) next-prefix)
                            (trie-end-word? (first lst))
                            (trie-word-up-to (first lst)))
                    (rest lst))]
                [else ; move to the right
                (cons (first lst)
                    (create-children char-list (rest lst) prefix-chars))]))

        ;; contract: trie? string? integer? -> trie?
        (define (insert root-trie word)
        (define char-list (string->list word))
        (trie
        (trie-char root-trie)
        (create-children char-list (trie-children root-trie) empty)
        (trie-end-word? root-trie)
        (trie-word-up-to root-trie)))

        ; contract: trie? trie? -> boolean?
        (define (trie<? trie-node1 trie-node2)
        (< (trie-char trie-node1) (trie-char trie-node2)))


        ;; contract: trie? (listof string?) -> trie?
        (define (build-trie-from-list-of-words trie list-of-words)
            (cond
                [(= (length list-of-words) 1)
                    (insert trie (first list-of-words))]
                [else
                    (build-trie-from-list-of-words
                    (insert trie (first list-of-words))
                                 (rest list-of-words))]))

        ;; ------------------ SORTING ---------------------- ;;

        (define (trie-sort list-of-words)
        (define new-trie (build-trie-from-list-of-words empty-trie list-of-words))
        (pre-order new-trie))

        ; THIS ONE WORKS (using con and flatten)
        ;; contract: trie? -> (listof string?)
        (define (pre-order trie-node)
            (if (trie-end-word? trie-node)
                (cons (list->string (trie-word-up-to trie-node))
                      (flatten (map pre-order (trie-children trie-node))))
                (flatten (map pre-order (trie-children trie-node)))))

        (define test-list
            (list
                "suppose"
                "believe"
                "changeable"
                "absent"
                "busy"
                "float"
                "debonair"
                "throat"
                "grey"
                "use"
                "measure"
                "van"
                "thirsty"
                "notify"
                "star"))

        (define expected-list
            (list
                "absent" "believe" "busy" "changeable" "debonair" "float" "grey" "measure" "notify" "star" "suppose" "thirsty" "throat" "use" "van"))
            
        (define trie1 (build-trie-from-list-of-words empty-trie test-list))
        (assert! (equal?
                        expected-list
                        (trie-sort test-list)))
        "#;

        assert_script(script);
    }
}
