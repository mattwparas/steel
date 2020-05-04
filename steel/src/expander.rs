use crate::env::Env;
use crate::parser::tokens::Token::*;
use crate::parser::Expr;
use crate::rerrs::SteelErr;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::rvals::Result;

#[derive(Clone, Debug, PartialEq)]
pub enum MacroPattern {
    Single(String),
    Syntax(String),
    Many(String),
    Nested(Vec<MacroPattern>),
}

impl MacroPattern {
    // TODO make this not so trash
    pub fn deconstruct(&self) -> Option<Vec<String>> {
        match self {
            Self::Syntax(s) => Some(vec![s.clone()]),
            Self::Single(s) => Some(vec![s.clone()]),
            Self::Many(s) => Some(vec![s.clone()]),
            Self::Nested(v) => Some(
                v.into_iter()
                    .filter_map(|x| x.deconstruct())
                    .flatten()
                    .collect(),
            ),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct MacroCase {
    args: Vec<MacroPattern>,
    body: Rc<Expr>,
}

impl MacroCase {
    pub fn new(args: Vec<MacroPattern>, body: Rc<Expr>) -> MacroCase {
        MacroCase { args, body }
    }

    pub fn expand(&self, list_of_tokens: &[Rc<Expr>]) -> Result<Rc<Expr>> {
        self.replace_exprs_in_body(list_of_tokens)
    }

    // this is a half-baked attempt to avoid namespace clashing inside of a macro body
    // not particularly sure if this is the answer, but recursively explore the body of the macro
    // and rename identifiers with a reserved keyword that could clash with the rest of the program
    // otherwise leave the same
    // TODO
    // fix this kinda junky function
    pub fn rename_identifiers(
        expr: Rc<Expr>,
        env: &Rc<RefCell<Env>>,
        args: &[MacroPattern],
    ) -> Rc<Expr> {
        // unimplemented!()
        let env = Rc::clone(env);
        let args_str: Vec<String> = args
            .iter()
            .filter_map(|x| x.deconstruct())
            .flatten()
            .collect();
        match expr.as_ref() {
            Expr::Atom(t) => {
                if let Identifier(s) = t {
                    if args_str.contains(s) || SteelMacro::is_reserved_keyword(&s) {
                        return expr;
                    } else if env.borrow().lookup(&s).is_err() {
                        return Rc::new(Expr::Atom(Identifier("##".to_string() + s)));
                    }
                }
                expr
            }
            Expr::VectorVal(vec_exprs) => Rc::new(Expr::VectorVal(
                vec_exprs
                    .into_iter()
                    .map(|x| Self::rename_identifiers(Rc::clone(x), &env, args))
                    .collect(),
            )),
        }
    }

    fn recursive_match(&self, list_of_tokens: &[Rc<Expr>]) -> bool {
        Self::match_vec_pattern_to_list_of_tokens(&self.args, list_of_tokens)
    }

    fn match_vec_pattern_to_list_of_tokens(
        args: &[MacroPattern],
        list_of_tokens: &[Rc<Expr>],
    ) -> bool {
        let mut token_iter = list_of_tokens.iter();
        for pat in args {
            // println!("Matching pattern: {:?}", pat);
            if let Some(val) = token_iter.next() {
                match pat {
                    MacroPattern::Single(_) | MacroPattern::Many(_) => {
                        continue;
                    }
                    MacroPattern::Syntax(v) => {
                        if let Expr::Atom(Identifier(s)) = val.as_ref() {
                            if s == v || v == "_" {
                                continue;
                            } else {
                                return false;
                            }
                        } else {
                            return false;
                        }
                    }
                    MacroPattern::Nested(vec) => {
                        if let Expr::VectorVal(l) = val.as_ref() {
                            if Self::match_vec_pattern_to_list_of_tokens(&vec, l) {
                                continue;
                            } else {
                                return false;
                            }
                        } else {
                            return false;
                        }
                    }
                }
            } else {
                true;
            }
        }
        true
    }

    // be able to recognize deep patterns like let
    // TODO parse patterns like this: ((args* ...) ...)
    fn parse_pattern_into_vec(
        macro_name: &str,
        special_forms: &[String],
        list_of_tokens: &[Rc<Expr>],
    ) -> Result<Vec<MacroPattern>> {
        let mut pattern_vec: Vec<MacroPattern> = Vec::new();
        let mut peek_token_iter = list_of_tokens.iter().peekable();

        while let Some(token) = peek_token_iter.next() {
            match token.as_ref() {
                Expr::Atom(Identifier(t)) => {
                    if t == macro_name || special_forms.contains(t) {
                        pattern_vec.push(MacroPattern::Syntax(t.clone()))
                    } else {
                        if let Some(nxt) = peek_token_iter.peek() {
                            if let Expr::Atom(Identifier(n)) = nxt.as_ref() {
                                if n == "..." {
                                    peek_token_iter.next();
                                    pattern_vec.push(MacroPattern::Many(t.clone()))
                                } else {
                                    pattern_vec.push(MacroPattern::Single(t.clone()))
                                }
                            } else {
                                pattern_vec.push(MacroPattern::Single(t.clone()));
                            }
                        } else {
                            pattern_vec.push(MacroPattern::Single(t.clone()));
                        }
                    }
                }
                Expr::VectorVal(l) => pattern_vec.push(MacroPattern::Nested(
                    Self::parse_pattern_into_vec(macro_name, special_forms, l)?,
                )),
                _ => stop!(BadSyntax => "syntax-rules requires identifiers in the pattern"),
            }
        }
        Ok(pattern_vec)
    }

    pub fn parse_from_tokens(
        macro_name: &str,
        special_forms: &[String],
        list_of_tokens: &[Rc<Expr>],
        env: &Rc<RefCell<Env>>,
    ) -> Result<MacroCase> {
        if let [pattern_expr, body_expr] = list_of_tokens {
            let pattern_expr_vec = pattern_expr.as_ref().vector_val_or_else(
                throw!(TypeMismatch => "syntax-rules expected a pattern in the case argument"),
            )?;

            let args = Self::parse_pattern_into_vec(macro_name, special_forms, pattern_expr_vec)?;
            let renamed_body = Self::rename_identifiers(Rc::clone(body_expr), env, &args);
            Ok(MacroCase::new(args, renamed_body))
        } else {
            stop!(ArityMismatch => "syntax-rules cases have 2 arguments")
        }
    }

    // count up the "..." and increment them?
    // TODO
    fn collect_bindings(
        args: &[MacroPattern],
        list_of_tokens: &[Rc<Expr>],
        bindings: &mut HashMap<String, Rc<Expr>>,
    ) -> Result<()> {
        let mut token_iter = list_of_tokens.into_iter().map(|x| Rc::clone(x));

        for arg in args {
            match arg {
                // bind the expression to the variable
                MacroPattern::Single(s) => {
                    if let Some(e) = token_iter.next() {
                        bindings.insert(s.to_string(), e);
                    } else {
                        stop!(ArityMismatch => "macro expansion failed in single pattern")
                    }
                }
                // actually check if the syntax matches
                MacroPattern::Syntax(s) => {
                    let e = token_iter
                        .next()
                        .ok_or_else(throw!(BadSyntax => "macro expansion expected keyword"))?;
                    let syn = e.as_ref().atom_identifier_or_else(
                        throw!(BadSyntax => "macro expansion expected keyword"),
                    )?;
                    if s != syn {
                        stop!(BadSyntax => "macro expansion expected keyword")
                    }
                }
                // TODO
                // bind the ellipses to the rest of the statement
                MacroPattern::Many(ident) => {
                    let rest: Vec<Rc<Expr>> = token_iter.collect();
                    bindings.insert(ident.to_string(), Rc::new(Expr::VectorVal(rest)));
                    break;
                }
                MacroPattern::Nested(children) => {
                    let child = token_iter
                        .next()
                        .ok_or_else(throw!(ArityMismatch => "Macro expected a pattern"))?;

                    let child_vec = child.as_ref().vector_val_or_else(
                        throw!(BadSyntax => "macro expected a vector of values"),
                    )?;

                    Self::collect_bindings(&children, child_vec, bindings)?;
                }
            }
        }

        Ok(())
    }

    // let recursive macros handle themselves - only expand the case that it can and then move on
    fn replace_exprs_in_body(&self, list_of_tokens: &[Rc<Expr>]) -> Result<Rc<Expr>> {
        let mut bindings: HashMap<String, Rc<Expr>> = HashMap::new();
        Self::collect_bindings(&self.args, list_of_tokens, &mut bindings)?;
        Self::recursive_replace(Rc::clone(&self.body), &bindings)
    }

    pub fn arity(&self) -> usize {
        self.args
            .iter()
            .map(|x| if let MacroPattern::Many(_) = x { 2 } else { 1 })
            .sum()
        // self.args.len()
    }

    // TODO also fix this
    pub fn has_ellipses(&self) -> bool {
        self.args
            .iter()
            .find(|x| {
                if let MacroPattern::Many(_) = x {
                    true
                } else {
                    false
                }
            })
            .is_some()
    }

    fn check_ellipses(expr: &Rc<Expr>) -> bool {
        let expr = Rc::clone(expr);
        if let Expr::Atom(t) = expr.as_ref() {
            if let Identifier(s) = t {
                s == "..."
            } else {
                false
            }
        } else {
            false
        }
    }

    // walk through the expression and replace all of the bindings with the expressions
    fn recursive_replace(expr: Rc<Expr>, bindings: &HashMap<String, Rc<Expr>>) -> Result<Rc<Expr>> {
        match expr.as_ref() {
            Expr::Atom(t) => {
                if let Identifier(s) = t {
                    if let Some(body) = bindings.get(s) {
                        Ok(Rc::clone(body))
                    } else {
                        Ok(expr)
                    }
                } else {
                    Ok(expr)
                }
            }
            Expr::VectorVal(vec_exprs) => {
                let mut vec_exprs = vec_exprs.clone();

                // TODO find this issue
                // Go to the position before the ellipses, look up that variable, insert all the expressions
                // you can there
                // find where the "..." is and insert all of the expressions there first
                if let Some(ellipses_pos) = vec_exprs.iter().position(Self::check_ellipses) {
                    let variable_to_lookup = vec_exprs.get(ellipses_pos - 1).ok_or_else(
                        throw!(BadSyntax => "macro expansion failed, could not find variable"),
                    )?;

                    let rest = bindings
                        .get(variable_to_lookup.as_ref().atom_identifier_or_else(
                            throw!(BadSyntax => "macro expansion failed at lookup!"),
                        )?)
                        .ok_or_else(throw!(BadSyntax => "macro expansion failed at lookup here"))?;

                    let list_of_exprs = rest.as_ref().vector_val_or_else(
                        throw!(BadSyntax => "macro expansion failed, expected list of expressions"),
                    )?;

                    let mut first_chunk = vec_exprs[0..ellipses_pos - 1].to_vec();
                    first_chunk.extend_from_slice(list_of_exprs);
                    first_chunk.extend_from_slice(&vec_exprs[(ellipses_pos + 1)..]);
                    vec_exprs = first_chunk;
                }

                Ok(Rc::new(Expr::VectorVal(
                    vec_exprs
                        .into_iter()
                        .map(|x| Self::recursive_replace(x, &bindings))
                        .collect::<Result<Vec<Rc<Expr>>>>()?,
                )))
            }
        }
    }
}

// rename identifiers used inside the macro as ##identifier
#[derive(Clone, Debug, PartialEq)]
pub struct SteelMacro {
    name: String,
    special_forms: Vec<String>,
    cases: Vec<MacroCase>,
}

impl SteelMacro {
    pub fn new(name: String, special_forms: Vec<String>, cases: Vec<MacroCase>) -> SteelMacro {
        SteelMacro {
            name,
            special_forms,
            cases,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn get_cases(&self) -> &[MacroCase] {
        &self.cases
    }

    fn parse_syntax_rules(
        macro_name: String,
        list_of_tokens: &[Rc<Expr>],
        env: &Rc<RefCell<Env>>,
    ) -> Result<SteelMacro> {
        // cannot check arity, only minimum
        if list_of_tokens.len() < 2 {
            stop!(ArityMismatch => "syntax-rules expected at least 3 arguments")
        }

        let mut token_iter = list_of_tokens.iter();
        let mut special_forms_vec: Vec<String> = Vec::new();
        let mut cases_vec: Vec<MacroCase> = Vec::new();

        let name = token_iter.next().ok_or_else(
            throw!(ArityMismatch => "syntax-rules expected an identifier in the first argument"),
        )?.as_ref().atom_identifier_or_else(
            throw!(TypeMismatch => "syntax-rules expected an identifier in the first argument"),
        )?;

        // make sure that we have found "syntax-rules" at the start of this
        if name != "syntax-rules" {
            stop!(BadSyntax => "macro-expansion failed: expected syntax-rules")
        }

        let list_of_idents = token_iter
            .next()
            .ok_or_else(throw!(ArityMismatch => "syntax-rules expected a list of identifiers"))?
            .as_ref()
            .vector_val_or_else(
                throw!(TypeMismatch => "syntax-rules expected a list of identifiers"),
            )?;

        // parse each special form inside the syntax-rules
        for special_form in list_of_idents {
            let name = special_form.as_ref().atom_identifier_or_else(
                throw!(TypeMismatch => "syntax-rules expected a list of identifiers"),
            )?;
            special_forms_vec.push(name.to_string())
        }

        // walk through cases and parse each individually
        while let Some(next_case) = token_iter.next() {
            cases_vec.push(MacroCase::parse_from_tokens(
                &macro_name,
                &special_forms_vec,
                next_case
                    .as_ref()
                    .vector_val_or_else(throw!(BadSyntax => "syntax-rules expected a pattern"))?,
                env,
            )?);
        }

        Ok(SteelMacro::new(macro_name, special_forms_vec, cases_vec))
    }

    // TODO
    pub fn parse_from_tokens(
        list_of_tokens: &[Rc<Expr>],
        env: &Rc<RefCell<Env>>,
    ) -> Result<SteelMacro> {
        if list_of_tokens.len() != 2 {
            stop!(ArityMismatch => "define-syntax takes 2 arguments, the name and the syntax-rules")
        }

        if let [name_expr, syntax_rules_expr] = list_of_tokens {
            let name = name_expr.as_ref().atom_identifier_or_else(
                throw!(TypeMismatch => "define-syntax expected a syntax-rules in the second position"),
            )?;

            let syntax_rules_tokens = syntax_rules_expr.as_ref().vector_val_or_else(
                throw!(TypeMismatch => "define-syntax expected a syntax-rules in the second position"),
            )?;

            Self::parse_syntax_rules(name.to_string(), syntax_rules_tokens, env)
        } else {
            stop!(ArityMismatch => "define-syntax expected 2 arguments")
        }
    }

    // TODO
    // its worth a shot
    fn match_case(&self, list_of_tokens: &[Rc<Expr>]) -> Result<&MacroCase> {
        for case in &self.cases {
            // println!(
            //     "{:?}",
            //     list_of_tokens
            //         .iter()
            //         .map(|x| x.to_string())
            //         .collect::<Vec<String>>()
            // );
            // println!(
            //     "{}, {}, {}",
            //     case.has_ellipses(),
            //     list_of_tokens.len(),
            //     case.arity()
            // );
            // println!("{:?}", case);
            // TODO this should actually be `case.arity() - num_ellipses_in_top_level`
            if (case.has_ellipses() && list_of_tokens.len() >= (case.arity() - 1))
                || case.arity() == list_of_tokens.len()
            {
                // println!("got inside the if");
                if case.recursive_match(list_of_tokens) {
                    return Ok(case);
                }
            }
        }
        stop!(ArityMismatch => "macro expansion could not match case")
    }

    fn is_reserved_keyword(word: &str) -> bool {
        // unimplemented!()
        match word {
            "lambda" | "define" | "map'" | "filter'" | "and" | "or" | "define-syntax-rule"
            | "eval" | "set!" | "let" | "begin" | "if" | "quote" | "..." => true,
            _ => false,
        }
    }

    pub fn expand(&self, list_of_tokens: &[Rc<Expr>]) -> Result<Rc<Expr>> {
        let case_to_expand = self.match_case(list_of_tokens)?;
        case_to_expand.expand(list_of_tokens)
    }
}

#[cfg(test)]
mod parse_macro_tests {

    use super::MacroPattern::*;
    use super::*;
    use crate::parser::Expr::*;
    use crate::parser::ParseError;
    use crate::parser::Parser;

    #[test]
    fn parse_single_syntax_rules() {
        // (define-syntax when
        //     (syntax-rules ()
        //       [(when a b ...)
        //        (if a (begin b ...) void)]))
        let input = &[
            Rc::new(Atom(Identifier("define-syntax".to_string()))),
            Rc::new(Atom(Identifier("when".to_string()))),
            Rc::new(VectorVal(vec![
                Rc::new(Atom(Identifier("syntax-rules".to_string()))),
                Rc::new(VectorVal(vec![])),
                Rc::new(VectorVal(vec![
                    Rc::new(VectorVal(vec![
                        Rc::new(Atom(Identifier("when".to_string()))),
                        Rc::new(Atom(Identifier("a".to_string()))),
                        Rc::new(Atom(Identifier("b".to_string()))),
                        Rc::new(Atom(Identifier("...".to_string()))),
                    ])),
                    Rc::new(VectorVal(vec![
                        Rc::new(Atom(Identifier("if".to_string()))),
                        Rc::new(Atom(Identifier("a".to_string()))),
                        Rc::new(VectorVal(vec![
                            Rc::new(Atom(Identifier("begin".to_string()))),
                            Rc::new(Atom(Identifier("b".to_string()))),
                        ])),
                        Rc::new(Atom(Identifier("void".to_string()))),
                    ])),
                ])),
            ])),
        ];

        let default_env = Rc::new(RefCell::new(Env::default_env()));
        let res = SteelMacro::parse_from_tokens(&input[1..], &default_env);

        let expected = SteelMacro {
            name: "when".to_string(),
            special_forms: vec![],
            cases: vec![MacroCase {
                args: vec![
                    Syntax("when".to_string()),
                    Single("a".to_string()),
                    Many("b".to_string()),
                ],
                body: Rc::new(VectorVal(vec![
                    Rc::new(Atom(Identifier("if".to_string()))),
                    Rc::new(Atom(Identifier("a".to_string()))),
                    Rc::new(VectorVal(vec![
                        Rc::new(Atom(Identifier("begin".to_string()))),
                        Rc::new(Atom(Identifier("b".to_string()))),
                    ])),
                    Rc::new(Atom(Identifier("void".to_string()))),
                ])),
            }],
        };

        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn parse_multiple_syntax_rules() {
        generate_macro_and_assert(
            "(define-syntax or
                (syntax-rules ()
                  [(or) #f]
                  [(or x) x]
                  [(or x y) (let ([z x])
                              (if z z y))]
                  [(or x y ...) (or x (or y ...))]))",
            SteelMacro {
                name: "or".to_string(),
                special_forms: vec![],
                cases: vec![
                    MacroCase {
                        args: vec![Syntax("or".to_string())],
                        body: Rc::new(Atom(BooleanLiteral(false))),
                    },
                    MacroCase {
                        args: vec![Syntax("or".to_string()), Single("x".to_string())],
                        body: Rc::new(Atom(Identifier("x".to_string()))),
                    },
                    MacroCase {
                        args: vec![
                            Syntax("or".to_string()),
                            Single("x".to_string()),
                            Single("y".to_string()),
                        ],
                        body: Rc::new(VectorVal(vec![
                            Rc::new(Atom(Identifier("let".to_string()))),
                            Rc::new(VectorVal(vec![Rc::new(VectorVal(vec![
                                Rc::new(Atom(Identifier("##z".to_string()))),
                                Rc::new(Atom(Identifier("x".to_string()))),
                            ]))])),
                            Rc::new(VectorVal(vec![
                                Rc::new(Atom(Identifier("if".to_string()))),
                                Rc::new(Atom(Identifier("##z".to_string()))),
                                Rc::new(Atom(Identifier("##z".to_string()))),
                                Rc::new(Atom(Identifier("y".to_string()))),
                            ])),
                        ])),
                    },
                    MacroCase {
                        args: vec![
                            Syntax("or".to_string()),
                            Single("x".to_string()),
                            Many("y".to_string()),
                        ],
                        body: Rc::new(VectorVal(vec![
                            Rc::new(Atom(Identifier("or".to_string()))),
                            Rc::new(Atom(Identifier("x".to_string()))),
                            Rc::new(VectorVal(vec![
                                Rc::new(Atom(Identifier("or".to_string()))),
                                Rc::new(Atom(Identifier("y".to_string()))),
                                Rc::new(Atom(Identifier("...".to_string()))),
                            ])),
                        ])),
                    },
                ],
            },
        );
    }

    #[test]
    fn parse_nested() {
        generate_macro_and_assert(
            "(define-syntax cond
                (syntax-rules (else)
                  [(cond [else e1 ...])
                   (begin e1 ...)]
                  [(cond [e1 e2 ...])
                   (when e1 e2 ...)]
                  [(cond [e1 e2 ...] c1 ...)
                   (if e1
                       (begin e2 ...)
                       (cond c1 ...))]))",
            SteelMacro {
                name: "cond".to_string(),
                special_forms: vec!["else".to_string()],
                cases: vec![
                    MacroCase {
                        args: vec![
                            Syntax("cond".to_string()),
                            Nested(vec![Syntax("else".to_string()), Many("e1".to_string())]),
                        ],
                        body: parse_with_empty_cache("(begin e1 ...)"),
                    },
                    MacroCase {
                        args: vec![
                            Syntax("cond".to_string()),
                            Nested(vec![Single("e1".to_string()), Many("e2".to_string())]),
                        ],
                        body: Rc::new(VectorVal(vec![
                            Rc::new(Atom(Identifier("##when".to_string()))),
                            Rc::new(Atom(Identifier("e1".to_string()))),
                            Rc::new(Atom(Identifier("e2".to_string()))),
                            Rc::new(Atom(Identifier("...".to_string()))),
                        ])),
                    },
                    MacroCase {
                        args: vec![
                            Syntax("cond".to_string()),
                            Nested(vec![Single("e1".to_string()), Many("e2".to_string())]),
                            Many("c1".to_string()),
                        ],
                        body: parse_with_empty_cache(
                            "(if e1
                                (begin e2 ...)
                                (cond c1 ...))",
                        ),
                    },
                ],
            },
        )
    }

    #[test]
    fn match_case_test() {
        let my_macro = SteelMacro {
            name: "cond".to_string(),
            special_forms: vec!["else".to_string()],
            cases: vec![
                MacroCase {
                    args: vec![
                        Syntax("cond".to_string()),
                        Nested(vec![Syntax("else".to_string()), Many("e1".to_string())]),
                    ],
                    body: parse_with_empty_cache("(begin e1 ...)"),
                },
                MacroCase {
                    args: vec![
                        Syntax("cond".to_string()),
                        Nested(vec![Single("e1".to_string()), Many("e2".to_string())]),
                    ],
                    body: Rc::new(VectorVal(vec![
                        Rc::new(Atom(Identifier("##when".to_string()))),
                        Rc::new(Atom(Identifier("e1".to_string()))),
                        Rc::new(Atom(Identifier("e2".to_string()))),
                        Rc::new(Atom(Identifier("...".to_string()))),
                    ])),
                },
                MacroCase {
                    args: vec![
                        Syntax("cond".to_string()),
                        Nested(vec![Single("e1".to_string()), Many("e2".to_string())]),
                        Many("c1".to_string()),
                    ],
                    body: parse_with_empty_cache(
                        "(if e1
                            (begin e2 ...)
                            (cond c1 ...))",
                    ),
                },
            ],
        };

        let input = parse_statement("(cond [else 10])");
        let expected = my_macro.get_cases()[0].clone();
        let res = my_macro.match_case(&input);
        assert_eq!(res.unwrap().clone(), expected);

        let input = parse_statement("(cond [#t 10])");
        let expected = my_macro.get_cases()[1].clone();
        let res = my_macro.match_case(&input);
        assert_eq!(res.unwrap().clone(), expected);

        let input = parse_statement("(cond [#f 10] [else 20])");
        let expected = my_macro.get_cases()[2].clone();
        let res = my_macro.match_case(&input);
        assert_eq!(res.unwrap().clone(), expected);
    }

    fn generate_macro_and_assert(s: &str, expected: SteelMacro) {
        let input = parse_statement(s);
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        let res = SteelMacro::parse_from_tokens(&input[1..], &default_env);

        assert_eq!(res.unwrap(), expected);
    }

    fn parse_with_empty_cache(s: &str) -> Rc<Expr> {
        let mut cache: HashMap<String, Rc<Expr>> = HashMap::new();
        let a: std::result::Result<Vec<Expr>, ParseError> = Parser::new(s, &mut cache).collect();
        let a = a.unwrap()[0].clone();
        Rc::new(a)
    }

    fn parse_statement(s: &str) -> Vec<Rc<Expr>> {
        let a = parse_with_empty_cache(s);
        a.vector_val_or_else(throw!(BadSyntax => "Malformed statement in the test"))
            .unwrap()
            .to_vec()
    }
}
