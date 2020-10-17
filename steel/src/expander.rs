// use crate::env::Env;
use crate::parser::tokens::TokenType::*;
use crate::parser::Expr;
use crate::parser::SyntaxObject;
use crate::rerrs::SteelErr;
// use std::cell::RefCell;
use std::collections::HashMap;
// use std::rc::Rc;

use crate::parser::span::Span;
// use crate::parser::SyntaxObject;

use crate::rvals::Result;

use crate::env::MacroEnv;

#[derive(Clone, Debug, PartialEq)]
pub enum MacroPattern {
    Single(String),
    Syntax(String),
    Many(String),
    Nested(Vec<MacroPattern>),
}

impl MacroPattern {
    // TODO make this not so trash
    pub fn deconstruct(&self) -> Vec<&str> {
        match self {
            Self::Syntax(s) => vec![&s],
            Self::Single(s) => vec![&s],
            Self::Many(s) => vec![&s],
            Self::Nested(v) => v.iter().map(|x| x.deconstruct()).flatten().collect(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct MacroCase {
    args: Vec<MacroPattern>,
    body: Expr,
}

impl MacroCase {
    pub fn new(args: Vec<MacroPattern>, body: Expr) -> MacroCase {
        MacroCase { args, body }
    }

    pub fn expand(&self, list_of_tokens: &[Expr]) -> Result<Expr> {
        self.replace_exprs_in_body(list_of_tokens)
    }

    pub fn clobber_span_information(t: &SyntaxObject) -> SyntaxObject {
        SyntaxObject::new(t.ty.clone(), Span::new(0, 0))
    }

    // pub fn clobber_span_information(expr: Rc<Expr>) -> Rc<Expr> {
    //     let expr = (*expr).clone();
    //     if let Expr::Atom(syn) = expr {

    //     } else {
    //         unreachable!();
    //     }
    // }

    // this is a half-baked attempt to avoid namespace clashing inside of a macro body
    // not particularly sure if this is the answer, but recursively explore the body of the macro
    // and rename identifiers with a reserved keyword that could clash with the rest of the program
    // otherwise leave the same
    // TODO
    // fix this kinda junky function
    pub fn rename_identifiers<M: MacroEnv>(expr: Expr, env: &M, args: &[MacroPattern]) -> Expr {
        // unimplemented!()
        // let env = Rc::clone(env);
        let args_str: Vec<&str> = args.iter().map(|x| x.deconstruct()).flatten().collect();
        match expr {
            Expr::Atom(ref t) => {
                if let Identifier(s) = &t.ty {
                    if args_str.contains(&s.as_str()) || SteelMacro::is_reserved_keyword(&s) {
                        return expr;
                    } else if !env.validate_identifier(&s) {
                        return Expr::Atom(SyntaxObject::default(Identifier("##".to_string() + s)));
                    }
                }

                Expr::Atom(Self::clobber_span_information(t))
                //  expr // Old TODO
                // expr
            }
            Expr::VectorVal(vec_exprs) => Expr::VectorVal(
                vec_exprs
                    .into_iter()
                    .map(|x| Self::rename_identifiers(x, env, args))
                    .collect(),
            ),
        }
    }

    fn recursive_match(&self, list_of_tokens: &[Expr]) -> bool {
        // println!("{:?}, {:?}", self.args, list_of_tokens);
        Self::match_vec_pattern_to_list_of_tokens(&self.args, list_of_tokens)
    }

    fn match_vec_pattern_to_list_of_tokens(args: &[MacroPattern], list_of_tokens: &[Expr]) -> bool {
        let mut token_iter = list_of_tokens.iter();
        for pat in args {
            if let Some(val) = token_iter.next() {
                // println!("")
                // println!("Matching pattern: {:?} to val: {:?}", pat, val);
                match pat {
                    MacroPattern::Single(_) | MacroPattern::Many(_) => {
                        continue;
                    }
                    MacroPattern::Syntax(v) => {
                        if let Expr::Atom(SyntaxObject { ty: t, .. }) = val {
                            if let Identifier(s) = t {
                                if s == v || v == "_" {
                                    continue;
                                } else {
                                    return false;
                                }
                            } else {
                                return false;
                            }
                        } else {
                            return false;
                        }
                    }
                    MacroPattern::Nested(vec) => {
                        if let Expr::VectorVal(l) = val {
                            // TODO more elegant let* case
                            if vec.is_empty() && !l.is_empty() {
                                return false;
                            }

                            // println!("Matching on {:?} with val {:?}", pat, val);
                            // TODO come back here and check this out
                            // this solves the destructuring test case
                            if vec.len() < l.len()
                                && vec
                                    .iter()
                                    .find(|x| {
                                        if let MacroPattern::Many(_) = x {
                                            true
                                        } else {
                                            false
                                        }
                                    })
                                    .is_none()
                            {
                                return false;
                            }

                            // if the vec contains a many
                            // if

                            // if pat.arity()
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
            }

            // TODO maybe this is it???
            // else {
            //     return false;
            // }
        }
        true
    }

    // be able to recognize deep patterns like let
    // TODO parse patterns like this: ((args* ...) ...)
    fn parse_pattern_into_vec(
        macro_name: &str,
        special_forms: &[String],
        list_of_tokens: &[Expr],
    ) -> Result<Vec<MacroPattern>> {
        let mut pattern_vec: Vec<MacroPattern> = Vec::new();
        let mut peek_token_iter = list_of_tokens.iter().peekable();

        while let Some(token) = peek_token_iter.next() {
            match token {
                Expr::Atom(SyntaxObject { ty: s, .. }) => {
                    if let Identifier(t) = s {
                        if t == macro_name || special_forms.contains(t) {
                            pattern_vec.push(MacroPattern::Syntax(t.clone()))
                        } else {
                            match peek_token_iter.peek() {
                                Some(Expr::Atom(SyntaxObject {
                                    ty: Identifier(n), ..
                                })) if n == "..." => {
                                    peek_token_iter.next();
                                    pattern_vec.push(MacroPattern::Many(t.clone()));
                                }
                                _ => {
                                    pattern_vec.push(MacroPattern::Single(t.clone()));
                                }
                            }
                        }
                    } else {
                        stop!(BadSyntax => "syntax-rules requires identifiers in the pattern")
                    }
                }
                Expr::VectorVal(l) => pattern_vec.push(MacroPattern::Nested(
                    Self::parse_pattern_into_vec(macro_name, special_forms, &l)?,
                )),
            }
        }
        Ok(pattern_vec)
    }

    pub fn parse_from_tokens<M: MacroEnv>(
        macro_name: &str,
        special_forms: &[String],
        list_of_tokens: &[Expr],
        env: &M,
    ) -> Result<MacroCase> {
        if let [pattern_expr, body_expr] = list_of_tokens {
            let pattern_expr_vec = pattern_expr.vector_val_or_else(
                throw!(TypeMismatch => "syntax-rules expected a pattern in the case argument"),
            )?;

            let args = Self::parse_pattern_into_vec(macro_name, special_forms, pattern_expr_vec)?;
            let renamed_body = Self::rename_identifiers(body_expr.clone(), env, &args);
            Ok(MacroCase::new(args, renamed_body))
        } else {
            stop!(ArityMismatch => "syntax-rules cases have 2 arguments")
        }
    }

    // count up the "..." and increment them?
    // TODO
    fn collect_bindings(
        args: &[MacroPattern],
        list_of_tokens: &[Expr],
        bindings: &mut HashMap<String, Expr>,
    ) -> Result<()> {
        let mut token_iter = list_of_tokens.iter();

        for arg in args {
            match arg {
                // bind the expression to the variable
                MacroPattern::Single(s) => {
                    if let Some(e) = token_iter.next() {
                        bindings.insert(s.to_string(), e.clone());
                    } else {
                        // println!("Macro Expansion Failed in Single Pattern: {}", s);
                        stop!(ArityMismatch => "macro expansion failed in single pattern")
                    }
                }
                // actually check if the syntax matches
                MacroPattern::Syntax(s) => {
                    let e = token_iter
                        .next()
                        .ok_or_else(throw!(BadSyntax => "macro expansion expected keyword"))?;
                    let syn = e.atom_identifier_or_else(
                        throw!(BadSyntax => "macro expansion expected keyword"),
                    )?;
                    if s != syn {
                        stop!(BadSyntax => "macro expansion expected keyword")
                    }
                }
                // TODO
                // bind the ellipses to the rest of the statement
                MacroPattern::Many(ident) => {
                    let rest: Vec<Expr> = token_iter.cloned().collect();
                    bindings.insert(ident.to_string(), Expr::VectorVal(rest));
                    break;
                }
                MacroPattern::Nested(children) => {
                    let child = token_iter
                        .next()
                        .ok_or_else(throw!(ArityMismatch => "Macro expected a pattern"))?;

                    let child_vec = child.vector_val_or_else(
                        throw!(BadSyntax => "macro expected a vector of values"),
                    )?;

                    Self::collect_bindings(&children, child_vec, bindings)?;
                }
            }
        }

        Ok(())
    }

    // let recursive macros handle themselves - only expand the case that it can and then move on
    fn replace_exprs_in_body(&self, list_of_tokens: &[Expr]) -> Result<Expr> {
        let mut bindings: HashMap<String, Expr> = HashMap::new();
        Self::collect_bindings(&self.args, list_of_tokens, &mut bindings)?;
        Self::recursive_replace(&self.body, &bindings)
    }

    pub fn arity(&self) -> usize {
        self.args
            .iter()
            .map(|x| if let MacroPattern::Many(_) = x { 2 } else { 1 })
            .sum()
    }

    // TODO also fix this
    pub fn has_ellipses(&self) -> bool {
        self.args.iter().any(|x| matches!(x, MacroPattern::Many(_)))
    }

    fn check_ellipses(expr: &Expr) -> bool {
        // let expr = Rc::clone(expr);
        if let Expr::Atom(t) = expr {
            if let Identifier(s) = &t.ty {
                s == "..."
            } else {
                false
            }
        } else {
            false
        }
    }

    // walk through the expression and replace all of the bindings with the expressions
    fn recursive_replace(expr: &Expr, bindings: &HashMap<String, Expr>) -> Result<Expr> {
        match expr {
            Expr::Atom(t) => {
                if let Identifier(s) = &t.ty {
                    if let Some(body) = bindings.get(s) {
                        Ok(body.clone())
                    } else {
                        Ok(expr.clone())
                    }
                } else {
                    Ok(expr.clone())
                }
            }
            Expr::VectorVal(vec_exprs) => {
                let mut vec_exprs = vec_exprs.clone();
                if let Some(checkdatum) = vec_exprs.get(0) {
                    if let Expr::Atom(SyntaxObject { ty: t, .. }) = checkdatum {
                        if let Identifier(check) = t {
                            if check == "datum->syntax" {
                                let mut buffer = String::new();
                                if let Some((_, rest)) = vec_exprs.split_first() {
                                    for syntax in rest {
                                        let transformer = syntax.atom_identifier_or_else(
                                            throw!(BadSyntax => "datum->syntax requires an identifier"),
                                        )?;

                                        if transformer.starts_with("##") {
                                            let (_, cdr) = transformer.split_at(2);
                                            buffer.push_str(cdr);
                                        } else {
                                            if let Some(body) = bindings.get(transformer) {
                                                // println!("{}", body.to_string());

                                                buffer.push_str(body.to_string().as_str());
                                            }
                                        }
                                    }

                                    return Ok(Expr::Atom(SyntaxObject::default(Identifier(
                                        buffer,
                                    ))));
                                }
                            }
                        }
                    }
                }

                // TODO find this issue
                // Go to the position before the ellipses, look up that variable, insert all the expressions
                // you can there
                // find where the "..." is and insert all of the expressions there first
                if let Some(ellipses_pos) = vec_exprs.iter().position(Self::check_ellipses) {
                    let variable_to_lookup = vec_exprs.get(ellipses_pos - 1).ok_or_else(
                        throw!(BadSyntax => "macro expansion failed, could not find variable"),
                    )?;

                    let rest = bindings
                        .get(variable_to_lookup.atom_identifier_or_else(
                            throw!(BadSyntax => "macro expansion failed at lookup!"),
                        )?)
                        .ok_or_else(throw!(BadSyntax => "macro expansion failed at lookup here"))?;

                    let list_of_exprs = rest.vector_val_or_else(
                        throw!(BadSyntax => "macro expansion failed, expected list of expressions"),
                    )?;

                    let mut first_chunk = vec_exprs[0..ellipses_pos - 1].to_vec();
                    first_chunk.extend_from_slice(list_of_exprs);
                    first_chunk.extend_from_slice(&vec_exprs[(ellipses_pos + 1)..]);
                    vec_exprs = first_chunk;
                }

                Ok(Expr::VectorVal(
                    vec_exprs
                        .iter()
                        .map(|x| Self::recursive_replace(x, &bindings))
                        .collect::<Result<Vec<Expr>>>()?,
                ))
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

    fn parse_syntax_rules<M: MacroEnv>(
        macro_name: String,
        list_of_tokens: &[Expr],
        env: &M,
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
        )?.atom_identifier_or_else(
            throw!(TypeMismatch => "syntax-rules expected an identifier in the first argument"),
        )?;

        // make sure that we have found "syntax-rules" at the start of this
        if name != "syntax-rules" {
            stop!(BadSyntax => "macro-expansion failed: expected syntax-rules")
        }

        let list_of_idents = token_iter
            .next()
            .ok_or_else(throw!(ArityMismatch => "syntax-rules expected a list of identifiers"))?
            .vector_val_or_else(
                throw!(TypeMismatch => "syntax-rules expected a list of identifiers"),
            )?;

        // parse each special form inside the syntax-rules
        for special_form in list_of_idents {
            let name = special_form.atom_identifier_or_else(
                throw!(TypeMismatch => "syntax-rules expected a list of identifiers"),
            )?;
            special_forms_vec.push(name.to_string())
        }

        // walk through cases and parse each individually
        for next_case in token_iter {
            cases_vec.push(MacroCase::parse_from_tokens(
                &macro_name,
                &special_forms_vec,
                next_case
                    .vector_val_or_else(throw!(BadSyntax => "syntax-rules expected a pattern"))?,
                env,
            )?);
        }

        Ok(SteelMacro::new(macro_name, special_forms_vec, cases_vec))
    }

    // TODO
    pub fn parse_from_tokens<M: MacroEnv>(list_of_tokens: &[Expr], env: &M) -> Result<SteelMacro> {
        if list_of_tokens.len() != 2 {
            stop!(ArityMismatch => "define-syntax takes 2 arguments, the name and the syntax-rules")
        }

        if let [name_expr, syntax_rules_expr] = list_of_tokens {
            let name = name_expr.atom_identifier_or_else(
                throw!(TypeMismatch => "define-syntax expected a syntax-rules in the second position"),
            )?;

            let syntax_rules_tokens = syntax_rules_expr.vector_val_or_else(
                throw!(TypeMismatch => "define-syntax expected a syntax-rules in the second position"),
            )?;

            Self::parse_syntax_rules(name.to_string(), syntax_rules_tokens, env)
        } else {
            stop!(ArityMismatch => "define-syntax expected 2 arguments")
        }
    }

    // TODO
    // its worth a shot
    fn match_case(&self, list_of_tokens: &[Expr]) -> Result<&MacroCase> {
        // println!(
        //     "{:?}",
        //     list_of_tokens
        //         .iter()
        //         .map(|x| x.to_string())
        //         .collect::<Vec<_>>()
        // );

        for case in &self.cases {
            // println!("Case: {:?}", case.args);

            // println!("len: {}", list_of_tokens.len());

            // TODO this should actually be `case.arity() - num_ellipses_in_top_level`
            if (case.has_ellipses() && list_of_tokens.len() >= (case.arity() - 1))
                || case.arity() == list_of_tokens.len()
            {
                // println!("got inside the if");
                if case.recursive_match(list_of_tokens) {
                    // println!("Matched case!");
                    return Ok(case);
                }
            }
        }
        // println!("getting to here...");
        stop!(ArityMismatch => "macro expansion could not match case")
    }

    fn is_reserved_keyword(word: &str) -> bool {
        // unimplemented!()
        match word {
            "lambda" | "define" | "map'" | "filter'" | "and" | "or" | "define-syntax-rule"
            | "eval" | "set!" | "let" | "begin" | "if" | "quote" | "..." | "struct"
            | "datum->syntax" => true,
            _ => false,
        }
    }

    // TODO
    // Stop clobbering the span information
    // Set the span of the resulting expr to be the original span passed in!
    // For now do an extra walk
    pub fn expand(&self, list_of_tokens: &[Expr]) -> Result<Expr> {
        let case_to_expand = self.match_case(list_of_tokens)?;
        let original_spans: Vec<Span> = list_of_tokens.iter().map(|x| x.span()).collect();
        let coalesced_span = Expr::coalesce_span(original_spans);
        let expanded_expr = case_to_expand.expand(list_of_tokens)?;
        Ok(Expr::rewrite_span(expanded_expr, coalesced_span))
    }
}

#[cfg(test)]
mod parse_macro_tests {

    use super::MacroPattern::*;
    use super::*;
    use crate::parser::tokens::TokenType;
    use crate::parser::Expr::*;
    use crate::parser::ParseError;
    use crate::parser::Parser;

    use crate::env::Env;
    use std::cell::RefCell;
    use std::rc::Rc;

    #[test]
    fn parse_single_syntax_rules() {
        // (define-syntax when
        //     (syntax-rules ()
        //       [(when a b ...)
        //        (if a (begin b ...) void)]))
        let input = &[
            Atom(SyntaxObject::default(Identifier(
                "define-syntax".to_string(),
            ))),
            Atom(SyntaxObject::default(Identifier("when".to_string()))),
            VectorVal(vec![
                Atom(SyntaxObject::default(Identifier(
                    "syntax-rules".to_string(),
                ))),
                VectorVal(vec![]),
                VectorVal(vec![
                    VectorVal(vec![
                        Atom(SyntaxObject::default(Identifier("when".to_string()))),
                        Atom(SyntaxObject::default(Identifier("a".to_string()))),
                        Atom(SyntaxObject::default(Identifier("b".to_string()))),
                        Atom(SyntaxObject::default(Identifier("...".to_string()))),
                    ]),
                    VectorVal(vec![
                        Atom(SyntaxObject::default(Identifier("if".to_string()))),
                        Atom(SyntaxObject::default(Identifier("a".to_string()))),
                        VectorVal(vec![
                            Atom(SyntaxObject::default(Identifier("begin".to_string()))),
                            Atom(SyntaxObject::default(Identifier("b".to_string()))),
                        ]),
                        Atom(SyntaxObject::default(Identifier("void".to_string()))),
                    ]),
                ]),
            ]),
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
                body: VectorVal(vec![
                    Atom(SyntaxObject::default(Identifier("if".to_string()))),
                    Atom(SyntaxObject::default(Identifier("a".to_string()))),
                    VectorVal(vec![
                        Atom(SyntaxObject::default(Identifier("begin".to_string()))),
                        Atom(SyntaxObject::default(Identifier("b".to_string()))),
                    ]),
                    Atom(SyntaxObject::default(Identifier("void".to_string()))),
                ]),
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
                        body: Atom(SyntaxObject::default(BooleanLiteral(false))),
                    },
                    MacroCase {
                        args: vec![Syntax("or".to_string()), Single("x".to_string())],
                        body: Atom(SyntaxObject::default(Identifier("x".to_string()))),
                    },
                    MacroCase {
                        args: vec![
                            Syntax("or".to_string()),
                            Single("x".to_string()),
                            Single("y".to_string()),
                        ],
                        body: VectorVal(vec![
                            Atom(SyntaxObject::default(Identifier("let".to_string()))),
                            VectorVal(vec![VectorVal(vec![
                                Atom(SyntaxObject::default(Identifier("##z".to_string()))),
                                Atom(SyntaxObject::default(Identifier("x".to_string()))),
                            ])]),
                            VectorVal(vec![
                                Atom(SyntaxObject::default(Identifier("if".to_string()))),
                                Atom(SyntaxObject::default(Identifier("##z".to_string()))),
                                Atom(SyntaxObject::default(Identifier("##z".to_string()))),
                                Atom(SyntaxObject::default(Identifier("y".to_string()))),
                            ]),
                        ]),
                    },
                    MacroCase {
                        args: vec![
                            Syntax("or".to_string()),
                            Single("x".to_string()),
                            Many("y".to_string()),
                        ],
                        body: VectorVal(vec![
                            Atom(SyntaxObject::default(Identifier("or".to_string()))),
                            Atom(SyntaxObject::default(Identifier("x".to_string()))),
                            VectorVal(vec![
                                Atom(SyntaxObject::default(Identifier("or".to_string()))),
                                Atom(SyntaxObject::default(Identifier("y".to_string()))),
                                Atom(SyntaxObject::default(Identifier("...".to_string()))),
                            ]),
                        ]),
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
                        body: VectorVal(vec![
                            Atom(SyntaxObject::default(Identifier("##when".to_string()))),
                            Atom(SyntaxObject::default(Identifier("e1".to_string()))),
                            Atom(SyntaxObject::default(Identifier("e2".to_string()))),
                            Atom(SyntaxObject::default(Identifier("...".to_string()))),
                        ]),
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
                    body: VectorVal(vec![
                        Atom(SyntaxObject::default(Identifier("##when".to_string()))),
                        Atom(SyntaxObject::default(Identifier("e1".to_string()))),
                        Atom(SyntaxObject::default(Identifier("e2".to_string()))),
                        Atom(SyntaxObject::default(Identifier("...".to_string()))),
                    ]),
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

    fn parse_with_empty_cache(s: &str) -> Expr {
        let mut cache: HashMap<String, Rc<TokenType>> = HashMap::new();
        let a: std::result::Result<Vec<Expr>, ParseError> = Parser::new(s, &mut cache).collect();
        let a = a.unwrap()[0].clone();
        a
    }

    fn parse_statement(s: &str) -> Vec<Expr> {
        let a = parse_with_empty_cache(s);
        a.vector_val_or_else(throw!(BadSyntax => "Malformed statement in the test"))
            .unwrap()
            .to_vec()
    }
}
