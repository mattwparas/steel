// use itertools::Itertools;

use quickscope::ScopeSet;

use crate::compiler::passes::reader::MultipleArityFunctions;
use crate::compiler::passes::Folder;
use crate::compiler::program::REQUIRE_DYLIB;
use crate::parser::ast::ExprKind;
use crate::parser::parser::SyntaxObject;
use crate::steel_vm::builtin::BuiltInModule;
use crate::steel_vm::engine::ModuleContainer;
use crate::{compiler::program::REQUIRE_BUILTIN, rvals::Result};
use crate::{compiler::program::STRUCT_KEYWORD, parser::visitors::ConsumingVisitor};
use crate::{
    compiler::program::{AS_KEYWORD, DOC_MACRO},
    parser::tokens::TokenType,
};

use steel_parser::expr_list;

use super::{
    ast::{Atom, Begin, Define, LambdaFunction, List, Quote},
    interner::InternedString,
    kernel::Kernel,
};

use std::borrow::Cow;
use std::collections::HashMap;

use crate::parser::expander::SteelMacro;

// pub const REQUIRE_BUILTIN: &str = "require-builtin";
// pub const DOC_MACRO: &str = "@doc";

pub fn extract_macro_defs(
    exprs: Vec<ExprKind>,
    macro_map: &mut HashMap<InternedString, SteelMacro>,
) -> Result<Vec<ExprKind>> {
    let mut non_macros = Vec::new();
    for expr in exprs {
        if let ExprKind::Macro(m) = expr {
            let generated_macro = SteelMacro::parse_from_ast_macro(m)?;
            let name = generated_macro.name();
            macro_map.insert(*name, generated_macro);
        } else {
            non_macros.push(expr)
        }
    }
    Ok(non_macros)
}

pub fn expand(expr: ExprKind, map: &HashMap<InternedString, SteelMacro>) -> Result<ExprKind> {
    Expander {
        map,
        changed: false,
        in_scope_values: ScopeSet::new(),
    }
    .visit(expr)
}

pub struct Expander<'a> {
    map: &'a HashMap<InternedString, SteelMacro>,
    pub(crate) changed: bool,
    // We're going to actually check if the macro is in scope
    in_scope_values: ScopeSet<InternedString>,
}

impl<'a> Expander<'a> {
    pub fn new(map: &'a HashMap<InternedString, SteelMacro>) -> Self {
        Self {
            map,
            changed: false,
            in_scope_values: ScopeSet::new(),
        }
    }

    pub fn expand(&mut self, expr: ExprKind) -> Result<ExprKind> {
        self.visit(expr)
    }
}

// TODO: See if we can do this without blowing the stack
impl<'a> ConsumingVisitor for Expander<'a> {
    type Output = Result<ExprKind>;

    fn visit_if(&mut self, mut f: Box<super::ast::If>) -> Self::Output {
        f.test_expr = self.visit(f.test_expr)?;
        f.then_expr = self.visit(f.then_expr)?;
        f.else_expr = self.visit(f.else_expr)?;
        Ok(ExprKind::If(f))
    }

    fn visit_define(&mut self, mut define: Box<super::ast::Define>) -> Self::Output {
        define.body = self.visit(define.body)?;
        Ok(ExprKind::Define(define))
    }

    fn visit_lambda_function(
        &mut self,
        mut lambda_function: Box<super::ast::LambdaFunction>,
    ) -> Self::Output {
        self.in_scope_values.push_layer();

        for value in &lambda_function.args {
            if let Some(ident) = value.atom_identifier() {
                self.in_scope_values.define(*ident);
            }
        }

        lambda_function.body = self.visit(lambda_function.body)?;

        self.in_scope_values.pop_layer();

        Ok(ExprKind::LambdaFunction(lambda_function))
    }

    fn visit_begin(&mut self, mut begin: super::ast::Begin) -> Self::Output {
        begin.exprs = begin
            .exprs
            .into_iter()
            .map(|e| self.visit(e))
            .collect::<Result<Vec<_>>>()?;
        Ok(ExprKind::Begin(begin))
    }

    fn visit_return(&mut self, mut r: Box<super::ast::Return>) -> Self::Output {
        r.expr = self.visit(r.expr)?;
        Ok(ExprKind::Return(r))
    }

    fn visit_quote(&mut self, quote: Box<super::ast::Quote>) -> Self::Output {
        // println!("Visiting quote with : {:?}", quote);
        // quote.expr = self.visit(quote.expr)?;
        Ok(ExprKind::Quote(quote))
    }

    fn visit_macro(&mut self, m: super::ast::Macro) -> Self::Output {
        stop!(BadSyntax => format!("unexpected macro definition in expand visitor: {}", m); m.location.span)
    }

    fn visit_atom(&mut self, a: Atom) -> Self::Output {
        Ok(ExprKind::Atom(a))
    }

    fn visit_list(&mut self, mut l: super::ast::List) -> Self::Output {
        // todo!()
        if let Some(ExprKind::Atom(Atom {
            syn:
                SyntaxObject {
                    ty: TokenType::Identifier(s),
                    span: sp,
                    ..
                },
        })) = l.first()
        {
            if let Some(m) = self.map.get(s) {
                // If this macro has been overwritten by any local value, respect
                // the local binding and do not expand the macro
                if !self.in_scope_values.contains(s) {
                    let expanded = m.expand(l.clone(), *sp)?;
                    self.changed = true;
                    return self.visit(expanded);
                }

                // let expanded = m.expand(l.clone(), *sp)?;
                // self.changed = true;
                // return self.visit(expanded);
            }
        }

        l.args = l
            .args
            .into_iter()
            .map(|e| self.visit(e))
            .collect::<Result<Vec<_>>>()?;

        Ok(ExprKind::List(l))
    }

    fn visit_syntax_rules(&mut self, l: super::ast::SyntaxRules) -> Self::Output {
        dbg!(l.to_string());

        stop!(Generic => "unexpected syntax-rules definition"; l.location.span)
    }

    fn visit_set(&mut self, mut s: Box<super::ast::Set>) -> Self::Output {
        s.variable = self.visit(s.variable)?;
        s.expr = self.visit(s.expr)?;
        Ok(ExprKind::Set(s))
    }

    fn visit_require(&mut self, s: super::ast::Require) -> Self::Output {
        Ok(ExprKind::Require(s))
        // stop!(Generic => "unexpected require statement during expansion"; s.location.span)
    }

    fn visit_let(&mut self, mut l: Box<super::ast::Let>) -> Self::Output {
        let mut visited_bindings = Vec::new();

        for (binding, expr) in l.bindings {
            visited_bindings.push((self.visit(binding)?, self.visit(expr)?));
        }

        l.bindings = visited_bindings;
        l.body_expr = self.visit(l.body_expr)?;

        Ok(ExprKind::Let(l))
    }
}

pub fn expand_kernel_in_env(
    expr: ExprKind,
    kernel: Option<&mut Kernel>,
    builtin_modules: ModuleContainer,
    env: String,
) -> Result<ExprKind> {
    KernelExpander {
        map: kernel,
        changed: false,
        builtin_modules,
        environment: Some(Cow::from(env)),
    }
    .visit(expr)
}

pub fn expand_kernel(
    expr: ExprKind,
    kernel: Option<&mut Kernel>,
    builtin_modules: ModuleContainer,
) -> Result<ExprKind> {
    KernelExpander {
        map: kernel,
        changed: false,
        builtin_modules,
        environment: None,
    }
    .visit(expr)
}

pub struct KernelExpander<'a> {
    map: Option<&'a mut Kernel>,
    pub(crate) changed: bool,
    builtin_modules: ModuleContainer,
    environment: Option<Cow<'static, str>>,
}

impl<'a> KernelExpander<'a> {
    pub fn new(map: Option<&'a mut Kernel>, builtin_modules: ModuleContainer) -> Self {
        Self {
            map,
            changed: false,
            builtin_modules,
            environment: None,
        }
    }

    pub fn expand(&mut self, expr: ExprKind) -> Result<ExprKind> {
        self.visit(expr)
    }
}

fn _expand_default_arguments(
    lambda_function: Box<super::ast::LambdaFunction>,
) -> Result<Box<super::ast::LambdaFunction>> {
    // todo!()

    let _args_len = lambda_function.args.len();

    let mut found_pair = false;
    for argument in &lambda_function.args {
        if let ExprKind::List(_) = argument {
            found_pair = true;
        } else if found_pair {
            stop!(BadSyntax => "Non default argument occurs after a default argument"; lambda_function.location.span)
        }
    }

    let _non_default_bindings = lambda_function
        .args
        .iter()
        .filter(|x| !matches!(x, ExprKind::List(_)))
        .collect::<Vec<_>>();

    // let bindings = lambda_function
    //     .args
    //     .iter()
    //     .enumerate()
    //     .filter(|x| matches!(x.1, ExprKind::List(_)))
    //     .map(|x| {
    //         if let ExprKind::List(l) = x.1 {
    //             let var_name = l
    //                 .get(0)
    //                 .ok_or_else(throw!(BadSyntax => "empty default argument"))?;
    //             let expr = l.get(1).ok_or_else(
    //                 throw!(BadSyntax => "default argument missing default expression"),
    //             )?;

    //             let index = ExprKind::integer_literal(x.0 as isize, lambda_function.location.span);

    //             let body = vec![

    //             ]

    //             todo!()
    //         } else {
    //             unreachable!()
    //         }
    //     })
    //     .collect::<Result<Vec<_>>>();

    todo!()
}

// Adjust the generated code to raise a specific error saying certain keys were missing.
fn expand_keyword_arguments(
    lambda_function: Box<super::ast::LambdaFunction>,
) -> Result<Box<super::ast::LambdaFunction>> {
    // todo!()

    // TODO: Check if this already has a rest argument - if so, the generated code will need to be changed.
    // The naive generated code will not handle rest arguments with keyword arguments, which can be a concern.
    // In addition, this naively assumes that keyword arguments cannot be applied before positional arguments - which
    // on its own is not the worst restriction, and perhaps we can leave that in place.
    //
    // If there are rest arguments though, we'll need to split the rest argument list into two - the first half will then get
    // applied to the hashmap list, while the rest of the arguments will get applied to the correct place.

    // We don't want to do this! This is going to allocate extra!
    let lambda_function = MultipleArityFunctions::new()
        .visit_lambda_function(lambda_function)
        .into_lambda_function()
        .unwrap();

    // If this already has a rest arguments, we need to slice out the
    // remaining function values from the keywords, and then bind those to whatever variable in the original
    // list before we create the hash. Making the hash itself is also not exactly my favorite pattern - we need
    // to allocate extra stuff - what we should probably do is create a special keyword allocation that we
    // can consistently reuse inside the VM. If we can reuse that allocation repeatedly, we should be able
    // to avoid much of the overhead of the allocation.

    // TODO: Can partition these directly into the two groups
    let keyword_args: Vec<&ExprKind> = lambda_function
        .args
        .iter()
        .skip_while(|x| {
            !matches!(
                x,
                ExprKind::Atom(Atom {
                    syn: SyntaxObject {
                        ty: TokenType::Keyword(_),
                        ..
                    }
                })
            )
        })
        .collect();

    // If there is a rest argument, we'll want to grab it for later use in the expansion
    // TODO: @Matt - Come back to this one
    // let mut rest_arg_expr = None;

    // Bail out if theres no keyword args
    if keyword_args.is_empty() {
        return Ok(lambda_function);
    }

    if (keyword_args.len() % 2 != 0 && !lambda_function.rest)
        || (lambda_function.rest && keyword_args.len() - 1 % 2 != 0)
    {
        // The last argument is going to be the rest argument
        // if lambda_function.rest {
        //     rest_arg_expr = keyword_args.pop();
        // }

        stop!(Generic => "keyword arguments malformed - each option requires a value"; lambda_function.location.span)
    }

    let mut non_keyword_args: Vec<ExprKind> = lambda_function
        .args
        .iter()
        .take_while(|x| {
            !matches!(
                x,
                ExprKind::Atom(Atom {
                    syn: SyntaxObject {
                        ty: TokenType::Keyword(_),
                        ..
                    }
                })
            )
        })
        .cloned()
        .collect();

    // From the keyword args, group them into pairs
    let keyword_map = keyword_args
        .chunks(2)
        .into_iter()
        .map(|x| (x[0], x[1]))
        .collect::<Vec<_>>();

    if !keyword_map.iter().map(|x| x.0).all(|x| {
        matches!(
            x,
            ExprKind::Atom(Atom {
                syn: SyntaxObject {
                    ty: TokenType::Keyword(_),
                    ..
                }
            })
        )
    }) {
        dbg!(&keyword_map);

        stop!(Generic => "Non keyword arguments found after the first keyword argument"; lambda_function.location.span)
    }

    let bindings = keyword_map
        .into_iter()
        .map(|x| {
            let keyword = x.0;
            let original_var_name = x.1;

            // This is a bit wasteful... come back to this
            let (var_name, expr) = if let ExprKind::List(l) = original_var_name {
                (l[0].clone(), l[1].clone())
            } else {
                (original_var_name.clone(), original_var_name.clone())
            };

            // println!("{:?}", original_var_name);

            // TODO: Go here to implement default arguments
            let expression = ExprKind::default_if(
                expr_list![
                    ExprKind::ident("hash-contains?"),
                    ExprKind::ident("!!dummy-rest-arg!!"),
                    ExprKind::Quote(Box::new(Quote::new(
                        keyword.clone(),
                        lambda_function.location.clone(),
                    ))),
                ],
                var_name.clone(),
                ExprKind::default_if(
                    ExprKind::bool_lit(matches!(original_var_name, ExprKind::List(_))),
                    expr,
                    expr_list![
                        ExprKind::ident("error!"),
                        ExprKind::string_lit(format!(
                            "Function application missing required keyword argument: {keyword}"
                        ))
                    ],
                ),
            );

            let func = ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
                vec![var_name.clone()],
                expression,
                SyntaxObject::default(TokenType::Lambda),
            )));

            let application = expr_list![
                func,
                expr_list![
                    ExprKind::ident("hash-try-get"),
                    ExprKind::ident("!!dummy-rest-arg!!"),
                    ExprKind::Quote(Box::new(Quote::new(
                        keyword.clone(),
                        lambda_function.location.clone(),
                    ))),
                ],
            ];

            (var_name, application)
        })
        .collect::<Vec<_>>();

    non_keyword_args.push(ExprKind::ident("!!dummy-rest-arg!!"));

    let mut inner_application = vec![ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
        bindings.iter().map(|x| x.0.clone()).collect(),
        lambda_function.body.clone(),
        SyntaxObject::default(TokenType::Lambda),
    )))];

    inner_application.extend(bindings.iter().map(|x| x.1.clone()));

    Ok(Box::new(LambdaFunction::new_with_rest_arg(
        non_keyword_args,
        expr_list![
            ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
                vec![ExprKind::ident("!!dummy-rest-arg!!")],
                ExprKind::List(List::new(inner_application)),
                SyntaxObject::default(TokenType::Lambda),
            ))),
            expr_list![
                ExprKind::ident("apply"),
                ExprKind::ident("%keyword-hash"), // This shouldn't be `hash` directly - something with a specific error
                // TODO: do like, `(take x !!dummy-rest-arg!!)`
                ExprKind::ident("!!dummy-rest-arg!!"),
            ],
        ],
        SyntaxObject::default(TokenType::Lambda),
    )))
}

impl<'a> ConsumingVisitor for KernelExpander<'a> {
    type Output = Result<ExprKind>;

    fn visit_if(&mut self, mut f: Box<super::ast::If>) -> Self::Output {
        f.test_expr = self.visit(f.test_expr)?;
        f.then_expr = self.visit(f.then_expr)?;
        f.else_expr = self.visit(f.else_expr)?;
        Ok(ExprKind::If(f))
    }

    fn visit_define(&mut self, mut define: Box<super::ast::Define>) -> Self::Output {
        define.body = self.visit(define.body)?;
        Ok(ExprKind::Define(define))
    }

    // TODO: Kernel expander should have the liberty to parse everything
    // As a normal expression in order to match behavior
    fn visit_lambda_function(
        &mut self,
        mut lambda_function: Box<super::ast::LambdaFunction>,
    ) -> Self::Output {
        // TODO: Unfortunately this wipes out the span
        // There needs to be

        lambda_function.body = self.visit(lambda_function.body)?;

        // Expand keyword arguments if we can
        lambda_function = expand_keyword_arguments(lambda_function)?;

        // Expand default arguments here

        // if let Some(map) = &mut self.map {
        //     // println!("Function: {}", lambda_function);
        //     if map.contains_macro("%test-lambda%") {
        //         let arguments = ExprKind::List(super::ast::List::new(vec![
        //             ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Lambda))),
        //             ExprKind::List(super::ast::List::new(lambda_function.args)),
        //             lambda_function.body,
        //         ]));

        //         let expanded = map.expand("%lambda%", arguments)?;
        //         self.changed = true;
        //         if let ExprKind::LambdaFunction(_) = &expanded {
        //             return Ok(expanded);
        //         } else {
        //             unreachable!("The expansion above should always return a lambda")
        //         }
        //     }
        // }

        // lambda_function.body = self.visit(lambda_function.body)?;
        Ok(ExprKind::LambdaFunction(lambda_function))
    }

    fn visit_begin(&mut self, mut begin: super::ast::Begin) -> Self::Output {
        begin.exprs = begin
            .exprs
            .into_iter()
            .map(|e| self.visit(e))
            .collect::<Result<Vec<_>>>()?;
        Ok(ExprKind::Begin(begin))
    }

    fn visit_return(&mut self, mut r: Box<super::ast::Return>) -> Self::Output {
        r.expr = self.visit(r.expr)?;
        Ok(ExprKind::Return(r))
    }

    fn visit_quote(&mut self, mut quote: Box<super::ast::Quote>) -> Self::Output {
        quote.expr = self.visit(quote.expr)?;
        Ok(ExprKind::Quote(quote))
    }

    fn visit_macro(&mut self, m: super::ast::Macro) -> Self::Output {
        stop!(BadSyntax => format!("unexpected macro definition in kernel expander: {}", m); m.location.span)
    }

    fn visit_atom(&mut self, a: Atom) -> Self::Output {
        Ok(ExprKind::Atom(a))
    }

    // TODO: This is not the best way of doing things, but for now we can accept it for the sake of progress
    fn visit_list(&mut self, mut l: super::ast::List) -> Self::Output {
        // todo!()
        if let Some(ExprKind::Atom(Atom {
            syn:
                SyntaxObject {
                    ty: TokenType::Identifier(s),
                    ..
                },
        })) = l.first().cloned()
        {
            if let Some(map) = &mut self.map {
                if s == *DOC_MACRO {
                    if l.len() != 3 {
                        stop!(BadSyntax => "Malformed @doc statement!")
                    }

                    let mut args = l.into_iter();
                    args.next(); // Skip past the doc macro

                    let comment = args.next().unwrap();
                    let top_level_define = args.next().unwrap();

                    match &top_level_define {
                        // A classic @doc case
                        // (@doc "comment" (define <name> <body>))
                        ExprKind::Define(d) => {
                            let doc_expr = ExprKind::Define(Box::new(Define::new(
                                ExprKind::atom(
                                    d.name.atom_identifier().unwrap().to_string() + "__doc__",
                                ),
                                comment,
                                SyntaxObject::default(TokenType::Define),
                            )));

                            // let ast_name = ExprKind::atom(
                            // d.name.atom_identifier().unwrap().to_string() + "__ast__",
                            // );

                            // Include the metadata table
                            let metadata_table_addition = ExprKind::List(List::new(vec![
                                ExprKind::atom("#%function-ptr-table-add"),
                                ExprKind::atom("#%function-ptr-table"),
                                ExprKind::atom(d.name.atom_identifier().unwrap().clone()),
                                ExprKind::atom(
                                    d.name.atom_identifier().unwrap().to_string() + "__doc__",
                                ),
                            ]));

                            let expanded_expr = self.visit(top_level_define)?;

                            // let quoted_ast = define_quoted_ast_node(ast_name, &expanded_expr);

                            return Ok(ExprKind::Begin(Begin::new(
                                vec![doc_expr, expanded_expr, metadata_table_addition],
                                SyntaxObject::default(TokenType::Begin),
                            )));
                        }

                        // An edge case that should eventually be realized:
                        //
                        ExprKind::Begin(b) => match &b.exprs.as_slice() {
                            // If this is a sequence of two things, catch the latter one like above
                            &[ExprKind::Define(d), ExprKind::Set(s), _]
                                if d.name.atom_identifier() == s.variable.atom_identifier() =>
                            {
                                let doc_expr = ExprKind::Define(Box::new(Define::new(
                                    ExprKind::atom(
                                        d.name.atom_identifier().unwrap().to_string() + "__doc__",
                                    ),
                                    comment,
                                    SyntaxObject::default(TokenType::Define),
                                )));

                                // let ast_name = ExprKind::atom(
                                // d.name.atom_identifier().unwrap().to_string() + "__ast__",
                                // );

                                let expanded_expr = self.visit(top_level_define)?;

                                // let quoted_ast = define_quoted_ast_node(ast_name, &expanded_expr);

                                return Ok(ExprKind::Begin(Begin::new(
                                    vec![doc_expr, expanded_expr],
                                    SyntaxObject::default(TokenType::Begin),
                                )));
                            }
                            _ => return self.visit(top_level_define),
                        },

                        ExprKind::List(struct_def)
                            if struct_def.first_ident() == Some(&STRUCT_KEYWORD) =>
                        {
                            if let Some(struct_name) =
                                struct_def.get(1).and_then(|x| x.atom_identifier())
                            {
                                let doc_expr = ExprKind::Define(Box::new(Define::new(
                                    ExprKind::atom(struct_name.to_string() + "__doc__"),
                                    comment,
                                    SyntaxObject::default(TokenType::Define),
                                )));

                                // let ast_name = ExprKind::atom(struct_name.to_string() + "__ast__");

                                // let quoted_ast =
                                // define_quoted_ast_node(ast_name, &top_level_define);

                                return Ok(ExprKind::Begin(Begin::new(
                                    vec![doc_expr, self.visit(top_level_define)?],
                                    SyntaxObject::default(TokenType::Begin),
                                )));
                            }

                            return self.visit(top_level_define);
                        }

                        _ => {
                            return self.visit(top_level_define);
                        }
                    }
                }

                if map
                    .contains_syntax_object_macro(&s, self.environment.as_ref().map(|x| x.as_ref()))
                {
                    let expanded = map.expand_syntax_object(
                        &s,
                        ExprKind::List(l),
                        self.environment
                            .as_ref()
                            .map(|x| x.as_ref())
                            .unwrap_or("default"),
                    )?;
                    self.changed = true;
                    return self.visit(expanded);
                }
            }

            //
            if s == *REQUIRE_DYLIB {
                match &l.args[1..] {
                    [ExprKind::Atom(Atom {
                        syn:
                            SyntaxObject {
                                ty: TokenType::StringLiteral(dylib_name),
                                ..
                            },
                    }), ExprKind::List(List { args, .. })] => {
                        // TODO: if it can't be found, the module needs to be marked as `MaybeDylib`
                        // and use the binds that are listed in the dylib require spec, something like:
                        // (require-builtin steel/obviouslydylib/sqlite (only-in ... ... ...)) <-
                        // Then, we can _attempt_ to load the dylib at runtime. If we can't we move on, and
                        // otherwise we can error if the identifiers are not lining up.
                        // (require-dylib "<name>.so" (onlt-in <spec> ))

                        // if let Some(module) = self.builtin_modules.get(s.as_str()) {
                        //     return Ok(module.to_syntax(None));
                        // } else {
                        //     stop!(BadSyntax => "require-builtin: module not found: {}", s);
                        // }

                        match args.as_slice() {
                            [ExprKind::Atom(Atom {
                                syn:
                                    SyntaxObject {
                                        ty: TokenType::Identifier(s),
                                        ..
                                    },
                            }), rest @ ..]
                                if s.resolve() == "only-in" =>
                            {
                                // self.builtin_modules.

                                let mut names = Vec::with_capacity(rest.len());

                                for expr in rest {
                                    if let Some(identifier) = expr.atom_identifier() {
                                        names.push(identifier);
                                    } else {
                                        stop!(BadSyntax => "require-dylib `only-in` modifier expects identifiers")
                                    }
                                }

                                return Ok(BuiltInModule::dylib_to_syntax(
                                    dylib_name.as_str(),
                                    names.iter().map(|x| x.resolve()),
                                    None,
                                ));
                            }
                            _ => {
                                stop!(BadSyntax => "require-dylib expects on `only-in` modifier")
                            }
                        }
                    }

                    _ => {
                        stop!(BadSyntax => "require-dylib malformed")
                    }
                }
            }

            if s == *REQUIRE_BUILTIN {
                match &l.args[1..] {
                    [ExprKind::Atom(Atom {
                        syn:
                            SyntaxObject {
                                ty: TokenType::StringLiteral(s),
                                ..
                            },
                    })] => {
                        // TODO: if it can't be found, the module needs to be marked as `MaybeDylib`
                        // and use the binds that are listed in the dylib require spec, something like:
                        // (require-builtin steel/obviouslydylib/sqlite (only-in ... ... ...)) <-
                        // Then, we can _attempt_ to load the dylib at runtime. If we can't we move on, and
                        // otherwise we can error if the identifiers are not lining up.
                        if let Some(module) = self.builtin_modules.get(s.as_str()) {
                            return Ok(module.to_syntax(None));
                        } else {
                            stop!(BadSyntax => "require-builtin: module not found: {}", s);
                        }
                    }

                    [ExprKind::Atom(Atom {
                        syn:
                            SyntaxObject {
                                ty: TokenType::Identifier(s),
                                ..
                            },
                    })] => {
                        if let Some(module) = self.builtin_modules.get(s.resolve()) {
                            return Ok(module.to_syntax(None));
                        } else {
                            stop!(BadSyntax => "require-builtin: module not found: {}", s);
                        }
                    }

                    [ExprKind::Atom(Atom {
                        syn:
                            SyntaxObject {
                                ty: TokenType::StringLiteral(s),
                                ..
                            },
                    }), ExprKind::Atom(Atom {
                        syn:
                            SyntaxObject {
                                ty: TokenType::Identifier(az),
                                ..
                            },
                    }), ExprKind::Atom(Atom {
                        syn:
                            SyntaxObject {
                                ty: TokenType::Identifier(prefix),
                                ..
                            },
                    })] if *az == *AS_KEYWORD => {
                        if let Some(module) = self.builtin_modules.get(s.as_str()) {
                            return Ok(module.to_syntax(Some(prefix.resolve())));
                        } else {
                            stop!(BadSyntax => "require-builtin: module not found: {}", s);
                        }
                    }

                    [ExprKind::Atom(Atom {
                        syn:
                            SyntaxObject {
                                ty: TokenType::Identifier(s),
                                ..
                            },
                    }), ExprKind::Atom(Atom {
                        syn:
                            SyntaxObject {
                                ty: TokenType::Identifier(az),
                                ..
                            },
                    }), ExprKind::Atom(Atom {
                        syn:
                            SyntaxObject {
                                ty: TokenType::Identifier(prefix),
                                ..
                            },
                    })] if *az == *AS_KEYWORD => {
                        if let Some(module) = self.builtin_modules.get(s.resolve()) {
                            return Ok(module.to_syntax(Some(prefix.resolve())));
                        } else {
                            stop!(BadSyntax => "require-builtin: module not found: {}", s);
                        }
                    }

                    _ => {
                        stop!(ArityMismatch => "require-builtin malformed - follows the pattern (require-builtin \"<module>\") or (require-builtin \"<module>\" as <prefix>")
                    }
                }
            }
        }

        l.args = l
            .args
            .into_iter()
            .map(|e| self.visit(e))
            .collect::<Result<Vec<_>>>()?;

        Ok(ExprKind::List(l))
    }

    fn visit_syntax_rules(&mut self, l: super::ast::SyntaxRules) -> Self::Output {
        dbg!(l.to_string());

        stop!(Generic => "unexpected syntax-rules definition"; l.location.span)
    }

    fn visit_set(&mut self, mut s: Box<super::ast::Set>) -> Self::Output {
        s.variable = self.visit(s.variable)?;
        s.expr = self.visit(s.expr)?;
        Ok(ExprKind::Set(s))
    }

    fn visit_require(&mut self, s: super::ast::Require) -> Self::Output {
        Ok(ExprKind::Require(s))
        // stop!(Generic => "unexpected require statement during expansion"; s.location.span)
    }

    fn visit_let(&mut self, mut l: Box<super::ast::Let>) -> Self::Output {
        let mut visited_bindings = Vec::new();

        for (binding, expr) in l.bindings {
            visited_bindings.push((self.visit(binding)?, self.visit(expr)?));
        }

        l.bindings = visited_bindings;
        l.body_expr = self.visit(l.body_expr)?;

        Ok(ExprKind::Let(l))
    }
}

fn _define_quoted_ast_node(ast_name: ExprKind, expanded_expr: &ExprKind) -> ExprKind {
    ExprKind::Define(Box::new(Define::new(
        ast_name,
        ExprKind::Quote(Box::new(Quote::new(
            expanded_expr.clone(),
            SyntaxObject::default(TokenType::Quote),
        ))),
        SyntaxObject::default(TokenType::Define),
    )))
}

#[cfg(test)]
mod expansion_tests {
    use super::*;

    use crate::parser::expander::MacroCase;
    use crate::parser::expander::MacroPattern;

    use crate::parser::ast::{Begin, If, List};
    use crate::parser::tokens::TokenType;

    fn atom_identifier(s: &str) -> ExprKind {
        ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Identifier(
            s.into(),
        ))))
    }

    fn ellipses() -> ExprKind {
        ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Ellipses)))
    }

    #[test]
    fn test_basic_expansion() {
        // (define-syntax when
        //     (syntax-rules ()
        //       [(when a b ...)
        //        (if a (begin b ...) void)]))
        let m = SteelMacro::new(
            "when".into(),
            Vec::new(),
            vec![MacroCase::new(
                vec![
                    MacroPattern::Syntax("when".into()),
                    MacroPattern::Single("a".into()),
                    MacroPattern::Many("b".into()),
                ],
                If::new(
                    atom_identifier("a"),
                    Begin::new(
                        vec![atom_identifier("b"), ellipses()],
                        SyntaxObject::default(TokenType::Begin),
                    )
                    .into(),
                    atom_identifier("void"),
                    SyntaxObject::default(TokenType::If),
                )
                .into(),
            )],
        );

        let mut map = HashMap::new();
        map.insert("when".into(), m);

        let input: ExprKind = List::new(vec![
            atom_identifier("when"),
            atom_identifier("blagh"),
            atom_identifier("do-thing"),
        ])
        .into();

        let expected: ExprKind = If::new(
            atom_identifier("blagh"),
            Begin::new(
                vec![atom_identifier("do-thing")],
                SyntaxObject::default(TokenType::Begin),
            )
            .into(),
            atom_identifier("void"),
            SyntaxObject::default(TokenType::If),
        )
        .into();

        let output = expand(input, &map).unwrap();

        assert_eq!(expected, output)
    }
}
