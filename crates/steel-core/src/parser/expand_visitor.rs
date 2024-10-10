use fxhash::{FxBuildHasher, FxHashMap, FxHashSet};
use quickscope::ScopeSet;
use steel_parser::ast::{parse_lambda, Begin};
use steel_parser::parser::SourceId;

use crate::parser::ast::ExprKind;
use crate::parser::parser::SyntaxObject;
use crate::parser::span_visitor::get_span;
use crate::steel_vm::engine::ModuleContainer;
use crate::{compiler::program::AS_KEYWORD, parser::tokens::TokenType};
use crate::{compiler::program::REQUIRE_BUILTIN, rvals::Result};

use steel_parser::expr_list;

use super::visitors::VisitorMutRef;
use super::{
    ast::{Atom, Define, LambdaFunction, List, Quote},
    interner::InternedString,
    kernel::Kernel,
};

use crate::parser::expander::SteelMacro;

pub fn extract_macro_defs(
    exprs: &mut Vec<ExprKind>,
    macro_map: &mut FxHashMap<InternedString, SteelMacro>,
) -> Result<()> {
    // let mut non_macros = Vec::new();
    // for expr in exprs {
    //     if let ExprKind::Macro(m) = expr {
    //         let generated_macro = SteelMacro::parse_from_ast_macro(m)?;
    //         let name = generated_macro.name();
    //         macro_map.insert(*name, generated_macro);
    //     } else {
    //         non_macros.push(expr)
    //     }
    // }
    // Ok(non_macros)

    let mut error = None;

    exprs.retain_mut(|expr| {
        if let ExprKind::Macro(_) = expr {
            // Replace with dummy begin value so we don't have to copy
            // everything other for every macro definition
            let mut taken_expr = ExprKind::Begin(Box::new(Begin::new(
                Vec::new(),
                SyntaxObject::default(TokenType::Begin),
            )));

            std::mem::swap(expr, &mut taken_expr);

            if let ExprKind::Macro(m) = taken_expr {
                match SteelMacro::parse_from_ast_macro(m) {
                    Ok(generated_macro) => {
                        let name = generated_macro.name();
                        macro_map.insert(*name, generated_macro);
                    }
                    Err(e) => {
                        error = Some(e);
                        return false;
                    }
                }
            } else {
                unreachable!();
            }

            return false;
        }

        true
    });

    if let Some(e) = error {
        return Err(e);
    }

    Ok(())
}

pub fn expand(expr: &mut ExprKind, map: &FxHashMap<InternedString, SteelMacro>) -> Result<()> {
    let mut expander = Expander {
        depth: 0,
        map,
        changed: false,
        in_scope_values: ScopeSet::default(),
        source_id: SourceId::none(),
    };
    expander.visit(expr)
}

pub fn expand_with_source_id(
    expr: &mut ExprKind,
    map: &FxHashMap<InternedString, SteelMacro>,
    source_id: Option<SourceId>,
) -> Result<()> {
    let mut expander = Expander {
        depth: 0,
        map,
        changed: false,
        in_scope_values: ScopeSet::default(),
        source_id,
    };

    expander.visit(expr)
}

pub struct Expander<'a> {
    map: &'a FxHashMap<InternedString, SteelMacro>,
    pub(crate) changed: bool,
    // We're going to actually check if the macro is in scope
    in_scope_values: ScopeSet<InternedString, FxBuildHasher>,
    source_id: Option<SourceId>,
    depth: usize,
}

impl<'a> Expander<'a> {
    pub fn new(map: &'a FxHashMap<InternedString, SteelMacro>) -> Self {
        Self {
            map,
            changed: false,
            in_scope_values: ScopeSet::default(),
            source_id: SourceId::none(),
            depth: 0,
        }
    }

    pub fn expand(&mut self, expr: &mut ExprKind) -> Result<()> {
        self.visit(expr)
    }
}

impl<'a> VisitorMutRef for Expander<'a> {
    type Output = Result<()>;

    fn visit(&mut self, expr: &mut ExprKind) -> Self::Output {
        // println!("expanding: {}", expr);

        if self.depth > 512 {
            stop!(Generic => "macro expansion depth reached!");
        }

        match expr {
            ExprKind::If(f) => self.visit_if(f),
            ExprKind::Define(d) => self.visit_define(d),
            ExprKind::LambdaFunction(l) => self.visit_lambda_function(l),
            ExprKind::Begin(b) => self.visit_begin(b),
            ExprKind::Return(r) => self.visit_return(r),
            ExprKind::Let(l) => self.visit_let(l),
            ExprKind::Quote(q) => self.visit_quote(q),
            ExprKind::Macro(m) => self.visit_macro(m),
            ExprKind::Atom(a) => self.visit_atom(a),
            ExprKind::List(l) => {
                match l.first() {
                    // TODO: Come back to this?
                    // Some(ExprKind::Atom(
                    //     ident @ Atom {
                    //         syn:
                    //             SyntaxObject {
                    //                 ty: TokenType::Identifier(s),
                    //                 ..
                    //             },
                    //     },
                    // )) if *s == *LAMBDA_SYMBOL || *s == *LAMBDA => {
                    //     if let ExprKind::LambdaFunction(mut lambda) =
                    //         parse_lambda(&ident.clone(), l.args.clone())?
                    //     {
                    //         self.visit_lambda_function(&mut lambda)?;

                    //         *expr = ExprKind::LambdaFunction(lambda);

                    //         return Ok(());

                    //         // return self.visit_lambda_function(&mut lambda);

                    //         // return self.visit_lambda_function(lambda);
                    //     } else {
                    //         unreachable!()
                    //     }
                    // }
                    Some(ExprKind::Atom(
                        ident @ Atom {
                            syn:
                                SyntaxObject {
                                    ty: TokenType::Lambda,
                                    ..
                                },
                        },
                    )) => {
                        if let ExprKind::LambdaFunction(mut lambda) =
                            parse_lambda(ident.clone(), std::mem::take(&mut l.args))?
                        {
                            if l.improper {
                                lambda.rest = true;
                            }

                            self.visit_lambda_function(&mut lambda)?;

                            *expr = ExprKind::LambdaFunction(lambda);

                            return Ok(());

                            // return self.visit_lambda_function(&mut lambda);
                        } else {
                            unreachable!()
                        }
                    }
                    Some(ExprKind::Atom(Atom {
                        syn:
                            SyntaxObject {
                                ty: TokenType::Identifier(s),
                                span: sp,
                                ..
                            },
                    })) => {
                        // if s.resolve().ends_with("skip-compile") {
                        //     println!("visiting {}", s.resolve());
                        // for key in self.map.keys() {
                        // println!("{}", key.resolve());
                        // }
                        // }

                        if let Some(m) = self.map.get(s) {
                            // If this macro has been overwritten by any local value, respect
                            // the local binding and do not expand the macro
                            if !self.in_scope_values.contains(s) {
                                if self.source_id.is_none()
                                    || self.source_id == m.location.source_id()
                                {
                                    let span = *sp;

                                    let mut expanded = m.expand(
                                        List::new_maybe_improper(
                                            std::mem::take(&mut l.args),
                                            l.improper,
                                        ),
                                        span,
                                    )?;
                                    self.changed = true;

                                    self.depth += 1;

                                    self.visit(&mut expanded)?;

                                    self.depth -= 1;

                                    *expr = expanded;

                                    return Ok(());
                                }

                                // let expanded = m.expand(l.clone(), *sp)?;
                                // self.changed = true;
                                // return self.visit(expanded);
                            }
                        }
                    }
                    _ => {}
                }

                for expr in l.args.iter_mut() {
                    self.visit(expr)?;
                }

                Ok(())

                // l.args = l
                //     .args
                //     .into_iter()
                //     .map(|e| self.visit(e))
                //     .collect::<Result<Vec<_>>>()?;
            }
            ExprKind::SyntaxRules(s) => self.visit_syntax_rules(s),
            ExprKind::Set(s) => self.visit_set(s),
            ExprKind::Require(r) => self.visit_require(r),
            ExprKind::Vector(v) => self.visit_vector(v),
        }
    }

    fn visit_if(&mut self, f: &mut super::ast::If) -> Self::Output {
        self.visit(&mut f.test_expr)?;
        self.visit(&mut f.then_expr)?;
        self.visit(&mut f.else_expr)?;
        Ok(())
    }

    fn visit_define(&mut self, define: &mut super::ast::Define) -> Self::Output {
        self.visit(&mut define.body)?;
        Ok(())
    }

    fn visit_lambda_function(
        &mut self,
        lambda_function: &mut super::ast::LambdaFunction,
    ) -> Self::Output {
        self.in_scope_values.push_layer();

        for value in &lambda_function.args {
            if let Some(ident) = value.atom_identifier() {
                self.in_scope_values.define(*ident);
            }
        }

        self.visit(&mut lambda_function.body)?;

        self.in_scope_values.pop_layer();

        Ok(())
    }

    fn visit_begin(&mut self, begin: &mut super::ast::Begin) -> Self::Output {
        for expr in begin.exprs.iter_mut() {
            self.visit(expr)?;
        }

        Ok(())
    }

    fn visit_return(&mut self, r: &mut super::ast::Return) -> Self::Output {
        self.visit(&mut r.expr)
    }

    fn visit_quote(&mut self, _quote: &mut super::ast::Quote) -> Self::Output {
        // println!("Visiting quote with : {:?}", quote);
        // quote.expr = self.visit(quote.expr)?;
        // Ok(ExprKind::Quote(quote))
        Ok(())
    }

    fn visit_macro(&mut self, m: &mut super::ast::Macro) -> Self::Output {
        stop!(BadSyntax => format!("unexpected macro definition in expand visitor: {}", m); m.location.span)
    }

    fn visit_atom(&mut self, _a: &mut Atom) -> Self::Output {
        // Ok(ExprKind::Atom(a))
        Ok(())
    }

    fn visit_syntax_rules(&mut self, l: &mut super::ast::SyntaxRules) -> Self::Output {
        stop!(Generic => "unexpected syntax-rules definition"; l.location.span)
    }

    fn visit_set(&mut self, s: &mut super::ast::Set) -> Self::Output {
        self.visit(&mut s.variable)?;
        self.visit(&mut s.expr)?;
        Ok(())
    }

    fn visit_require(&mut self, _s: &mut super::ast::Require) -> Self::Output {
        // Ok(ExprKind::Require(s))
        Ok(())
    }

    fn visit_let(&mut self, l: &mut super::ast::Let) -> Self::Output {
        for (binding, expr) in l.bindings.iter_mut() {
            self.visit(binding)?;
            self.visit(expr)?;
        }

        self.visit(&mut l.body_expr)
    }

    fn visit_list(&mut self, _l: &mut List) -> Self::Output {
        Ok(())
    }

    fn visit_vector(&mut self, v: &mut super::ast::Vector) -> Self::Output {
        Ok(())
    }
}

pub fn expand_kernel_in_env_with_allowed(
    mut expr: ExprKind,
    kernel: Option<&mut Kernel>,
    builtin_modules: ModuleContainer,
    env: &str,
    allowed: &FxHashSet<InternedString>,
) -> Result<(ExprKind, bool)> {
    let mut expander = KernelExpander {
        map: kernel,
        changed: false,
        builtin_modules,
        environment: Some(env),
        depth: 0,
        allowed_macros: Some(allowed),
    };

    expander.visit(&mut expr)?;

    Ok((expr, expander.changed))

    // expander.visit(expr).map(|x| (x, expander.changed))
}

pub fn expand_kernel_in_env_with_change(
    expr: &mut ExprKind,
    kernel: Option<&mut Kernel>,
    builtin_modules: ModuleContainer,
    env: &str,
) -> Result<bool> {
    let mut expander = KernelExpander {
        map: kernel,
        changed: false,
        builtin_modules,
        environment: Some(env),
        depth: 0,
        allowed_macros: None,
    };

    expander.visit(expr)?;

    Ok(expander.changed)

    // Ok((expr, expander.changed))
}

pub fn expand_kernel_in_env(
    expr: &mut ExprKind,
    kernel: Option<&mut Kernel>,
    builtin_modules: ModuleContainer,
    env: &str,
) -> Result<()> {
    let mut expander = KernelExpander {
        map: kernel,
        changed: false,
        builtin_modules,
        environment: Some(env),
        depth: 0,
        allowed_macros: None,
    };

    expander.visit(expr)
}

pub fn expand_kernel(
    mut expr: ExprKind,
    kernel: Option<&mut Kernel>,
    builtin_modules: ModuleContainer,
) -> Result<ExprKind> {
    let mut expander = KernelExpander {
        map: kernel,
        changed: false,
        builtin_modules,
        environment: None,
        depth: 0,
        allowed_macros: None,
    };

    expander.visit(&mut expr)?;

    Ok(expr)
}

pub struct KernelExpander<'a> {
    map: Option<&'a mut Kernel>,
    pub(crate) changed: bool,
    builtin_modules: ModuleContainer,
    environment: Option<&'a str>,
    depth: usize,
    allowed_macros: Option<&'a FxHashSet<InternedString>>,
}

impl<'a> KernelExpander<'a> {
    pub fn new(map: Option<&'a mut Kernel>, builtin_modules: ModuleContainer) -> Self {
        Self {
            map,
            changed: false,
            builtin_modules,
            environment: None,
            depth: 0,
            allowed_macros: None,
        }
    }

    pub fn expand(&mut self, mut expr: ExprKind) -> Result<ExprKind> {
        self.visit(&mut expr)?;

        Ok(expr)
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
fn expand_keyword_arguments(lambda_function: &mut super::ast::LambdaFunction) -> Result<()> {
    // TODO: Check if this already has a rest argument - if so, the generated code will need to be changed.
    // The naive generated code will not handle rest arguments with keyword arguments, which can be a concern.
    // In addition, this naively assumes that keyword arguments cannot be applied before positional arguments - which
    // on its own is not the worst restriction, and perhaps we can leave that in place.
    //
    // If there are rest arguments though, we'll need to split the rest argument list into two - the first half will then get
    // applied to the hashmap list, while the rest of the arguments will get applied to the correct place.

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
        return Ok(());
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

    *lambda_function = LambdaFunction::new_with_rest_arg(
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
    );

    Ok(())
}

// VisitorMutRef
impl<'a> VisitorMutRef for KernelExpander<'a> {
    type Output = Result<()>;

    fn visit_if(&mut self, f: &mut super::ast::If) -> Self::Output {
        self.visit(&mut f.test_expr)?;
        self.visit(&mut f.then_expr)?;
        self.visit(&mut f.else_expr)
    }

    fn visit_define(&mut self, define: &mut super::ast::Define) -> Self::Output {
        self.visit(&mut define.body)
    }

    // TODO: Kernel expander should have the liberty to parse everything
    // As a normal expression in order to match behavior
    fn visit_lambda_function(
        &mut self,
        lambda_function: &mut super::ast::LambdaFunction,
    ) -> Self::Output {
        // TODO: Unfortunately this wipes out the span
        // There needs to be

        self.visit(&mut lambda_function.body)?;

        // Expand keyword arguments if we can
        expand_keyword_arguments(lambda_function)?;

        Ok(())
    }

    fn visit_begin(&mut self, begin: &mut super::ast::Begin) -> Self::Output {
        for expr in begin.exprs.iter_mut() {
            self.visit(expr)?;
        }

        Ok(())
    }

    fn visit_return(&mut self, r: &mut super::ast::Return) -> Self::Output {
        self.visit(&mut r.expr)
    }

    fn visit_quote(&mut self, quote: &mut super::ast::Quote) -> Self::Output {
        self.visit(&mut quote.expr)
    }

    fn visit_macro(&mut self, m: &mut super::ast::Macro) -> Self::Output {
        stop!(BadSyntax => format!("unexpected macro definition in kernel expander: {}", m); m.location.span)
    }

    fn visit_atom(&mut self, _a: &mut Atom) -> Self::Output {
        Ok(())
    }

    fn visit_list(&mut self, _l: &mut super::ast::List) -> Self::Output {
        Ok(())
    }

    fn visit_syntax_rules(&mut self, l: &mut super::ast::SyntaxRules) -> Self::Output {
        stop!(Generic => "unexpected syntax-rules definition"; l.location.span)
    }

    fn visit_set(&mut self, s: &mut super::ast::Set) -> Self::Output {
        self.visit(&mut s.variable)?;
        self.visit(&mut s.expr)?;

        Ok(())
    }

    fn visit_require(&mut self, _s: &mut super::ast::Require) -> Self::Output {
        Ok(())
    }

    fn visit_let(&mut self, l: &mut super::ast::Let) -> Self::Output {
        for (binding, expr) in l.bindings.iter_mut() {
            self.visit(binding)?;
            self.visit(expr)?;
        }

        // l.bindings = visited_bindings;
        self.visit(&mut l.body_expr)

        // Ok(ExprKind::Let(l))
    }

    fn visit(&mut self, expr: &mut ExprKind) -> Self::Output {
        if self.depth > 96 {
            stop!(BadSyntax => "Current expansion depth of defmacro style macros exceeded: depth capped at 96"; get_span(&expr));
        }

        let res = match expr {
            ExprKind::If(f) => self.visit_if(f),
            ExprKind::Define(d) => self.visit_define(d),
            ExprKind::LambdaFunction(l) => self.visit_lambda_function(l),
            ExprKind::Begin(b) => self.visit_begin(b),
            ExprKind::Return(r) => self.visit_return(r),
            ExprKind::Let(l) => self.visit_let(l),
            ExprKind::Quote(q) => self.visit_quote(q),
            ExprKind::Macro(m) => self.visit_macro(m),
            ExprKind::Atom(a) => self.visit_atom(a),
            ExprKind::List(l) => {
                {
                    // todo!()
                    if let Some(s) = l.first().and_then(|x| {
                        if let ExprKind::Atom(Atom {
                            syn:
                                SyntaxObject {
                                    ty: TokenType::Identifier(s),
                                    ..
                                },
                        }) = x
                        {
                            Some(*s)
                        } else {
                            None
                        }
                    }) {
                        if let Some(map) = &mut self.map {
                            if map.contains_syntax_object_macro(
                                &s,
                                self.environment.as_ref().map(|x| x.as_ref()),
                            ) && self
                                .allowed_macros
                                .as_ref()
                                .map(|x| x.contains(&s))
                                .unwrap_or(true)
                            {
                                let mut expanded = map.expand_syntax_object(
                                    &s,
                                    ExprKind::List(std::mem::replace(l, List::new(Vec::new()))),
                                    self.environment
                                        .as_ref()
                                        .map(|x| x.as_ref())
                                        .unwrap_or("default"),
                                )?;
                                self.changed = true;

                                self.depth += 1;

                                self.visit(&mut expanded)?;

                                self.depth -= 1;

                                *expr = expanded;

                                return Ok(());

                                // return result;
                            }
                        }

                        if s == *REQUIRE_BUILTIN {
                            match &l.args[1..] {
                                [ExprKind::Atom(Atom {
                                    syn:
                                        SyntaxObject {
                                            ty: TokenType::StringLiteral(s),
                                            span,
                                            ..
                                        },
                                })] => {
                                    // TODO: if it can't be found, the module needs to be marked as `MaybeDylib`
                                    // and use the binds that are listed in the dylib require spec, something like:
                                    // (require-builtin steel/obviouslydylib/sqlite (only-in ... ... ...)) <-
                                    // Then, we can _attempt_ to load the dylib at runtime. If we can't we move on, and
                                    // otherwise we can error if the identifiers are not lining up.
                                    if let Some(module) = self.builtin_modules.get(s.as_str()) {
                                        *expr = module.to_syntax(None);
                                        return Ok(());

                                        // return Ok(module.to_syntax(None));
                                    } else {
                                        stop!(BadSyntax => format!("require-builtin: module not found: {}", s); *span);
                                    }
                                }

                                [ExprKind::Atom(Atom {
                                    syn:
                                        SyntaxObject {
                                            ty: TokenType::Identifier(s),
                                            span,
                                            ..
                                        },
                                })] => {
                                    if let Some(module) = self.builtin_modules.get(s.resolve()) {
                                        *expr = module.to_syntax(None);
                                        return Ok(());
                                    } else {
                                        stop!(BadSyntax => format!("require-builtin: module not found: {}", s); *span);
                                    }
                                }

                                [ExprKind::Atom(Atom {
                                    syn:
                                        SyntaxObject {
                                            ty: TokenType::StringLiteral(s),
                                            span,
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
                                        *expr = module.to_syntax(Some(prefix.resolve()));

                                        return Ok(());
                                    } else {
                                        stop!(BadSyntax => format!("require-builtin: module not found: {}", s); *span);
                                    }
                                }

                                [ExprKind::Atom(Atom {
                                    syn:
                                        SyntaxObject {
                                            ty: TokenType::Identifier(s),
                                            span,
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
                                        *expr = module.to_syntax(Some(prefix.resolve()));

                                        return Ok(());
                                    } else {
                                        stop!(BadSyntax => format!("require-builtin: module not found: {}", s); *span);
                                    }
                                }

                                _ => {
                                    stop!(ArityMismatch => "require-builtin malformed - follows the pattern (require-builtin \"<module>\") or (require-builtin \"<module>\" as <prefix>")
                                }
                            }
                        }
                    }

                    for expr in l.args.iter_mut() {
                        self.visit(expr)?;
                    }

                    Ok(())
                }
            }
            ExprKind::SyntaxRules(s) => self.visit_syntax_rules(s),
            ExprKind::Set(s) => self.visit_set(s),
            ExprKind::Require(r) => self.visit_require(r),
            ExprKind::Vector(v) => self.visit_vector(v),
        };

        res
    }

    fn visit_vector(&mut self, v: &mut super::ast::Vector) -> Self::Output {
        Ok(())
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
    use steel_parser::span::Span;

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
            Span::default(),
        );

        let mut map = FxHashMap::default();
        map.insert("when".into(), m);

        let mut input: ExprKind = List::new(vec![
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

        expand(&mut input, &map).unwrap();

        assert_eq!(expected, input)
    }
}
