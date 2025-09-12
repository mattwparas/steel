extern crate proc_macro;
extern crate proc_macro2;
#[macro_use]
extern crate syn;
extern crate quote;
use std::collections::HashMap;

use proc_macro::TokenStream;
use proc_macro2::Group;
use quote::{quote, ToTokens};
use syn::{
    punctuated::Punctuated, spanned::Spanned, token::Comma, Attribute, Data, DeriveInput, Expr,
    ExprGroup, ExprLit, FnArg, Ident, ItemFn, Lit, LitStr, Meta, Pat, ReturnType, Signature, Type,
    TypeReference,
};

#[proc_macro]
pub fn steel_quote(input: TokenStream) -> TokenStream {
    let token_iter = proc_macro2::TokenStream::from(input).into_iter();

    let mut identifiers: Vec<Ident> = Vec::new();
    let mut list_identifiers: Vec<Ident> = Vec::new();
    let mut tokens: Vec<proc_macro2::TokenTree> = Vec::new();

    walk(
        token_iter,
        &mut list_identifiers,
        &mut identifiers,
        &mut tokens,
    );

    let original = proc_macro2::TokenStream::from_iter(tokens).to_string();

    let identifier_str: Vec<String> = identifiers.iter().map(|x| x.to_string()).collect();

    let list_identifiers_str: Vec<String> =
        list_identifiers.iter().map(|x| x.to_string()).collect();

    quote! {
        ::steel::parser::replace_idents::expand_template_pair(
            steel::::parser::parser::Parser::parse(#original).unwrap(),
            vec![
                #(
                    (#identifier_str.into(), (::steel::parser::expander::BindingKind::Single, #identifiers)),
                )*

                #(
                   (#list_identifiers_str.into(), (::steel::parser::expander::BindingKind::Many, #list_identifiers)),
                )*
            ]
        )

    }
    .into()
}

#[proc_macro]
pub fn internal_steel_quote(input: TokenStream) -> TokenStream {
    let token_iter = proc_macro2::TokenStream::from(input).into_iter();

    let mut identifiers: Vec<Ident> = Vec::new();
    let mut list_identifiers: Vec<Ident> = Vec::new();
    let mut tokens: Vec<proc_macro2::TokenTree> = Vec::new();

    walk(
        token_iter,
        &mut list_identifiers,
        &mut identifiers,
        &mut tokens,
    );

    let original = proc_macro2::TokenStream::from_iter(tokens).to_string();

    let identifier_str: Vec<String> = identifiers.iter().map(|x| x.to_string()).collect();

    let list_identifiers_str: Vec<String> =
        list_identifiers.iter().map(|x| x.to_string()).collect();

    quote! {
        crate::parser::replace_idents::expand_template_pair(
            crate::parser::parser::Parser::parse(#original).unwrap(),
            vec![
                #(
                    (#identifier_str.into(), (crate::parser::expander::BindingKind::Single, #identifiers)),
                )*

                #(
                   (#list_identifiers_str.into(), (crate::parser::expander::BindingKind::Many, #list_identifiers)),
                )*
            ]
        )

    }
    .into()
}

fn walk(
    mut token_iter: proc_macro2::token_stream::IntoIter,
    list_identifiers: &mut Vec<Ident>,
    identifiers: &mut Vec<Ident>,
    tokens: &mut Vec<proc_macro2::TokenTree>,
) {
    while let Some(next) = token_iter.next() {
        match &next {
            proc_macro2::TokenTree::Group(g) => {
                let mut child = Vec::new();

                walk(
                    g.stream().into_iter(),
                    list_identifiers,
                    identifiers,
                    &mut child,
                );

                tokens.push(proc_macro2::TokenTree::Group(Group::new(
                    g.delimiter(),
                    proc_macro2::TokenStream::from_iter(child.into_iter()),
                )));
            }
            proc_macro2::TokenTree::Punct(p) if p.as_char() == '@' => {
                if let Some(proc_macro2::TokenTree::Ident(next_ident)) = token_iter.next() {
                    list_identifiers.push(next_ident.clone());
                    tokens.push(proc_macro2::TokenTree::Ident(next_ident));
                } else {
                    panic!();
                };
            }
            proc_macro2::TokenTree::Punct(p) if p.as_char() == '#' => {
                if let Some(proc_macro2::TokenTree::Ident(next_ident)) = token_iter.next() {
                    identifiers.push(next_ident.clone());
                    tokens.push(proc_macro2::TokenTree::Ident(next_ident));
                } else {
                    panic!();
                };
            }
            _ => {
                tokens.push(next);
            }
        }
    }
}

fn derive_steel_impl(input: DeriveInput, prefix: proc_macro2::TokenStream) -> TokenStream {
    let name = &input.ident;
    let mut names = Vec::new();
    let mut values = Vec::new();

    let should_impl_equals = should_derive_param(&input, "equality");
    let should_impl_getters = should_derive_param(&input, "getters");
    let should_impl_constructor =
        should_derive_param(&input, "constructor") || should_derive_param(&input, "constructors");

    match &input.data {
        Data::Struct(s) => {
            match &s.fields {
                syn::Fields::Named(_) => {
                    let field_names_args = s.fields.iter().filter_map(|x| x.ident.clone());
                    let field_names_body = s.fields.iter().filter_map(|x| x.ident.clone());

                    if should_impl_constructor {
                        names.push(format!("{}", name));
                        values.push(quote! {
                            |#(
                                #field_names_args,
                            )*| #name {
                                #(
                                    #field_names_body,
                                )*
                            }
                        });
                    }

                    if should_impl_getters {
                        for field in s.fields.iter() {
                            if !filter_out_ignored(field) {
                                continue;
                            }

                            if let Some(field_name) = &field.ident {
                                let accessor_func = format!("{}-{}", name, field_name);

                                // Accessors
                                names.push(accessor_func.clone());

                                values.push(quote! {
                                    |value: &#name| {
                                        use #prefix::rvals::IntoSteelVal;
                                        value.#field_name.clone().into_steelval()
                                    }
                                });
                            }
                        }
                    }
                }
                // TODO:
                syn::Fields::Unnamed(_) => {
                    if should_impl_constructor {
                        names.push(format!("{}", name,));
                        values.push(quote! {
                            #name
                        });
                    }

                    if should_impl_getters {
                        for (index, field) in s.fields.iter().enumerate() {
                            if !filter_out_ignored(field) {
                                continue;
                            }

                            let accessor_func = format!("{}-{}", name, index);

                            let index = syn::Index::from(index);

                            // Accessors
                            names.push(accessor_func.clone());

                            values.push(quote! {
                                |value: &#name| {
                                    use #prefix::rvals::IntoSteelVal;
                                    value.#index.clone().into_steelval()
                                }
                            });
                        }
                    }
                }
                syn::Fields::Unit => {
                    if should_impl_constructor {
                        names.push(format!("{}", name));
                        values.push(quote! {
                            || #name
                        });
                    }
                }
            }

            let equality_impl = if should_impl_equals {
                quote! {
                    fn equality_hint(&self, other: &dyn #prefix::rvals::CustomType) -> bool {
                        if let Some(other) = #prefix::rvals::as_underlying_type::<#name>(other) {
                            self == other
                        } else {
                            false
                        }
                    }
                }
            } else {
                quote! {}
            };

            let gen = quote! {
                impl #prefix::rvals::Custom for #name {
                    #equality_impl
                }

                impl #name {
                    #[doc = "Registers the struct functions with this module"]
                    fn register_type(module: &mut #prefix::steel_vm::builtin::BuiltInModule) ->
                        &mut #prefix::steel_vm::builtin::BuiltInModule {
                        use #prefix::steel_vm::register_fn::RegisterFn;
                        #(
                            module.register_fn(#names, #values);
                        )*

                        module
                    }
                }
            };

            gen.into()
        }
        Data::Enum(e) => {
            let mut names = Vec::new();
            let mut values = Vec::new();

            // Iterate over the variants, and generate a function
            // to check each of the variants.
            'variant: for variant in &e.variants {
                let identifier = &variant.ident;

                for attr in &variant.attrs {
                    if !filter_out_ignored_attr(attr) {
                        continue 'variant;
                    }
                }

                match variant.fields {
                    syn::Fields::Named(_) => {
                        names.push(format!("{}-{}?", name, identifier));
                        values.push(quote! {
                        |value: #prefix::rvals::SteelVal| {
                            use #prefix::gc::ShareableMut;
                            if let #prefix::rvals::SteelVal::Custom(c) = value {
                                if let Some(inner) = #prefix::rvals::as_underlying_type::<#name>(c.read().as_ref())
                                {
                                    return matches!(inner, #name::#identifier{..});
                                }
                            }
                            false
                        }});

                        if should_impl_constructor {
                            let field_names_args = variant
                                .fields
                                .iter()
                                // TODO: Filter out those we want to ignore for accessors, but
                                // otherwise don't do it here?
                                .filter_map(|x| x.ident.clone());

                            let field_names_body =
                                variant.fields.iter().filter_map(|x| x.ident.clone());

                            names.push(format!("{}-{}", name, identifier));
                            values.push(quote! {
                                |#(
                                    #field_names_args,
                                )*| #name::#identifier {
                                    #(
                                        #field_names_body,
                                    )*
                                }
                            });
                        }

                        if should_impl_getters {
                            for field in variant.fields.iter() {
                                if !filter_out_ignored(field) {
                                    continue;
                                }

                                if let Some(field_name) = &field.ident {
                                    let accessor_func =
                                        format!("{}-{}-{}", name, identifier, field_name);

                                    // Accessors
                                    names.push(accessor_func.clone());

                                    values.push(quote! {
                                        |value: &#name| {
                                            use #prefix::rvals::IntoSteelVal;
                                            use #prefix::stop;
                                            use #prefix::rerrs::SteelErr;

                                            if let #name::#identifier { #field_name, .. } = &value {
                                                #field_name.clone().into_steelval()
                                            } else {
                                                #prefix::stop!(TypeMismatch =>
                                                    format!("{} expected {}-{}, found {:?}",
                                                    #accessor_func,
                                                    stringify!(#name),
                                                    stringify!(#identifier),
                                                    value));
                                            }
                                        }
                                    });
                                }
                            }
                        }
                    }
                    syn::Fields::Unnamed(_) => {
                        names.push(format!("{}-{}?", name, identifier));
                        values.push(quote! {
                        |value: #prefix::rvals::SteelVal| {
                            use #prefix::gc::ShareableMut;
                            if let #prefix::rvals::SteelVal::Custom(c) = value {
                                if let Some(inner) = #prefix::rvals::as_underlying_type::<#name>(c.read().as_ref())
                                {
                                    return matches!(inner, #name::#identifier(..));
                                }
                            }
                            false
                        }});

                        if should_impl_constructor {
                            names.push(format!("{}-{}", name, identifier));
                            values.push(quote! {
                                #name::#identifier
                            });
                        }

                        if should_impl_getters {
                            for (field_name, _) in variant
                                .fields
                                .iter()
                                .filter(|x| filter_out_ignored(x))
                                .enumerate()
                            {
                                let accessor_func =
                                    format!("{}-{}-{}", name, identifier, field_name);

                                let blank = vec![quote!(_); field_name];

                                // Accessors
                                names.push(accessor_func.clone());

                                values.push(quote! {
                                    |value: &#name| {
                                        use #prefix::rvals::IntoSteelVal;
                                        use #prefix::stop;
                                        use #prefix::rerrs::SteelErr;

                                        if let #name::#identifier(#(#blank,)* value, ..) = value {
                                            value.clone().into_steelval()
                                        } else {
                                            #prefix::stop!(TypeMismatch =>
                                                format!("{} expected {}-{}, found {:?}",
                                                #accessor_func,
                                                stringify!(#name),
                                                stringify!(#identifier), value));
                                        }
                                    }
                                });
                            }
                        }
                    }
                    syn::Fields::Unit => {
                        names.push(format!("{}-{}?", name, identifier));
                        values.push(quote! {
                        |value: #prefix::rvals::SteelVal| {
                            use #prefix::gc::ShareableMut;
                            if let #prefix::rvals::SteelVal::Custom(c) = value {
                                if let Some(inner) = #prefix::rvals::as_underlying_type::<#name>(c.read().as_ref())
                                {
                                    return matches!(inner, #name::#identifier);
                                }
                            }
                            false
                        }});

                        if should_impl_getters {
                            names.push(format!("{}-{}", name, identifier));
                            values.push(quote! {
                                || #name::#identifier
                            });
                        }
                    }
                }
            }

            let equality_impl = if should_impl_equals {
                quote! {
                    fn equality_hint(&self, other: &dyn #prefix::rvals::CustomType) -> bool {
                        if let Some(other) = #prefix::rvals::as_underlying_type::<#name>(other) {
                            self == other
                        } else {
                            false
                        }
                    }
                }
            } else {
                quote! {}
            };

            let gen = quote! {
                impl #prefix::rvals::Custom for #name {
                    #equality_impl
                }

                impl #name {
                    #[doc = "Registers the enum variant functions with this module"]
                    fn register_enum_variants(module: &mut #prefix::steel_vm::builtin::BuiltInModule) ->
                        &mut #prefix::steel_vm::builtin::BuiltInModule {
                        use #prefix::steel_vm::register_fn::RegisterFn;
                        #(
                            module.register_fn(#names, #values);
                        )*

                        module
                    }
                }
            };

            gen.into()
        }
        _ => {
            let output = quote! {};
            output.into()
        }
    }
}

fn should_derive_param(derive: &DeriveInput, kind: &str) -> bool {
    for attr in &derive.attrs {
        match &attr.meta {
            Meta::Path(_) => {}
            Meta::List(p) => {
                if p.path.is_ident("steel") {
                    let args = ::syn::parse::Parser::parse2(
                        Punctuated::<Ident, Token![,]>::parse_terminated,
                        p.tokens.clone(),
                    )
                    .unwrap();

                    for arg in args {
                        if arg == kind {
                            return true;
                        }
                    }
                }
            }
            Meta::NameValue(_) => {}
        }
    }

    false
}

fn filter_out_ignored_attr(attr: &Attribute) -> bool {
    match &attr.meta {
        Meta::Path(_) => {}
        Meta::List(p) => {
            if p.path.is_ident("steel") {
                let args = ::syn::parse::Parser::parse2(
                    Punctuated::<Ident, Token![,]>::parse_terminated,
                    p.tokens.clone(),
                )
                .unwrap();

                for arg in args {
                    if arg == "ignore" {
                        return false;
                    }
                }
            }
        }
        Meta::NameValue(_) => {}
    }

    true
}

fn filter_out_ignored(field: &syn::Field) -> bool {
    for attr in &field.attrs {
        match &attr.meta {
            Meta::Path(_) => {}
            Meta::List(p) => {
                if p.path.is_ident("steel") {
                    let args = ::syn::parse::Parser::parse2(
                        Punctuated::<Ident, Token![,]>::parse_terminated,
                        p.tokens.clone(),
                    )
                    .unwrap();

                    for arg in args {
                        if arg == "ignore" {
                            return false;
                        }
                    }
                }
            }
            Meta::NameValue(_) => {}
        }
    }

    true
}

#[proc_macro_derive(Steel, attributes(steel))]
pub fn derive_steel(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let prefix = quote! { ::steel };

    derive_steel_impl(input, prefix)
}

#[proc_macro_derive(_Steel, attributes(steel))]
pub fn derive_steel_internal(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let prefix = quote! { crate };
    derive_steel_impl(input, prefix)
}

fn parse_key_value_pairs(args: &Punctuated<Meta, Token![,]>) -> HashMap<String, String> {
    let mut map = HashMap::new();

    for nested_meta in args.iter() {
        if let Meta::NameValue(n) = nested_meta {
            let key = n.path.get_ident().unwrap().to_string();

            let mut value = &n.value;

            loop {
                match value {
                    Expr::Lit(ExprLit {
                        lit: Lit::Str(s), ..
                    }) => {
                        map.insert(key, s.value());
                        break;
                    }
                    Expr::Lit(ExprLit {
                        lit: Lit::Bool(b), ..
                    }) => {
                        map.insert(key, b.value().to_string());
                        break;
                    }
                    Expr::Group(ExprGroup { expr, .. }) => {
                        value = &**expr;
                    }
                    _ => break,
                }
            }
        }
    }

    map
}

fn parse_doc_comment(input: ItemFn) -> Option<proc_macro2::TokenStream> {
    let span = input.span();

    let maybe_str_literals = input
        .attrs
        .into_iter()
        .filter_map(|attr| match attr.meta {
            Meta::NameValue(name_value) if name_value.path.is_ident("doc") => {
                Some(name_value.value)
            }
            _ => None,
        })
        .map(|expr| match expr {
            Expr::Lit(ExprLit {
                lit: Lit::Str(s), ..
            }) => Ok(s),
            e => Err(e),
        })
        .collect::<Vec<_>>();

    if maybe_str_literals.is_empty() {
        return None;
    }

    if let Some(literals) = maybe_str_literals
        .iter()
        .map(|item| item.as_ref().ok())
        .collect::<Option<Vec<_>>>()
    {
        let trimmed: Vec<_> = literals
            .iter()
            .flat_map(|lit| {
                lit.value()
                    .split('\n')
                    .map(|s| s.to_string())
                    .collect::<Vec<_>>()
            })
            .map(|line| line.trim().to_string())
            .collect();

        let doc = trimmed.join("\n");

        return Some(quote! { #doc });
    }

    let mut args = vec![];

    for (i, item) in maybe_str_literals.into_iter().enumerate() {
        if i > 0 {
            args.push(Expr::Lit(ExprLit {
                attrs: vec![],
                lit: Lit::Str(LitStr::new("\n", span)),
            }));
        }

        let expr = match item {
            Ok(lit) => Expr::Lit(ExprLit {
                attrs: vec![],
                lit: Lit::Str(lit),
            }),
            Err(expr) => expr,
        };

        args.push(expr);
    }

    Some(quote! {
        concat![#(#args),*]
    })
}

#[proc_macro_attribute]
pub fn define_module(
    args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let args = parse_macro_input!(args with Punctuated::<Meta, Token![,]>::parse_terminated);
    let input = parse_macro_input!(input as ItemFn);
    let keyword_map = parse_key_value_pairs(&args);

    let value = keyword_map
        .get("name")
        .expect("native definition requires a name!");

    let sign: Signature = input.sig.clone();

    let maybe_doc_comments = parse_doc_comment(input.clone());

    let function_name = sign.ident;

    if let Some(doc_comments) = maybe_doc_comments {
        quote! {
            pub fn #function_name() -> BuiltInModule {
                #input

                let mut module = #function_name();

                module.register_doc(#value, crate::steel_vm::builtin::MarkdownDoc(#doc_comments.into()));

                module
            }
        }
        .into()
    } else {
        quote! {
            #input
        }
        .into()
    }
}

fn arity_code_injection(
    input: &ItemFn,
    args: &Punctuated<Meta, Comma>,
    is_context_function: bool,
) -> ItemFn {
    let keyword_map = parse_key_value_pairs(args);

    let arity_number = keyword_map
        .get("arity")
        .expect("native definition requires an arity");

    let func_name = input.sig.ident.to_string();

    //Context functions have 'ctx' as the first argument, so we get the second one for them
    let sig_inputs = if is_context_function {
        input.sig.inputs.get(1)
    } else {
        input.sig.inputs.first()
    };

    //This is to account for the parameter sometimes being "args", other times "values"
    let parameter_name = if let FnArg::Typed(pat_type) = sig_inputs.unwrap() {
        let pat_type = pat_type.clone();
        if let Pat::Ident(ident) = *pat_type.pat {
            ident.ident
        } else {
            panic!("Could not extract parameter name")
        }
    } else {
        panic!("Could not extract parameter name")
    };

    // This function extracts the Arity type and integer values
    let (name, numb, end_numb) = arity_number
        .strip_suffix(')')
        .and_then(|stripped| stripped.split_once('('))
        .map(|(name, rest)| {
            if let Some((start, end)) = rest.split_once(',') {
                // Handle Range(start, end)
                let start = start
                    .parse::<usize>()
                    .expect("Arity start value must be an integer");
                let end = end
                    .parse::<usize>()
                    .expect("Arity end value must be an integer");
                (name, start, end)
            } else {
                // Handle AtLeast(n) or single-value cases
                let num = rest
                    .parse::<usize>()
                    .expect("Arity value must be an integer");
                (name, num, 0)
            }
        })
        .expect("Arity header is wrongly formatted");

    //Context functions return values wrapped in Option, so we must use builtin_stop for them
    let stop_type = if is_context_function {
        quote! {crate::builtin_stop!}
    } else {
        quote! {stop!}
    };

    //Determines which line of code to inject into the beginning of the function as an Arity check
    let injected_code = match name {
        "AtLeast" => {
            // If AtLeast = 0 , then do not perform arity check, since any amount is valid
            if numb == 0 {
                quote! {{}}
            } else {
                quote! {
                    if #parameter_name.len() < #numb {
                        #stop_type(ArityMismatch => "{} expects at least {} arguments, found: {}", #func_name, #numb, #parameter_name.len());
                    }
                }
            }
        }

        "AtMost" => quote! {
            if #parameter_name.len() > #numb {
                   #stop_type(ArityMismatch => "{} expects at most {} arguments, found: {}",#func_name, #numb ,#parameter_name.len());
               }
        },

        "Exact" => quote! {
            if #parameter_name.len() != #numb {
                   #stop_type(ArityMismatch => "{} expects exactly {} arguments, found: {}",#func_name, #numb ,#parameter_name.len());
               }
        },
        "Range" => quote! {
            if (#parameter_name.len() < #numb) || (#parameter_name.len() > #end_numb) {
                   #stop_type(ArityMismatch => "{} expects {} to {} arguments, found: {}",#func_name, #numb , #end_numb ,#parameter_name.len());
               }
        },

        _ => panic!("Unsupported Arity Type"),
    };
    // Inject the new statements at the beginning of the function
    let mut modified_input = input.clone();
    modified_input
        .block
        .stmts
        .insert(0, syn::parse_quote!(#injected_code));
    modified_input
}

struct NativeMacroComponents {
    pub doc_field: proc_macro2::TokenStream,
    pub doc_name: proc_macro2::Ident,
    pub steel_function_name: String,
    pub rust_function_name: proc_macro2::Ident,
    pub arity_number: syn::Expr,
    pub is_const: bool,
}

fn native_macro_setup(input: &ItemFn, args: &Punctuated<Meta, Comma>) -> NativeMacroComponents {
    let keyword_map = parse_key_value_pairs(args);

    let steel_function_name = keyword_map
        .get("name")
        .expect("native definition requires a name!")
        .to_string();

    let arity_number = keyword_map
        .get("arity")
        .expect("native definition requires an arity");

    let is_const = keyword_map
        .get("constant")
        .map(|x| x == "true")
        .unwrap_or_default();

    let arity_number: syn::Expr =
        syn::parse_str(arity_number).expect("Unable to parse arity definition");

    let sign: Signature = input.clone().sig;

    let maybe_doc_comments = parse_doc_comment(input.clone());
    let rust_function_name = sign.ident.clone();

    let doc_name = Ident::new(
        &(rust_function_name.to_string().to_uppercase() + "_DEFINITION"),
        sign.ident.span(),
    );
    let doc_field = if let Some(doc) = maybe_doc_comments {
        quote! { Some(crate::steel_vm::builtin::MarkdownDoc::from_str(#doc)) }
    } else {
        quote! { None }
    };

    NativeMacroComponents {
        doc_field,
        doc_name,
        steel_function_name,
        rust_function_name,
        arity_number,
        is_const,
    }
}

#[proc_macro_attribute]
pub fn native(
    args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let args = parse_macro_input!(args with Punctuated::<Meta, Token![,]>::parse_terminated);
    let input = parse_macro_input!(input as ItemFn);

    let modified_input = if cfg!(feature = "disable-arity-checking") {
        input.clone()
    } else {
        arity_code_injection(&input, &args, false)
    };

    let NativeMacroComponents {
        doc_field,
        doc_name,
        steel_function_name,
        rust_function_name,
        arity_number,
        is_const,
    } = native_macro_setup(&input, &args);

    let definition_struct = quote! {
        pub const #doc_name: crate::steel_vm::builtin::NativeFunctionDefinition = crate::steel_vm::builtin::NativeFunctionDefinition {
            name: #steel_function_name,
            aliases: &[],
            func: crate::steel_vm::builtin::BuiltInFunctionType::Reference(#rust_function_name),
            arity: crate::steel_vm::builtin::Arity::#arity_number,
            doc: #doc_field,
            is_const: #is_const,
            signature: None,
        };
    };

    let output = quote! {
        // Not sure why, but it says this is unused even when generating functions
        // marked as pub
        #[allow(dead_code)]
        #modified_input

        #definition_struct
    };

    // Uncomment this to see the generated code
    // eprintln!("{}", output.to_string());

    output.into()
}

#[proc_macro_attribute]
pub fn context(
    args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let args = parse_macro_input!(args with Punctuated::<Meta, Token![,]>::parse_terminated);
    let input = parse_macro_input!(input as ItemFn);
    let modified_input = if cfg!(feature = "disable-arity-checking") {
        input.clone()
    } else {
        arity_code_injection(&input, &args, true)
    };

    let NativeMacroComponents {
        doc_field,
        doc_name,
        steel_function_name,
        rust_function_name,
        arity_number,
        is_const,
    } = native_macro_setup(&input, &args);

    let definition_struct = quote! {
        pub const #doc_name: crate::steel_vm::builtin::NativeFunctionDefinition = crate::steel_vm::builtin::NativeFunctionDefinition {
            name: #steel_function_name,
            aliases: &[],
            func: crate::steel_vm::builtin::BuiltInFunctionType::Context(#rust_function_name),
            arity: crate::steel_vm::builtin::Arity::#arity_number,
            doc: #doc_field,
            is_const: #is_const,
            signature: None,
        };
    };

    let output = quote! {
        // Not sure why, but it says this is unused even when generating functions
        // marked as pub
        #[allow(dead_code)]
        #modified_input

        #definition_struct
    };

    output.into()
}

#[proc_macro_attribute]
pub fn native_mut(
    args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let args = parse_macro_input!(args with Punctuated::<Meta, Token![,]>::parse_terminated);
    let input = parse_macro_input!(input as ItemFn);

    let modified_input = if cfg!(feature = "disable-arity-checking") {
        input.clone()
    } else {
        arity_code_injection(&input, &args, false)
    };

    let NativeMacroComponents {
        doc_field,
        doc_name,
        steel_function_name,
        rust_function_name,
        arity_number,
        is_const,
    } = native_macro_setup(&input, &args);

    let definition_struct = quote! {
        pub const #doc_name: crate::steel_vm::builtin::NativeFunctionDefinition = crate::steel_vm::builtin::NativeFunctionDefinition {
            name: #steel_function_name,
            aliases: &[],
            func: crate::steel_vm::builtin::BuiltInFunctionType::Mutable(#rust_function_name),
            arity: crate::steel_vm::builtin::Arity::#arity_number,
            doc: #doc_field,
            is_const: #is_const,
            signature: None,
        };
    };

    let output = quote! {
        // Not sure why, but it says this is unused even when generating functions
        // marked as pub
        #[allow(dead_code)]
        #modified_input

        #definition_struct
    };

    // Uncomment this to see the generated code
    // eprintln!("{}", output.to_string());

    output.into()
}
// See REmacs : https://github.com/remacs/remacs/blob/16b6fb9319a6d48fbc7b27d27c3234990f6718c5/rust_src/remacs-macros/lib.rs#L17-L161
// TODO: Pass the new name in to this function
#[proc_macro_attribute]
pub fn function(
    args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let args = parse_macro_input!(args with Punctuated::<Meta, Token![,]>::parse_terminated);

    // let (_, value) = parse_key_value_pair(&args);

    let keyword_map = parse_key_value_pairs(&args);

    let value = keyword_map
        .get("name")
        .expect("native definition requires a name!");

    // If this is constant evaluatable
    let is_const = keyword_map
        .get("constant")
        .map(|x| x == "true")
        .unwrap_or_default();

    let function_name_with_colon = value.clone() + ": ";

    let input = parse_macro_input!(input as ItemFn);

    let modified_input = input.clone();
    // let ident = input.sig.ident.clone();
    let sign: Signature = input.clone().sig;

    let maybe_doc_comments = parse_doc_comment(input);

    let return_type: ReturnType = sign.output;

    let ret_val = match return_type {
        ReturnType::Default => quote! {
            Ok(SteelVal::Void)
        },
        ReturnType::Type(_, r) => {
            if let Type::Path(val) = *r {
                let last = val.path.segments.into_iter().last();
                if let Some(last) = last {
                    match last.ident.into_token_stream().to_string().as_str() {
                        "Result" => quote! { res },
                        _ => quote! {
                            res.into_steelval().map_err(err_thunk)
                        },
                    }
                } else {
                    quote! {
                        Ok(SteelVal::Void)
                    }
                }
            } else {
                quote! {
                    res.into_steelval().map_err(err_thunk)
                }
            }
        }
    };

    let mut type_vec: Vec<Box<Type>> = Vec::new();

    let mut rest_arg_generic_inner_type = false;

    // let mut argument_signatures: Vec<&'static str> = Vec::new();
    // let mut return_type = "void";

    for (i, arg) in sign.inputs.iter().enumerate() {
        if let FnArg::Typed(pat_ty) = arg.clone() {
            if let Type::Path(p) = pat_ty.ty.as_ref() {
                let primary_type = p.path.segments.iter().last();
                if let Some(ty) = primary_type {
                    match ty.ident.to_token_stream().to_string().as_str() {
                        "RestArgs" | "RestArgsIter" => {
                            if rest_arg_generic_inner_type {
                                panic!("There cannot be multiple `RestArg`s for a given function.")
                            }

                            if i != sign.inputs.len() - 1 {
                                panic!(
                                    "The rest argument must be the last argument in the function."
                                )
                            }
                            rest_arg_generic_inner_type = true;
                        }
                        _ => {}
                    }
                }
            }

            /*
            TODO: Attempt to bake the type information into the native
            function definition. This can give a lot more help to the optimizer
            and also the LSP if we have the types for every built in definition
            when we make it.

            // Attempt to calculate the function signature
            match pat_ty.ty.clone().into_token_stream().to_string().as_str() {
                "char" => argument_signatures.push("char"),
                "bool" => argument_signatures.push("bool"),
                "f64" => argument_signatures.push("f64"),
                "isize" => argument_signatures.push("isize"),
                "SteelString" => argument_signatures.push("string"),
                "&SteelString" => argument_signatures.push("string"),
                _ => argument_signatures.push("any"),
            }
            */

            type_vec.push(pat_ty.ty);
        }
    }

    let mut arity_number = type_vec.len();

    // TODO: Awful hack, but this just keeps track of which
    // variables are presented as mutable, which we can then use to chn
    let promote_to_mutable = type_vec.iter().any(|x| {
        matches!(
            **x,
            Type::Reference(TypeReference {
                mutability: Some(_),
                ..
            })
        )
    });

    let conversion_functions = type_vec.clone().into_iter().map(|x| {
        if let Type::Reference(_) = *x {
            quote! { primitive_as_ref }
        } else if x.to_token_stream().to_string().starts_with("Either") {
            quote! { primitive_as_ref }
        } else {
            quote! { from_steelval }
        }
    });

    let arg_enumerate = type_vec.iter().enumerate();
    let arg_type = arg_enumerate.clone().map(|(_, x)| x);
    let arg_index = arg_enumerate.clone().map(|(i, _)| i);
    // let function_names_with_colon = std::iter::repeat(function_name_with_colon.clone());
    let function_name = sign.ident.clone();
    let _arity_name = Ident::new(
        &(function_name.to_string().to_uppercase() + "_ARITY"),
        sign.ident.span(),
    );
    let copied_function_name = Ident::new(
        &("steel_".to_string() + &function_name.to_string()),
        sign.ident.span(),
    );

    let doc_name = Ident::new(
        &(function_name.to_string().to_uppercase() + "_DEFINITION"),
        sign.ident.span(),
    );

    let arity_exactness = if rest_arg_generic_inner_type {
        arity_number -= 1;
        quote! { AtLeast }
    } else {
        quote! { Exact }
    };

    let function_type = if promote_to_mutable {
        quote! {
            crate::steel_vm::builtin::BuiltInFunctionType::Mutable(#copied_function_name)
        }
    } else {
        quote! {
            crate::steel_vm::builtin::BuiltInFunctionType::Reference(#copied_function_name)
        }
    };

    let aliases = match keyword_map.get("alias") {
        Some(alias) => quote! { &[ #alias ] },
        None => quote! { &[] },
    };

    let doc_field = if let Some(doc) = maybe_doc_comments {
        quote! { Some(crate::steel_vm::builtin::MarkdownDoc::from_str(#doc)) }
    } else {
        quote! { None }
    };

    let definition_struct = quote! {
        pub const #doc_name: crate::steel_vm::builtin::NativeFunctionDefinition = crate::steel_vm::builtin::NativeFunctionDefinition {
            name: #value,
            aliases: #aliases,
            func: #function_type,
            arity: crate::steel_vm::builtin::Arity::#arity_exactness(#arity_number),
            doc: #doc_field,
            is_const: #is_const,
            signature: None,
        };
    };

    // If we have a rest arg, we need to modify passing in values to pass in a slice to the remaining
    // values in the
    if rest_arg_generic_inner_type {
        let mut conversion_functions = conversion_functions.collect::<Vec<_>>();
        let mut arg_index = arg_enumerate
            .map(|(i, _)| quote! { #i })
            .collect::<Vec<_>>();

        if let Some(last) = arg_index.last_mut() {
            *last = quote! { #last.. };
        }

        if let Some(last) = conversion_functions.last_mut() {
            *last = quote! { from_slice };
        }

        let function_name = sign.ident;

        let output = quote! {
            // Not sure why, but it says this is unused even when generating functions
            // marked as pub
            #[allow(dead_code)]
            #modified_input

            #definition_struct

            pub fn #copied_function_name(args: &[SteelVal]) -> std::result::Result<SteelVal, crate::rerrs::SteelErr> {

                use crate::rvals::{IntoSteelVal, FromSteelVal, PrimitiveAsRef};

                if args.len() < #arity_number {
                    crate::stop!(ArityMismatch => format!("{} expected {} arguments, got {}", #value, #arity_number.to_string(), args.len()))
                }

                fn err_thunk(mut err: crate::rerrs::SteelErr) -> crate::rerrs::SteelErr {
                    err.prepend_message(#function_name_with_colon);
                    err.set_kind(crate::rerrs::ErrorKind::TypeMismatch);
                    err
                };

                let res = #function_name(
                    #(
                        // TODO: Distinguish reference types here if possible - make a special implementation
                        // for builtin pointer types here to distinguish them
                        <#arg_type>::#conversion_functions(&args[#arg_index])
                            .map_err(err_thunk)
                        ?,
                    )*
                );

                #ret_val
            }
        };

        // Uncomment this to see the generated code
        // eprintln!("{}", output.to_string());

        return output.into();
    }

    // TODO: Promotion to mutable means we can both avoid allocations and also
    // take advantage of linear types to reduce ref count thrash

    if promote_to_mutable {
        let temporary_fields: Vec<_> = type_vec
            .iter()
            .enumerate()
            .map(|(i, _)| {
                Ident::new(
                    &("temporary_".to_string() + &i.to_string()),
                    sign.ident.span(),
                )
            })
            .collect();

        let output = quote! {
                // Not sure why, but it says this is unused even when generating functions
                // marked as pub
                #[allow(dead_code)]
                #modified_input

                #definition_struct

                pub fn #copied_function_name(args: &mut [SteelVal]) -> std::result::Result<SteelVal, crate::rerrs::SteelErr> {

                    use crate::rvals::{IntoSteelVal, FromSteelVal, PrimitiveAsRef, PrimitiveAsRefMut};

                    // if args.len() != #arity_number {
                    //     crate::stop!(ArityMismatch => format!("{} expected {} arguments, got {}", #value, #arity_number.to_string(), args.len()))
                    // }


                    fn err_thunk(mut err: crate::rerrs::SteelErr) -> crate::rerrs::SteelErr {
                        err.prepend_message(#function_name_with_colon);
                        err.set_kind(crate::rerrs::ErrorKind::TypeMismatch);
                        err
                    };

                    if let [ #(#temporary_fields,)* ] = args {

                        let res = #function_name(
                            #(
                                // TODO: Distinguish reference types here if possible - make a special implementation
                                // for builtin pointer types here to distinguish them
                                <#arg_type>::#conversion_functions(#temporary_fields)
                                    .map_err(err_thunk)?,
                            )*
                        );

                        #ret_val
                    } else {
                        crate::stop!(ArityMismatch => format!("{} expected {} arguments, got {}", #value, #arity_number.to_string(), args.len()))

                    }

                        // if let [arg1, arg2, ..] = &mut vec[..] {
            //  println!("{}:{} says '{}'", ip, port, msg);
            // check_two(arg1, arg2);
        // }

                    // let res = #function_name(
                    //     #(
                    //         // TODO: Distinguish reference types here if possible - make a special implementation
                    //         // for builtin pointer types here to distinguish them
                    //         <#arg_type>::#conversion_functions(&mut std::mem::replace(&mut args[#arg_index], SteelVal::Void))
                    //             .map_err(err_thunk)?,
                    //     )*
                    // );

                    // #ret_val
                }
            };

        return output.into();
    }

    let output = quote! {
        // Not sure why, but it says this is unused even when generating functions
        // marked as pub
        #[allow(dead_code)]
        #modified_input

        #definition_struct

        pub fn #copied_function_name(args: &[SteelVal]) -> std::result::Result<SteelVal, crate::rerrs::SteelErr> {

            use crate::rvals::{IntoSteelVal, FromSteelVal, PrimitiveAsRef};

            if args.len() != #arity_number {
                crate::stop!(ArityMismatch => format!("{} expected {} arguments, got {}", #value, #arity_number.to_string(), args.len()))
            }


            fn err_thunk(mut err: crate::rerrs::SteelErr) -> crate::rerrs::SteelErr {
                err.prepend_message(#function_name_with_colon);
                err.set_kind(crate::rerrs::ErrorKind::TypeMismatch);
                err
            };

            let res = #function_name(
                #(
                    // TODO: Distinguish reference types here if possible - make a special implementation
                    // for builtin pointer types here to distinguish them
                    <#arg_type>::#conversion_functions(&args[#arg_index])
                        .map_err(err_thunk)?,
                )*
            );

            #ret_val
        }
    };

    // Uncomment this to see the generated code
    // eprintln!("{}", output.to_string());

    output.into()
}

#[proc_macro_attribute]
pub fn custom_function(
    args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let args = parse_macro_input!(args with Punctuated::<Meta, Token![,]>::parse_terminated);

    let keyword_map = parse_key_value_pairs(&args);

    let value = keyword_map
        .get("name")
        .expect("native definition requires a name!");

    // If this is constant evaluatable
    let is_const = keyword_map
        .get("constant")
        .map(|x| x == "true")
        .unwrap_or_default();

    let function_name_with_colon = value.clone() + ": ";

    let input = parse_macro_input!(input as ItemFn);

    let modified_input = input.clone();
    // let ident = input.sig.ident.clone();
    let sign: Signature = input.clone().sig;

    let maybe_doc_comments = parse_doc_comment(input);

    let return_type: ReturnType = sign.output;

    let ret_val = match return_type {
        ReturnType::Default => quote! {
            Ok(SteelVal::Void)
        },
        ReturnType::Type(_, r) => {
            if let Type::Path(val) = *r {
                let last = val.path.segments.into_iter().last();
                if let Some(last) = last {
                    match last.ident.into_token_stream().to_string().as_str() {
                        "Result" => quote! { res },
                        _ => quote! {
                            res.into_steelval().map_err(err_thunk)
                        },
                    }
                } else {
                    quote! {
                        Ok(SteelVal::Void)
                    }
                }
            } else {
                quote! {
                    res.into_steelval().map_err(err_thunk)
                }
            }
        }
    };

    let mut type_vec: Vec<Box<Type>> = Vec::new();

    let mut rest_arg_generic_inner_type = false;

    // let mut argument_signatures: Vec<&'static str> = Vec::new();
    // let mut return_type = "void";

    for (i, arg) in sign.inputs.iter().enumerate() {
        if let FnArg::Typed(pat_ty) = arg.clone() {
            if let Type::Path(p) = pat_ty.ty.as_ref() {
                let primary_type = p.path.segments.iter().last();
                if let Some(ty) = primary_type {
                    match ty.ident.to_token_stream().to_string().as_str() {
                        "RestArgs" | "RestArgsIter" => {
                            if rest_arg_generic_inner_type {
                                panic!("There cannot be multiple `RestArg`s for a given function.")
                            }

                            if i != sign.inputs.len() - 1 {
                                panic!(
                                    "The rest argument must be the last argument in the function."
                                )
                            }
                            rest_arg_generic_inner_type = true;
                        }
                        _ => {}
                    }
                }
            }

            /*
            TODO: Attempt to bake the type information into the native
            function definition. This can give a lot more help to the optimizer
            and also the LSP if we have the types for every built in definition
            when we make it.

            // Attempt to calculate the function signature
            match pat_ty.ty.clone().into_token_stream().to_string().as_str() {
                "char" => argument_signatures.push("char"),
                "bool" => argument_signatures.push("bool"),
                "f64" => argument_signatures.push("f64"),
                "isize" => argument_signatures.push("isize"),
                "SteelString" => argument_signatures.push("string"),
                "&SteelString" => argument_signatures.push("string"),
                _ => argument_signatures.push("any"),
            }
            */

            type_vec.push(pat_ty.ty);
        }
    }

    let mut arity_number = type_vec.len();

    // TODO: Awful hack, but this just keeps track of which
    // variables are presented as mutable, which we can then use to chn
    let promote_to_mutable = type_vec.iter().any(|x| {
        matches!(
            **x,
            Type::Reference(TypeReference {
                mutability: Some(_),
                ..
            })
        )
    });

    let conversion_functions = type_vec.clone().into_iter().map(|x| {
        if let Type::Reference(_) = *x {
            quote! { primitive_as_ref }
        } else if x.to_token_stream().to_string().starts_with("Either") {
            quote! { primitive_as_ref }
        } else {
            quote! { from_steelval }
        }
    });

    let arg_enumerate = type_vec.iter().enumerate();
    let arg_type = arg_enumerate.clone().map(|(_, x)| x);
    let arg_index = arg_enumerate.clone().map(|(i, _)| i);
    // let function_names_with_colon = std::iter::repeat(function_name_with_colon.clone());
    let function_name = sign.ident.clone();
    let _arity_name = Ident::new(
        &(function_name.to_string().to_uppercase() + "_ARITY"),
        sign.ident.span(),
    );
    let copied_function_name = Ident::new(
        &("steel_".to_string() + &function_name.to_string()),
        sign.ident.span(),
    );

    let doc_name = Ident::new(
        &(function_name.to_string().to_uppercase() + "_DEFINITION"),
        sign.ident.span(),
    );

    let arity_exactness = if rest_arg_generic_inner_type {
        // We don't want to include the rest argument in the count
        arity_number -= 1;
        quote! { AtLeast }
    } else {
        quote! { Exact }
    };

    let function_type = if promote_to_mutable {
        quote! {
            crate::steel_vm::builtin::BuiltInFunctionType::Mutable(#copied_function_name)
        }
    } else {
        quote! {
            crate::steel_vm::builtin::BuiltInFunctionType::Reference(#copied_function_name)
        }
    };

    let aliases = match keyword_map.get("alias") {
        Some(alias) => quote! { &[ #alias ] },
        None => quote! { &[] },
    };

    let doc_field = if let Some(doc) = maybe_doc_comments {
        quote! { Some(crate::steel_vm::builtin::MarkdownDoc::from_str(#doc)) }
    } else {
        quote! { None }
    };

    let definition_struct = quote! {
        pub const #doc_name: crate::steel_vm::builtin::NativeFunctionDefinition = crate::steel_vm::builtin::NativeFunctionDefinition {
            name: #value,
            aliases: #aliases,
            func: #function_type,
            arity: crate::steel_vm::builtin::Arity::#arity_exactness(#arity_number),
            doc: #doc_field,
            is_const: #is_const,
            signature: None,
        };
    };

    // If we have a rest arg, we need to modify passing in values to pass in a slice to the remaining
    // values in the
    if rest_arg_generic_inner_type {
        let mut conversion_functions = conversion_functions.collect::<Vec<_>>();
        let mut arg_index = arg_enumerate
            .map(|(i, _)| quote! { #i })
            .collect::<Vec<_>>();

        if let Some(last) = arg_index.last_mut() {
            *last = quote! { #last.. };
        }

        if let Some(last) = conversion_functions.last_mut() {
            *last = quote! { from_slice };
        }

        let function_name = sign.ident;

        let output = quote! {
            // Not sure why, but it says this is unused even when generating functions
            // marked as pub
            #[allow(dead_code)]
            #modified_input

            #definition_struct

            pub fn #copied_function_name(args: &[SteelVal]) -> std::result::Result<SteelVal, crate::rerrs::SteelErr> {

                use crate::rvals::{IntoSteelVal, FromSteelVal, PrimitiveAsRef};

                if args.len() < #arity_number {
                    crate::stop!(ArityMismatch => format!("{} expected {} arguments, got {}", #value, #arity_number.to_string(), args.len()))
                }

                fn err_thunk(mut err: crate::rerrs::SteelErr) -> crate::rerrs::SteelErr {
                    err.prepend_message(#function_name_with_colon);
                    err.set_kind(crate::rerrs::ErrorKind::TypeMismatch);
                    err
                };

                let res = #function_name(
                    #(
                        // TODO: Distinguish reference types here if possible - make a special implementation
                        // for builtin pointer types here to distinguish them
                        <#arg_type>::#conversion_functions(&args[#arg_index])
                            .map_err(err_thunk)
                        ?,
                    )*
                );

                #ret_val
            }
        };

        // Uncomment this to see the generated code
        // eprintln!("{}", output.to_string());

        return output.into();
    }

    // TODO: Promotion to mutable means we can both avoid allocations and also
    // take advantage of linear types to reduce ref count thrash

    if promote_to_mutable {
        let temporary_fields: Vec<_> = type_vec
            .iter()
            .enumerate()
            .map(|(i, _)| {
                Ident::new(
                    &("temporary_".to_string() + &i.to_string()),
                    sign.ident.span(),
                )
            })
            .collect();

        let output = quote! {
                // Not sure why, but it says this is unused even when generating functions
                // marked as pub
                #[allow(dead_code)]
                #modified_input

                #definition_struct

                pub fn #copied_function_name(args: &mut [SteelVal]) -> std::result::Result<SteelVal, crate::rerrs::SteelErr> {

                    use crate::rvals::{IntoSteelVal, FromSteelVal, PrimitiveAsRef, PrimitiveAsRefMut};

                    // if args.len() != #arity_number {
                    //     crate::stop!(ArityMismatch => format!("{} expected {} arguments, got {}", #value, #arity_number.to_string(), args.len()))
                    // }


                    fn err_thunk(mut err: crate::rerrs::SteelErr) -> crate::rerrs::SteelErr {
                        err.prepend_message(#function_name_with_colon);
                        err.set_kind(crate::rerrs::ErrorKind::TypeMismatch);
                        err
                    };

                    if let [ #(#temporary_fields,)* ] = args {

                        let res = #function_name(
                            #(
                                // TODO: Distinguish reference types here if possible - make a special implementation
                                // for builtin pointer types here to distinguish them
                                <#arg_type>::#conversion_functions(#temporary_fields)
                                    .map_err(err_thunk)?,
                            )*
                        );

                        #ret_val
                    } else {
                        crate::stop!(ArityMismatch => format!("{} expected {} arguments, got {}", #value, #arity_number.to_string(), args.len()))

                    }

                        // if let [arg1, arg2, ..] = &mut vec[..] {
            //  println!("{}:{} says '{}'", ip, port, msg);
            // check_two(arg1, arg2);
        // }

                    // let res = #function_name(
                    //     #(
                    //         // TODO: Distinguish reference types here if possible - make a special implementation
                    //         // for builtin pointer types here to distinguish them
                    //         <#arg_type>::#conversion_functions(&mut std::mem::replace(&mut args[#arg_index], SteelVal::Void))
                    //             .map_err(err_thunk)?,
                    //     )*
                    // );

                    // #ret_val
                }
            };

        return output.into();
    }

    let output = quote! {
        // Not sure why, but it says this is unused even when generating functions
        // marked as pub
        #[allow(dead_code)]
        #modified_input

        #definition_struct

        pub fn #copied_function_name(args: &[SteelVal]) -> std::result::Result<SteelVal, crate::rerrs::SteelErr> {

            use crate::rvals::{IntoSteelVal, FromSteelVal, PrimitiveAsRef};

            if args.len() != #arity_number {
                crate::stop!(ArityMismatch => format!("{} expected {} arguments, got {}", #value, #arity_number.to_string(), args.len()))
            }


            fn err_thunk(mut err: crate::rerrs::SteelErr) -> crate::rerrs::SteelErr {
                err.prepend_message(#function_name_with_colon);
                err.set_kind(crate::rerrs::ErrorKind::TypeMismatch);
                err
            };

            let res = #function_name(
                #(
                    // TODO: Distinguish reference types here if possible - make a special implementation
                    // for builtin pointer types here to distinguish them
                    <#arg_type>::#conversion_functions(&args[#arg_index])
                        .map_err(err_thunk)?,
                )*
            );

            #ret_val
        }
    };

    // Uncomment this to see the generated code
    // eprintln!("{}", output.to_string());

    output.into()
}
