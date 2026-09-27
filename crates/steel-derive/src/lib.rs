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

#[proc_macro_attribute]
pub fn cross_platform_fn(_attr: TokenStream, item: TokenStream) -> TokenStream {
    // Parse the input function
    let input = parse_macro_input!(item as ItemFn);

    let expanded = quote! {
        #[cfg(target_os = "windows")]
        #[allow(improper_ctypes_definitions)]
        pub extern "sysv64-unwind" #input

        #[cfg(not(target_os = "windows"))]
        #[allow(improper_ctypes_definitions)]
        pub extern "C-unwind" #input
    };

    TokenStream::from(expanded)
}

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

    let identifier_str = identifiers.iter().map(|x| x.to_string());
    let list_identifiers_str = list_identifiers.iter().map(|x| x.to_string());

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

    let identifier_str = identifiers.iter().map(|x| x.to_string());
    let list_identifiers_str = list_identifiers.iter().map(|x| x.to_string());

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
    let should_impl_hash = should_derive_param(&input, "hash");

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

            let generated = quote! {
                impl #prefix::rvals::Custom for #name {
                    #equality_impl
                }

                impl #name {
                    #[doc = "Registers the struct functions with this module"]
                    pub fn register_type(module: &mut #prefix::steel_vm::builtin::BuiltInModule) ->
                        &mut #prefix::steel_vm::builtin::BuiltInModule {
                        use #prefix::steel_vm::register_fn::RegisterFn;
                        #(
                            module.register_fn(#names, #values);
                        )*

                        module
                    }
                }
            };

            generated.into()
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

            let hash_impl = if should_impl_hash {
                quote! {
                    fn try_as_dyn_hash(&self) -> Option<&dyn #prefix::rvals::DynHash> {
                        Some(self)
                    }
                }
            } else {
                quote! {}
            };

            let generated = quote! {
                impl #prefix::rvals::Custom for #name {
                    #equality_impl

                    #hash_impl
                }

                impl #name {
                    #[doc = "Registers the enum variant functions with this module"]
                    pub fn register_enum_variants(module: &mut #prefix::steel_vm::builtin::BuiltInModule) ->
                        &mut #prefix::steel_vm::builtin::BuiltInModule {
                        use #prefix::steel_vm::register_fn::RegisterFn;
                        #(
                            module.register_fn(#names, #values);
                        )*

                        module
                    }
                }
            };

            generated.into()
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
            .map(|line| {
                line.strip_prefix(" ")
                    .map(ToOwned::to_owned)
                    .unwrap_or(line)
            })
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
    pub return_type: proc_macro2::TokenStream,
    pub arg_types: proc_macro2::TokenStream,
    pub raises: proc_macro2::TokenStream,
}

/// The `crate::value_type::ValueType` a native function returns. This is the macro's
/// `returns = "..."` if given, otherwise what its Rust return type guarantees.
///
/// The jit optimizes on this, so the inference only claims what the conversion
/// to a `SteelVal` always produces. `usize` becomes a bignum past `isize::MAX`,
/// so it is a `number` and not an `int` (a fixnum).
fn return_type_tokens(
    keyword_map: &HashMap<String, String>,
    output: Option<&ReturnType>,
) -> proc_macro2::TokenStream {
    let name = match keyword_map.get("returns") {
        Some(explicit) => explicit.as_str(),
        None => output.map(rust_return_type_name).unwrap_or("any"),
    };

    value_type_tokens(name, "return")
}

const VALUE_TYPE_NAMES: &str = "any, bool, int, float, number, char, string, symbol, list, \
     pair, vector, mutable-vector, flat-vector, hashmap, hashset, bytevector, void, procedure \
     (args can join them with |, as in list|pair)";

/// The `crate::value_type::ValueType` variant spelled `name` in `returns` / `args`.
fn value_type_variant(name: &str) -> Option<&'static str> {
    Some(match name {
        "any" => "Any",
        "bool" => "Bool",
        "int" => "Int",
        "float" => "Float",
        "number" => "Number",
        "char" => "Char",
        "string" => "String",
        "symbol" => "Symbol",
        "list" => "List",
        "pair" => "Pair",
        "vector" => "Vector",
        "mutable-vector" => "MutableVector",
        "hashmap" => "HashMap",
        "hashset" => "HashSet",
        "bytevector" => "ByteVector",
        "void" => "Void",
        "flat-vector" => "FlatVector",
        "procedure" => "Procedure",
        _ => return None,
    })
}

/// One entry of `args = "..."`: a type, or a union of them (`list|pair`).
fn arg_type_tokens(entry: &str) -> proc_macro2::TokenStream {
    let members = entry
        .split('|')
        .map(|member| value_type_tokens(member.trim(), "argument"));
    quote! { crate::value_type::ArgType(&[#(#members),*]) }
}

fn value_type_tokens(name: &str, what: &str) -> proc_macro2::TokenStream {
    let variant = value_type_variant(name).unwrap_or_else(|| {
        panic!("unknown {what} type {name:?}: expected one of {VALUE_TYPE_NAMES}")
    });
    let variant = Ident::new(variant, proc_macro2::Span::call_site());
    quote! { crate::value_type::ValueType::#variant }
}

/// How many arguments a function always takes, and whether it can take more.
/// `args = "..."` has to line up with this.
#[derive(Clone, Copy)]
struct ArgShape {
    fixed: usize,
    more: bool,
}

/// The shape of a native function's `arity = "..."` (`Exact(n)`, `AtLeast(n)`,
/// `AtMost(n)`, `Range(a, b)`).
fn arity_shape(arity: &str) -> ArgShape {
    let arity: String = arity.chars().filter(|c| !c.is_whitespace()).collect();
    let (kind, numbers) = arity
        .strip_suffix(')')
        .and_then(|a| a.split_once('('))
        .unwrap_or_else(|| panic!("unable to parse arity {arity:?}"));
    let numbers: Vec<usize> = numbers
        .split(',')
        .map(|n| {
            n.parse()
                .unwrap_or_else(|_| panic!("unable to parse arity {arity:?}"))
        })
        .collect();
    match (kind, numbers.as_slice()) {
        ("Exact", [n]) => ArgShape {
            fixed: *n,
            more: false,
        },
        ("AtLeast", [n]) => ArgShape {
            fixed: *n,
            more: true,
        },
        ("AtMost", [_]) => ArgShape {
            fixed: 0,
            more: true,
        },
        ("Range", [a, _]) => ArgShape {
            fixed: *a,
            more: true,
        },
        _ => panic!("unable to parse arity {arity:?}"),
    }
}

/// The macro's `args = "..."` and `raises = "..."` as the
/// `(Option<ArgTypes>, Raises)` expressions of a definition. `args` lists one
/// type per argument, the last one ending in `...` when the function takes
/// more (`"int, any..."`); it is checked against `shape`.
fn signature_tokens(
    keyword_map: &HashMap<String, String>,
    shape: ArgShape,
) -> (proc_macro2::TokenStream, proc_macro2::TokenStream) {
    let name = keyword_map.get("name").cloned().unwrap_or_default();

    let raises = match keyword_map.get("raises") {
        None => quote! { crate::value_type::Raises::Maybe },
        Some(raises) => {
            if !keyword_map.contains_key("args") {
                panic!("`{name}`: `raises` needs `args` too - it is a promise about them");
            }
            let variant = match raises.as_str() {
                "maybe" => "Maybe",
                "bad-args" => "BadArgs",
                "never" => "Never",
                other => panic!(
                    "`{name}`: unknown raises {other:?}: expected one of maybe, bad-args, never"
                ),
            };
            let variant = Ident::new(variant, proc_macro2::Span::call_site());
            quote! { crate::value_type::Raises::#variant }
        }
    };

    let Some(args) = keyword_map.get("args") else {
        return (quote! { None }, raises);
    };

    let mut fixed = Vec::new();
    let mut rest = None;
    let entries: Vec<&str> = if args.trim().is_empty() {
        Vec::new()
    } else {
        args.split(',').map(str::trim).collect()
    };
    for (i, entry) in entries.iter().enumerate() {
        match entry.strip_suffix("...") {
            Some(ty) if i == entries.len() - 1 => rest = Some(arg_type_tokens(ty.trim())),
            Some(_) => panic!("`{name}`: only the last of `args` can end in `...`"),
            None => fixed.push(arg_type_tokens(entry)),
        }
    }

    if fixed.len() != shape.fixed || rest.is_some() != shape.more {
        panic!(
            "`{name}`: `args = {args:?}` does not match the arity: expected {} argument type(s){}",
            shape.fixed,
            if shape.more {
                " and a last one ending in `...` for the rest"
            } else {
                ""
            }
        );
    }

    let rest = match rest {
        Some(rest) => quote! { Some(#rest) },
        None => quote! { None },
    };
    (
        quote! { Some(crate::value_type::ArgTypes { fixed: &[#(#fixed),*], rest: #rest }) },
        raises,
    )
}

/// The debug-build check around a call to a function with `args` (or `raises`).
/// The first half runs before the body, on the slice `args` as passed in, and
/// the second after it, on the result bound to `__ret`.
fn call_check_tokens(
    keyword_map: &HashMap<String, String>,
    shape: ArgShape,
    args: &proc_macro2::TokenStream,
) -> (proc_macro2::TokenStream, proc_macro2::TokenStream) {
    if !keyword_map.contains_key("args") {
        return (quote! {}, quote! {});
    }
    let name = keyword_map.get("name").cloned().unwrap_or_default();
    let (arg_types, raises) = signature_tokens(keyword_map, shape);
    (
        quote! {
            #[cfg(debug_assertions)]
            let __arg_match = {
                let arg_types: Option<crate::value_type::ArgTypes> = #arg_types;
                arg_types.unwrap().check(&#args[..])
            };
        },
        quote! {
            #[cfg(debug_assertions)]
            crate::value_type::check_call(&__ret, &__arg_match, #raises, #name);
        },
    )
}

/// A debug-build check of what a function with an explicit `returns = "..."`
/// returned (the value bound to `__ret`) against the declaration. Inferred
/// types come from the Rust signature, so they need no check.
fn return_check_tokens(
    keyword_map: &HashMap<String, String>,
    steel_name: &str,
) -> proc_macro2::TokenStream {
    if !keyword_map.contains_key("returns") {
        return quote! {};
    }
    let declared = return_type_tokens(keyword_map, None);
    quote! {
        #[cfg(debug_assertions)]
        crate::value_type::check_return(&__ret, #declared, #steel_name);
    }
}

/// Runs the body of a `native` / `context` / `native_mut` function with an
/// explicit `returns = "..."` or `args = "..."` as a closure of the same type,
/// with the debug-build checks around it. `return` and `?` in the body leave
/// the closure, as before.
fn wrap_with_checks(
    mut input: ItemFn,
    args: &Punctuated<Meta, Comma>,
    is_context_function: bool,
) -> ItemFn {
    let keyword_map = parse_key_value_pairs(args);
    if !keyword_map.contains_key("returns") && !keyword_map.contains_key("args") {
        if keyword_map.contains_key("raises") {
            // Reported by signature_tokens.
            signature_tokens(
                &keyword_map,
                ArgShape {
                    fixed: 0,
                    more: false,
                },
            );
        }
        return input;
    }
    let ReturnType::Type(_, returned) = &input.sig.output else {
        return input;
    };
    let steel_name = keyword_map.get("name").cloned().unwrap_or_default();
    let return_check = return_check_tokens(&keyword_map, &steel_name);

    // Context functions take the vm first; the argument slice is the next one.
    let slice = input
        .sig
        .inputs
        .iter()
        .nth(usize::from(is_context_function))
        .and_then(|arg| match arg {
            FnArg::Typed(pat_type) => match &*pat_type.pat {
                Pat::Ident(ident) => Some(ident.ident.clone()),
                _ => None,
            },
            _ => None,
        })
        .unwrap_or_else(|| panic!("`{steel_name}`: could not find the argument slice"));
    let shape = arity_shape(
        keyword_map
            .get("arity")
            .expect("native definition requires an arity"),
    );
    let (before, after) = call_check_tokens(&keyword_map, shape, &quote! { #slice });

    let block = &input.block;
    let wrapped: syn::Block = syn::parse_quote! {
        {
            #before
            #[allow(clippy::redundant_closure_call)]
            let __ret: #returned = (|| -> #returned #block)();
            #return_check
            #after
            __ret
        }
    };
    input.block = Box::new(wrapped);
    input
}

fn rust_return_type_name(output: &ReturnType) -> &'static str {
    match output {
        ReturnType::Default => "void",
        ReturnType::Type(_, ty) => rust_type_name(ty),
    }
}

fn rust_type_name(ty: &Type) -> &'static str {
    match ty {
        Type::Reference(reference) => rust_type_name(&reference.elem),
        Type::Paren(inner) => rust_type_name(&inner.elem),
        Type::Tuple(tuple) if tuple.elems.is_empty() => "void",
        Type::Path(path) => {
            let Some(last) = path.path.segments.last() else {
                return "any";
            };
            match last.ident.to_string().as_str() {
                "bool" => "bool",
                "char" => "char",
                "f64" | "f32" => "float",
                "isize" | "i32" | "i16" | "i8" | "u8" | "u16" => "int",
                "usize" | "u32" | "u64" | "i64" | "u128" | "i128" => "number",
                "String" | "str" | "SteelString" => "string",
                // Their IntoSteelVal impls always build HashMapV / HashSetV.
                "HashMap" => "hashmap",
                "HashSet" => "hashset",
                // `Result<T>` / `Result<T, E>`. An error never returns.
                "Result" => match &last.arguments {
                    syn::PathArguments::AngleBracketed(args) => args
                        .args
                        .iter()
                        .find_map(|arg| match arg {
                            syn::GenericArgument::Type(ty) => Some(rust_type_name(ty)),
                            _ => None,
                        })
                        .unwrap_or("any"),
                    _ => "any",
                },
                _ => "any",
            }
        }
        _ => "any",
    }
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

    // These take and return `SteelVal`s, so only an explicit `returns` says more.
    let return_type = return_type_tokens(&keyword_map, None);
    let (arg_types, raises) = signature_tokens(
        &keyword_map,
        arity_shape(
            keyword_map
                .get("arity")
                .expect("native definition requires an arity"),
        ),
    );

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
        return_type,
        arg_types,
        raises,
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
    let modified_input = wrap_with_checks(modified_input, &args, false);

    let NativeMacroComponents {
        doc_field,
        doc_name,
        steel_function_name,
        rust_function_name,
        arity_number,
        is_const,
        return_type,
        arg_types,
        raises,
    } = native_macro_setup(&input, &args);

    let definition_struct = quote! {
        pub const #doc_name: crate::steel_vm::builtin::NativeFunctionDefinition = crate::steel_vm::builtin::NativeFunctionDefinition {
            name: #steel_function_name,
            aliases: &[],
            func: crate::steel_vm::builtin::BuiltInFunctionType::Reference(#rust_function_name),
            arity: crate::steel_vm::builtin::Arity::#arity_number,
            doc: #doc_field,
            is_const: #is_const,
            return_type: #return_type,
            arg_types: #arg_types,
            raises: #raises,
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
    let modified_input = wrap_with_checks(modified_input, &args, true);

    let NativeMacroComponents {
        doc_field,
        doc_name,
        steel_function_name,
        rust_function_name,
        arity_number,
        is_const,
        return_type,
        arg_types,
        raises,
    } = native_macro_setup(&input, &args);

    let definition_struct = quote! {
        pub const #doc_name: crate::steel_vm::builtin::NativeFunctionDefinition = crate::steel_vm::builtin::NativeFunctionDefinition {
            name: #steel_function_name,
            aliases: &[],
            func: crate::steel_vm::builtin::BuiltInFunctionType::Context(#rust_function_name),
            arity: crate::steel_vm::builtin::Arity::#arity_number,
            doc: #doc_field,
            is_const: #is_const,
            return_type: #return_type,
            arg_types: #arg_types,
            raises: #raises,
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
    let modified_input = wrap_with_checks(modified_input, &args, false);

    let NativeMacroComponents {
        doc_field,
        doc_name,
        steel_function_name,
        rust_function_name,
        arity_number,
        is_const,
        return_type,
        arg_types,
        raises,
    } = native_macro_setup(&input, &args);

    let definition_struct = quote! {
        pub const #doc_name: crate::steel_vm::builtin::NativeFunctionDefinition = crate::steel_vm::builtin::NativeFunctionDefinition {
            name: #steel_function_name,
            aliases: &[],
            func: crate::steel_vm::builtin::BuiltInFunctionType::Mutable(#rust_function_name),
            arity: crate::steel_vm::builtin::Arity::#arity_number,
            doc: #doc_field,
            is_const: #is_const,
            return_type: #return_type,
            arg_types: #arg_types,
            raises: #raises,
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

    let value_type = return_type_tokens(&keyword_map, Some(&sign.output));

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

    let return_check = return_check_tokens(&keyword_map, &value);
    let ret_val_inner = ret_val;

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

    let shape = ArgShape {
        fixed: arity_number,
        more: rest_arg_generic_inner_type,
    };
    let (arg_types, raises) = signature_tokens(&keyword_map, shape);
    let (before_check, after_check) = call_check_tokens(&keyword_map, shape, &quote! { args });
    // The checks that follow the call, on its result `__ret`. The argument
    // conversions and the call run in a closure so that a conversion error
    // reaches them too.
    let ret_tail = quote! {
        #return_check
        #after_check
        __ret
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
            return_type: #value_type,
            arg_types: #arg_types,
            raises: #raises,
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

                #before_check


                fn err_thunk(mut err: crate::rerrs::SteelErr) -> crate::rerrs::SteelErr {
                    err.prepend_message(#function_name_with_colon);
                    err.set_kind(crate::rerrs::ErrorKind::TypeMismatch);
                    err
                };

                #[allow(clippy::redundant_closure_call)]
                let __ret: std::result::Result<SteelVal, crate::rerrs::SteelErr> = (|| {
                let res = #function_name(
                    #(
                        // TODO: Distinguish reference types here if possible - make a special implementation
                        // for builtin pointer types here to distinguish them
                        <#arg_type>::#conversion_functions(&args[#arg_index])
                            .map_err(err_thunk)
                        ?,
                    )*
                );

                #ret_val_inner
                })();
                #ret_tail
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


                    #before_check



                    fn err_thunk(mut err: crate::rerrs::SteelErr) -> crate::rerrs::SteelErr {
                        err.prepend_message(#function_name_with_colon);
                        err.set_kind(crate::rerrs::ErrorKind::TypeMismatch);
                        err
                    };

                    if let [ #(#temporary_fields,)* ] = args {

                        #[allow(clippy::redundant_closure_call)]
                let __ret: std::result::Result<SteelVal, crate::rerrs::SteelErr> = (|| {
                let res = #function_name(
                            #(
                                // TODO: Distinguish reference types here if possible - make a special implementation
                                // for builtin pointer types here to distinguish them
                                <#arg_type>::#conversion_functions(#temporary_fields)
                                    .map_err(err_thunk)?,
                            )*
                        );

                        #ret_val_inner
                })();
                #ret_tail
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


            #before_check



            fn err_thunk(mut err: crate::rerrs::SteelErr) -> crate::rerrs::SteelErr {
                err.prepend_message(#function_name_with_colon);
                err.set_kind(crate::rerrs::ErrorKind::TypeMismatch);
                err
            };

            #[allow(clippy::redundant_closure_call)]
                let __ret: std::result::Result<SteelVal, crate::rerrs::SteelErr> = (|| {
                let res = #function_name(
                #(
                    // TODO: Distinguish reference types here if possible - make a special implementation
                    // for builtin pointer types here to distinguish them
                    <#arg_type>::#conversion_functions(&args[#arg_index])
                        .map_err(err_thunk)?,
                )*
            );

            #ret_val_inner
                })();
                #ret_tail
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

    let value_type = return_type_tokens(&keyword_map, Some(&sign.output));

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

    let return_check = return_check_tokens(&keyword_map, &value);
    let ret_val_inner = ret_val;

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

    let shape = ArgShape {
        fixed: arity_number,
        more: rest_arg_generic_inner_type,
    };
    let (arg_types, raises) = signature_tokens(&keyword_map, shape);
    let (before_check, after_check) = call_check_tokens(&keyword_map, shape, &quote! { args });
    // The checks that follow the call, on its result `__ret`. The argument
    // conversions and the call run in a closure so that a conversion error
    // reaches them too.
    let ret_tail = quote! {
        #return_check
        #after_check
        __ret
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
            return_type: #value_type,
            arg_types: #arg_types,
            raises: #raises,
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

                #before_check


                fn err_thunk(mut err: crate::rerrs::SteelErr) -> crate::rerrs::SteelErr {
                    err.prepend_message(#function_name_with_colon);
                    err.set_kind(crate::rerrs::ErrorKind::TypeMismatch);
                    err
                };

                #[allow(clippy::redundant_closure_call)]
                let __ret: std::result::Result<SteelVal, crate::rerrs::SteelErr> = (|| {
                let res = #function_name(
                    #(
                        // TODO: Distinguish reference types here if possible - make a special implementation
                        // for builtin pointer types here to distinguish them
                        <#arg_type>::#conversion_functions(&args[#arg_index])
                            .map_err(err_thunk)
                        ?,
                    )*
                );

                #ret_val_inner
                })();
                #ret_tail
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


                    #before_check



                    fn err_thunk(mut err: crate::rerrs::SteelErr) -> crate::rerrs::SteelErr {
                        err.prepend_message(#function_name_with_colon);
                        err.set_kind(crate::rerrs::ErrorKind::TypeMismatch);
                        err
                    };

                    if let [ #(#temporary_fields,)* ] = args {

                        #[allow(clippy::redundant_closure_call)]
                let __ret: std::result::Result<SteelVal, crate::rerrs::SteelErr> = (|| {
                let res = #function_name(
                            #(
                                // TODO: Distinguish reference types here if possible - make a special implementation
                                // for builtin pointer types here to distinguish them
                                <#arg_type>::#conversion_functions(#temporary_fields)
                                    .map_err(err_thunk)?,
                            )*
                        );

                        #ret_val_inner
                })();
                #ret_tail
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


            #before_check



            fn err_thunk(mut err: crate::rerrs::SteelErr) -> crate::rerrs::SteelErr {
                err.prepend_message(#function_name_with_colon);
                err.set_kind(crate::rerrs::ErrorKind::TypeMismatch);
                err
            };

            #[allow(clippy::redundant_closure_call)]
                let __ret: std::result::Result<SteelVal, crate::rerrs::SteelErr> = (|| {
                let res = #function_name(
                #(
                    // TODO: Distinguish reference types here if possible - make a special implementation
                    // for builtin pointer types here to distinguish them
                    <#arg_type>::#conversion_functions(&args[#arg_index])
                        .map_err(err_thunk)?,
                )*
            );

            #ret_val_inner
                })();
                #ret_tail
        }
    };

    // Uncomment this to see the generated code
    // eprintln!("{}", output.to_string());

    output.into()
}
