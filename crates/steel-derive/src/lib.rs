extern crate proc_macro;
extern crate proc_macro2;
#[macro_use]
extern crate syn;
extern crate quote;
use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    parse::Parser, AttributeArgs, Data, DeriveInput, FnArg, Ident, ItemFn, ReturnType, Signature,
    Type,
};

#[proc_macro_derive(Steel)]
pub fn derive_steel(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;

    match &input.data {
        Data::Struct(_) | Data::Enum(_) => {
            let gen = quote! {
                impl steel::rvals::Custom for #name {}
            };

            return gen.into();
        }
        _ => {
            let output = quote! { #input };
            output.into()
        }
    }
}

trait NestedMeta {
    fn is_meta(&self) -> bool;
    fn get_meta(&self) -> &syn::Meta;
}

/// Can be either a 👉 [syn::NestedMeta::Meta] or a [syn::NestedMeta::Lit].
impl NestedMeta for syn::NestedMeta {
    fn is_meta(&self) -> bool {
        match self {
            syn::NestedMeta::Meta(_) => true,
            syn::NestedMeta::Lit(_) => false,
        }
    }

    fn get_meta(&self) -> &syn::Meta {
        match self {
            syn::NestedMeta::Meta(meta) => meta,
            syn::NestedMeta::Lit(_) => panic!("Lit found"),
        }
    }
}

trait MetaExt {
    fn is_meta_name_value(&self) -> bool;
    fn get_meta_name_value_str(&self) -> String;
    fn get_meta_name_value_ident(&self) -> Ident;
}

/// Can be either a 👉 [syn::Meta::NameValue], [syn::Meta::List], or [syn::Meta::Path].
impl MetaExt for syn::Meta {
    fn is_meta_name_value(&self) -> bool {
        match self {
            syn::Meta::Path(_) => false,
            syn::Meta::List(_) => false,
            syn::Meta::NameValue(_) => true,
        }
    }

    fn get_meta_name_value_str(&self) -> String {
        match self {
            syn::Meta::Path(_) => panic!("Path found"),
            syn::Meta::List(_) => panic!("List found"),
            syn::Meta::NameValue(meta_name_value) => {
                let lit_str = match &meta_name_value.lit {
                    syn::Lit::Str(lit_str) => lit_str.value(),
                    _ => panic!("Expected a string literal"),
                };
                lit_str
            }
        }
    }

    /// ```no_run
    /// Path {
    ///   leading_colon: None,
    ///   segments: [
    ///       PathSegment {
    ///           ident: Ident {
    ///               ident: "key",
    ///               span: #0 bytes(510..513),
    ///           },
    ///           arguments: None,
    ///       },
    ///   ],
    /// }
    /// ```
    fn get_meta_name_value_ident(&self) -> Ident {
        match self {
            syn::Meta::Path(_) => panic!("Path found"),
            syn::Meta::List(_) => panic!("List found"),
            syn::Meta::NameValue(meta_name_value) => {
                if let Some(ident) = meta_name_value.path.get_ident() {
                    ident.clone()
                } else {
                    panic!("Expected an ident")
                }
            }
        }
    }
}

fn parse_key_value_pair(args: &AttributeArgs) -> (String, String) {
    for nested_meta in args.iter() {
        if nested_meta.is_meta() {
            let meta = nested_meta.get_meta();
            if meta.is_meta_name_value() {
                let key = meta.get_meta_name_value_ident().to_string();
                let value = meta.get_meta_name_value_str();
                return (key, value);
            }
        }
        panic!("Expected a key value pair");
    }

    panic!("Expected a key value pair");
}

// See REmacs : https://github.com/remacs/remacs/blob/16b6fb9319a6d48fbc7b27d27c3234990f6718c5/rust_src/remacs-macros/lib.rs#L17-L161
// TODO: Pass the new name in to this function
#[proc_macro_attribute]
pub fn function(
    args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let args = parse_macro_input!(args as AttributeArgs);
    let (_, value) = parse_key_value_pair(&args);
    let function_name_with_colon = value.clone() + ": ";

    let input = parse_macro_input!(input as ItemFn);

    let mut modified_input = input.clone();
    modified_input.attrs = Vec::new();

    // This snags the `Signature` from the function definition
    let sign: Signature = input.clone().sig;

    // This is the `ReturnType`
    let return_type: ReturnType = sign.output;

    // let ret_val = match return_type {
    //     ReturnType::Default => quote! {
    //         Ok(SteelVal::Void)
    //     },
    //     ReturnType::Type(_, a) => match *a {
    //         Type::Path(a) => {
    //             todo!()
    //         }
    //         _ => quote! {
    //             res.into_steelval()
    //         }
    //     }

    //     // ReturnType::Type(_, _) => quote! {
    //     //     res.into_steelval()
    //     // },
    // };

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
                            res.into_steelval()
                        },
                    }
                } else {
                    quote! {
                        Ok(SteelVal::Void)
                    }
                }
            } else {
                quote! {
                    res.into_steelval()
                }
            }
        }
    };

    let mut type_vec: Vec<Box<Type>> = Vec::new();

    for arg in sign.inputs {
        if let FnArg::Typed(pat_ty) = arg.clone() {
            type_vec.push(pat_ty.ty);
        }
    }

    let arity_number = type_vec.len();

    let arg_enumerate = type_vec.into_iter().enumerate();
    let arg_type = arg_enumerate.clone().map(|(_, x)| x);
    let arg_index = arg_enumerate.clone().map(|(i, _)| i);
    let function_names_with_colon = std::iter::repeat(function_name_with_colon);
    let function_name = sign.ident.clone();
    let copied_function_name = Ident::new(
        &("steel_".to_string() + &function_name.to_string()),
        sign.ident.span(),
    );

    let output = quote! {

        #modified_input

        pub fn #copied_function_name(args: &[SteelVal]) -> std::result::Result<SteelVal, crate::rerrs::SteelErr> {

            use crate::rvals::{IntoSteelVal, FromSteelVal};


            if args.len() != #arity_number {
                crate::stop!(ArityMismatch => format!("{} expected {} arguments, got {}", #value, #arity_number.to_string(), args.len()))
            }

            let res = #function_name(
                #(
                    <#arg_type>::from_steelval(&args[#arg_index]).map_err(|mut err| { err.prepend_message(#function_names_with_colon); err } )?,
                )*
            );

            #ret_val
        }
    };

    // Uncomment this to see the generated code
    // eprintln!("{}", output.to_string());

    output.into()
}
