extern crate proc_macro;
extern crate proc_macro2;
#[macro_use]
extern crate syn;
extern crate quote;
extern crate steel;
use proc_macro::TokenStream;
use quote::quote;
use syn::{Data, DataStruct, DeriveInput, Fields};
// use syn::ItemFn;

/// Derives the `CustomType` trait for the given struct, and also implements the
/// `StructFunctions` trait, which generates the predicate, constructor, and the getters
/// and setters for using the struct inside the interpreter.
#[proc_macro_derive(Scheme)]
pub fn derive_scheme(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = &input.ident;

    if let Data::Struct(DataStruct {
        fields: Fields::Unnamed(_),
        ..
    }) = &input.data
    {
        let gen = quote! {

            impl crate::rvals::CustomType for #name {
                fn box_clone(&self) -> Box<dyn CustomType> {
                    Box::new((*self).clone())
                }
                fn as_any(&self) -> Box<dyn Any> {
                    Box::new((*self).clone())
                }
                fn new_steel_val(&self) -> SteelVal {
                    SteelVal::Custom(Box::new(self.clone()))
                }
            }
            impl From<#name> for SteelVal {
                fn from(val: #name) -> SteelVal {
                    val.new_steel_val()
                }
            }

            impl From<&SteelVal> for #name {
                fn from(val: &SteelVal) -> #name {
                    unwrap!(val.clone(), #name).unwrap()
                }
            }
        };

        return gen.into();
    };

    let fields = match &input.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(fields),
            ..
        }) => &fields.named,
        Data::Struct(DataStruct {
            fields: Fields::Unnamed(fields),
            ..
        }) => &fields.unnamed,
        _ => panic!("expected a struct with named or unnamed fields"),
    };

    let field_name = fields.iter().map(|field| &field.ident);
    let field_name2 = field_name.clone();
    let field_type = fields.iter().map(|field| &field.ty);
    let field_type2 = field_type.clone();

    let gen = quote! {

        impl crate::rvals::CustomType for #name {
            fn box_clone(&self) -> Box<dyn CustomType> {
                Box::new((*self).clone())
            }
            fn as_any(&self) -> Box<dyn Any> {
                Box::new((*self).clone())
            }
            fn new_steel_val(&self) -> SteelVal {
                SteelVal::Custom(Box::new(self.clone()))
            }
        }

        impl From<#name> for SteelVal {
            fn from(val: #name) -> SteelVal {
                val.new_steel_val()
            }
        }


        impl From<&SteelVal> for #name {
            fn from(val: &SteelVal) -> #name {
                unwrap!(val.clone(), #name).unwrap()
            }
        }

        impl crate::rvals::StructFunctions for #name {
            fn generate_bindings() -> Vec<(String, SteelVal)> {
                use std::convert::TryFrom;
                use steel::rvals::SteelVal;
                use steel::rerrs::SteelErr;
                use steel::unwrap;
                use steel::stop;
                use std::rc::Rc;
                let mut vec_binding = vec![];

                // generate predicate
                let name = concat!(stringify!(#name), "?").to_string();
                let func =
                        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>, SteelErr> {
                        let mut args_iter = args.into_iter();
                        if let Some(first) = args_iter.next() {
                            return Ok(Rc::new(SteelVal::BoolV(unwrap!((*first).clone(), #name).is_ok())));
                        }
                        stop!(ArityMismatch => "set! expected 2 arguments");
                    });
                vec_binding.push((name, func));

                // generate constructor
                let name = concat!(stringify!(#name)).to_string();
                let func =
                        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>, SteelErr> {
                            let mut args_iter = args.into_iter();
                            let new_struct = #name {
                                #(
                                    #field_name2: {
                                    if let Some(arg) = args_iter.next() {
                                        match arg.as_ref() {
                                            SteelVal::Custom(_) => unwrap!((*arg).clone(), #field_type2)?,
                                            _ => <#field_type2>::try_from(&(*arg).clone())?
                                        }
                                    } else {
                                        stop!(ArityMismatch => "Struct not given correct arguments");
                                    }},

                                )*
                            };
                            Ok(Rc::new(new_struct.new_steel_val()))
                        });
                vec_binding.push((name, func));

                #(
                    // generate setters
                    let name = concat!("set-", stringify!(#name), "-", stringify!(#field_name), "!").to_string();
                    let func =
                            SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>, SteelErr> {
                            let mut args_iter = args.into_iter();
                            if let Some(first) = args_iter.next() {
                                if let Some(second) = args_iter.next() {
                                    let mut my_struct = unwrap!((*first).clone(), #name)?;
                                    my_struct.#field_name = match second.as_ref() {
                                        SteelVal::Custom(_) => {
                                            unwrap!((*second).clone(), #field_type)?
                                        },
                                        _ => {
                                            <#field_type>::try_from(&(*second).clone())?
                                            }
                                    };
                                    return Ok(Rc::new(my_struct.new_steel_val()));
                                }
                                stop!(ArityMismatch => "set! expected 2 arguments");
                            }
                            stop!(ArityMismatch => "set! expected 2 arguments");
                        });
                    vec_binding.push((name, func));

                    // generate getters
                    let name = concat!(stringify!(#name), "-", stringify!(#field_name)).to_string();
                    let func =
                            SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>, SteelErr> {
                            let mut args_iter = args.into_iter();
                            if let Some(first) = args_iter.next() {
                                let my_struct = unwrap!((*first).clone(), #name)?;
                                let return_val: SteelVal = my_struct.#field_name.into();
                                return Ok(Rc::new(return_val));
                            }
                            stop!(ArityMismatch => "set! expected 2 arguments");
                        });
                    vec_binding.push((name, func));
                ) *
                vec_binding
            }
        }
    };

    gen.into()
}

/// Catch all attribute for embedding structs into the `SteelInterpreter`.
/// Derives Scheme, Clone, and Debug on the attached struct.
/// # Example
/// ```ignore
///
/// #[steel]
/// pub struct Foo {
///     bar: f64,
///     qux: String
/// }
///
/// ```
#[proc_macro_attribute]
pub fn steel(
    _metadata: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let input: proc_macro2::TokenStream = input.into();
    let output = quote! {
        #[derive(Clone, Debug, Scheme)]
        #input
    };
    output.into()
}

// See REmacs : https://github.com/remacs/remacs/blob/16b6fb9319a6d48fbc7b27d27c3234990f6718c5/rust_src/remacs-macros/lib.rs#L17-L161
// attribute to transform function into a Steel Embeddable FuncV
// #[proc_macro_attribute]
// pub fn function(
//     _metadata: proc_macro::TokenStream,
//     input: proc_macro::TokenStream,
// ) -> proc_macro::TokenStream {
//     // let input: proc_macro2::TokenStream = input.into();
//     let input = parse_macro_input!(input as ItemFn);
//     // let function_name = parse_macro_input!(input as DeriveInput);

//     let output = quote! {};
//     output.into()
// }
