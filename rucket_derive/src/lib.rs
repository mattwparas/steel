extern crate proc_macro;
extern crate proc_macro2;
#[macro_use]
extern crate syn;
extern crate quote;
extern crate rucket;
use proc_macro::TokenStream;
use quote::quote;
use syn::{Data, DataStruct, DeriveInput, Fields};

// #[macro_export]
#[proc_macro_derive(Scheme)]
pub fn derive_scheme(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = &input.ident;

    let fields = match &input.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(fields),
            ..
        }) => &fields.named,
        _ => panic!("expected a struct with named fields"),
    };
    let field_name = fields.iter().map(|field| &field.ident);
    let field_type = fields.iter().map(|field| &field.ty);

    let gen = quote! {

        impl crate::rvals::CustomType for #name {
            fn box_clone(&self) -> Box<dyn CustomType> {
                Box::new((*self).clone())
            }
            fn as_any(&self) -> Box<dyn Any> {
                Box::new((*self).clone())
            }
            fn new_rucket_val(&self) -> RucketVal {
                RucketVal::Custom(Box::new(self.clone()))
            }
        }

        impl From<#name> for RucketVal {
            fn from(val: #name) -> RucketVal {
                val.new_rucket_val()
            }
        }


        impl From<RucketVal> for #name {
            fn from(val: RucketVal) -> #name {
                unwrap!(val, #name).unwrap()
            }
        }

        impl crate::rvals::StructFunctions for #name {
            fn generate_bindings() -> Vec<(&'static str, RucketVal)> {
                use std::convert::TryFrom;
                use rucket::rvals::RucketVal;
                use rucket::rerrs::RucketErr;
                use rucket::unwrap;
                use rucket::stop;
                let mut vec_binding = vec![];

                // generate predicate
                let name = concat!(stringify!(#name), "?");
                let func =
                        RucketVal::FuncV(|args: Vec<RucketVal>| -> Result<RucketVal, RucketErr> {
                        let mut args_iter = args.into_iter();
                        if let Some(first) = args_iter.next() {
                            return Ok(RucketVal::BoolV(unwrap!(first, #name).is_ok()));
                        }
                        stop!(ArityMismatch => "set! expected 2 arguments");
                    });
                vec_binding.push((name, func));
                #(
                    // generate setters
                    let name = concat!("set-", stringify!(#name), "-", stringify!(#field_name), "!");
                    let func =
                            RucketVal::FuncV(|args: Vec<RucketVal>| -> Result<RucketVal, RucketErr> {
                            let mut args_iter = args.into_iter();
                            if let Some(first) = args_iter.next() {
                                if let Some(second) = args_iter.next() {
                                    let my_struct = unwrap!(first, #name)?;
                                    let new_struct = #name {
                                        #field_name : match second {
                                            RucketVal::Custom(_) => {
                                                unwrap!(second, #field_type)?
                                            },
                                            _ => {
                                                <#field_type>::try_from(second)?
                                                }
                                        },
                                        ..my_struct
                                    };
                                    return Ok(new_struct.new_rucket_val());
                                }
                                stop!(ArityMismatch => "set! expected 2 arguments");
                            }
                            stop!(ArityMismatch => "set! expected 2 arguments");
                        });
                    vec_binding.push((name, func));

                    // generate getters
                    let name = concat!(stringify!(#name), "-", stringify!(#field_name));
                    let func =
                            RucketVal::FuncV(|args: Vec<RucketVal>| -> Result<RucketVal, RucketErr> {
                            let mut args_iter = args.into_iter();
                            if let Some(first) = args_iter.next() {
                                let my_struct = unwrap!(first, #name)?;
                                return Ok(my_struct.#field_name.into());
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
