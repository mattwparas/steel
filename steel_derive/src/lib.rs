extern crate proc_macro;
extern crate proc_macro2;
#[macro_use]
extern crate syn;
extern crate quote;
// extern crate steel;
use proc_macro::TokenStream;
use quote::quote;
use quote::ToTokens;
use syn::FnArg;
use syn::ItemFn;
use syn::ReturnType;
use syn::Signature;
use syn::Type;
use syn::{Data, DataStruct, DeriveInput, Fields, Pat};

/*

Derive CustomType for enums:

#[steel]
pub enum SteelVal {
    BoolV(bool),
    NumV(f64),
    CharV(char),
}

SteelVal::BoolV(bool)


/*
Example of using the derive macro with attributes for the fields
#[steel]
pub enum Foo {
    #[]
    Bar,
    #[name]
    Baz(String),
    #[number]
    Bat(usize),
}
*/

// This should translate to definitions exactly
// Enum constants basically...
// Discriminant::Bar ;; 1
// Discriminant::Baz ;; 2
// Discriminant::Bat ;; 3
// (Discriminant->list) ;; '(Discriminant::Bar, Discriminant::Baz, Discriminant::Bat)
// #[derive(EnumTest)]
// pub enum Discriminant {
//     Bar = 1,
//     Baz = 2,
//     Bat = 3,
// }

*/

/*

#[proc_macro_derive(EnumTest)]
pub fn derive_enum_test(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let _name = &input.ident;

    // try to identify enum here
    match &input.data {
        Data::Enum(data_enum) => {
            let variants = &data_enum.variants;

            /*
            attrs: Vec<Attribute>
            [−]
            Attributes tagged on the variant.

            ident: Ident
            [−]
            Name of the variant.

            fields: Fields
            [−]
            Content stored in the variant.

            discriminant: Option<(Eq, Expr)>
            [−]
            Explicit discriminant: Variant = 1
            */

            for variant in variants {
                let _attributes = &variant.attrs;
                let _ident = &variant.ident;
                let _fields = &variant.fields;
                let discriminant = &variant.discriminant;

                if let Some(v) = discriminant {
                    eprintln!("{}", v.1.clone().into_token_stream().to_string())
                }
            }
        }
        _ => panic!("not supported"),
    }

    let output = quote! {
        // #input
    };

    output.into()
}

#[proc_macro_derive(Schenum)]
pub fn derive_schenum(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = &input.ident;

    // try to identify enum here
    match &input.data {
        Data::Enum(data_enum) => {
            let variants = &data_enum.variants;

            /*
            attrs: Vec<Attribute>
            [−]
            Attributes tagged on the variant.

            ident: Ident
            [−]
            Name of the variant.

            fields: Fields
            [−]
            Content stored in the variant.

            discriminant: Option<(Eq, Expr)>
            [−]
            Explicit discriminant: Variant = 1
            */

            for variant in variants {
                let _attributes = &variant.attrs;
                let _ident = &variant.ident;
                let _fields = &variant.fields;
                let _discriminant = &variant.discriminant;
            }
        }
        _ => panic!("not supported"),
    }

    // if let Data::Enum(DataEnum {
    //     variants:
    // })

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

    let number_of_fields = fields.iter().collect::<Vec<&syn::Field>>().len();

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
            fn display(&self) -> std::result::Result<String, std::fmt::Error> {
                let mut buf = String::new();
                write!(buf, "{:?}", &self)?;
                Ok(buf)
            }
        }

        impl From<#name> for SteelVal {
            fn from(val: #name) -> SteelVal {
                val.new_steel_val()
            }
        }

        impl TryFrom<SteelVal> for #name {
            type Error = SteelErr;
            fn try_from(value: SteelVal) -> std::result::Result<#name, Self::Error> {
                unwrap!(value.clone(), #name)
            }
        }

        impl TryFrom<&SteelVal> for #name {
            type Error = SteelErr;
            fn try_from(value: &SteelVal) -> std::result::Result<#name, Self::Error> {
                unwrap!(value.clone(), #name)
            }
        }

        impl crate::rvals::StructFunctions for #name {
            fn generate_bindings() -> Vec<(String, SteelVal)> {
                use std::convert::TryFrom;
                use std::convert::TryInto;
                use steel::rvals::SteelVal;
                use steel::rerrs::SteelErr;
                use steel::unwrap;
                use steel::stop;
                use std::Gc::Gc;
                let mut vec_binding = vec![];

                // generate predicate
                let name = concat!(stringify!(#name), "?").to_string();
                let func =
                        SteelVal::FuncV(|args: Vec<Gc<SteelVal>>| -> Result<Gc<SteelVal>, SteelErr> {
                            if args.len() == 1 {
                                let mut args_iter = args.into_iter();
                                if let Some(first) = args_iter.next() {
                                    return Ok(Gc::new(SteelVal::BoolV(unwrap!((*first).clone(), #name).is_ok())));
                                }
                                stop!(ArityMismatch => concat!(stringify!(#name), "? expected one argument"));
                            }
                            stop!(ArityMismatch => concat!(stringify!(#name), "? expected one argument"));
                    });
                vec_binding.push((name, func));

                // generate constructor
                let name = concat!(stringify!(#name)).to_string();
                let func =
                        SteelVal::FuncV(|args: Vec<Gc<SteelVal>>| -> Result<Gc<SteelVal>, SteelErr> {

                            if args.len() != #number_of_fields {
                                steel::stop!(ArityMismatch => format!("{} expected {} argument(s), got {}", stringify!(#name), #number_of_fields.to_string(), args.len()))
                            }

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
                                        stop!(ArityMismatch => concat!(stringify!(#name), "expected", stringify!(#number_of_fields),  "arguments"));
                                    }},

                                )*
                            };
                            Ok(Gc::new(new_struct.new_steel_val()))
                        });
                vec_binding.push((name, func));

                #(
                    // generate setters
                    let name = concat!("set-", stringify!(#name), "-", stringify!(#field_name), "!").to_string();
                    let func =
                            SteelVal::FuncV(|args: Vec<Gc<SteelVal>>| -> Result<Gc<SteelVal>, SteelErr> {
                            let arity = args.len();

                            if arity != 2 {
                                stop!(ArityMismatch => format!("{} expected {} argument(s), got {}", concat!("set-", stringify!(#name), "-", stringify!(#field_name), "!"), 2, arity));
                            }

                            let mut args_iter = args.into_iter();
                            if let (Some(first), Some(second)) = (args_iter.next(), args_iter.next()) {
                                let mut my_struct = unwrap!((*first).clone(), #name)?;
                                my_struct.#field_name = match second.as_ref() {
                                    SteelVal::Custom(_) => {
                                        unwrap!((*second).clone(), #field_type)?
                                    },
                                    _ => {
                                        <#field_type>::try_from(&(*second).clone())?
                                        }
                                };
                                return Ok(Gc::new(my_struct.new_steel_val()));
                            } else {
                                stop!(ArityMismatch => format!("{} expected {} argument(s), got {}", concat!("set-", stringify!(#name), "-", stringify!(#field_name), "!"), 2, arity));
                            }
                        });
                    vec_binding.push((name, func));

                    // generate getters
                    let name = concat!(stringify!(#name), "-", stringify!(#field_name)).to_string();
                    let func =
                            SteelVal::FuncV(|args: Vec<Gc<SteelVal>>| -> Result<Gc<SteelVal>, SteelErr> {
                                let arity = args.len();
                                if arity != 1 {
                                    stop!(ArityMismatch => format!("{} expected {} argument(s), got {}", concat!(stringify!(#name), "-", stringify!(#field_name)), 1, arity));
                                }
                                let mut args_iter = args.into_iter();
                                if let Some(first) = args_iter.next() {
                                    let my_struct = unwrap!((*first).clone(), #name)?;
                                    let return_val: SteelVal = my_struct.#field_name.try_into()?; // TODO
                                    return Ok(Gc::new(return_val));
                                }
                                stop!(ArityMismatch => format!("{} expected {} argument(s), got {}", concat!(stringify!(#name), "-", stringify!(#field_name)), 2, arity));
                        });
                    vec_binding.push((name, func));
                ) *
                vec_binding
            }
        }
    };

    gen.into()
}

*/

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
                fn display(&self) -> std::result::Result<String, std::fmt::Error> {
                    let mut buf = String::new();
                    write!(buf, "{:?}", &self)?;
                    Ok(buf)
                }
            }
            impl From<#name> for SteelVal {
                fn from(val: #name) -> SteelVal {
                    val.new_steel_val()
                }
            }

            impl TryFrom<SteelVal> for #name {
                type Error = SteelErr;
                fn try_from(value: SteelVal) -> std::result::Result<#name, Self::Error> {
                    unwrap!(value.clone(), #name)
                }
            }
            impl TryFrom<&SteelVal> for #name {
                type Error = SteelErr;
                fn try_from(value: &SteelVal) -> std::result::Result<#name, Self::Error> {
                    unwrap!(value.clone(), #name)
                }
            }

            impl crate::rvals::StructFunctions for #name {
                fn generate_bindings() -> Vec<(String, SteelVal)> {
                    Vec::new()
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

    let number_of_fields = fields.iter().collect::<Vec<&syn::Field>>().len();

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
            fn display(&self) -> std::result::Result<String, std::fmt::Error> {
                let mut buf = String::new();
                write!(buf, "{:?}", &self)?;
                Ok(buf)
            }
        }

        impl From<#name> for SteelVal {
            fn from(val: #name) -> SteelVal {
                val.new_steel_val()
            }
        }

        impl TryFrom<SteelVal> for #name {
            type Error = SteelErr;
            fn try_from(value: SteelVal) -> std::result::Result<#name, Self::Error> {
                unwrap!(value.clone(), #name)
            }
        }

        impl TryFrom<&SteelVal> for #name {
            type Error = SteelErr;
            fn try_from(value: &SteelVal) -> std::result::Result<#name, Self::Error> {
                unwrap!(value.clone(), #name)
            }
        }

        impl crate::rvals::StructFunctions for #name {
            fn generate_bindings() -> Vec<(String, SteelVal)> {
                use std::convert::TryFrom;
                use std::convert::TryInto;
                use steel::rvals::SteelVal;
                use steel::rerrs::SteelErr;
                use steel::unwrap;
                use steel::stop;
                use steel::gc::Gc;
                let mut vec_binding = vec![];

                // generate predicate
                let name = concat!(stringify!(#name), "?").to_string();
                let func =
                        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>, SteelErr> {
                            if args.len() == 1 {
                                let mut args_iter = args.into_iter().map(Gc::clone);
                                if let Some(first) = args_iter.next() {
                                    return Ok(Gc::new(SteelVal::BoolV(unwrap!((*first).clone(), #name).is_ok())));
                                }
                                stop!(ArityMismatch => concat!(stringify!(#name), "? expected one argument"));
                            }
                            stop!(ArityMismatch => concat!(stringify!(#name), "? expected one argument"));
                    });
                vec_binding.push((name, func));

                // generate constructor
                let name = concat!(stringify!(#name)).to_string();
                let func =
                        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>, SteelErr> {

                            if args.len() != #number_of_fields {
                                steel::stop!(ArityMismatch => format!("{} expected {} argument(s), got {}", stringify!(#name), #number_of_fields.to_string(), args.len()))
                            }

                            let mut args_iter = args.into_iter().map(Gc::clone);

                            let new_struct = #name {
                                #(
                                    #field_name2: {
                                    if let Some(arg) = args_iter.next() {
                                        match arg.as_ref() {
                                            SteelVal::Custom(_) => unwrap!((*arg).clone(), #field_type2)?,
                                            _ => <#field_type2>::try_from(&(*arg).clone())?
                                        }
                                    } else {
                                        stop!(ArityMismatch => concat!(stringify!(#name), "expected", stringify!(#number_of_fields),  "arguments"));
                                    }},

                                )*
                            };
                            Ok(Gc::new(new_struct.new_steel_val()))
                        });
                vec_binding.push((name, func));

                #(
                    // generate setters
                    let name = concat!("set-", stringify!(#name), "-", stringify!(#field_name), "!").to_string();
                    let func =
                            SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>, SteelErr> {
                            let arity = args.len();

                            if arity != 2 {
                                stop!(ArityMismatch => format!("{} expected {} argument(s), got {}", concat!("set-", stringify!(#name), "-", stringify!(#field_name), "!"), 2, arity));
                            }

                            let mut args_iter = args.into_iter().map(Gc::clone);
                            if let (Some(first), Some(second)) = (args_iter.next(), args_iter.next()) {
                                let mut my_struct = unwrap!((*first).clone(), #name)?;
                                my_struct.#field_name = match second.as_ref() {
                                    SteelVal::Custom(_) => {
                                        unwrap!((*second).clone(), #field_type)?
                                    },
                                    _ => {
                                        <#field_type>::try_from(&(*second).clone())?
                                        }
                                };
                                return Ok(Gc::new(my_struct.new_steel_val()));
                            } else {
                                stop!(ArityMismatch => format!("{} expected {} argument(s), got {}", concat!("set-", stringify!(#name), "-", stringify!(#field_name), "!"), 2, arity));
                            }
                        });
                    vec_binding.push((name, func));

                    // generate getters
                    let name = concat!(stringify!(#name), "-", stringify!(#field_name)).to_string();
                    let func =
                            SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>, SteelErr> {
                                let arity = args.len();
                                if arity != 1 {
                                    stop!(ArityMismatch => format!("{} expected {} argument(s), got {}", concat!(stringify!(#name), "-", stringify!(#field_name)), 1, arity));
                                }
                                let mut args_iter = args.into_iter().map(Gc::clone);
                                if let Some(first) = args_iter.next() {
                                    let my_struct = unwrap!((*first).clone(), #name)?;
                                    let return_val: SteelVal = my_struct.#field_name.try_into()?; // TODO
                                    return Ok(Gc::new(return_val));
                                }
                                stop!(ArityMismatch => format!("{} expected {} argument(s), got {}", concat!(stringify!(#name), "-", stringify!(#field_name)), 2, arity));
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

/// Attribute that wraps a given function to transform it into a SteelVal embeddable function
/// The `#[function]` attribute macro operates on functions. It _transforms_ the function from a normal rust function into a function that matches the form used inside the `Steel` interpreter. Functions inside the `Steel` interpreter have the following signature:
/// ```ignore
/// fn(Vec<Gc<SteelVal>>) -> Result<Gc<SteelVal>>
/// ```
/// This macro attempts to remove a great deal of the boilerplate with respect to transferring values in and out of the semantics of the interpreter. However, this means that a function tagged with the `#[function]` attribute **_cannot_** be used as a standard Rust function with the original signature. For a rough idea of what this function does, let's look at a function and its resultant expansion:
/// Example function:
/// ```ignore
/// #[function]
/// pub fn multiple_types(val: u64) -> u64 {
///     val + 25
/// }
/// ```
/// Expands to:
/// ```ignore
/// pub fn multiple_types(args: Vec<Gc<SteelVal>>) -> Result<Gc<SteelVal>, SteelErr>
/// {
///     pub fn multiple_types(val: u64) -> u64 { val + 25 }
///     if args.len () != 1usize {
///         steel::stop!(ArityMismatch => format!("{} expected {} arguments, got {}", stringify!(multiple_types), 1usize.to_string (), args.len()))
///     }
///     let res = multiple_types(u64::try_from((*(args [0usize])).clone())?);
///     Ok(Gc::new(SteelVal::try_from(res)?))
/// }
/// ```
/// The macro operates by defining a wrapper function arounds the original definition. The original definition shadows the wrapper, which allows us to call the original function with some boilerplate for going in and out of `SteelVals`.
///
#[proc_macro_attribute]
pub fn function(
    _metadata: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as ItemFn);

    let mut modified_input = input.clone();
    modified_input.attrs = Vec::new();

    // This snags the `Signature` from the function definition
    let sign: Signature = input.clone().sig;

    // This is the `ReturnType`
    let return_type: ReturnType = sign.output;

    // TODO handle `Result<T>` and `Option<T>` here
    // match on the Result / Option and map into the result (i.e. return Err(...) or Ok(...))
    // add clause for ReturnType::Type(Result<T>, _) or something like that
    // let ret_val = match return_type {
    //     ReturnType::Default => quote! {
    //         Ok(Gc::new(SteelVal::Void))
    //     },
    //     ReturnType::Type(_, _) => quote! {
    //         Ok(Gc::new(SteelVal::try_from(res)?))
    //     },
    // };
    let ret_val = match return_type {
        ReturnType::Default => quote! {
            Ok(Gc::new(SteelVal::Void))
        },
        ReturnType::Type(_, r) => {
            if let Type::Path(val) = *r {
                let last = val.path.segments.into_iter().last();
                if let Some(last) = last {
                    match last.ident.into_token_stream().to_string().as_str() {
                        "Result" => quote! {
                            match res {
                                Ok(x) => {
                                    Ok(Gc::new(SteelVal::try_from(x)?))
                                }
                                Err(e) => {
                                    Err(SteelErr::Generic(e.to_string(), None))
                                }
                            }
                        },
                        "Option" => quote! { // TODO document
                            match res {
                                Some(x) => {
                                    Ok(Gc::new(SteelVal::try_from(x)?))
                                }
                                None => {
                                    Ok(Gc::new(SteelVal::BoolV(false)))
                                }
                            }
                        },
                        _ => quote! {
                            Ok(Gc::new(SteelVal::try_from(res)?))
                        },
                    }
                } else {
                    quote! {
                        Ok(Gc::new(SteelVal::Void))
                    }
                }
            } else {
                quote! {
                    Ok(Gc::new(SteelVal::try_from(res)?))
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
    let function_name = sign.ident;

    let output = quote! {
        pub fn #function_name(args: &[Gc<SteelVal>]) -> std::result::Result<Gc<SteelVal>, SteelErr> {
            #modified_input

            if args.len() != #arity_number {
                steel::stop!(ArityMismatch => format!("{} expected {} arguments, got {}", stringify!(#function_name), #arity_number.to_string(), args.len()))
            }

            let res = #function_name(
                #(
                    <#arg_type>::try_from((*(args[#arg_index])).clone())?,
                )*
            );

            #ret_val
        }
    };

    // eprintln!("{}", output.to_string());

    output.into()
}

#[proc_macro]
pub fn embedded_function(input: TokenStream) -> TokenStream {
    // "5".parse().unwrap()

    let input = parse_macro_input!(input as ItemFn);

    let body = input.clone().block;

    // This snags the `Signature` from the function definition
    let sign: Signature = input.clone().sig;

    let mut type_vec: Vec<Box<Type>> = Vec::new();
    let mut arg_names: Vec<Box<Pat>> = Vec::new();

    for arg in sign.inputs {
        if let FnArg::Typed(pat_ty) = arg.clone() {
            type_vec.push(pat_ty.ty);
            arg_names.push(pat_ty.pat);
        }
    }

    let arity_number = type_vec.len();

    let mut arg_enumerate = type_vec.into_iter().enumerate();
    arg_enumerate.next();

    let arg_type = arg_enumerate.clone().map(|(_, x)| x);
    let arg_index = arg_enumerate.clone().map(|(i, _)| i);
    // let function_name = sign.ident;

    let mut arg_names = arg_names.into_iter();
    let function_name = arg_names.next().unwrap();

    let output = quote! {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> std::result::Result<Gc<SteelVal>, SteelErr> {

            if args.len() != #arity_number {
                stop!(TypeMismatch => format!("{} expected takes {} arguments, found {}", args.len(), arity))
            }

            #(
                let #arg_names = if let Some(SteelVal::#arg_type(inner_value)) = &args[#arg_index].as_ref() {
                    Ok(inner_value)
                } else {
                    stop!(TypeMismatch => format!("{} type mismatch", #function_name))
                }?;
            )*

            #body

        })
    };

    output.into()
}
