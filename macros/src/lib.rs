use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, punctuated::Punctuated, token::Comma, Data, DataStruct, DeriveInput, Field, Fields, Ident};

#[proc_macro_derive(Deserialise)]
pub fn derive_deserialise(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let name = ast.ident;

    match ast.data {
        Data::Struct(data) => generate_deserialise_impl(name, data),
        _ => {
            quote! {
                compile_error!("Deserialise can only be derived for structs with named fields.")
            }.into()
        },
    }
}

fn generate_deserialise_impl(name: Ident, data: DataStruct) -> TokenStream {
    let fields = if let Fields::Named(fields) = data.fields {
        fields.named
    } else {
        return quote! {
            compile_error!("Deserialise can only be derived for structs with named fields.");
        }.into();
    };

    let impl_output = generate_tryfrom_impl(name.clone(), fields.clone());
    let des_output = generate_deserialise_method_impl(name.clone(), fields.clone());

    let output = quote! {
        #impl_output
        #des_output
    };

    proc_macro::TokenStream::from(output)
}

fn generate_tryfrom_impl(name: Ident, fields: Punctuated<Field, Comma>) -> proc_macro2::TokenStream {
    let fields_vals = fields.into_iter().map(|field| {
        let field_name = field.ident.unwrap();
        quote! {
            #field_name:
                value.as_mut_map()
                    .unwrap()
                    .remove(stringify!(#field_name))
                    .unwrap_or(jsoxiden::Value::Null)
                    .try_from_value()?
        }
    });

    quote! {
        impl jsoxiden::TryFromValue for #name {
            fn try_from_value(mut value: jsoxiden::Value) -> Result<Self, jsoxiden::ValueError> {
                Ok(Self { #(#fields_vals),* })
            }
        }
    }
}

fn generate_deserialise_method_impl(name: Ident, fields: Punctuated<Field, Comma>) -> proc_macro2::TokenStream {
    let fields_vals = fields.into_iter().map(|field| {
        let field_name = field.ident.unwrap();
        quote! {
            #field_name:
                value.as_mut_map()
                    .unwrap()
                    .remove(stringify!(#field_name))
                    .unwrap_or(jsoxiden::Value::Null)
                    .try_from_value()?
        }
    });

    quote! {
        impl Deserialise for #name {
            fn from_str(input: &str) -> Result<Self, jsoxiden::DeserialiseError> {
                let mut value = jsoxiden::from_str(input)?;

                Ok(Self { #(#fields_vals),* })
            }
        }
    }
}

#[proc_macro_derive(Serialise)]
pub fn derive_serialise(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let name = ast.ident;

    match ast.data {
        Data::Struct(data) => generate_serialise_impl(name, data),
        _ => {
            quote! {
                compile_error!("Deserialise can only be derived for structs with named fields.")
            }.into()
        },
    }
}

fn generate_serialise_impl(name: Ident, data: DataStruct) -> TokenStream {
    let fields = if let Fields::Named(fields) = data.fields {
        fields.named
    } else {
        return quote! {
            compile_error!("Deserialise can only be derived for structs with named fields.");
        }.into();
    };

    let impl_output = generate_from_impl(name.clone(), fields);
    let ser_output = generate_serialise_method_impl(name);

    let output = quote! {
        #impl_output
        #ser_output
    };

    proc_macro::TokenStream::from(output)
}

fn generate_from_impl(name: Ident, fields: Punctuated<Field, Comma>) -> proc_macro2::TokenStream {
    let field_vec_vals = fields.iter().map(|field| {
        let field_val = field.ident.clone().unwrap();
        let field_name = format!("{}", field.ident.clone().unwrap());
        quote! {
            (#field_name.to_string(), #field_val)
        }
    });

    let field_vec = quote! {
        let fields = vec![#(#field_vec_vals),*];
    };

    let fields_vals = fields.into_iter().map(|field| {
        let field_name = field.ident.unwrap();
        quote! {
            let #field_name: jsoxiden::Value = input.#field_name.into();
        }
    });
    
    quote! {
        impl From<#name> for jsoxiden::Value {
            fn from(input: #name) -> Self {
                #(#fields_vals)*
                #field_vec

                let mut object = jsoxiden::Value::Object(indexmap::IndexMap::new());
                for (key, value) in fields {
                    _ = object.as_mut_map().unwrap().insert(key, value);
                }

                object
            }
        }
    }
}

fn generate_serialise_method_impl(name: Ident) -> proc_macro2::TokenStream {
    quote! {
        impl Serialise for #name {
            fn to_json_string(self) -> String {
                let value: jsoxiden::Value = self.into();
                value.to_json_string()
            }
        }
    }
}