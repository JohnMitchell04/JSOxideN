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
    let fields_vals = fields.into_iter().filter_map(|field| {
        let field_name = field.ident.unwrap();
        quote! {
            #field_name:
                value.remove(stringify!(#field_name))?
                    .try_from_value()?
        }.into()
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
    let fields_vals = fields.into_iter().filter_map(|field| {
        let field_name = field.ident.unwrap();
        quote! {
            #field_name:
                value.remove(stringify!(#field_name))?
                    .try_from_value()?
        }.into()
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