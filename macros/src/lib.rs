use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Data, DataStruct, DeriveInput, Fields, Ident};

// TODO: Add the ability for compile time checks to prevent the user from trying to create a struct from invalid JSON
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

    let fields_vals = fields.into_iter().filter_map(|field| {
        let field_name = field.ident.unwrap();
        quote! {
            #field_name:
                value.remove(stringify!(#field_name))?
                    .try_into()?
        }.into()
    });

    let temp = fields_vals.clone();
    let impl_output = quote! {
        impl TryFrom<jsoxiden::Value> for #name {
            type Error = jsoxiden::DeserialiseError;

            fn try_from(mut value: jsoxiden::Value) -> Result<Self, Self::Error> {
                Ok(Self { #(#fields_vals),* })
            }
        }
    };

    let fields_vals = temp;
    let des_output = quote! {
        impl Deserialise for #name {
            fn from_str(input: &str) -> Result<Self, jsoxiden::DeserialiseError> {
                let mut value = jsoxiden::from_str(input)?;

                Ok(Self { #(#fields_vals),* })
            }
        }
    };

    let output = quote! {
        #impl_output
        #des_output
    };

    proc_macro::TokenStream::from(output)
}