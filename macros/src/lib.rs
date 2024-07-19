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
                value.remove(stringify!(#field_name))
                .ok_or_else(|| format!("Missing field: {}", stringify!(#field_name)))?
                .try_into()
                .map_err(|_| format!("Failed to parse field: {}", stringify!(#field_name)))?
        }.into()
    });

    let output = quote! {
        impl Deserialise for #name {
            fn from_str(input: &str) -> Result<Self, jsoxiden::ParseError> {
                let mut value = jsoxiden::from_str(input)?;

                Ok(Self { #(#fields_vals),* })
            }
        }
    };

    proc_macro::TokenStream::from(output)
}