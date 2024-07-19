use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Data, DeriveInput, Fields};

// TODO: Add the ability for compile time checks to prevent the user from trying to create a struct from invalid JSON
#[proc_macro_derive(Deserialise)]
pub fn derive_deserialise(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let name = ast.ident;
    let data = ast.data;

    if let Data::Struct(data) = data {
        let mut fields_vals = quote!();

        let fields: Fields = data.fields;
        if let Fields::Named(named) = fields {
            for field in named.named {
                let name = field.ident.unwrap();
                
                fields_vals.extend(quote!(
                    #name: value
                        .remove(stringify!(#name))
                        .unwrap()
                        .try_into()
                        .unwrap(),
                ));
            }
        } else {
            unimplemented!()
        }

        let output = quote! {
            impl Deserialise for #name {
                fn from_str(input: &str) -> Self {
                    let mut value = jsoxiden::from_str(input).unwrap();
    
                    Self { #fields_vals }
                }
            }
        };
    
        proc_macro::TokenStream::from(output)
    } else {
        unimplemented!()
    }
}