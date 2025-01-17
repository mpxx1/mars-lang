extern crate proc_macro;

use convert_case::{Case, Casing};
use quote::quote;
use syn::__private::TokenStream;
use syn::{parse_macro_input, ItemFn};

#[proc_macro_attribute]
pub fn provider_method(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let input_fn = parse_macro_input!(item as ItemFn);
    let fn_name = &input_fn.sig.ident;

    let inputs = &input_fn.sig.inputs;
    let output = &input_fn.sig.output;

    if inputs.len() != 2 {
        return syn::Error::new_spanned(
            &input_fn.sig,
            "The function must take exactly two arguments: `TypeContext` and a key."
        )
            .to_compile_error()
            .into();
    }

    let key_type = match inputs.iter().nth(1) {
        Some(syn::FnArg::Typed(pat_type)) => &*pat_type.ty,
        _ => {
            return syn::Error::new_spanned(
                &input_fn.sig,
                "Expected `TypeContext` and a key argument."
            )
                .to_compile_error()
                .into();
        }
    };

    let return_type = match output {
        syn::ReturnType::Type(_, ty) => ty,
        syn::ReturnType::Default => {
            return syn::Error::new_spanned(
                &input_fn.sig,
                "The function must have a return type."
            )
                .to_compile_error()
                .into();
        }
    };

    let trait_name = syn::Ident::new(
        &format!("{}Provider", fn_name.to_string().to_case(Case::Pascal)),
        fn_name.span(),
    );

    let expanded = quote! {
        #input_fn

        pub trait #trait_name<'ctx> {
            fn #fn_name(&self, key: #key_type<'ctx>) -> #return_type::<'ctx>;
        }

        impl<'ctx> #trait_name<'ctx> for marsc_query_system::provider::Providers<'ctx> {
            fn #fn_name(&self, key: #key_type::<'ctx>) -> #return_type::<'ctx> {
                #fn_name(self.type_context, key)
            }
        }
    };

    TokenStream::from(expanded)
}