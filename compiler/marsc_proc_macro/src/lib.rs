extern crate proc_macro;

use convert_case::{Case, Casing};
use quote::quote;
use syn::__private::TokenStream;
use syn::{parse_macro_input, ItemFn};

#[proc_macro_attribute]
pub fn provider_method(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let input_fn = parse_macro_input!(item as ItemFn);
    let fn_name = &input_fn.sig.ident;

    // Извлечение аргументов функции
    let inputs = &input_fn.sig.inputs;
    let output = &input_fn.sig.output;

    // Проверка, что у функции два аргумента
    if inputs.len() != 2 {
        return syn::Error::new_spanned(
            &input_fn.sig,
            "The function must take exactly two arguments: `TypeContext` and a key."
        )
            .to_compile_error()
            .into();
    }

    // Первый аргумент — `TypeContext`, второй — ключ (тип K)
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

    // Получение возвращаемого типа функции
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

    // Имя трейта
    let trait_name = syn::Ident::new(
        &format!("{}Provider", fn_name.to_string().to_case(Case::Pascal)),
        fn_name.span(),
    );

    // Генерация кода
    let expanded = quote! {
        #input_fn

        pub trait #trait_name<'ctx> {
            fn #fn_name(&self, context: TypeContext<'ctx>, key: #key_type) -> #return_type;
        }

        impl<'ctx> #trait_name<'ctx> for marsc_query_system::provider::GlobalProviders {
            fn #fn_name(&self, context: marsc_context::context::TypeContext<'ctx>, key: #key_type) -> #return_type {
                #fn_name(context, key)
            }
        }
    };

    TokenStream::from(expanded)
}