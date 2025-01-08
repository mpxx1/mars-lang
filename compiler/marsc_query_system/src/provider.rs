use std::collections::HashMap;
use std::sync::Mutex;
use marsc_context::context::TypeContext;
use crate::key::Key;
use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, ItemImpl, Path, Type, ImplItem, ImplItemFn,
};

type ProviderFunction<'tcx, K, R> = fn(TypeContext<'tcx>, K) -> R;

struct GlobalProviders<'tcx> {
    providers: Mutex<HashMap<String, Box<dyn Fn(TypeContext<'tcx>, Box<dyn Key>) -> Box<dyn std::any::Any>>>>,
}

impl<'tcx> GlobalProviders<'tcx> {
    fn new() -> Self {
        Self {
            providers: Mutex::new(HashMap::new()),
        }
    }

    fn register<K, R>(&self, key: &str, provider: ProviderFunction<'tcx, K, R>)
    where
        K: Key + 'static,
        R: 'static,
    {
        let mut providers = self.providers.lock().unwrap();
        providers.insert(
            key.to_string(),
            Box::new(move |ctx, k| {
                let k = k.downcast_ref::<K>().expect("Key type mismatch");
                let result = provider(ctx, k.clone());
                Box::new(result) as Box<dyn std::any::Any>
            }),
        );
    }

    fn get_provider<K, R>(&self, key: &str) -> Option<ProviderFunction<'tcx, K, R>>
    where
        K: Key + 'static,
        R: 'static,
    {
        let providers = self.providers.lock().unwrap();
        if let Some(provider) = providers.get(key) {
            Some(provider.as_ref() as _)
        } else {
            None
        }
    }
}

lazy_static::lazy_static! {
    static ref GLOBAL_PROVIDERS: GlobalProviders<'static> = GlobalProviders::new();
}

pub trait Provider<'tcx, K, R>
where
    K: Key,
{
    fn provide(type_context: TypeContext<'tcx>, key: K) -> R;
}

#[proc_macro_attribute]
pub fn register_provider(_attr: TokenStream, item: TokenStream) -> TokenStream {
    // Парсим входной код как реализацию трейта
    let input = parse_macro_input!(item as ItemImpl);

    // Проверяем, что реализация относится к трейту
    let trait_path = match input.trait_ {
        Some((_, ref path, _)) => path,
        None => panic!("register_provider can only be applied to trait implementations"),
    };

    // Тип структуры или enum, реализующего трейт
    let self_ty = &input.self_ty;

    // Находим метод `provide` в реализации
    let provide_method: &ImplItemFn = input
        .items
        .iter()
        .find_map(|item| match item {
            ImplItem::Fn(method) if method.sig.ident == "provide" => Some(method),
            _ => None,
        })
        .expect("Trait implementation must contain a method named `provide`");

    // Извлекаем тип ключа (третий аргумент метода `provide`)
    let key_ty = provide_method
        .sig
        .inputs
        .iter()
        .nth(2)
        .and_then(|arg| match arg {
            syn::FnArg::Typed(pat_type) => Some(&*pat_type.ty),
            _ => None,
        })
        .expect("Failed to extract key type from `provide` method");

    // Извлекаем возвращаемый тип метода `provide`
    let return_ty = match &provide_method.sig.output {
        syn::ReturnType::Type(_, ty) => &**ty,
        syn::ReturnType::Default => panic!("`provide` method must have a return type"),
    };

    // Генерация кода регистрации
    let expanded = quote! {
        #input

        lazy_static::lazy_static! {
            static ref REGISTERED: () = {
                let provider_fn: fn(TypeContext, #key_ty) -> #return_ty =
                    <#self_ty as #trait_path>::provide;
                GLOBAL_PROVIDERS.register::<#key_ty, #return_ty>(
                    stringify!(#self_ty),
                    provider_fn,
                );
            };
        }
    };

    expanded.into()
}

