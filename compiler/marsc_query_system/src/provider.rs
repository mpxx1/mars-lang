use marsc_context::context::TypeContext;
use syn::Result;
use syn::{parse::Parse, parse::ParseStream};


pub type ProviderFunction<K, R> = fn(TypeContext, K) -> R;

pub struct StaticMethodAttr;

impl Parse for StaticMethodAttr {
    fn parse(_input: ParseStream) -> Result<Self> {
        Ok(StaticMethodAttr)
    }
}

pub struct GlobalProviders;