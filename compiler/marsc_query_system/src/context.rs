use marsc_context::context::TypeContext;

use crate::provider::Providers;

pub trait TypeContextProviders {
     fn providers(&self) -> Providers;
}

impl<'tcx> TypeContextProviders for TypeContext<'tcx> {
    fn providers(&self) -> Providers {
        Providers {
            type_context: self,
        }
    }
}