use crate::context::TypeContext;

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

pub struct Providers<'tcx> {
    pub type_context: &'tcx TypeContext<'tcx>,
}