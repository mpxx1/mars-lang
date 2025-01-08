use marsc_context::context::TypeContext;

use crate::provider::GlobalProviders;

pub trait TypeContextProviders {
     fn providers(&self) -> GlobalProviders;
}

impl<'tcx> TypeContextProviders for TypeContext<'tcx> {
    fn providers(&self) -> GlobalProviders {
        GlobalProviders {}
    }
}