use marsc_context::context::TypeContext;

pub struct Providers<'tcx> {
    pub type_context: &'tcx TypeContext<'tcx>,
}