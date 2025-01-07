use crate::interface::{Compiler, Result};
use marsc_mir::types::context::GlobalContext;
use std::sync::OnceLock;

pub fn create_global_context<'tcx>(
    compiler: &'tcx Compiler,
    mut project: ast::Project, // TODO
    global_context_cell: &'tcx OnceLock<GlobalContext<'tcx>>,
    /*arena: &'tcx WorkerLocal<Arena<'tcx>>, TODO
    hir_arena: &'tcx WorkerLocal<rustc_hir::Arena<'tcx>>,*/
) -> Result<&'tcx GlobalContext<'tcx>> {
    let session = &compiler.session;
}
