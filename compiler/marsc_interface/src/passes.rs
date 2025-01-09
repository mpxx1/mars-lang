use std::fs;
use crate::interface::{Compiler, Result};
use marsc_context::context::{GlobalContext, TypeContext};
use std::sync::OnceLock;
use marsc_hir::ast;
use marsc_hir::parser::build_ast;
use marsc_session::session::Session;
use marsc_span::ErrorGuaranteed;

pub fn parse(session: &Session) -> Result<ast::AST> {
   let data = fs::read_to_string(session.io.input_file.to_path_buf()).unwrap();
   match build_ast(data.as_str()) {
      Ok(result) => Ok(result),
      Err(_) => Err(ErrorGuaranteed {}),
   }
}

pub fn create_global_context<'tcx>(
   compiler: &'tcx Compiler,
   // mut ast: ast::AST, TODO
   global_context_cell: &'tcx OnceLock<GlobalContext<'tcx>>,
) -> Result<&'tcx GlobalContext<'tcx>> {
   let session = &compiler.session;
   
   let qcx = global_context_cell.get_or_init(move || {
      TypeContext::create_global_context(
         session
      )
   });

   Ok(qcx)
}
