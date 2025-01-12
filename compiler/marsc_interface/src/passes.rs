use crate::interface::{Compiler, Result};
use marsc_context::context::{CurrentGlobalContext, GlobalContext, TypeContext};
use marsc_hir::ast;
use marsc_hir::parser::build_ast;
use marsc_session::session::Session;
use marsc_span::ErrorGuaranteed;
use std::fs;
use std::sync::OnceLock;

pub fn parse(session: &Session) -> Result<ast::AST> {
   let data = fs::read_to_string(session.io.input_file.to_path_buf()).unwrap();
   match build_ast(data.as_str()) {
      Ok(result) => Ok(result),
      Err(_) => Err(ErrorGuaranteed {}),
   }
}

pub fn create_and_enter_global_context<T, F: for<'tcx> FnOnce(TypeContext<'tcx>) -> T>(
   compiler: &Compiler,
   f: F,
) -> T {
   let global_context_cell = OnceLock::new();
   
   let inner: Box<dyn for<'tcx> FnOnce(
      &'tcx Session,
      CurrentGlobalContext,
      &'tcx OnceLock<GlobalContext<'tcx>>,
      F,
   ) -> T> = Box::new(move |session, current_global_context, global_context_cell, f| {
      TypeContext::create_global_context(
         global_context_cell,
         session,
         current_global_context,
         |type_context| {
            let res = f(type_context);
            // type_context.finish - save depth graph
            res
         }
      )
   });
   
   inner(&compiler.session, compiler.current_global_context.clone(), &global_context_cell, f)
}
