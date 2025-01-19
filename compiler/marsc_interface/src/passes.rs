use crate::interface::Compiler;
use marsc_mir::context::{CurrentGlobalContext, GlobalContext, TypeContext};
use hir;
use marsc_session::session::Session;
use std::fs;
use std::sync::OnceLock;
use hir::Hir;
use hir::parser::compile_hir;

pub fn parse<'a>(session: &Session) -> Hir<'a> {
   let data = fs::read_to_string(session.io.input_file.to_path_buf()).unwrap();
   compile_hir(Box::leak(data.into_boxed_str())).unwrap()
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
         |type_context| {
            let res = f(type_context);
            res
         }
      )
   });
   
   inner(&compiler.session, compiler.current_global_context.clone(), &global_context_cell, f)
}
