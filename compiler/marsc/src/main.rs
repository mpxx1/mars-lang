use std::{fs, path::PathBuf};
use anyhow::{Result};

use marsc_context::context::{GlobalContext, TypeContext};
use marsc_hir::ast::AST;
use marsc_proc_macro::provider_method;
use marsc_query_system::{context::TypeContextProviders, key};
use marsc_session::session::{CompilerIO, Session};
use marsc_hir::parser::build_ast;

fn main() {
    let session: Session = Session {
        io: CompilerIO {
            input_file: PathBuf::from("C:\\Users\\nikit\\Downloads\\test.ms"),
            output_file: PathBuf::from("./out.exe"),
        }
    };

    let gcx = GlobalContext::new(&session);
    let tcx = TypeContext::new(&gcx);
    let result = tcx.providers().parse_file(tcx, session.io.input_file.clone());
    println!("{:?}", result);
}

#[provider_method]
pub fn parse_file<'tcx>(context: TypeContext<'tcx>, key: PathBuf) -> Result<AST> {
    let data = fs::read_to_string(key).unwrap();
    context.providers().parse_ast(context, data.as_str())
}

#[provider_method]
pub fn parse_ast<'tcx>(_context: TypeContext<'tcx>, key: &str) -> Result<AST> {
    build_ast(key)
}

#[cfg(test)]
mod tests {}
