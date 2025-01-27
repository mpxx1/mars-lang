pub(crate) mod stages;

use crate::stages::parser::parse;
use crate::stages::simplifier::simplify;
use err::CompileError;
use stages::arr_expander::arr_expand;

pub struct Hir<'src> {
    pub last_id: usize,
    pub ast: ast::Ast<'src>,
    pub code: &'src str,
}

pub fn compile_hir(source_code: &str) -> Result<Hir, CompileError> {
    let mut hir = parse(source_code)?;

    hir.ast = simplify(hir.ast)?;
    hir.ast = arr_expand(hir.ast);

    Ok(hir)
}

pub trait ToHir<'src> {
    fn compile_hir(self) -> Result<Hir<'src>, CompileError<'src>>;
}

impl<'src> ToHir<'src> for &'src str {
    fn compile_hir(self) -> Result<Hir<'src>, CompileError<'src>> {
        compile_hir(self)
    }
}
