mod stages;

use err::CompileError;
use stages::arr_expander::arr_expand;
use crate::stages::parser::parse;
use crate::stages::simplifier::simplify;

pub struct Hir<'src> {
    pub ast: ast::Ast<'src>,
    pub code: &'src str,
}

pub fn compile_hir<'src>(
    source_code: &'src str,
) -> Result<Hir<'src>, CompileError<'src>> {
    
    let mut hir = parse(source_code)?;
    
    hir.ast = simplify(hir.ast)?;
    hir.ast = arr_expand(hir.ast);
    
    Ok(hir)
}