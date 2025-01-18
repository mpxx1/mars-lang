pub mod ast;
pub mod parser;
pub mod simplifier;

pub struct Hir<'src> {
    pub ast: ast::Ast<'src>,
    pub code: &'src str,
}
pub mod ast_simplifier;
