use ast::AST;

pub mod parser;
pub mod simplifier;

pub struct Hir<'src> {
    pub ast: AST<'src>,
    pub code: &'src str,
}
