use ast::AST;

pub mod parser;

pub struct Hir<'src> {
    pub ast:  AST<'src>,
    pub code: &'src str
}