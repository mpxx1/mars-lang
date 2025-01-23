pub(crate) mod check_after_ret;
pub(crate) mod check_main;
pub(crate) mod check_types;
pub(crate) mod sys_funs;

use std::collections::HashMap;

use ast::{ArgDecl, Stmt, StructDecl, Type};
use check_after_ret::check_after_return;
use check_main::check_main;
use check_types::check_types;
use err::CompileError;
use pest::Span;

#[derive(Debug)]
pub struct MirS1<'src> {
    pub code: &'src str,
    pub scopes: HashMap<usize, Scope<'src>>,
    pub sys_funs: Vec<&'src str>,
}

#[derive(Debug)]
pub struct Scope<'src> {
    pub parent_id: usize,
    pub node_id: usize,
    pub structs: HashMap<&'src str, StructProto<'src>>,
    pub funs: HashMap<&'src str, FuncProto<'src>>,
    pub vars: HashMap<&'src str, Variable<'src>>,
    pub instrs: Vec<Stmt<'src>>,
    pub scope_type: ScopeType,
}

#[derive(Debug, Clone)]
pub struct FuncProto<'src> {
    pub parent_id: usize,
    pub node_id: usize,
    pub ident: &'src str,
    pub args: Vec<ArgDecl<'src>>,
    pub return_type: Type<'src>,
    pub span: Span<'src>,
}

#[derive(Debug, Clone)]
pub struct StructProto<'src> {
    pub parent_id: usize,
    pub node_id: usize,
    pub ident: &'src str,
    pub fields: Vec<ArgDecl<'src>>,
    pub span: Span<'src>,
}

#[derive(Debug, Clone)]
pub struct Variable<'src> {
    pub parent_id: usize,
    pub node_id: usize,
    pub ident: &'src str,
    pub ty: Type<'src>,
    pub decl_span: Span<'src>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ScopeType {
    Function,
    Block,
    Global,
}

impl<'src> StructProto<'src> {
    pub fn from(scope_id: usize, s: StructDecl<'src>) -> Self {
        Self {
            parent_id: scope_id,
            node_id: s.node_id,
            ident: s.ident,
            fields: s.fields,
            span: s.span,
        }
    }
}

pub fn compile_mir_s1(hir: hir::Hir) -> Result<MirS1, CompileError> {
    let mir = check_types(hir)?;
    let mir = check_main(mir)?;
    let mir = check_after_return(mir)?;

    Ok(mir)
}
