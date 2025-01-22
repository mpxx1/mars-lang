use std::collections::HashMap;

use ast::{ArgDecl, Stmt, StructDecl, Type};
use err::CompileError;
use pest::Span;
use stages::check_after_ret::check_after_return;
use stages::check_main::check_main;
use stages::check_types::check_types;
use stages::init_new_block_var::block_var_decl;

pub mod stages;

pub const GLOBAL_SCOPE_ID: usize = 0;
pub static mut GLOBAL_COUNTER: usize = 2_000_000;

pub(crate) fn gen_id() -> usize {
    unsafe {
        GLOBAL_COUNTER += 1;
        GLOBAL_COUNTER
    }
}

#[derive(Debug)]
pub struct Mir<'src> {
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
    pub is_used: bool,
    pub span: Span<'src>,
}

#[derive(Debug, Clone)]
pub struct StructProto<'src> {
    pub parent_id: usize,
    pub node_id: usize,
    pub ident: &'src str,
    pub fields: Vec<ArgDecl<'src>>,
    pub span: Span<'src>,
    pub is_used: bool,
}

#[derive(Debug, Clone)]
pub struct Variable<'src> {
    pub parent_id: usize,
    pub node_id: usize,
    pub ident: &'src str,
    pub ty: Type<'src>,
    pub is_used: bool,
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
            is_used: false,
        }
    }
}

pub fn compile_mir(hir: hir::Hir) -> Result<Mir, CompileError> {
    let mir = check_types(hir)?;
    let mir = check_main(mir)?;
    let mir = check_after_return(mir)?;
    let mir = block_var_decl(mir)?;
    // todo - check usages
    // todo - check references

    Ok(mir)
}

pub trait ToMir<'src> {
    fn compile_mir(self) -> Result<Mir<'src>, CompileError<'src>>;
}

impl<'src> ToMir<'src> for hir::Hir<'src> {
    fn compile_mir(self) -> Result<Mir<'src>, CompileError<'src>> {
        compile_mir(self)
    }
}
