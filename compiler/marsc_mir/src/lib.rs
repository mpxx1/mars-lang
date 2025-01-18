use std::collections::HashMap;

use ast::{ArgDecl, Stmt, StructDecl, Type};
use pest::Span;

pub mod stages;

pub const GLOBAL_SCOPE_ID: usize = 0;

#[derive(Debug)]
pub struct Mir<'src, 'sf> {
    pub code: &'src str,
    pub scopes: HashMap<usize, Scope<'src>>,
    pub sys_funs: HashMap<&'sf str, usize>,
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
    pub node_id: usize,
    pub ident: &'src str,
    pub args: Vec<ArgDecl<'src>>,
    pub return_type: Type<'src>,
    pub is_used: bool,
    pub span: Span<'src>,
}

#[derive(Debug, Clone)]
pub struct StructProto<'src> {
    pub node_id: usize,
    pub ident: &'src str,
    pub fields: Vec<ArgDecl<'src>>,
    pub span: Span<'src>,
    pub is_used: bool,
}

#[derive(Debug, Clone)]
pub struct Variable<'src> {
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
    pub fn from(s: StructDecl<'src>) -> Self {
        Self {
            node_id: s.node_id,
            ident: s.ident,
            fields: s.fields,
            span: s.span,
            is_used: false,
        }
    }
}
