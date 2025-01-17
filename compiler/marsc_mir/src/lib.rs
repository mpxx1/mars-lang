use std::collections::HashMap;

use pest::Span;
use ast::{FuncDecl, Stmt, StructDecl, Type, ArgDecl};

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
    pub structs: HashMap<&'src str, StructDecl<'src>>,
    pub funs: HashMap<&'src str, FuncProto<'src>>,
    pub vars: HashMap<&'src str, Variable<'src>>,
    pub instrs: Vec<Stmt<'src>>,
}

#[derive(Debug, Clone)]
pub struct FuncProto<'src> {
    pub node_id: usize,
    pub ident: &'src str,
    pub args: Vec<ArgDecl<'src>>,
    pub return_type: Type<'src>,
    pub span: Span<'src>,
}

#[derive(Debug, Clone)]
pub struct Variable<'src> {
    pub ident: &'src str,
    pub ty: Type<'src>,
    pub is_used: bool,
}