use marsc_hir::ast::{Assignment, Block, FuncDecl, ProgStmt, Stmt, AST};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub enum ScopeKind {
    #[default]
    Normal
}

pub struct Scope<'ast> {
    bindings: HashMap<&'ast str, u32>,
    kind: ScopeKind
}

impl<'ast> Scope<'ast> {
    pub fn new(kind: ScopeKind) -> Self {
        Scope {
            kind,
            bindings: Default::default(),
        }
    }
}

pub struct Resolver<'ast> {
    scopes: Vec<Scope<'ast>>,
}

impl<'ast> Resolver<'ast> {
    
    pub fn visit_assignment(&mut self, assignment: &'ast Assignment) {
        
    }
    
    pub fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        match stmt {
            Stmt::Block(block) => self.resolve_block(block),
            Stmt::Return(_) => {}
            Stmt::Break => {}
            Stmt::StructDecl(_) => {}
            Stmt::FuncDecl(func_decl) => self.resolve_func(func_decl),
            Stmt::Assignment(assignment) => {}
            Stmt::Assign(_) => {}
            Stmt::FuncCall(_) => {}
            Stmt::IfElse(_) => {}
            Stmt::Loop(_) => {}
        }
    }
    
    pub fn resolve_block<'tcx>(&mut self, block: &'ast Block) {
        self.scopes.push(Scope::new(ScopeKind::Normal));
        
        for stmt in &block.stmts {
           
        }
        
        self.scopes.pop();
    }
    
    pub fn resolve_func(&self, func: &'ast FuncDecl) {
        todo!()
    }
    
    pub fn resolve_prog_stmt(&self, prog_stmt: &'ast ProgStmt) {
        match prog_stmt {
            ProgStmt::StructDecl(struct_decl) => {}
            ProgStmt::FuncDecl(func_decl) => {}
        }
    }
    
    pub fn resolve_ast(&mut self, ast: &AST) {
        for prog_stmt in &ast.program {
            self.resolve_stmt(prog_stmt);
        }
    }
}