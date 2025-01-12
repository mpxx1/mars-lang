use marsc_hir::ast::{Assignment, Block, Expr, FuncCall, FuncDecl, Literal, LogicalExpr, MathExpr, NodeId, ProgStmt, Stmt, StructDecl, StructInit, AST};
use std::collections::HashMap;
use marsc_context::context::TypeContext;
use marsc_proc_macro::provider_method;

pub enum ScopeKind {
    Normal
}

#[derive(Copy, Clone)]
pub enum BindingType {
    Func,
    Struct,
    Var,
}

pub struct Bindings<T>
{
    functions: T,
    structs: T,
    variables: T,
}

impl<T> std::ops::Index<BindingType> for Bindings<T> {
    type Output = T;
    
    fn index(&self, binding_type: BindingType) -> &T {
        match binding_type {
            BindingType::Func => &self.functions,
            BindingType::Struct => &self.structs,
            BindingType::Var => &self.variables,
        }
    }
}

impl<T> std::ops::IndexMut<BindingType> for Bindings<T> {
    fn index_mut(&mut self, binding_type: BindingType) -> &mut Self::Output {
        match binding_type {
            BindingType::Func => &mut self.functions,
            BindingType::Struct => &mut self.structs,
            BindingType::Var => &mut self.variables,
        }
    }
}

pub struct Scope<'ast> {
    bindings: Bindings<HashMap<&'ast str, NodeId>>,
    kind: ScopeKind
}

impl<'ast> Scope<'ast> {
    pub fn new(kind: ScopeKind) -> Self {
        Scope {
            kind,
            bindings: Bindings {
                functions: HashMap::new(),
                structs: HashMap::new(),
                variables: HashMap::new(),
            },
        }
    }
}

pub struct Resolver<'ast> {
    scopes: Vec<Scope<'ast>>,
}

impl<'ast> Resolver<'ast> {
    pub fn new() -> Self {
        Resolver {
            scopes: vec![],
        }
    }
}

impl <'ast> Resolver<'ast> {
    fn get_last_scope_binding(&self, name: &'ast String, binding_type: BindingType) -> Option<&NodeId> {
        match self.scopes.last() {
            None => None,
            Some(scope) => {
                scope.bindings[binding_type].get(name.as_str())
            }
        }
    }
    fn get_binding(&self, name: &'ast String, binding_type: BindingType) -> Option<&NodeId> {
        for scope in self.scopes.iter().rev() {
            match scope.kind {
                ScopeKind::Normal => {
                    if scope.bindings[binding_type].contains_key(name.as_str()) {
                        return scope.bindings[binding_type].get(name.as_str());
                    }
                }
            }
        }
        
        None
    }
    
    fn add_binding(&mut self, name: &'ast String, node_id: NodeId, binding_type: BindingType) {
        if let Some(last_scope) = self.scopes.last_mut() {
            last_scope.bindings[binding_type.clone()].insert(name.as_str(), node_id);
        }
    }
}

impl<'ast> Resolver<'ast> {
    fn resolve_literal(&mut self, literal: &'ast Literal) {
        match literal {
            Literal::Int(int_value) => {}
            Literal::Float(float_value) => {}
            Literal::Str(str_value) => {}
            Literal::Char(char_value) => {}
            Literal::Bool(bool_value) => {}
        }
    }

    fn resolve_math_expr(&mut self, math_expr: &'ast MathExpr) {
        match math_expr {
            MathExpr::Additive(_, _, _) => todo!(),
            MathExpr::Multiplicative(_, _, _) => todo!(),
            MathExpr::Power(_, _) => todo!(),
            MathExpr::Primary(expr) => self.resolve_expr(expr),
        }
    }
    
    fn resolve_logical_expr(&mut self, logical_expr: &'ast LogicalExpr) {
        match logical_expr {
            LogicalExpr::Not(_) => todo!(),
            LogicalExpr::Or(_, _) => todo!(),
            LogicalExpr::And(_, _) => todo!(),
            LogicalExpr::Comparison(_, _, _) => todo!(),
            LogicalExpr::Primary(expr) => self.resolve_expr(expr),
        }
    }
    
    fn resolve_identifier(&self, identifier: &'ast String) {
        match self.get_binding(identifier, BindingType::Var) {
            None => {
                panic!("Identifier \"{}\" not found", identifier);
            }
            Some(_) => {}
        }
    }
    
    fn resolve_expr(&mut self, expr: &'ast Expr) {
        match expr {
            Expr::Identifier(identifier) => self.resolve_identifier(identifier),
            Expr::FuncCall(func_call) => self.resolve_func_call(func_call),
            Expr::ArrayDecl(_) => todo!(),
            Expr::MemLookup(_) => todo!(),
            Expr::StructFieldCall(_) => todo!(),
            Expr::StructInit(struct_init) => self.resolve_struct_init(struct_init),
            Expr::IfElse(_) => todo!(),
            Expr::Loop(_) => todo!(),
            Expr::CastType(_) => todo!(),
            Expr::Dereference(_) => todo!(),
            Expr::Reference(_) => todo!(),
            Expr::LogicalExpr(logical_expr) => self.resolve_logical_expr(logical_expr),
            Expr::MathExpr(math_expr) => self.resolve_math_expr(math_expr),
            Expr::Literal(literal) => self.resolve_literal(literal),
        }
    }
    
    fn resolve_assignment(&mut self, assignment: &'ast Assignment) {
        match self.get_last_scope_binding(&assignment.var_name, BindingType::Var) {
            None => {
                self.add_binding(&assignment.var_name, assignment.id, BindingType::Var)
            }
            Some(node_id) => {
                panic!("Variable \"{}\" already exists ({:?})", assignment.var_name, node_id)
            }
        }

        self.resolve_expr(&assignment.expr);
    }
    
    fn resolve_stmt(&mut self, stmt: &'ast Stmt) {
        match stmt {
            Stmt::Block(block) => self.resolve_block(block),
            Stmt::Return(return_expr) => {
                match return_expr {
                    None => {}
                    Some(expr) => self.resolve_expr(expr)
                }
            }
            Stmt::Break => {}
            Stmt::StructDecl(struct_decl) => self.resolve_struct_decl(struct_decl),
            Stmt::FuncDecl(func_decl) => self.resolve_func_decl(func_decl),
            Stmt::Assignment(assignment) => self.resolve_assignment(assignment),
            Stmt::Assign(_) => {}
            Stmt::FuncCall(func_call) => self.resolve_func_call(func_call),
            Stmt::IfElse(_) => {}
            Stmt::Loop(_) => {}
        }
    }
    
    fn resolve_block<'tcx>(&mut self, block: &'ast Block) {
        self.scopes.push(Scope::new(ScopeKind::Normal));
        
        for stmt in &block.stmts {
           self.resolve_stmt(stmt);
        }
        
        self.scopes.pop();
    }

    fn resolve_struct_decl(&mut self, struct_decl: &'ast StructDecl) {
        match self.get_last_scope_binding(&struct_decl.name, BindingType::Struct) {
            None => {
                self.add_binding(&struct_decl.name, struct_decl.id, BindingType::Struct)
            }
            Some(node_id) => {
                panic!("Struct \"{}\" already exists ({:?})", struct_decl.name, node_id)
            }
        }
    }

    fn resolve_struct_init(&mut self, struct_init: &'ast StructInit) {
        match self.get_binding(&struct_init.name, BindingType::Struct) {
            None => {
                panic!("Struct with name \"{}\" not found", struct_init.name);
            }
            Some(_) => {}
        }
    }
    
    fn resolve_func_decl(&mut self, func_decl: &'ast FuncDecl) {
        println!("enter func {}", func_decl.name);
        match self.get_last_scope_binding(&func_decl.name, BindingType::Func) {
            None => {
                self.add_binding(&func_decl.name, func_decl.id, BindingType::Func)
            }
            Some(node_id) => {
                panic!("Function \"{}\" already exists ({:?})", func_decl.name, node_id)
            }
        }
        
        self.resolve_block(&func_decl.body);
        println!("exit func {}", func_decl.name);
    }

    fn resolve_func_call(&mut self, func_call: &'ast FuncCall) {
        match self.get_binding(&func_call.name, BindingType::Func) {
            None => {
                panic!("Function with name \"{}\" not found", func_call.name);
            }
            Some(_) => {}
        }

        for arg in &func_call.args {
            self.resolve_expr(arg);
        }
    }
    
    fn resolve_prog_stmt(&mut self, prog_stmt: &'ast ProgStmt) {
        match prog_stmt {
            ProgStmt::StructDecl(struct_decl) => self.resolve_struct_decl(struct_decl),
            ProgStmt::FuncDecl(func_decl) => self.resolve_func_decl(func_decl),
        }
    }
    
    pub fn resolve_ast(&mut self, ast: &'ast AST) {
        self.scopes.push(Scope::new(ScopeKind::Normal));
        for prog_stmt in &ast.program {
            self.resolve_prog_stmt(prog_stmt);
        }
        self.scopes.pop();
    }
}

#[provider_method]
pub fn resolve_names<'tcx>(_type_context: &'tcx TypeContext<'tcx>, key: &AST) -> bool {
    let mut resolver = Resolver::new();
    resolver.resolve_ast(key);

    true
}