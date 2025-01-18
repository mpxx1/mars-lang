use marsc_context::context::TypeContext;
use marsc_hir::ast;
use marsc_mir::mir;
use std::collections::HashMap;
use pest::Span;
use marsc_proc_macro::provider_method;

pub enum ScopeKind {
    Normal,
    Function,
}

#[derive(Copy, Clone, Debug)]
pub enum BindingType {
    Func,
    Struct,
    Var,
}

pub struct Bindings<T> {
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

pub struct DefInfo {
    pub node_id: ast::NodeId,
    pub def_id: mir::DefId,
}

pub struct Scope<'ast> {
    bindings: Bindings<HashMap<&'ast str, DefInfo>>,
    kind: ScopeKind,
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
        Resolver { scopes: vec![] }
    }
}

impl<'ast> Resolver<'ast> {
    fn get_last_scope_binding(
        &self,
        ident: &'ast str,
        binding_type: BindingType,
    ) -> Option<&DefInfo> {
        match self.scopes.last() {
            None => None,
            Some(scope) => scope.bindings[binding_type].get(ident),
        }
    }

    fn get_binding(&self, ident: &'ast str, binding_type: BindingType) -> Option<&DefInfo> {
        for scope in self.scopes.iter().rev() {
            match scope.kind {
                ScopeKind::Normal | ScopeKind::Function => {
                    if scope.bindings[binding_type].contains_key(ident) {
                        return scope.bindings[binding_type].get(ident);
                    }
                }
            }
        }

        None
    }

    fn add_binding(
        &mut self,
        ident: &'ast str,
        node_id: ast::NodeId,
        def_id: mir::DefId,
        binding_type: BindingType,
    ) {
        if let Some(last_scope) = self.scopes.last_mut() {
            last_scope.bindings[binding_type.clone()].insert(ident, DefInfo { node_id, def_id });
        }
    }
}

impl<'ast> Resolver<'ast> {
    fn with_scope<R, F: FnOnce() -> R>(&mut self, f: F, scope_kind: ScopeKind) -> R {
        self.scopes.push(Scope::new(scope_kind));
        let result = f();
        self.scopes.pop();
        result
    }

    fn resolve_literal(&mut self, literal: ast::Literal<'ast>) -> mir::Literal<'ast> {
        match literal {
            ast::Literal::Int { node_id, lit, span } => mir::Literal::Int {
                node_id: convert_node_id(node_id),
                lit: lit.clone(),
                span: span.clone(),
            },
            ast::Literal::Float { node_id, lit, span } => mir::Literal::Float {
                node_id: convert_node_id(node_id),
                lit: lit.clone(),
                span: span.clone(),
            },
            ast::Literal::Str { node_id, lit, span } => mir::Literal::Str {
                node_id: convert_node_id(node_id),
                lit: lit.clone(),
                span: span.clone(),
            },
            ast::Literal::Char { node_id, lit, span } => mir::Literal::Char {
                node_id: convert_node_id(node_id),
                lit: lit.clone(),
                span: span.clone(),
            },
            ast::Literal::Bool { node_id, lit, span } => mir::Literal::Bool {
                node_id: convert_node_id(node_id),
                lit: lit.clone(),
                span: span.clone(),
            },
        }
    }

    fn resolve_math_expr(&mut self, math_expr: ast::MathExpr<'ast>) -> mir::MathExpr<'ast> {
        match math_expr {
            ast::MathExpr::Additive {
                node_id,
                left,
                right,
                op,
                span,
            } => mir::MathExpr::Additive {
                node_id: convert_node_id(node_id),
                left: Box::new(self.resolve_math_expr(*left)),
                right: Box::new(self.resolve_math_expr(*right)),
                op: match op {
                    ast::AddOp::Add => mir::AddOp::Add,
                    ast::AddOp::Sub => mir::AddOp::Sub,
                },
                span: span.clone(),
            },
            ast::MathExpr::Multiplicative {
                node_id,
                left,
                right,
                op,
                span,
            } => mir::MathExpr::Multiplicative {
                node_id: convert_node_id(node_id),
                left: Box::new(self.resolve_math_expr(*left)),
                right: Box::new(self.resolve_math_expr(*right)),
                op: match op {
                    ast::MulOp::Mul => mir::MulOp::Mul,
                    ast::MulOp::Div => mir::MulOp::Div,
                    ast::MulOp::Mod => mir::MulOp::Mod,
                    ast::MulOp::DivFloor => mir::MulOp::DivFloor,
                },
                span: span.clone(),
            },
            ast::MathExpr::Power {
                node_id,
                base,
                exp,
                span,
            } => mir::MathExpr::Power {
                node_id: convert_node_id(node_id),
                base: Box::new(self.resolve_math_expr(*base)),
                exp: Box::new(self.resolve_math_expr(*exp)),
                span: span.clone(),
            },
            ast::MathExpr::Primary(expr) => {
                mir::MathExpr::Primary(Box::from(self.resolve_expr(*expr)))
            }
        }
    }

    fn resolve_logical_expr(
        &mut self,
        logical_expr: ast::LogicalExpr<'ast>,
    ) -> mir::LogicalExpr<'ast> {
        match logical_expr {
            ast::LogicalExpr::Not {
                node_id,
                inner,
                span,
            } => mir::LogicalExpr::Not {
                node_id: convert_node_id(node_id),
                inner: Box::new(self.resolve_logical_expr(*inner)),
                span: span.clone(),
            },
            ast::LogicalExpr::Or {
                node_id,
                left,
                right,
                span,
            } => mir::LogicalExpr::Or {
                node_id: convert_node_id(node_id),
                left: Box::new(self.resolve_logical_expr(*left)),
                right: Box::new(self.resolve_logical_expr(*right)),
                span: span.clone(),
            },
            ast::LogicalExpr::And {
                node_id,
                left,
                right,
                span,
            } => mir::LogicalExpr::And {
                node_id: convert_node_id(node_id),
                left: Box::new(self.resolve_logical_expr(*left)),
                right: Box::new(self.resolve_logical_expr(*right)),
                span: span.clone(),
            },
            ast::LogicalExpr::Comparison {
                node_id,
                left,
                right,
                op,
                span,
            } => mir::LogicalExpr::Comparison {
                node_id: convert_node_id(node_id),
                left: Box::new(self.resolve_math_expr(*left)),
                right: Box::new(self.resolve_math_expr(*right)),
                op: match op {
                    ast::CmpOp::Equal => mir::CmpOp::Equal,
                    ast::CmpOp::NotEqual => mir::CmpOp::NotEqual,
                    ast::CmpOp::More => mir::CmpOp::More,
                    ast::CmpOp::MoreEqual => mir::CmpOp::MoreEqual,
                    ast::CmpOp::Less => mir::CmpOp::Less,
                    ast::CmpOp::LessEqual => mir::CmpOp::LessEqual,
                },
                span: span.clone(),
            },
            ast::LogicalExpr::Primary(expr) => {
                mir::LogicalExpr::Primary(Box::new(self.resolve_expr(*expr)))
            }
        }
    }

    fn resolve_type(&self, ty: ast::Type<'ast>) -> mir::Type<'ast> {
        match ty {
            ast::Type::I64 => mir::Type::I64,
            ast::Type::F64 => mir::Type::F64,
            ast::Type::Str => mir::Type::Str,
            ast::Type::Char => mir::Type::Char,
            ast::Type::Bool => mir::Type::Bool,
            ast::Type::Void => mir::Type::Void,
            ast::Type::Custom(identifier) => {
                let error_span = identifier.span;
                mir::Type::Custom(self.resolve_identifier(identifier, BindingType::Struct, error_span))
            }
            ast::Type::Array(ty, size) => {
                mir::Type::Array(Box::new(self.resolve_type(*ty)), size.clone())
            }
            ast::Type::Vec(ty) => mir::Type::Vec(Box::new(self.resolve_type(*ty))),
            ast::Type::Ref(ty) => mir::Type::Ref(Box::new(self.resolve_type(*ty))),
        }
    }

    fn resolve_identifier(
        &self,
        identifier: ast::Identifier<'ast>,
        binding_type: BindingType,
        error_span: Span<'ast>
    ) -> mir::Identifier<'ast> {
        match self.get_binding(identifier.ident, binding_type) {
            None => {
                panic!(
                    "Identifier \"{}\" ({:?}) not found:\n{}",
                    identifier.ident, binding_type, error_span.as_str()
                );
            }
            Some(def_info) => mir::Identifier {
                def_id: def_info.def_id,
                node_id: convert_node_id(identifier.node_id),
                ident: identifier.ident,
                span: identifier.span,
            },
        }
    }

    fn resolve_expr(&mut self, expr: ast::Expr<'ast>) -> mir::Expr<'ast> {
        match expr {
            ast::Expr::Identifier(identifier) => {
                let error_span = identifier.span;
                mir::Expr::Identifier(self.resolve_identifier(identifier, BindingType::Var, error_span))
            }
            ast::Expr::FuncCall(func_call) => {
                mir::Expr::FuncCall(self.resolve_func_call(func_call))
            }
            ast::Expr::ArrayDecl { node_id, list, span } => {
                
                let mut array_items_expr = vec![];
                
                for array_item_expr in list {
                    array_items_expr.push(self.resolve_expr(array_item_expr));
                }
                
                let array_decl = mir::Expr::ArrayDecl {
                    node_id: convert_node_id(node_id),
                    list: array_items_expr,
                    span,
                };
                
                array_decl
            },
            ast::Expr::MemLookup { .. } => todo!(),
            ast::Expr::StructFieldCall { .. } => todo!(),
            ast::Expr::StructInit {
                node_id,
                ident,
                fields,
                span,
            } => {
                mir::Expr::StructInit {
                    node_id: convert_node_id(node_id),
                    ident: self.resolve_identifier(ident, BindingType::Struct, span),
                    fields: vec![], // TODO
                    span,
                }
            }
            ast::Expr::CastType { .. } => todo!(),
            ast::Expr::Dereference { .. } => todo!(),
            ast::Expr::Reference { .. } => todo!(),
            ast::Expr::LogicalExpr(logical_expr) => {
                mir::Expr::LogicalExpr(self.resolve_logical_expr(logical_expr))
            }
            ast::Expr::MathExpr(math_expr) => {
                mir::Expr::MathExpr(self.resolve_math_expr(math_expr))
            }
            ast::Expr::Literal(literal) => mir::Expr::Literal(self.resolve_literal(literal)),
        }
    }

    fn resolve_stmt(&mut self, stmt: ast::Stmt<'ast>) -> mir::Stmt<'ast> {
        match stmt {
            ast::Stmt::Block(block) => mir::Stmt::Block(self.resolve_block(block)),
            ast::Stmt::Return {
                node_id,
                expr,
                span,
            } => mir::Stmt::Return {
                node_id: convert_node_id(node_id),
                expr: match expr {
                    None => None,
                    Some(expr) => Some(self.resolve_expr(expr)),
                },
                span: span.clone(),
            },
            ast::Stmt::Break { node_id, span } => mir::Stmt::Break {
                node_id: convert_node_id(node_id),
                span,
            },
            ast::Stmt::StructDecl(struct_decl) => {
                mir::Stmt::StructDecl(self.resolve_struct_decl(struct_decl))
            }
            ast::Stmt::FuncDecl(func_decl) => {
                mir::Stmt::FuncDecl(self.resolve_func_decl(func_decl))
            }
            ast::Stmt::Assignment {
                node_id,
                ident,
                ty,
                expr,
                span,
            } => match self.get_last_scope_binding(ident, BindingType::Var) {
                None => {
                    let def_id = mir::gen_def_id();
                    self.add_binding(ident, node_id, def_id, BindingType::Var);

                    mir::Stmt::Assignment {
                        def_id,
                        node_id: convert_node_id(node_id),
                        ident,
                        ty: match ty {
                            None => None,
                            Some(ty) => Some(self.resolve_type(ty)),
                        },
                        expr: self.resolve_expr(expr),
                        span: span.clone(),
                    }
                }
                Some(info) => {
                    panic!("Variable \"{}\" already exists ({:?})", ident, info.node_id)
                }
            },
            ast::Stmt::Assign {
                node_id,
                lhs,
                rhs,
                span,
            } => mir::Stmt::Assign {
                node_id: convert_node_id(node_id),
                lhs: self.resolve_expr(lhs),
                rhs: self.resolve_expr(rhs),
                span: span.clone(),
            },
            ast::Stmt::FuncCall(func_call) => {
                mir::Stmt::FuncCall(self.resolve_func_call(func_call))
            }
            ast::Stmt::IfElse {
                node_id,
                cond,
                then_block,
                else_block,
                span,
            } => mir::Stmt::IfElse {
                node_id: convert_node_id(node_id),
                cond: Box::new(self.resolve_expr(*cond)),
                then_block: self.resolve_block(then_block),
                else_block: match else_block {
                    None => None,
                    Some(block) => Some(self.resolve_block(block)),
                },
                span,
            },
            ast::Stmt::WhileLoop {
                node_id,
                cond,
                body,
                span,
            } => mir::Stmt::WhileLoop {
                node_id: convert_node_id(node_id),
                cond: Box::new(self.resolve_expr(*cond)),
                body: self.resolve_block(body),
                span,
            },
        }
    }

    fn resolve_block<'tcx>(&mut self, block: ast::Block<'ast>) -> mir::Block<'ast> {
        self.scopes.push(Scope::new(ScopeKind::Normal));

        let mut mir_block = mir::Block {
            node_id: convert_node_id(block.node_id),
            stmts: vec![],
            span: block.span,
        };

        for stmt in block.stmts {
            mir_block.stmts.push(self.resolve_stmt(stmt));
        }

        self.scopes.pop();

        mir_block
    }

    fn resolve_struct_decl(&mut self, struct_decl: ast::StructDecl<'ast>) -> mir::StructDecl<'ast> {
        match self.get_last_scope_binding(struct_decl.ident, BindingType::Struct) {
            None => {
                let mir_struct_decl = mir::StructDecl {
                    def_id: mir::gen_def_id(),
                    node_id: convert_node_id(struct_decl.node_id),
                    ident: struct_decl.ident,
                    fields: vec![], // TODO
                    span: struct_decl.span,
                };

                self.add_binding(
                    struct_decl.ident,
                    struct_decl.node_id,
                    mir_struct_decl.def_id,
                    BindingType::Struct,
                );

                mir_struct_decl
            }
            Some(info) => {
                panic!(
                    "Struct \"{}\" already exists ({:?})",
                    struct_decl.ident, info.node_id
                )
            }
        }
    }

    fn resolve_func_decl(&mut self, func_decl: ast::FuncDecl<'ast>) -> mir::FuncDecl<'ast> {
        match self.get_last_scope_binding(func_decl.ident, BindingType::Func) {
            None => {
                let mir_func_decl = mir::FuncDecl {
                    def_id: mir::gen_def_id(),
                    node_id: convert_node_id(func_decl.node_id),
                    ident: func_decl.ident,
                    args: vec![], // TODO
                    return_type: self.resolve_type(func_decl.return_type),
                    body: self.resolve_block(func_decl.body),
                    span: func_decl.span,
                };

                self.add_binding(
                    func_decl.ident,
                    func_decl.node_id,
                    mir_func_decl.def_id,
                    BindingType::Func,
                );

                mir_func_decl
            }
            Some(info) => {
                panic!(
                    "Function \"{}\" already exists ({:?})",
                    func_decl.ident, info.node_id
                )
            }
        }
    }

    fn resolve_func_call(&mut self, func_call: ast::FuncCall<'ast>) -> mir::FuncCall<'ast> {
        let mut mir_func_call = mir::FuncCall {
            node_id: convert_node_id(func_call.node_id),
            ident: self.resolve_identifier(func_call.ident, BindingType::Func, func_call.span),
            args: vec![],
            span: func_call.span,
        };

        for arg in func_call.args {
            mir_func_call.args.push(self.resolve_expr(arg));
        }

        mir_func_call
    }

    fn resolve_prog_stmt(&mut self, prog_stmt: ast::ProgStmt<'ast>) -> mir::ProgStmt<'ast> {
        match prog_stmt {
            ast::ProgStmt::StructDecl(struct_decl) => {
                mir::ProgStmt::StructDecl(self.resolve_struct_decl(struct_decl))
            }
            ast::ProgStmt::FuncDecl(func_decl) => {
                mir::ProgStmt::FuncDecl(self.resolve_func_decl(func_decl))
            }
        }
    }

    pub fn resolve_ast(&mut self, ast: ast::AST<'ast>) -> mir::MIR<'ast> {
        self.scopes.push(Scope::new(ScopeKind::Normal));
        let mut mir_tree = mir::MIR { program: vec![] };
        for prog_stmt in ast.program {
            mir_tree.program.push(self.resolve_prog_stmt(prog_stmt));
        }
        self.scopes.pop();
        mir_tree
    }
}

fn convert_node_id(ast_node_id: ast::NodeId) -> mir::NodeId {
    mir::NodeId(ast_node_id.0)
}

pub fn resolve_names<'tcx, 'a>(
    _type_context: &'tcx TypeContext<'tcx>,
    key: ast::AST<'a>,
) -> mir::MIR<'a> {
    let mut resolver = Resolver::<'a>::new();
    let mir_tree = resolver.resolve_ast(key);
    mir_tree
}

pub trait ResolveNamesProvider<'ctx> {
    fn resolve_names(&self, key: ast::AST<'ctx>) -> mir::MIR<'ctx>;
}

impl<'ctx> ResolveNamesProvider<'ctx> for marsc_query_system::provider::Providers<'ctx> {
    fn resolve_names(&self, key: ast::AST<'ctx>) -> mir::MIR<'ctx> {
        resolve_names(self.type_context, key)
    }
}
