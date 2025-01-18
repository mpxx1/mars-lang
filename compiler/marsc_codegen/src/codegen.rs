use std::collections::HashMap;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{AnyType, BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, PointerValue};
use marsc_mir::mir::{Block, Expr, FuncCall, FuncDecl, Literal, LogicalExpr, MathExpr, ProgStmt, Stmt, StructDecl, Type, MIR};

pub struct Codegen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    var_table: HashMap<&'ctx str, PointerValue<'ctx>>,
}

impl<'ctx> Codegen<'ctx> {
    
    fn codegen_mir(&mut self, root_ast: &MIR) -> String {
        for prog_stmt in &root_ast.program {
            match prog_stmt {
                ProgStmt::StructDecl(struct_decl) => self.codegen_struct(struct_decl),
                ProgStmt::FuncDecl(func_decl) => self.codegen_func(func_decl),
            };
        }
        
        self.module.print_to_string().to_string()
    }
    
    fn codegen_struct(&self, struct_decl: &StructDecl) {
        let struct_type = self.context.opaque_struct_type(struct_decl.ident);
        
        let field_types: Vec<BasicTypeEnum> = struct_decl
            .fields
            .iter()
            .map(|field| self.codegen_type(&field.ty))
            .collect();
        
        struct_type.set_body(&field_types, false);
    }

    fn codegen_func(&mut self, func_decl: &FuncDecl) {
        let arg_types: Vec<BasicMetadataTypeEnum> = func_decl
            .args
            .iter()
            .map(|arg| self.codegen_type(&arg.ty).into())
            .collect();
        
        let return_type = self.codegen_type(&func_decl.return_type);
        let fn_type = return_type.fn_type(&arg_types, false);
        let function = self.module.add_function(&func_decl.ident, fn_type, None);
        let entry = self.context.append_basic_block(function, "entry"); 
        self.builder.position_at_end(entry);

        self.codegen_block(&func_decl.body);
    }
    
    fn codegen_block(&mut self, block: &Block) {
        for stmt in &block.stmts {
            self.codegen_stmt(stmt);
        }
    }
    
    fn codegen_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Block(_) => {}
            Stmt::Return { expr, .. } => {
                if let Some(expr) = expr {
                    let value = self.codegen_expr(expr);
                    self.builder.build_return(Some(&value)).unwrap();
                } else {
                    self.builder.build_return(None).unwrap();
                }
            }
            Stmt::Break { .. } => {}
            Stmt::StructDecl(struct_decl) => {
                self.codegen_struct(struct_decl);
            }
            Stmt::FuncDecl(func_decl) => {
                self.codegen_func(func_decl);
            }
            Stmt::Assignment { 
                def_id,
                node_id,
                ident,
                ty,
                expr,
                span
            } => {
                match ty {
                    None => panic!("Cannot specify type for: {}", span.as_str()),
                    Some(ty) => {
                        let variable_type = self.codegen_type(&ty);
                        let variable = self.builder.build_alloca(variable_type, ident).unwrap();
                        
                        let value = self.codegen_expr(expr);
                        
                        // self.var_table.insert(ident, variable);
                        
                        self.builder.build_store(variable, value).unwrap();
                    }
                }
            }
            Stmt::Assign { .. } => {}
            Stmt::FuncCall(_) => {}
            Stmt::IfElse { .. } => {}
            Stmt::WhileLoop { .. } => {}
        }
    }
    
    fn codegen_expr(&self, expr: &Expr) -> BasicValueEnum  {
        match expr {
            Expr::Identifier(identifier) => todo!(),
            Expr::FuncCall(func_call) => self.codegen_func_call(func_call),
            Expr::ArrayDecl { .. } => todo!(),
            Expr::MemLookup { .. } => todo!(),
            Expr::StructFieldCall { .. } => todo!(),
            Expr::StructInit { .. } => todo!(),
            Expr::CastType { .. } => todo!(),
            Expr::Dereference { .. } => todo!(),
            Expr::Reference { .. } => todo!(),
            Expr::LogicalExpr(logical_expr) => self.codegen_logical_expr(logical_expr),
            Expr::MathExpr(math_expr) => self.codegen_math_expr(math_expr),
            Expr::Literal(literal) => self.codegen_literal(literal),
        }
    }
    
    /*fn codegen_identifier(&mut self, identifier: &Identifier) -> BasicValueEnum {
        if let Some(value) = self.var_table.get(identifier.ident) {
            *value.into()
        } else {
            panic!("Cannot find variable: {}", identifier.span.as_str());
        }
    }*/
    
    fn codegen_literal(&self, literal: &Literal) -> BasicValueEnum {
        match literal {
            Literal::Int { lit, .. } => self.context.i64_type().const_int(*lit as u64, false).into(),
            Literal::Float { lit, .. } => self.context.f64_type().const_float(*lit as f64).into(),
            Literal::Str { lit, .. } => self.context.const_string(lit.as_bytes(), true).as_basic_value_enum(),
            Literal::Char { lit, .. } => self.context.i8_type().const_int(*lit as u64, false).into(),
            Literal::Bool { lit, ..} => self.context.bool_type().const_int(*lit as u64, false).into(),
        }
    }

    fn codegen_math_expr(&self, math_expr: &MathExpr) -> BasicValueEnum {
        match math_expr {
            MathExpr::Additive { .. } => todo!(),
            MathExpr::Multiplicative { .. } => todo!(),
            MathExpr::Power { .. } => todo!(),
            MathExpr::Primary(primary) => {
                self.codegen_expr(primary)
            }
        }
    }
    
    fn codegen_logical_expr(&self, logical_expr: &LogicalExpr) -> BasicValueEnum {
        match logical_expr {
            LogicalExpr::Not { .. } => todo!(),
            LogicalExpr::Or { .. } => todo!(),
            LogicalExpr::And { .. } => todo!(),
            LogicalExpr::Comparison { .. } => todo!(),
            LogicalExpr::Primary(primary) => {
                self.codegen_expr(primary)
            }
        }
    }
    
    fn codegen_func_call(&self, func_call: &FuncCall) -> BasicValueEnum  {
        let function = self.module.get_function(func_call.ident.ident).unwrap();
        
        let args: Vec<BasicMetadataValueEnum> = func_call
            .args
            .iter()
            .map(|arg| self.codegen_expr(arg).into())
            .collect();
        
        let call_site = self.builder.build_call(function, &args, "call");
        
        call_site
            .unwrap().try_as_basic_value()
            .left()
            .unwrap()
    }
    
    fn codegen_type(&self, type_ast: &Type) -> BasicTypeEnum<'ctx> {
        match type_ast {
            Type::I64 => self.context.i64_type().into(),
            Type::F64 => self.context.f64_type().into(),
            Type::Str => todo!(),
            Type::Char => self.context.i8_type().into(),
            Type::Bool => self.context.bool_type().into(),
            Type::Void => todo!(),
            Type::Custom(identifier) => {
                self.module.get_struct_type(identifier.ident).unwrap().into()
            },
            Type::Array(_, _) => todo!(),
            Type::Vec(_) => todo!(),
            Type::Ref(_) => todo!(),
        }
    }
    
    fn test(&self) -> String {
        let i64_type = self.context.i64_type();
        let fn_type = i64_type.fn_type(&[i64_type.into(), i64_type.into(), i64_type.into()], false);
        let function = self.module.add_function("sum", fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(basic_block);

        let x = function.get_nth_param(0).unwrap().into_int_value();
        let y = function.get_nth_param(1).unwrap().into_int_value();
        let z = function.get_nth_param(2).unwrap().into_int_value();

        let sum = self.builder.build_int_add(x, y, "sum").unwrap();
        let sum = self.builder.build_int_add(sum, z, "sum").unwrap();

        self.builder.build_return(Some(&sum)).unwrap();

        self.module.print_to_string().to_string()
    }
}

pub fn codegen(mir: &MIR) -> String {
    let context = Context::create();
    let module = context.create_module("test_module");
    let mut codegen = Codegen {
        context: &context,
        module,
        builder: context.create_builder(),
        var_table: HashMap::new(),
    };
    
    codegen.codegen_mir(mir)
}