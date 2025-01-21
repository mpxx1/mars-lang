use ast::{Block, Expr, FuncCall, Identifier, Literal, LogicalExpr, MathExpr, Stmt, Type};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, PointerValue};
use mir::{FuncProto, Mir, Scope, StructProto};
use std::collections::HashMap;
use std::path::PathBuf;
use inkwell::AddressSpace;
use inkwell::targets::{FileType, InitializationConfig, Target, TargetMachine};

pub struct Codegen<'ctx, 'src> {
    pub(crate) context: &'ctx Context,
    pub(crate) module: Module<'ctx>,
    pub(crate) builder: Builder<'ctx>,
    pub(crate) mir: &'ctx Mir<'src>,
    pub(crate) var_table: HashMap<&'src str, PointerValue<'ctx>>,
}

impl<'ctx, 'src> Codegen<'ctx, 'src>
where
    'src: 'ctx
{
    pub(crate) fn codegen(&mut self) -> String {
        let global_scope: &'ctx Scope = self.mir.scopes.get(&0_usize).unwrap();
        self.codegen_scope(global_scope);
        
        self.module.print_to_string().to_string()
    }

    pub(crate) fn codegen_scope_by_id(&mut self, id: &usize) {
        let scope = &self.mir.scopes[id];
        self.codegen_scope(scope);
    }

    pub(crate) fn codegen_scope(&mut self, scope: &'ctx Scope<'src>) {

        for scope_struct in &scope.structs {
            self.codegen_struct(scope_struct.1);
        }

        for scope_fun in &scope.funs {
            match scope_fun.1.return_type {
                Type::Void => self.codegen_void_func(scope_fun.1),
                _ => self.codegen_func(scope_fun.1),
            }
        }

        for stmt in &scope.instrs {
            self.codegen_stmt(stmt, scope);
        }
    }

    pub(crate) fn codegen_struct(&self, struct_decl: &'ctx StructProto<'src>) {
        let struct_type = self.context.opaque_struct_type(struct_decl.ident);
        
        let field_types: Vec<BasicTypeEnum> = struct_decl
            .fields
            .iter()
            .map(|field| self.codegen_type(&field.ty))
            .collect();
        
        struct_type.set_body(&field_types, false);
    }

    pub(crate) fn codegen_func(&mut self, func_decl: &'ctx FuncProto<'src>) {

        if self.mir.sys_funs.contains(&func_decl.ident) {
            self.generate_external_function(func_decl);
            return;
        }

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

        self.codegen_scope_by_id(&func_decl.node_id);
}

    pub(crate) fn codegen_void_func(&mut self, func_decl: &'ctx FuncProto<'src>) {
        let arg_types: Vec<BasicMetadataTypeEnum> = func_decl
            .args
            .iter()
            .map(|arg| self.codegen_type(&arg.ty).into())
            .collect();

        let return_type = self.context.void_type();
        let fn_type = return_type.fn_type(&arg_types, false);
        let function = self.module.add_function(&func_decl.ident, fn_type, None);
        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        self.codegen_scope_by_id(&func_decl.node_id);
        
        self.builder.build_return(None).unwrap();
    }

    pub(crate) fn _codegen_block(&mut self, block: &'ctx Block<'src>, scope: &'ctx Scope<'ctx>) {
        for stmt in &block.stmts {
            self.codegen_stmt(stmt, scope);
        }
    }

    pub(crate) fn codegen_stmt(&mut self, stmt: &'ctx Stmt<'src>, scope: &'ctx Scope<'ctx>) {
        match stmt {
            Stmt::Block(_) => {}
            Stmt::Return { expr, .. } => {
                if let Some(expr) = expr {
                    let value = self.codegen_expr(expr, scope);
                    self.builder.build_return(Some(&value)).unwrap();
                } else {
                    self.builder.build_return(None).unwrap();
                }
            }
            Stmt::Break { .. } => {}
            Stmt::Assignment {
                node_id: _,
                ident,
                ty,
                expr,
                span: _,
            } => {
                let variable_type = self.codegen_type(&ty);
                let variable = self.builder.build_alloca(variable_type, ident).unwrap();

                let value = self.codegen_expr(expr, scope);

                self.builder.build_store(variable, value).unwrap();
                
                self.var_table.insert(ident, variable);
            }
            Stmt::Assign { .. } => {}
            Stmt::FuncCall(func_call) => {
                self.codegen_func_call(func_call, scope);
            },
            Stmt::IfElse { .. } => {}
            Stmt::WhileLoop { .. } => {}
            _ => unimplemented!(),
        }
    }

    pub(crate) fn codegen_expr(&self, expr: &'ctx Expr<'src>, scope: &'ctx Scope<'ctx>) -> BasicValueEnum<'ctx>  {
        match expr {
            Expr::Identifier(identifier) => self.codegen_identifier(identifier, scope),
            Expr::FuncCall(func_call) => {
                if let Some(value) = self.codegen_func_call(func_call, scope) {
                    value
                } else { 
                    panic!("Function has no return value")
                }
            },
            Expr::ArrayDecl { .. } => todo!(),
            Expr::MemLookup { .. } => todo!(),
            Expr::StructFieldCall { .. } => todo!(),
            Expr::StructInit { .. } => todo!(),
            Expr::CastType { .. } => todo!(),
            Expr::Dereference { .. } => todo!(),
            Expr::Reference { .. } => todo!(),
            Expr::LogicalExpr(logical_expr) => self.codegen_logical_expr(logical_expr, scope),
            Expr::MathExpr(math_expr) => self.codegen_math_expr(math_expr, scope),
            Expr::Literal(literal) => self.codegen_literal(literal),
        }
    }

    pub(crate) fn codegen_identifier(&self, identifier: &'ctx Identifier<'src>, scope: &'ctx Scope<'ctx>) -> BasicValueEnum<'ctx> {
        let variable = scope.vars.get(identifier.ident).unwrap();
        
        if let Some(ptr) = self.var_table.get(identifier.ident) {
            let ty = self.codegen_type(&variable.ty);
            self.builder.build_load(ty, *ptr, identifier.ident).unwrap()
        } else {
            panic!("Cannot find variable: {}", identifier.span.as_str());
        }
    }

    pub(crate) fn codegen_literal(&self, literal: &'ctx Literal<'src>) -> BasicValueEnum<'ctx> {
        match literal {
            Literal::Int { lit, .. } => self.context.i64_type().const_int(*lit as u64, false).into(),
            Literal::Float { lit, .. } => self.context.f64_type().const_float(*lit as f64).into(),
            Literal::Str { lit, .. } => {
                let lit2 = lit.replace("\\n", "\n");
                let global_string = self.context.const_string(lit2.as_bytes(), true);
                let value = self.module.add_global(global_string.get_type(), None, "global_str");
                value.set_initializer(&global_string);
                value.as_basic_value_enum()
            },
            Literal::Char { lit, .. } => self.context.i8_type().const_int(*lit as u64, false).into(),
            Literal::Bool { lit, ..} => self.context.bool_type().const_int(*lit as u64, false).into(),
            Literal::NullRef { .. } => todo!(),
        }
    }

    pub(crate) fn codegen_math_expr(&self, math_expr: &'ctx MathExpr<'src>, scope: &'ctx Scope<'ctx>) -> BasicValueEnum<'ctx> {
        match math_expr {
            MathExpr::Additive { .. } => todo!(),
            MathExpr::Multiplicative { .. } => todo!(),
            MathExpr::Power { .. } => todo!(),
            MathExpr::Primary(primary) => {
                self.codegen_expr(primary, scope)
            }
        }
    }

    pub(crate) fn codegen_logical_expr(&self, logical_expr: &'ctx LogicalExpr<'src>, scope: &'ctx Scope<'ctx>) -> BasicValueEnum<'ctx> {
        match logical_expr {
            LogicalExpr::Not { .. } => todo!(),
            LogicalExpr::Or { .. } => todo!(),
            LogicalExpr::And { .. } => todo!(),
            LogicalExpr::Comparison { .. } => todo!(),
            LogicalExpr::Primary(primary) => {
                self.codegen_expr(primary, scope)
            }
        }
    }

    pub(crate) fn codegen_func_call(&self, func_call: &'ctx FuncCall<'src>, scope: &'ctx Scope<'ctx>) -> Option<BasicValueEnum<'ctx>> {
        let error_msg = format!("Cannot find function '{}'", func_call.ident.ident);
        
        let function = self.module
            .get_function(func_call.ident.ident)
            .expect(error_msg.as_str());
        
        let args: Vec<BasicMetadataValueEnum> = func_call
            .args
            .iter()
            .map(|arg| self.codegen_expr(arg, scope).into())
            .collect();
        
        let call_site = self.builder.build_call(function, &args, "call");
        
        if let Some(_) = function.get_type().get_return_type() {
            let value = call_site
                .unwrap().try_as_basic_value()
                .left()
                .unwrap();
            
            Some(value)
        } else {
            call_site.unwrap();
            None
        }
    }

    pub(crate) fn codegen_type(&self, type_ast: &'ctx Type<'src>) -> BasicTypeEnum<'ctx> {
        match type_ast {
            Type::I64 => self.context.i64_type().into(),
            Type::F64 => self.context.f64_type().into(),
            Type::Str => self.context.ptr_type(AddressSpace::default()).into(),
            Type::Char => self.context.i8_type().into(),
            Type::Bool => self.context.bool_type().into(),
            Type::Void => unreachable!(),
            Type::Custom(identifier) => {
                self.module.get_struct_type(identifier.ident).unwrap().into()
            },
            Type::Array(_, _) => todo!(),
            Type::Vec(_) => todo!(),
            Type::Ref(_) => todo!(),
            Type::Any => todo!(),
            Type::Unresolved => todo!(),
        }
    }

    pub fn codegen_bytecode(&self, output: &PathBuf) -> Result<(), String> {
        self.module.write_bitcode_to_path(output);
        
        Target::initialize_all(&InitializationConfig::default());
        
        // Получаем целевую платформу
        let target_triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&target_triple).map_err(|e| e.to_string())?;
        let target_machine = target
            .create_target_machine(
                &target_triple,
                "generic", // CPU
                "",        // Features
                inkwell::OptimizationLevel::Default,
                inkwell::targets::RelocMode::Default,
                inkwell::targets::CodeModel::Default,
            )
            .ok_or_else(|| "Failed to create target machine".to_string())?;
        
        // Генерация объектного файла
        target_machine
            .write_to_file(&self.module, FileType::Object, output.as_ref())
            .map_err(|e| e.to_string())
    }

    
}

pub fn codegen<'src>(mir: &'src Mir) -> String {
    let context = Context::create();
    let module = context.create_module("test_module");
    let mut codegen = Codegen::<'_, 'src> {
        context: &context,
        module,
        builder: context.create_builder(),
        mir,
        var_table: HashMap::new(),
    };

    let result = codegen.codegen();
    
    // let out_dir = env::var("OUT_DIR").unwrap();
    // let output = out_dir + "/program.o";
    
    // codegen.codegen_bytecode(&PathBuf::from(output)).unwrap();
    
    result
}