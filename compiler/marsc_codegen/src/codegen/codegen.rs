use ast::{Block, Expr, Identifier, Literal, MathExpr, Stmt, Type};
use err::CompileError;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::{FileType, InitializationConfig, Target, TargetMachine};
use inkwell::types::{ArrayType, BasicType, BasicTypeEnum, StructType};
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue};
use inkwell::AddressSpace;
use mir::{FuncProto, Mir, Scope};
use std::collections::HashMap;
use std::path::PathBuf;
use std::process::Command;

#[derive(Debug)]
pub(super) enum VariableData<'ctx> {
    Plain {
        pointer: PointerValue<'ctx>,
    },
    Array {
        array_type: ArrayType<'ctx>,
        pointer: PointerValue<'ctx>,
    },
    Struct {
        struct_type: StructType<'ctx>,
        pointer: PointerValue<'ctx>,
    }
}

pub struct Codegen<'ctx, 'src> {
    pub(crate) context: &'ctx Context,
    pub(crate) module: Module<'ctx>,
    pub(crate) builder: Builder<'ctx>,
    pub(crate) mir: &'ctx Mir<'src>,
    pub(crate) var_table: HashMap<&'src str, VariableData<'ctx>>,
}

impl<'ctx, 'src> Codegen<'ctx, 'src>
where
    'src: 'ctx
{
    pub(crate) fn store_variable(&mut self, ident: &'src str, pointer_value: PointerValue<'ctx>, ty: BasicTypeEnum<'ctx>) {
        match ty {
            BasicTypeEnum::ArrayType(array_type) => {
                self.var_table.insert(ident, VariableData::Array {
                    pointer: pointer_value,
                    array_type,
                });
            }
            BasicTypeEnum::IntType(_)
            | BasicTypeEnum::FloatType(_)
            | BasicTypeEnum::PointerType(_) => {
                self.var_table.insert(ident, VariableData::Plain { pointer: pointer_value });
            }
            BasicTypeEnum::StructType(_) => {}
            BasicTypeEnum::VectorType(_) => {}
        }
    }
}

impl<'ctx, 'src> Codegen<'ctx, 'src>
where
    'src: 'ctx
{
    pub(crate) fn codegen(&mut self) -> String {
        let global_scope: &'ctx Scope = self.mir.scopes.get(&0_usize).unwrap();
        self.codegen_scope(global_scope, None);

        self.module.print_to_string().to_string()
    }

    pub(crate) fn codegen_scope_by_id(
        &mut self,
        id: &usize,
        parent_function_value: Option<FunctionValue>)
    {
        let scope = &self.mir.scopes[id];
        self.codegen_scope(scope, parent_function_value);
    }

    pub(crate) fn codegen_scope(
        &mut self,
        scope: &'ctx Scope<'src>,
        parent_function_value: Option<FunctionValue>
    ) {

        for scope_struct in &scope.structs {
            self.codegen_struct(scope_struct.1);
        }

        let funs: Vec<&FuncProto> = scope.funs.values().collect();

        let functions: Vec<Option<FunctionValue>> = funs
            .iter()
            .map(|proto| match proto.return_type {
                Type::Void => self.codegen_void_function_definition(proto),
                _ => self.codegen_function_definition(proto),
            })
            .collect();

        for (proto, function) in funs.iter().zip(functions.iter()) {
            if let Some(function) = function {
                self.codegen_function_entry(proto, *function);
            }
        }

        if !scope.instrs.is_empty() {
            match parent_function_value {
                None => unreachable!(),
                Some(parent_function_value) => {
                    for stmt in &scope.instrs {
                        self.codegen_stmt(stmt, scope, parent_function_value.clone());
                    }
                }
            }
        }
    }

    pub(crate) fn _codegen_block(&mut self, block: &'ctx Block<'src>, scope: &'ctx Scope<'ctx>) {
        unimplemented!()
    }

    pub(crate) fn codegen_stmt(
        &mut self,
        stmt: &'ctx Stmt<'src>,
        scope: &'ctx Scope<'ctx>,
        function_value: FunctionValue,
    ) -> Result<(), CompileError> {
        match stmt {
            Stmt::Block(_) => todo!(),
            Stmt::Return { expr, .. } => {
                if let Some(expr) = expr {
                    let value = self.codegen_expr(expr, scope);
                    self.builder.build_return(Some(&value)).unwrap();
                    Ok(())
                } else {
                    self.builder.build_return(None).unwrap();
                    Ok(())
                }
            }
            Stmt::Break { .. } => todo!(),
            Stmt::Assignment {
                node_id: _,
                ident,
                ty,
                expr,
                span,
            } => {
                let variable_type = self.codegen_type(&ty);
                match variable_type {
                    BasicTypeEnum::ArrayType(_) => {
                        self.codegen_array_assignment(ident, variable_type, expr, span.clone(), scope)
                    }
                    BasicTypeEnum::IntType(_) | BasicTypeEnum::FloatType(_) => {
                        let variable = self.builder.build_alloca(variable_type, ident).unwrap();

                        let value = self.codegen_expr(expr, scope);

                        self.builder.build_store(variable, value).unwrap();

                        self.store_variable(ident, variable, variable_type);

                        Ok(())
                    }
                    BasicTypeEnum::PointerType(_) => {
                        let variable = self.builder.build_alloca(variable_type, ident).unwrap();

                        let value = self.codegen_expr(expr, scope);

                        if let BasicValueEnum::PointerValue(pointer_value) = value {
                            let loaded_reference = self.builder.build_load(
                                variable_type,
                                pointer_value,
                                "loaded_reference",
                            ).unwrap();

                            self.builder.build_store(variable, loaded_reference).unwrap();

                            println!();
                            self.store_variable(ident, variable, variable_type);

                            Ok(())
                        } else {
                            unreachable!()
                        }
                    }
                    BasicTypeEnum::StructType(_) => todo!(),
                    BasicTypeEnum::VectorType(_) => todo!(),
                }
            }
            Stmt::Assign { lhs, rhs, .. } => {
                match lhs { // TODO array redeclaration
                    Expr::MemLookup { ident, indices, span, .. } => {
                        let rhs_value = self.codegen_expr(rhs, scope);
                        self.codegen_set_array_element(ident, indices, rhs_value, span.clone(), scope)
                    }
                    Expr::Identifier(identifier) => {
                        let variable = self.codegen_identifier_pointer(identifier.clone(), scope);

                        let value = self.codegen_expr(rhs, scope);

                        self.builder.build_store(variable, value).unwrap();

                        Ok(())
                    }
                    _ => unreachable!()
                }
            }
            Stmt::FuncCall(func_call) => {
                self.codegen_func_call(func_call, scope);
                Ok(())
            },
            Stmt::IfElse { .. } => unreachable!(),
            Stmt::WhileLoop { .. } => unreachable!(),
            Stmt::StructDecl(_) => unimplemented!(),
            Stmt::FuncDecl(_) => unimplemented!(),
            Stmt::GoToBlock { .. } => unimplemented!(),
            Stmt::GoToIfCond { cond, then_block_id, else_block_id } => {
                let cond_value = self.codegen_expr(cond, scope).into_int_value();

                let then_bb = self.context.append_basic_block(function_value, "then");
                let else_bb = if else_block_id.is_some() {
                    Some(self.context.append_basic_block(function_value, "else"))
                } else {
                    None
                };
                let merge_bb = self.context.append_basic_block(function_value, "merge");

                if let Some(else_bb) = else_bb {
                    self.builder.build_conditional_branch(cond_value, then_bb, else_bb).unwrap();
                } else {
                    self.builder.build_conditional_branch(cond_value, then_bb, merge_bb).unwrap();
                }

                self.builder.position_at_end(then_bb);
                self.codegen_volatile_mark();
                self.codegen_scope_by_id(then_block_id, Some(function_value));
                self.builder.build_unconditional_branch(merge_bb).unwrap();

                if let Some(else_bb) = else_bb {
                    self.builder.position_at_end(else_bb);
                    self.codegen_volatile_mark();
                    self.codegen_scope_by_id(&else_block_id.unwrap(), Some(function_value));
                    self.builder.build_unconditional_branch(merge_bb).unwrap();
                }

                self.builder.position_at_end(merge_bb);

                Ok(())
            },
            Stmt::GoToWhile { .. } => unimplemented!(),
        }
    }

    fn codegen_volatile_mark(&self) {
        let alloca = self.builder.build_alloca(self.context.i64_type(), "block_mark").unwrap();
        let const_value = self.context.i64_type().const_int(0, false);
        self.builder.build_store(alloca, const_value).unwrap().set_volatile(true).unwrap()
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
            Expr::ArrayDecl { list, .. } => {
                self.codegen_array_declaration(list, scope)
            },
            Expr::MemLookup { ident, indices, span, .. } => {
                self.codegen_get_array_element(ident, indices, span.clone(), scope).unwrap()
            },
            Expr::StructFieldCall { .. } => todo!(),
            Expr::StructInit { .. } => todo!(),
            Expr::CastType { .. } => todo!(),
            Expr::Dereference { inner, .. } => {
                let value = self.codegen_expr(inner, scope);
                let pointer = match value {
                    BasicValueEnum::ArrayValue(_) => unreachable!(),
                    BasicValueEnum::IntValue(_) => unreachable!(),
                    BasicValueEnum::FloatValue(_) => unreachable!(),
                    BasicValueEnum::PointerValue(pointer) => {
                        pointer
                    }
                    BasicValueEnum::StructValue(_) => unreachable!(),
                    BasicValueEnum::VectorValue(_) => unreachable!(),
                };
                println!("pointer: {:#?}", pointer);

                let dereferenced_value = self.builder.build_load(
                    self.context.i64_type(),
                    pointer,
                    "dereferenced_value").unwrap();

                dereferenced_value
            },
            Expr::Reference { inner, .. } => {
                let inner = inner.clone();
                let pointer = match *inner {
                    Expr::Identifier(identifier) => {
                        self.codegen_identifier_pointer(identifier, scope)
                    },
                    _ => unreachable!(),
                };

                let variable = self.builder.build_alloca(pointer.get_type(), "referenced").unwrap();

                self.builder.build_store(variable, pointer).unwrap();

                variable.as_basic_value_enum()
            },
            Expr::LogicalExpr(logical_expr) => self.codegen_logical_expr(logical_expr, scope),
            Expr::MathExpr(math_expr) => self.codegen_math_expr(math_expr, scope),
            Expr::Literal(literal) => self.codegen_literal(literal),
        }
    }

    pub(crate) fn codegen_identifier(&self, identifier: &'ctx Identifier<'src>, scope: &'ctx Scope<'ctx>) -> BasicValueEnum<'ctx> {
        let variable = scope.vars.get(identifier.ident).unwrap(); // TODO remove vars using

        if let Some(variable_data) = self.var_table.get(identifier.ident) {
            let ty = self.codegen_type(&variable.ty);
            match variable_data {
                VariableData::Plain { pointer } => {
                    self.builder.build_load(ty, *pointer, identifier.ident).unwrap()
                }
                VariableData::Array { pointer, .. } => {
                    self.builder.build_load(ty, *pointer, identifier.ident).unwrap()
                }
                VariableData::Struct { .. } => {
                    todo!()
                }
            }
        } else {
            panic!("Cannot find variable: {}", identifier.span.as_str());
        }
    }

    pub(crate) fn codegen_identifier_pointer(&self, identifier: Identifier<'src>, scope: &'ctx Scope<'ctx>) -> PointerValue<'ctx> {
        let variable = scope.vars.get(identifier.ident).unwrap(); // TODO remove vars using

        if let Some(variable_data) = self.var_table.get(identifier.ident) {
            let ty = self.codegen_type(&variable.ty);
            match variable_data {
                VariableData::Plain { pointer } => {
                    *pointer
                }
                VariableData::Array { pointer, .. } => {
                    *pointer
                }
                VariableData::Struct { pointer, .. } => {
                    *pointer
                }
            }
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
            MathExpr::Additive { left, right, op, .. } => {
                self.codegen_math_add_expr(left, right, op, scope)
            },
            MathExpr::Multiplicative { left, right, op, .. } => {
                self.codegen_math_mul_expr(left, right, op, scope)
            },
            MathExpr::Power { base, exp, .. } => {
                self.codegen_math_power_expr(base, exp, scope)
            },
            MathExpr::Primary(primary) => {
                self.codegen_expr(primary, scope)
            }
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
            Type::Custom(identifier) => self.module.get_struct_type(identifier.ident).unwrap().into(),
            Type::Array(ty, size) => self.codegen_type(ty).array_type(*size as u32).into(),
            Type::Vec(ty) => {
                match self.codegen_type(ty) {
                    BasicTypeEnum::ArrayType(_) => unreachable!(),
                    BasicTypeEnum::FloatType(ty) => todo!(),
                    BasicTypeEnum::IntType(ty) => todo!(),
                    BasicTypeEnum::PointerType(ty) => todo!(),
                    BasicTypeEnum::StructType(_) => unreachable!(),
                    BasicTypeEnum::VectorType(_) => unreachable!(),
                }
            },
            Type::Ref(ty) => self.context.ptr_type(AddressSpace::default()).into(),
            Type::Any => unreachable!(),
            Type::Unresolved => unreachable!(),
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

    println!("{:#?}", mir);

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

    println!("{}", result);

    let object_file = "/users/nzaguta/RustroverProjects/mars-lang/examples/test.o";
    let std_libio = "/users/nzaguta/mars/std/libio.so";

    codegen.codegen_bytecode(&PathBuf::from(object_file)).unwrap();

    let status = Command::new("clang")
        .args([std_libio, object_file])
        .args(["-o", "/users/nzaguta/RustroverProjects/mars-lang/examples/test"])
        .status()
        .expect("Failed to invoke C compiler and build executable file");

    if !status.success() {
        panic!("Status is not success");
    }

    result
}