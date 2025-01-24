use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::{FileType, InitializationConfig, Target, TargetMachine};
use inkwell::types::{ArrayType, BasicType, BasicTypeEnum, StructType};
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue};
use inkwell::AddressSpace;
use mir::stages::s2::{MIRExpr, MIRFunc, MIRInstruction, MIRLiteral, MIRMathExpr, MIRScope, MIRType};
use mir::Mir;
use std::collections::HashMap;
use std::path::PathBuf;
use std::process::Command;

#[derive(Debug)]
pub(super) enum VariableData<'ctx> {
    Primitive {
        primitive_type: BasicTypeEnum<'ctx>,
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
    pub(in crate::codegen) context: &'ctx Context,
    pub(in crate::codegen) module: Module<'ctx>,
    pub(in crate::codegen) builder: Builder<'ctx>,
    pub(in crate::codegen) mir: &'ctx Mir<'src>,
    pub(in crate::codegen) var_table: HashMap<&'ctx str, VariableData<'ctx>>,
    pub(in crate::codegen) types_table: HashMap<&'ctx str, StructType<'ctx>>,
}

impl<'ctx, 'src> Codegen<'ctx, 'src>
where
    'src: 'ctx
{
    pub(crate) fn store_variable(
        &mut self,
        ident: &'ctx str,
        pointer_value: PointerValue<'ctx>,
        ty: BasicTypeEnum<'ctx>,
    ) {
        match ty {
            BasicTypeEnum::ArrayType(array_type) => {
                self.var_table.insert(ident, VariableData::Array {
                    pointer: pointer_value,
                    array_type,
                });
            }
            BasicTypeEnum::IntType(int_type) => {
                self.var_table.insert(ident, VariableData::Primitive {
                    pointer: pointer_value,
                    primitive_type: int_type.into(),
                });
            },
            BasicTypeEnum::FloatType(float_type) => {
                self.var_table.insert(ident, VariableData::Primitive {
                    pointer: pointer_value,
                    primitive_type: float_type.into(),
                });
            }
            BasicTypeEnum::PointerType(primitive_type) => {
                self.var_table.insert(ident, VariableData::Primitive {
                    pointer: pointer_value,
                    primitive_type: primitive_type.into(),
                });
            }
            BasicTypeEnum::StructType(struct_type) => {
                self.var_table.insert(ident, VariableData::Struct {
                    pointer: pointer_value,
                    struct_type,
                });
            }
            BasicTypeEnum::VectorType(_) => {}
        }
    }

    pub(in crate::codegen) fn get_variable(&self, ident: &'ctx str) -> &VariableData<'ctx> {
        match self.var_table.get(ident) {
            Some(data) => data,
            None => unreachable!("variable: {}", ident),
        }
    }

    pub(in crate::codegen) fn store_struct_type(&mut self, ident: &'ctx str, struct_type: StructType<'ctx>) {
        self.types_table.insert(ident, struct_type);
    }

    pub(in crate::codegen) fn get_struct_type(&self, ident: &'ctx str) -> StructType<'ctx> {
        match self.types_table.get(ident) {
            Some(ty) => *ty,
            None => unreachable!("struct_type: {}", ident),
        }
    }
}

impl<'ctx, 'src> Codegen<'ctx, 'src>
where
    'src: 'ctx
{
    pub(crate) fn codegen(&mut self) -> String {
        let global_scope: &'ctx MIRScope = self.mir.scopes.get(&0_usize).unwrap();
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
        scope: &'ctx MIRScope<'src>,
        parent_function_value: Option<FunctionValue>
    ) {

        for (_, scope_struct) in &scope.structs {
            self.codegen_struct(scope_struct);
        }

        let funs: Vec<&MIRFunc> = scope.funs.values().collect();

        let functions: Vec<Option<FunctionValue>> = funs
            .iter()
            .map(|proto| match proto.return_type {
                MIRType::Void => self.codegen_void_function_definition(proto),
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
                    for instruction in &scope.instrs {
                        self.codegen_instruction(instruction, scope, parent_function_value.clone());
                    }
                }
            }
        }
    }

    pub(crate) fn codegen_instruction(
        &mut self,
        instruction: &'ctx MIRInstruction<'src>,
        scope: &'ctx MIRScope<'ctx>,
        function_value: FunctionValue,
    ) {
        match instruction {
            MIRInstruction::Return { expr, .. } => {
                if let Some(expr) = expr {
                    let value = self.codegen_expr(expr, scope);
                    self.builder.build_return(Some(&value)).unwrap();
                } else {
                    self.builder.build_return(None).unwrap();
                }
            }
            MIRInstruction::Break { .. } => todo!(),
            MIRInstruction::Assignment {
                ident,
                ty,
                expr,
                span,
            } => {
                let variable_type = self.codegen_type(&ty);
                match variable_type {
                    BasicTypeEnum::ArrayType(_) => {
                        self.codegen_array_assignment(ident, variable_type, expr, scope);
                    }
                    BasicTypeEnum::IntType(_)
                    | BasicTypeEnum::FloatType(_)
                    | BasicTypeEnum::PointerType(_) => {
                        let variable = self.builder.build_alloca(variable_type, ident).unwrap();

                        let value = self.codegen_expr(expr, scope);

                        self.builder.build_store(variable, value).unwrap();

                        self.store_variable(ident, variable, variable_type);
                    },
                    BasicTypeEnum::StructType(_) => {
                        self.codegen_struct_assignment(ident, variable_type, expr, scope);
                    },
                    BasicTypeEnum::VectorType(_) => todo!(),
                }
            }
            MIRInstruction::Assign { lhs, rhs, .. } => {
                match lhs { // TODO array redeclaration
                    MIRExpr::MemLookup { ident, indices, span, .. } => {
                        let rhs_value = self.codegen_expr(rhs, scope);
                        self.codegen_set_array_element(
                            ident,
                            indices,
                            rhs_value,
                            *span,
                            scope)
                            .unwrap();
                    }
                    MIRExpr::StructInit { ident, struct_id, fields, .. } => {
                        let variable = self.codegen_identifier_pointer(ident, scope);

                        let value = self.codegen_expr(rhs, scope);

                        self.builder.build_store(variable, value).unwrap();
                    }
                    _ => unreachable!()
                }
            }
            MIRInstruction::FuncCall(func_call) => {
                self.codegen_func_call(func_call, scope);
            },
            MIRInstruction::GoToBlock { block_id } => self.codegen_scope_by_id(block_id, Some(function_value)),
            MIRInstruction::GoToIfCond { cond, then_block_id, else_block_id } => {
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
            },
            MIRInstruction::GoToWhile { .. } => unimplemented!(),
        }
    }

    fn codegen_volatile_mark(&self) {
        let alloca = self.builder.build_alloca(self.context.i64_type(), "block_mark").unwrap();
        let const_value = self.context.i64_type().const_int(0, false);
        self.builder.build_store(alloca, const_value).unwrap().set_volatile(true).unwrap()
    }

    pub(crate) fn codegen_expr(&self, expr: &'ctx MIRExpr<'src>, scope: &'ctx MIRScope<'ctx>) -> BasicValueEnum<'ctx>  {
        match expr {
            MIRExpr::Identifier { ident, .. } => {
                self.codegen_identifier_value(ident, scope)
            },
            MIRExpr::FuncCall(func_call) => {
                self.codegen_func_call(func_call, scope).unwrap()
            },
            MIRExpr::ArrayDecl { list, .. } => {
                self.codegen_array_declaration(list, scope)
            },
            MIRExpr::MemLookup { ident, indices, span, .. } => {
                self.codegen_get_array_element(ident, indices, *span, scope).unwrap()
            },
            MIRExpr::StructFieldCall { ident, field, .. } => {
                self.codegen_get_struct_field(ident, field, scope)
            },
            MIRExpr::StructInit { ident, fields, .. } => {
                self.codegen_struct_init(ident, fields, scope)
            },
            MIRExpr::CastType { .. } => todo!(),
            MIRExpr::Dereference { inner, .. } => {
                self.codegen_get_dereferenced(inner, scope)
            },
            MIRExpr::Reference { inner, .. } => {
                self.codegen_get_referenced(inner, scope)
            },
            MIRExpr::LogicalExpr(logical_expr) => {
                self.codegen_logical_expr(logical_expr, scope)
            },
            MIRExpr::MathExpr(math_expr) => {
                self.codegen_math_expr(math_expr, scope)
            },
            MIRExpr::Literal(literal) => {
                self.codegen_literal(literal)
            },
        }
    }

    pub(crate) fn codegen_identifier_value(
        &self,
        ident: &'ctx str,
        scope: &'ctx MIRScope<'ctx>
    ) -> BasicValueEnum<'ctx> {
        let variable_data = self.get_variable(ident);
        match variable_data {
            VariableData::Primitive { pointer, primitive_type } => {
                self.builder.build_load(*primitive_type, *pointer, ident).unwrap()
            }
            VariableData::Array { pointer, array_type } => {
                self.builder.build_load(*array_type, *pointer, ident).unwrap()
            }
            VariableData::Struct { .. } => {
                todo!()
            }
        }
    }

    pub(crate) fn codegen_identifier_pointer(&self, ident: &'ctx str, scope: &'ctx MIRScope<'ctx>) -> PointerValue<'ctx> {
        let variable_data = self.get_variable(ident);
        match variable_data {
            VariableData::Primitive { pointer, .. } => {
                *pointer
            }
            VariableData::Array { pointer, .. } => {
                *pointer
            }
            VariableData::Struct { pointer, .. } => {
                *pointer
            }
        }
    }

    pub(crate) fn codegen_literal(
        &self,
        literal: &'ctx MIRLiteral<'src>
    ) -> BasicValueEnum<'ctx> {
        match literal {
            MIRLiteral::Int { lit, .. } => self.context.i64_type().const_int(*lit as u64, false).into(),
            MIRLiteral::Float { lit, .. } => self.context.f64_type().const_float(*lit as f64).into(),
            MIRLiteral::Str { lit, .. } => {
                let lit2 = lit.replace("\\n", "\n");
                let global_string = self.context.const_string(lit2.as_bytes(), true);
                let value = self.module.add_global(global_string.get_type(), None, "global_str");
                value.set_initializer(&global_string);
                value.as_basic_value_enum()
            },
            MIRLiteral::Char { lit, .. } => self.context.i8_type().const_int(*lit as u64, false).into(),
            MIRLiteral::Bool { lit, ..} => self.context.bool_type().const_int(*lit as u64, false).into(),
            MIRLiteral::NullRef { .. } => todo!(),
        }
    }

    pub(crate) fn codegen_math_expr(&self, math_expr: &'ctx MIRMathExpr<'src>, scope: &'ctx MIRScope<'ctx>) -> BasicValueEnum<'ctx> {
        match math_expr {
            MIRMathExpr::Additive { left, right, op, .. } => {
                self.codegen_math_add_expr(left, right, op, scope)
            },
            MIRMathExpr::Multiplicative { left, right, op, .. } => {
                self.codegen_math_mul_expr(left, right, op, scope)
            },
            MIRMathExpr::Power { base, exp, .. } => {
                self.codegen_math_power_expr(base, exp, scope)
            },
            MIRMathExpr::Primary(primary) => {
                self.codegen_expr(primary, scope)
            }
        }
    }

    pub(crate) fn codegen_type(&self, type_ast: &'ctx MIRType) -> BasicTypeEnum<'ctx> {
        match type_ast {
            MIRType::I64 => self.context.i64_type().into(),
            MIRType::F64 => self.context.f64_type().into(),
            MIRType::Str => self.context.ptr_type(AddressSpace::default()).into(),
            MIRType::Char => self.context.i8_type().into(),
            MIRType::Bool => self.context.bool_type().into(),
            MIRType::Void => unreachable!(),
            MIRType::StructType(identifier) => {
                self.get_struct_type(identifier.ident.as_str()).into()
            },
            MIRType::Array(ty, size) => {
                self.codegen_type(ty).array_type(*size as u32).into()
            },
            MIRType::Vec(ty) => {
                match self.codegen_type(ty) {
                    BasicTypeEnum::ArrayType(_) => unreachable!(),
                    BasicTypeEnum::FloatType(ty) => todo!(),
                    BasicTypeEnum::IntType(ty) => todo!(),
                    BasicTypeEnum::PointerType(ty) => todo!(),
                    BasicTypeEnum::StructType(_) => unreachable!(),
                    BasicTypeEnum::VectorType(_) => unreachable!(),
                }
            },
            MIRType::Ref(ty) => self.context.ptr_type(AddressSpace::default()).into(),
            MIRType::Any => unreachable!(),
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
        types_table: HashMap::new(),
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