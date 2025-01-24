use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::{FileType, InitializationConfig, Target, TargetMachine};
use inkwell::types::{BasicType, BasicTypeEnum, StructType};
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue};
use inkwell::AddressSpace;
use lir::{LIRExpr, LIRFunc, LIRInstruction, LIRLiteral, LIRMathExpr, LIRType, Lir};
use std::collections::HashMap;
use std::path::PathBuf;
use std::process::Command;

#[derive(Debug)]
pub(super) enum VariableData<'ctx> {
    Primitive {
        lir_type: LIRType,
        llvm_type: BasicTypeEnum<'ctx>,
        pointer: PointerValue<'ctx>,
    },
    Array {
        lir_type: LIRType,
        llvm_type: BasicTypeEnum<'ctx>,
        pointer: PointerValue<'ctx>,
    },
    Struct {
        lir_type: LIRType,
        llvm_type: BasicTypeEnum<'ctx>,
        pointer: PointerValue<'ctx>,
    }
}

pub struct Codegen<'ctx, 'src> {
    pub(in crate::codegen) context: &'ctx Context,
    pub(in crate::codegen) module: Module<'ctx>,
    pub(in crate::codegen) builder: Builder<'ctx>,
    pub(in crate::codegen) lir: &'ctx Lir<'src>,
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
        lir_type: LIRType,
        llvm_type: BasicTypeEnum<'ctx>,
    ) {
        match &lir_type {
            LIRType::Array(_, _) => {
                self.var_table.insert(ident, VariableData::Array {
                    pointer: pointer_value,
                    lir_type,
                    llvm_type,
                });
            }
            LIRType::I64 => {
                self.var_table.insert(ident, VariableData::Primitive {
                    pointer: pointer_value,
                    lir_type,
                    llvm_type,
                });
            },
            LIRType::F64 => {
                self.var_table.insert(ident, VariableData::Primitive {
                    pointer: pointer_value,
                    lir_type,
                    llvm_type,
                });
            },
            LIRType::Str => {
                self.var_table.insert(ident, VariableData::Primitive {
                    pointer: pointer_value,
                    lir_type,
                    llvm_type,
                });
            }
            LIRType::Char => {
                self.var_table.insert(ident, VariableData::Primitive {
                    pointer: pointer_value,
                    lir_type,
                    llvm_type,
                });
            }
            LIRType::Bool => {
                self.var_table.insert(ident, VariableData::Primitive {
                    pointer: pointer_value,
                    lir_type,
                    llvm_type,
                });
            }
            LIRType::Ref(_) => {
                self.var_table.insert(ident, VariableData::Primitive {
                    pointer: pointer_value,
                    lir_type,
                    llvm_type,
                });
            },
            LIRType::StructType(_) => {
                self.var_table.insert(ident, VariableData::Struct {
                    pointer: pointer_value,
                    lir_type,
                    llvm_type,
                });
            },
            LIRType::Vec(_) => todo!(),
            LIRType::Void => unreachable!(),
            LIRType::Any => unreachable!(),
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

    pub(in crate::codegen) fn get_struct_type(&self, ident: String) -> StructType<'ctx> {
        match self.types_table.get(ident.as_str()) {
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
        for (_, scope_struct) in &self.lir.structs {
            self.codegen_struct(scope_struct);
        }

        let functions: Vec<&LIRFunc> = self.lir.functions.values().collect();

        let functions_definition: Vec<Option<FunctionValue>> = functions
            .iter()
            .map(|proto| match proto.return_type {
                LIRType::Void => self.codegen_void_function_definition(proto),
                _ => self.codegen_function_definition(proto),
            })
            .collect();

        for (proto, function) in functions.iter()
            .zip(functions_definition.iter())
        {
            if let Some(function) = function {
                self.codegen_function_entry(proto, *function);
            }
        }

        self.module.print_to_string().to_string()
    }

    pub(crate) fn codegen_block_by_id(
        &mut self,
        id: &usize,
        parent_function_value: FunctionValue<'ctx>)
    {
        let scope = &self.lir.blocks[id];
        self.codegen_block(scope, parent_function_value);
    }

    pub(crate) fn codegen_block(
        &mut self,
        block: &'ctx Vec<LIRInstruction>,
        parent_function_value: FunctionValue<'ctx>,
    ) {
        for instruction in block {
            self.codegen_instruction(instruction, parent_function_value.clone());
        }
    }

    pub(crate) fn codegen_instruction(
        &mut self,
        instruction: &'ctx LIRInstruction,
        function_value: FunctionValue<'ctx>,
    ) {
        match instruction {
            LIRInstruction::Return { expr, .. } => {
                if let Some(expr) = expr {
                    let value = self.codegen_expr(expr);
                    self.builder.build_return(Some(&value)).unwrap();
                } else {
                    self.builder.build_return(None).unwrap();
                }
            }
            LIRInstruction::Break { .. } => todo!(),
            LIRInstruction::Assignment {
                ident,
                ty,
                expr,
            } => {
                let variable_type = self.codegen_type(ty.clone());
                match variable_type {
                    BasicTypeEnum::ArrayType(_) => {
                        self.codegen_array_assignment(ident, ty.clone(), variable_type, expr);
                    }
                    BasicTypeEnum::IntType(_)
                    | BasicTypeEnum::FloatType(_)
                    | BasicTypeEnum::PointerType(_) => {
                        let variable = self.builder.build_alloca(variable_type, ident).unwrap();

                        let value = self.codegen_expr(expr);

                        self.builder.build_store(variable, value).unwrap();

                        self.store_variable(ident, variable, ty.clone(), variable_type);
                    },
                    BasicTypeEnum::StructType(_) => {
                        self.codegen_struct_assignment(ident, ty.clone(), variable_type, expr);
                    },
                    BasicTypeEnum::VectorType(_) => todo!(),
                }
            }
            LIRInstruction::Assign { lhs, rhs, .. } => {
                match lhs { // TODO array redeclaration
                    LIRExpr::MemLookup { base, indices, .. } => {
                        let rhs_value = self.codegen_expr(rhs);
                        self.codegen_set_array_element(base, indices, rhs_value)
                            .unwrap();
                    }
                    LIRExpr::StructInit { struct_name, fields } => {
                        let variable = self.codegen_identifier_pointer(struct_name);

                        let value = self.codegen_expr(rhs);

                        self.builder.build_store(variable, value).unwrap();
                    }
                    _ => unreachable!()
                }
            }
            LIRInstruction::FuncCall(func_call) => {
                self.codegen_func_call(func_call);
            },
            LIRInstruction::GoToBlock { block_id } => self.codegen_block_by_id(block_id, function_value),
            LIRInstruction::GoToIfCond { cond, then_block, else_block } => {
                let cond_value = self.codegen_expr(cond).into_int_value();

                let then_bb = self.context.append_basic_block(function_value, "then");
                let else_bb = if else_block.is_some() {
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
                self.codegen_block_by_id(then_block, function_value);
                self.builder.build_unconditional_branch(merge_bb).unwrap();

                if let Some(else_bb) = else_bb {
                    self.builder.position_at_end(else_bb);
                    self.codegen_volatile_mark();
                    self.codegen_block_by_id(&else_block.unwrap(), function_value);
                    self.builder.build_unconditional_branch(merge_bb).unwrap();
                }

                self.builder.position_at_end(merge_bb);
            },
            LIRInstruction::GoToWhile { .. } => unimplemented!(),
        }
    }

    fn codegen_volatile_mark(&self) {
        let alloca = self.builder.build_alloca(self.context.i64_type(), "block_mark").unwrap();
        let const_value = self.context.i64_type().const_int(0, false);
        self.builder.build_store(alloca, const_value).unwrap().set_volatile(true).unwrap()
    }

    pub(crate) fn codegen_expr(&self, expr: &'ctx LIRExpr) -> BasicValueEnum<'ctx>  {
        match expr {
            LIRExpr::Identifier(ident) => {
                self.codegen_identifier_value(ident)
            },
            LIRExpr::FuncCall(func_call) => {
                self.codegen_func_call(func_call).unwrap()
            },
            LIRExpr::Array(list) => {
                self.codegen_array_declaration(list)
            },
            LIRExpr::MemLookup { base, indices, .. } => {
                self.codegen_get_array_element(base, indices).unwrap()
            },
            LIRExpr::StructFieldCall { struct_name, field_index, .. } => {
                self.codegen_get_struct_field(struct_name, *field_index)
            },
            LIRExpr::StructInit { struct_name, fields, .. } => {
                self.codegen_struct_init(struct_name, fields)
            },
            LIRExpr::Cast { .. } => todo!(),
            LIRExpr::Dereference { refer, .. } => {
                self.codegen_get_dereferenced(refer)
            },
            LIRExpr::Reference(expr) => {
                self.codegen_get_referenced(expr)
            },
            LIRExpr::Logical(logical_expr) => {
                self.codegen_logical_expr(logical_expr)
            },
            LIRExpr::Math(math_expr) => {
                self.codegen_math_expr(math_expr)
            },
            LIRExpr::Literal(literal) => {
                self.codegen_literal(literal)
            },
        }
    }

    pub(crate) fn codegen_identifier_value_with_types(
        &self,
        ident: &'ctx str
    ) -> (BasicValueEnum<'ctx>, LIRType, BasicTypeEnum<'ctx>)
    {
        let variable_data = self.get_variable(ident);
        match variable_data {
            VariableData::Primitive { pointer, lir_type, llvm_type } => {
                (
                    self.builder.build_load(*llvm_type, *pointer, ident).unwrap(),
                    lir_type.clone(),
                    *llvm_type,
                )
            }
            VariableData::Struct { pointer, lir_type, llvm_type } => {
                (
                    self.builder.build_load(*llvm_type, *pointer, ident).unwrap(),
                    lir_type.clone(),
                    *llvm_type,
                )
            }
            VariableData::Array { pointer, lir_type, llvm_type } => {
                (
                    self.builder.build_load(*llvm_type, *pointer, ident).unwrap(),
                    lir_type.clone(),
                    *llvm_type,
                )
            }
        }
    }

    pub(crate) fn codegen_identifier_value(&self, ident: &'ctx str) -> BasicValueEnum<'ctx> {
        let variable_data = self.get_variable(ident);
        match variable_data {
            VariableData::Primitive { pointer, lir_type, llvm_type } => {
                self.builder.build_load(*llvm_type, *pointer, ident).unwrap()
            }
            VariableData::Array { pointer, lir_type, llvm_type } => {
                self.builder.build_load(*llvm_type, *pointer, ident).unwrap()
            }
            VariableData::Struct { .. } => {
                todo!()
            }
        }
    }

    pub(crate) fn codegen_identifier_pointer(&self, ident: &'ctx str) -> PointerValue<'ctx> {
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

    pub(crate) fn codegen_literal(&self, literal: &'ctx LIRLiteral) -> BasicValueEnum<'ctx> {
        match literal {
            LIRLiteral::Int(value) => {
                self.context.i64_type().const_int(*value as u64, false).into()
            },
            LIRLiteral::Float(value) => {
                self.context.f64_type().const_float(*value).into()
            },
            LIRLiteral::Str(value) => {
                let raw_value = value.replace("\\n", "\n");
                let global_string = self.context.const_string(raw_value.as_bytes(), true);
                let value = self.module.add_global(global_string.get_type(), None, "global_str");
                value.set_initializer(&global_string);
                value.as_basic_value_enum()
            },
            LIRLiteral::Char(value) => {
                self.context.i8_type().const_int(*value as u64, false).into()
            },
            LIRLiteral::Bool(value) => {
                self.context.bool_type().const_int(*value as u64, false).into()
            },
            LIRLiteral::NullRef => {
                self.context.ptr_type(AddressSpace::default()).const_null().into()
            },
        }
    }

    pub(crate) fn codegen_math_expr(&self, math_expr: &'ctx LIRMathExpr) -> BasicValueEnum<'ctx> {
        match math_expr {
            LIRMathExpr::Additive { left, right, op, .. } => {
                self.codegen_math_add_expr(left, right, op)
            },
            LIRMathExpr::Multiplicative { left, right, op, .. } => {
                self.codegen_math_mul_expr(left, right, op)
            },
            LIRMathExpr::Power { base, exp, .. } => {
                self.codegen_math_power_expr(base, exp)
            },
            LIRMathExpr::Primary(primary) => {
                self.codegen_expr(primary)
            }
        }
    }

    pub(crate) fn codegen_type(&self, type_ast: LIRType) -> BasicTypeEnum<'ctx> {
        match type_ast {
            LIRType::I64 => self.context.i64_type().into(),
            LIRType::F64 => self.context.f64_type().into(),
            LIRType::Str => self.context.ptr_type(AddressSpace::default()).into(),
            LIRType::Char => self.context.i8_type().into(),
            LIRType::Bool => self.context.bool_type().into(),
            LIRType::Void => unreachable!(),
            LIRType::StructType(identifier) => {
                self.get_struct_type(identifier).into()
            },
            LIRType::Array(ty, size) => {
                self.codegen_type(*ty).array_type(size as u32).into()
            },
            LIRType::Vec(ty) => {
                match self.codegen_type(*ty) {
                    BasicTypeEnum::ArrayType(_) => unreachable!(),
                    BasicTypeEnum::FloatType(ty) => todo!(),
                    BasicTypeEnum::IntType(ty) => todo!(),
                    BasicTypeEnum::PointerType(ty) => todo!(),
                    BasicTypeEnum::StructType(_) => unreachable!(),
                    BasicTypeEnum::VectorType(_) => unreachable!(),
                }
            },
            LIRType::Ref(ty) => self.context.ptr_type(AddressSpace::default()).into(),
            LIRType::Any => unreachable!(),
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

pub fn codegen<'src>(lir: &'src Lir) -> String {

    let context = Context::create();
    let module = context.create_module("test_module");
    let mut codegen = Codegen::<'_, 'src> {
        context: &context,
        module,
        builder: context.create_builder(),
        lir,
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