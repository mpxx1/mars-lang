use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::{FileType, InitializationConfig, Target, TargetMachine};
use inkwell::types::{BasicType, BasicTypeEnum, StructType};
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue};
use inkwell::AddressSpace;
use lir::{LIRExpr, LIRFunc, LIRInstruction, LIRLiteral, LIRMathExpr, LIRType, Lir};
use std::collections::HashMap;
use std::fmt::format;
use std::{env, fs};
use std::path::{Path, PathBuf};
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
    Vector {
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
            LIRType::Vec(_) => {
                self.var_table.insert(ident, VariableData::Vector {
                    pointer: pointer_value,
                    lir_type,
                    llvm_type,
                });
            },
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
                let llvm_type = self.codegen_type(ty.clone());
                match ty {
                    LIRType::I64
                    | LIRType::F64
                    | LIRType::Str
                    | LIRType::Char
                    | LIRType::Bool
                    | LIRType::Ref(_) => {
                        let variable = self.builder.build_alloca(llvm_type, ident).unwrap();

                        let value = self.codegen_expr(expr);

                        self.builder.build_store(variable, value).unwrap();

                        self.store_variable(ident, variable, ty.clone(), llvm_type);
                    }
                    LIRType::StructType(_) => {
                        self.codegen_struct_assignment(ident, ty.clone(), llvm_type, expr);
                    }
                    LIRType::Array(_, _) => {
                        self.codegen_array_assignment(ident, ty.clone(), llvm_type, expr);
                    }
                    LIRType::Vec(_) => {
                        self.codegen_vec_assignment(ident, ty.clone(), llvm_type, expr);
                    }
                    LIRType::Any => unreachable!(),
                    LIRType::Void => unreachable!(),
                }
            }
            LIRInstruction::Assign { lhs, rhs, .. } => {
                match lhs { // TODO array redeclaration
                    LIRExpr::MemLookup { base, indices, .. } => {
                        let variable_data = self.get_variable(base);
                        match variable_data {
                            VariableData::Array { .. } => {
                                let rhs_value = self.codegen_expr(rhs);
                                self.codegen_set_array_element(base, indices, rhs_value).unwrap();
                            }
                            VariableData::Vector { lir_type, .. } => {
                                let rhs_value = self.codegen_expr(rhs);
                                self.codegen_set_vec_element(base, indices, rhs_value);
                            }
                            _ => unreachable!(),
                        }
                        
                    }
                    LIRExpr::StructInit { struct_name, fields } => { // TODO
                        let variable = self.codegen_identifier_pointer(struct_name);

                        let value = self.codegen_expr(rhs);

                        self.builder.build_store(variable, value).unwrap();
                    }
                    LIRExpr::Identifier(ident) => {
                        let variable = self.codegen_identifier_pointer(ident);

                        let value = self.codegen_expr(rhs);

                        self.builder.build_store(variable, value).unwrap();
                    },
                    _ => unreachable!()
                }
            }
            LIRInstruction::FuncCall(func_call) => {
                self.codegen_func_call(func_call);
            },
            LIRInstruction::GoToBlock { block_id } => {
                self.codegen_block_by_id(block_id, function_value);
            },
            LIRInstruction::GoToIfCond { cond, then_block, else_block } => {
                self.codegen_conditional(cond, *then_block, *else_block, function_value);
            },
            LIRInstruction::GoToWhile { cond, loop_block } => {
                self.codegen_while(cond, *loop_block, function_value);
            },
        }
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
                let variable_data = self.get_variable(base);
                match variable_data {
                    VariableData::Primitive { .. } => unreachable!(),
                    VariableData::Struct { .. } => unreachable!(),
                    VariableData::Array { .. } => {
                        self.codegen_get_array_element(base, indices).unwrap()
                    }
                    VariableData::Vector { .. } => {
                        self.codegen_get_vec_element(base, variable_data, indices)
                    }
                }
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
            VariableData::Vector { pointer, lir_type, llvm_type } => {
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
            VariableData::Struct { pointer, lir_type, llvm_type } => {
                self.builder.build_load(*llvm_type, *pointer, ident).unwrap()
            },
            VariableData::Vector { pointer, lir_type, llvm_type } => {
                self.builder.build_load(*llvm_type, *pointer, ident).unwrap()
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
            },
            VariableData::Vector { pointer, .. } => {
                *pointer
            }
        }
    }

    pub(crate) fn codegen_literal(&self, literal: &'ctx LIRLiteral) -> BasicValueEnum<'ctx> {
        match literal {
            LIRLiteral::Int(value) => {
                self.context.i64_type().const_int(*value as u64, true).into()
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
                self.context.i8_type().const_int(*value as u64, true).into()
            },
            LIRLiteral::Bool(value) => {
                self.context.bool_type().const_int(*value as u64, true).into()
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
                    BasicTypeEnum::FloatType(ty) => {
                        self.context.ptr_type(AddressSpace::default()).as_basic_type_enum()
                    },
                    BasicTypeEnum::IntType(ty) => {
                        self.context.ptr_type(AddressSpace::default()).as_basic_type_enum()
                    },
                    BasicTypeEnum::PointerType(ty) => {
                        self.context.ptr_type(AddressSpace::default()).as_basic_type_enum()
                    },
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

pub fn codegen<'src>(lir: &'src Lir, output: &str) -> String {

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

    let output_filename = PathBuf::from(output);
    let object_filename = to_object_file_path(output);

    codegen.codegen_bytecode(&object_filename).unwrap();
    
    let home_path = PathBuf::from(env::var("HOME").unwrap());

    let status = Command::new("clang")
        .arg(home_path.join("mars/std/libmarsio.dylib"))
        .arg(home_path.join("mars/std/libmarsvector.dylib"))
        .arg(object_filename.to_str().unwrap())
        .args(["-o", output_filename.to_str().unwrap()])
        .status()
        .expect("Failed to invoke C compiler and build executable file");
    
    fs::remove_file(object_filename).expect("Cannot delete object file");

    if !status.success() {
        panic!("Status is not success");
    }

    result
}

fn to_object_file_path(file_path: &str) -> PathBuf {
    let path = Path::new(file_path);
    let mut new_path = path.to_path_buf();
    new_path.set_extension("o");
    
    new_path
}