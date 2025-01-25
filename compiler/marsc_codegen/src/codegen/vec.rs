use inkwell::AddressSpace;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue};
use err::CompileError;
use lir::{LIRExpr, LIRType};
use crate::codegen::codegen::{Codegen, VariableData};

impl<'ctx, 'src> Codegen<'ctx, 'src>
where
    'src: 'ctx
{
    pub(in crate::codegen) fn codegen_vec_declaration(
        &self,
        item_type: LIRType,
        list: &'ctx Vec<LIRExpr>,
    ) -> BasicValueEnum<'ctx> {
        let elements_values: Vec<BasicValueEnum<'ctx>> = list.iter()
            .map(|element_expr| {
                self.codegen_expr(element_expr)
            })
            .collect();

        let element_type = self.codegen_type(item_type);
        let capacity_value = self.context.i64_type().const_int(list.len() as u64, true);
        let call_site = self.builder.build_call(
            self.get_vector_new_function(element_type),
            &[capacity_value.into()],
            "vector_new_call"
        ).unwrap();

        let vector_pointer = call_site
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_pointer_value();
        
        elements_values.iter()
            .for_each(|value| self.codegen_push_vec_element(
                "init_element", vector_pointer, *value));
        
        vector_pointer.as_basic_value_enum()
    }

    pub(super) fn codegen_push_vec_element(
        &self,
        ident: &'ctx str,
        pointer: PointerValue,
        value: BasicValueEnum<'ctx>,
    ) {
        let vector_variable = self.builder.build_alloca(pointer.get_type(), "vec_init").unwrap();
        let _ = self.builder.build_store(vector_variable, pointer).unwrap();
        
        let load = self.builder.build_load(pointer.get_type(), vector_variable, "vec_init_item")
            .unwrap()
            .into_pointer_value();
        
        let function_value = self.get_vector_push_function(value.get_type());
        let _ = self.builder.build_call(
            function_value,
            &[
                load.as_basic_value_enum().into(),
                value.as_basic_value_enum().into(),
            ],
            "vector_push_call"
        ).unwrap();
    }

    pub(in crate::codegen) fn codegen_vec_assignment(
        &mut self,
        ident: &'ctx str,
        lir_type: LIRType,
        llvm_type: BasicTypeEnum<'ctx>,
        expr: &'ctx LIRExpr,
    ) {
        let value = match expr { 
            LIRExpr::Array(list) => {
                match &lir_type { 
                    LIRType::Vec(inner) => {
                        self.codegen_vec_declaration(*inner.clone(), list)
                    }
                    _ => unreachable!(),
                }
            }
            LIRExpr::Dereference { .. } => {
                self.codegen_expr(expr)
            }
            LIRExpr::Identifier(ident) => {
                self.codegen_identifier_value(ident)
            }
            LIRExpr::FuncCall(func_call) => {
                self.codegen_func_call(func_call).unwrap()
            }
            _ => unreachable!("{:#?}", expr),
        };

        match value {
            BasicValueEnum::PointerValue(pointer_value) => {
                let pointer = self.builder.build_alloca(value.get_type(), ident).unwrap();
                self.builder.build_store(pointer, pointer_value).unwrap();
                self.store_variable(ident, pointer, lir_type, llvm_type)
            },
            BasicValueEnum::StructValue(value) => {
                let pointer = self.builder.build_alloca(value.get_type(), ident).unwrap();
                self.builder.build_store(pointer, value).unwrap();
                self.store_variable(ident, pointer, lir_type, llvm_type)
            }
            _ => unreachable!("{:#?}", value),
        }
    }

    pub(super) fn codegen_set_vec_element(
        &self,
        ident: &'ctx str,
        indices: &'ctx Vec<LIRExpr>,
        value: BasicValueEnum<'ctx>,
    ) {
        let vector_pointer = self.codegen_identifier_value(ident).into_pointer_value();
        let function_value = self.get_vector_set_function(value.get_type());
        let index_value = self.codegen_expr(indices.first().unwrap()).into_int_value();
        let _ = self.builder.build_call(
            function_value,
            &[
                vector_pointer.as_basic_value_enum().into(),
                index_value.as_basic_value_enum().into(),
                value.as_basic_value_enum().into(),
            ],
            "vector_set_call"
        ).unwrap();
    }

    pub(super) fn codegen_get_vec_element(
        &self,
        ident: &'ctx str,
        variable_data: &VariableData<'ctx>,
        indices: &'ctx Vec<LIRExpr>,
    ) -> BasicValueEnum<'ctx> {
        match variable_data {
            VariableData::Vector { pointer, llvm_type, lir_type } => {
                match lir_type {
                    LIRType::Vec(inner_type) => {
                        let vector_pointer = self.codegen_identifier_value(ident).into_pointer_value();
                        let inner_llvm_type = self.codegen_type(*inner_type.clone());
                        let function_value = self.get_vector_get_function(inner_llvm_type);
                        let index_value = self.codegen_expr(indices.first().unwrap()).into_int_value();
                        let call_site = self.builder.build_call(
                            function_value,
                            &[
                                vector_pointer.as_basic_value_enum().into(),
                                index_value.as_basic_value_enum().into(),
                            ],
                            "vector_get_call"
                        ).unwrap();

                        call_site.try_as_basic_value()
                            .left()
                            .unwrap()
                    }
                    _ => unreachable!(),
                }
            }
            _ => unreachable!()
        }
    }
    
    fn get_vector_new_function(&self, ty: BasicTypeEnum) -> FunctionValue<'ctx> {
        let fn_name = format!("vector_new_{}", Self::map_type_to_postfix(ty));
        self.module.get_function(fn_name.as_str()).unwrap()
    }

    fn get_vector_get_function(&self, ty: BasicTypeEnum) -> FunctionValue<'ctx> {
        let fn_name = format!("vector_get_{}", Self::map_type_to_postfix(ty));
        self.module.get_function(fn_name.as_str()).unwrap()
    }

    fn get_vector_set_function(&self, ty: BasicTypeEnum) -> FunctionValue<'ctx> {
        let fn_name = format!("vector_set_{}", Self::map_type_to_postfix(ty));
        self.module.get_function(fn_name.as_str()).unwrap()
    }

    fn get_vector_push_function(&self, ty: BasicTypeEnum) -> FunctionValue<'ctx> {
        let fn_name = format!("vector_push_{}", Self::map_type_to_postfix(ty));
        self.module.get_function(fn_name.as_str()).unwrap()
    }

    fn get_vector_pop_function(&self, ty: BasicTypeEnum) -> FunctionValue<'ctx> {
        let fn_name = format!("vector_pop_{}", Self::map_type_to_postfix(ty));
        self.module.get_function(fn_name.as_str()).unwrap()
    }
    
    fn map_type_to_postfix(ty: BasicTypeEnum) -> &str {
        match ty {
            BasicTypeEnum::ArrayType(_) => unreachable!(),
            BasicTypeEnum::FloatType(_) => "f64",
            BasicTypeEnum::IntType(_) => "i64",
            BasicTypeEnum::PointerType(_) => "str",
            BasicTypeEnum::StructType(_) => unreachable!(),
            BasicTypeEnum::VectorType(_) => unreachable!(),
        }
    }
}