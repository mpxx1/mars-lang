use crate::codegen::codegen::{Codegen, VariableData};
use err::CompileError;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{BasicValue, BasicValueEnum};
use lir::{LIRExpr, LIRType};

impl<'ctx, 'src> Codegen<'ctx, 'src>
where
    'src: 'ctx
{
    pub(super) fn codegen_array_declaration(
        &self,
        list: &'ctx Vec<LIRExpr>,
    ) -> BasicValueEnum<'ctx> {
        let elements_values: Vec<BasicValueEnum<'ctx>> = list.iter()
            .map(|element_expr| {
                self.codegen_expr(element_expr)
            })
            .collect();
        
        let element_type = elements_values.first().unwrap().get_type();
        let array_type = element_type.array_type(list.len() as u32);
        let array_alloca = self.builder.build_alloca(array_type, "array").unwrap();

        for (i, element_value) in elements_values.iter().enumerate() {
            let index = self.context.i32_type().const_int(i as u64, true);
            let element_ptr = unsafe {
                self.builder.build_gep(array_type, array_alloca, &[index], "element_ptr").unwrap()
            };
            self.builder.build_store(element_ptr, *element_value).unwrap();
        };

        array_alloca.as_basic_value_enum()
    }

    pub(super) fn codegen_get_array_element(
        &self,
        ident: &'ctx str,
        indices: &'ctx Vec<LIRExpr>
    ) -> Result<BasicValueEnum<'ctx>, CompileError> {
        let array_data = self.get_variable(ident);

        if let VariableData::Array {
            pointer,
            lir_type,
            llvm_type,
        } = array_data {
            let array_type = llvm_type.into_array_type();

            let mut index_value = match self.codegen_expr(&indices[0]) {
                BasicValueEnum::IntValue(int_value) => int_value,
                _ => unreachable!()
            };

            let index = index_value.get_sign_extended_constant().unwrap();

            if index < 0 {
                index_value = self.context.i32_type().const_int(
                    Self::convert_negative_array_index(
                        index_value.get_sign_extended_constant().unwrap(),
                        array_type.len()),
                    true,
                );
            }

            if index_value.get_sign_extended_constant().unwrap() as u32 >= array_type.len() {
                panic!("Index out of array len: {}", array_type.len())
            }

            let element_ptr = unsafe {
                self.builder.build_gep(
                    array_type,
                    *pointer,
                    &[index_value],
                    "array_element_ptr"
                ).unwrap()
            };

            Ok(
                self.builder
                    .build_load(array_type.get_element_type(), element_ptr, "array_element_value")
                    .unwrap()
            )
        } else {
            unreachable!();
        }
    }

    pub(super) fn codegen_set_array_element(
        &self,
        ident: &'ctx str,
        indices: &'ctx Vec<LIRExpr>,
        value: BasicValueEnum<'ctx>,
    ) -> Result<(), CompileError> {
        let array_data = self.get_variable(ident);

        if let VariableData::Array {
            pointer,
            lir_type,
            llvm_type,
        } = array_data {
            let array_type = llvm_type.into_array_type();
            
            let mut index_value = match self.codegen_expr(&indices[0]) {
                BasicValueEnum::IntValue(int_value) => int_value,
                _ => unreachable!()
            };

            let index = index_value.get_sign_extended_constant().unwrap();

            if index < 0 {
                index_value = self.context.i32_type().const_int(
                    Self::convert_negative_array_index(
                        index_value.get_sign_extended_constant().unwrap(),
                        array_type.len()),
                    true,
                );
            }

            if index_value.get_sign_extended_constant().unwrap() as u32 >= array_type.len() {
                panic!("Index out of array len: {}", array_type.len());
            }

            let element_ptr = unsafe {
                self.builder.build_gep(
                    array_type,
                    *pointer,
                    &[index_value],
                    "array_element_ptr"
                ).unwrap()
            };

            self.builder
                .build_store(element_ptr, value)
                .unwrap();

            Ok(())
        } else {
            unreachable!();
        }
    }

    pub(super) fn codegen_array_assignment(
        &mut self,
        ident: &'ctx str,
        lir_type: LIRType,
        llvm_type: BasicTypeEnum<'ctx>,
        expr: &'ctx LIRExpr,
    )
    {
        let value = match expr {
            LIRExpr::Array(list) => {
                self.codegen_array_declaration(list)
            }
            LIRExpr::Dereference { .. } => {
                self.codegen_expr(expr)
            }
            LIRExpr::Identifier(ident) => {
                self.codegen_identifier_value(ident)
            }
            _ => unreachable!("{:#?}", expr),
        };
        
        match value {
            BasicValueEnum::PointerValue(pointer) => {
                self.store_variable(ident, pointer, lir_type, llvm_type);
            }
            BasicValueEnum::ArrayValue(value) => {
                let pointer = self.builder.build_alloca(value.get_type(), ident).unwrap();
                self.builder.build_store(pointer, value).unwrap();
                self.store_variable(ident, pointer, lir_type, llvm_type);
            }
            _ => unreachable!("{:#?} => {:#?}", expr, value),
        }
    }

    fn convert_negative_array_index(index: i64, array_size: u32) -> u64 {
        ((index / array_size as i64 + 1) * array_size as i64 + index) as u64
    }
}