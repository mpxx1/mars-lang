use crate::codegen::codegen::{Codegen, VariableData};
use err::CompileError;
use inkwell::types::{ArrayType, BasicTypeEnum};
use inkwell::values::{BasicValue, BasicValueEnum, PointerValue};
use mir::stages::s2::{MIRExpr, MIRScope};
use pest::Span;

impl<'ctx, 'src> Codegen<'ctx, 'src>
where
    'src: 'ctx
{
    pub(super) fn codegen_array_declaration(
        &self,
        list: &'ctx Vec<MIRExpr>,
        scope: &'ctx MIRScope<'ctx>
    ) -> BasicValueEnum<'ctx> {
        let element_type = self.context.i64_type();
        let array_type = element_type.array_type(list.len() as u32);
        let array_alloca = self.builder.build_alloca(array_type, "array").unwrap();

        for (i, item) in list.iter().enumerate() {
            let value = self.codegen_expr(item, scope);
            let index = self.context.i32_type().const_int(i as u64, false);
            let element_ptr = unsafe {
                self.builder.build_gep(array_type, array_alloca, &[index], "element_ptr").unwrap()
            };
            self.builder.build_store(element_ptr, value).unwrap();
        };

        array_alloca.as_basic_value_enum()
    }

    pub(super) fn codegen_get_array_element(
        &self,
        ident: &'ctx str,
        indices: &'ctx Vec<MIRExpr<'src>>,
        span: Span<'src>,
        scope: &'ctx MIRScope<'ctx>
    ) -> Result<BasicValueEnum<'ctx>, CompileError> {
        println!("{:#?}", self.var_table);
        let array_data = self.var_table.get(ident).expect(ident);

        if let VariableData::Array {
            pointer,
            array_type,
        } = array_data {
            let mut index_value = match self.codegen_expr(&indices[0], scope) {
                BasicValueEnum::IntValue(int_value) => int_value,
                _ => unreachable!()
            };

            let index = index_value.get_sign_extended_constant().unwrap();

            if index < 0 {
                index_value = self.context.i32_type().const_int(
                    Self::convert_negative_array_index(
                        index_value.get_sign_extended_constant().unwrap(),
                        array_type.len()),
                    false,
                );
            }

            if index_value.get_sign_extended_constant().unwrap() as u32 >= array_type.len() {
                return Err(CompileError::new(span, format!("Index out of array len: {}", array_type.len())))
            }

            let element_ptr = unsafe {
                self.builder.build_gep(
                    *array_type,
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
        indices: &'ctx Vec<MIRExpr<'src>>,
        value: BasicValueEnum<'ctx>,
        span: Span<'src>,
        scope: &'ctx MIRScope<'ctx>
    ) -> Result<(), CompileError> {
        let array_data = self.var_table.get(ident).unwrap();

        if let VariableData::Array {
            pointer,
            array_type,
        } = array_data {
            let mut index_value = match self.codegen_expr(&indices[0], scope) {
                BasicValueEnum::IntValue(int_value) => int_value,
                _ => unreachable!()
            };

            let index = index_value.get_sign_extended_constant().unwrap();

            if index < 0 {
                index_value = self.context.i32_type().const_int(
                    Self::convert_negative_array_index(
                        index_value.get_sign_extended_constant().unwrap(),
                        array_type.len()),
                    false,
                );
            }

            if index_value.get_sign_extended_constant().unwrap() as u32 >= array_type.len() {
                return Err(CompileError::new(span, format!("Index out of array len: {}", array_type.len())))
            }

            let element_ptr = unsafe {
                self.builder.build_gep(
                    *array_type,
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
        variable_type: BasicTypeEnum<'ctx>,
        expr: &'ctx MIRExpr<'src>,
        span: Span<'src>,
        scope: &'ctx MIRScope<'ctx>
    ) -> Result<(), CompileError>
    {
        let value = self.codegen_expr(expr, scope);
        if let BasicValueEnum::PointerValue(value) = value {
            self.store_variable(ident, value, variable_type);
            Ok(())
        } else {
            Err(CompileError::new(span, "Invalid right value type".to_owned()))
        }
    }

    fn convert_negative_array_index(index: i64, array_size: u32) -> u64 {
        ((index / array_size as i64 + 1) * array_size as i64 + index) as u64
    }

    pub(super) fn move_array(
        &self,
        array_type: ArrayType,
        src_var: PointerValue<'ctx>,
        dest_var: PointerValue<'ctx>,
    ) {
        let element_type = array_type.get_element_type();
        let zero = self.context.i32_type().const_int(0, false);

        for i in 0..array_type.len() {
            let index = self.context.i32_type().const_int(i as u64, false);

            let src_ptr = unsafe {
                self.builder.build_gep(
                    element_type,
                    src_var,
                    &[zero, index],
                    &format!("src_ptr_{}", i)
                ).unwrap()
            };
            let value = self.builder.build_load(
                src_var.get_type(),
                src_ptr,
                &format!("load_{}", i)
            ).unwrap();

            let dest_ptr = unsafe {
                self.builder.build_gep(
                    element_type,
                    dest_var,
                    &[zero, index],
                    &format!("dest_ptr_{}", i)
                ).unwrap()
            };
            self.builder.build_store(dest_ptr, value).unwrap();
        }
    }
}