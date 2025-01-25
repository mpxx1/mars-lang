use crate::codegen::codegen::{Codegen, VariableData};
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValue, BasicValueEnum};
use lir::{LIRExpr, LIRStruct, LIRType};

impl<'ctx, 'src> Codegen<'ctx, 'src>
where
    'src: 'ctx
{
    pub(crate) fn codegen_struct(&mut self, struct_decl: &'ctx LIRStruct) {
        let field_types: Vec<BasicTypeEnum> = struct_decl
            .fields
            .iter()
            .map(|(_, field)| self.codegen_type(field.clone()))
            .collect();

        let struct_type = self.context.struct_type(&field_types, false);

        self.store_struct_type(struct_decl.name.as_str(), struct_type);

        struct_type.set_body(&field_types, false);
    }

    pub(crate) fn codegen_struct_init(
        &self,
        ident: &'ctx str,
        fields: &'ctx Vec<LIRExpr>,
    ) -> BasicValueEnum<'ctx> {
        let struct_type = self.get_struct_type(ident.to_owned());
        let pointer = self.builder.build_alloca(struct_type, ident).unwrap();

        for (i, expr) in fields.iter().enumerate()
        {
            let field_value = self.codegen_expr(expr);

            let field_ptr = self.builder.build_struct_gep(
                struct_type,
                pointer,
                i as u32,
                "field_ptr")
                .unwrap();

            self.builder.build_store(field_ptr, field_value).unwrap();
        }

        pointer.as_basic_value_enum()
    }

    pub(super) fn codegen_struct_assignment(
        &mut self,
        ident: &'ctx str,
        lir_type: LIRType,
        llvm_type: BasicTypeEnum<'ctx>,
        expr: &'ctx LIRExpr,
    ) {
        let value = self.codegen_expr(expr);

        match value {
            BasicValueEnum::PointerValue(pointer) => {
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

    pub(in crate::codegen) fn codegen_get_struct_field(
        &self,
        ident: &'ctx str,
        field_index: usize,
    ) -> BasicValueEnum<'ctx>
    {
        let struct_data = self.get_variable(ident);
        if let VariableData::Struct {
            pointer,
            lir_type,
            llvm_type
        } = struct_data {
            let struct_type = llvm_type.into_struct_type();

            let field_ptr = self.builder.build_struct_gep(
                struct_type,
                *pointer,
                field_index as u32,
                "field_ptr"
            ).unwrap();

            self.builder.build_load(
                struct_type.get_field_type_at_index(field_index as u32).unwrap(),
                field_ptr,
                "field_value"
            ).unwrap()
        } else {
            unreachable!()
        }
    }

    pub(in crate::codegen) fn codegen_set_struct_field(
        &self,
        ident: &'ctx str,
        field_index: usize,
        value: BasicValueEnum<'ctx>
    )
    {
        let struct_data = self.get_variable(ident);
        if let VariableData::Struct {
            pointer,
            lir_type,
            llvm_type
        } = struct_data {
            let struct_type = llvm_type.into_struct_type();

            let field_ptr = self.builder.build_struct_gep(
                struct_type,
                *pointer,
                field_index as u32,
                "field_ptr"
            ).unwrap();

            self.builder.build_store(
                field_ptr,
                value,
            ).unwrap();
        } else {
            unreachable!()
        }
    }
}