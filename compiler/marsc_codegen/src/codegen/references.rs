use crate::codegen::codegen::Codegen;
use inkwell::values::{BasicValue, BasicValueEnum};
use lir::{LIRExpr, LIRType};

impl<'ctx, 'src> Codegen<'ctx, 'src>
where
    'src: 'ctx
{
    pub(in crate::codegen) fn codegen_get_referenced(
        &self,
        inner: &'ctx LIRExpr,
    ) -> BasicValueEnum<'ctx> {
        let pointer = match inner {
            LIRExpr::Identifier(ident) => {
                self.codegen_identifier_pointer(ident)
            },
            _ => unreachable!(),
        };
        
        pointer.as_basic_value_enum()
    }

    pub(in crate::codegen) fn codegen_get_dereferenced(
        &self,
        inner: &'ctx LIRExpr,
    ) -> BasicValueEnum<'ctx> {
        let (value, deref_type) = match inner {
            LIRExpr::Identifier(ident) => {
                let (
                    pointer,
                    lir_type,
                    llvm_type
                ) = self.codegen_identifier_value_with_types(ident);
                
                match lir_type {
                    LIRType::Ref(deref_type) => {
                        println!("{:#?}", deref_type);
                        (pointer, deref_type)
                    }
                    _ => unreachable!(),
                }
            },
            _ => unreachable!(),
        };
        
        match value {
            BasicValueEnum::PointerValue(pointer_value) => {
                let dereferenced_value = self.builder.build_load(
                    self.codegen_type(*deref_type),
                    pointer_value,
                    "dereferenced_value").unwrap();

                dereferenced_value
            }
            _ => unreachable!()
        }
    }
}