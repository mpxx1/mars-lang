use crate::codegen::codegen::Codegen;
use inkwell::types::BasicTypeEnum;
use inkwell::values::BasicValueEnum;
use lir::{LIRAddOp, LIRMathExpr, LIRMulOp};

impl<'ctx, 'src> Codegen<'ctx, 'src>
where
    'src: 'ctx
{
    pub(crate) fn codegen_math_add_expr(
        &self,
        left: &'ctx Box<LIRMathExpr>,
        right: &'ctx Box<LIRMathExpr>,
        op: &'ctx LIRAddOp,
    ) -> BasicValueEnum<'ctx> {
        let left_value = self.codegen_math_expr(left);
        let right_value = self.codegen_math_expr(right);

        let expr_type = left_value.get_type();

        match expr_type {
            BasicTypeEnum::FloatType(_) => self.codegen_math_add_f64_expr(left_value, right_value, op),
            BasicTypeEnum::IntType(_) => self.codegen_math_add_i64_expr(left_value, right_value, op),
            _ => unimplemented!(),
        }
    }

    pub(crate) fn codegen_math_mul_expr(
        &self,
        left: &'ctx Box<LIRMathExpr>,
        right: &'ctx Box<LIRMathExpr>,
        op: &'ctx LIRMulOp,
    ) -> BasicValueEnum<'ctx> {
        let left_value = self.codegen_math_expr(left);
        let right_value = self.codegen_math_expr(right);

        let expr_type = left_value.get_type();

        match expr_type {
            BasicTypeEnum::FloatType(_) => self.codegen_math_mul_f64_expr(left_value, right_value, op),
            BasicTypeEnum::IntType(_) => self.codegen_math_mul_i64_expr(left_value, right_value, op),
            _ => unimplemented!(),
        }
    }

    pub(crate) fn codegen_math_power_expr(
        &self,
        base: &'ctx Box<LIRMathExpr>,
        exp: &'ctx Box<LIRMathExpr>,
    ) -> BasicValueEnum<'ctx> {
        let left_value = self.codegen_math_expr(base);
        let right_value = self.codegen_math_expr(exp);

        let expr_type = left_value.get_type();

        match expr_type {
            BasicTypeEnum::FloatType(_) => self.codegen_math_power_f64_expr(left_value, right_value),
            BasicTypeEnum::IntType(_) => self.codegen_math_power_i64_expr(left_value, right_value),
            _ => unimplemented!(),
        }
    }

    fn codegen_math_add_i64_expr(
        &self,
        left_value: BasicValueEnum<'ctx>,
        right_value: BasicValueEnum<'ctx>,
        op: &'ctx LIRAddOp,
    ) -> BasicValueEnum<'ctx> {
        let left_int = left_value.into_int_value();
        let right_int = right_value.into_int_value();

        match op {
            LIRAddOp::Add => self.builder.build_int_add(left_int, right_int, "addtmp").unwrap().into(),
            LIRAddOp::Sub => self.builder.build_int_sub(left_int, right_int, "subtmp").unwrap().into(),
        }
    }

    fn codegen_math_add_f64_expr(
        &self,
        left_value: BasicValueEnum<'ctx>,
        right_value: BasicValueEnum<'ctx>,
        op: &'ctx LIRAddOp,
    ) -> BasicValueEnum<'ctx> {
        let left_float = left_value.into_float_value();
        let right_float = right_value.into_float_value();

        match op {
            LIRAddOp::Add => self.builder.build_float_add(left_float, right_float, "addtmp").unwrap().into(),
            LIRAddOp::Sub => self.builder.build_float_sub(left_float, right_float, "subtmp").unwrap().into(),
        }
    }

    fn codegen_math_mul_i64_expr(
        &self,
        left_value: BasicValueEnum<'ctx>,
        right_value: BasicValueEnum<'ctx>,
        op: &'ctx LIRMulOp,
    ) -> BasicValueEnum<'ctx> {
        let left_int = left_value.into_int_value();
        let right_int = right_value.into_int_value();

        match op {
            LIRMulOp::Mul => self.builder.build_int_mul(left_int, right_int, "multmp").unwrap().into(),
            LIRMulOp::Div => self.builder.build_int_signed_div(left_int, right_int, "divtmp").unwrap().into(),
            LIRMulOp::Mod => self.builder.build_int_signed_rem(left_int, right_int, "modtmp").unwrap().into(),
            LIRMulOp::DivFloor => {
                unimplemented!()
            }
        }
    }

    fn codegen_math_mul_f64_expr(
        &self,
        left_value: BasicValueEnum<'ctx>,
        right_value: BasicValueEnum<'ctx>,
        op: &'ctx LIRMulOp,
    ) -> BasicValueEnum<'ctx> {
        let left_float = left_value.into_float_value();
        let right_float = right_value.into_float_value();

        match op {
            LIRMulOp::Mul => self.builder.build_float_mul(left_float, right_float, "multmp").unwrap().into(),
            LIRMulOp::Div => self.builder.build_float_div(left_float, right_float, "divtmp").unwrap().into(),
            LIRMulOp::Mod => self.builder.build_float_rem(left_float, right_float, "modtmp").unwrap().into(),
            LIRMulOp::DivFloor => {
                unimplemented!()
            }
        }
    }

    fn codegen_math_power_i64_expr(
        &self,
        base_value: BasicValueEnum<'ctx>,
        exp_value: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
       self.builder
           .build_call(
               self.module.get_function("llvm.powi.i64.i64").unwrap(), // TODO
               &[base_value.into(), exp_value.into()],
               "powtmp"
           )
           .unwrap()
           .try_as_basic_value()
           .left()
           .unwrap()
    }

    fn codegen_math_power_f64_expr(
        &self,
        base_value: BasicValueEnum<'ctx>,
        exp_value: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        self.builder
            .build_call(
                self.module.get_function("llvm.powi.f64.f64").unwrap(), // TODO
                &[base_value.into(), exp_value.into()],
                "powtmp"
            )
            .unwrap()
            .try_as_basic_value()
            .left()
            .unwrap()
    }
}