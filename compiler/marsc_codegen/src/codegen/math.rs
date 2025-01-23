use inkwell::types::BasicTypeEnum;
use inkwell::values::BasicValueEnum;
use ast::{AddOp, MathExpr, MulOp};
use mir::Scope;
use crate::codegen::codegen::Codegen;

impl<'ctx, 'src> Codegen<'ctx, 'src>
where
    'src: 'ctx
{
    pub(crate) fn codegen_math_add_expr(
        &self,
        left: &'ctx Box<MathExpr<'src>>,
        right: &'ctx Box<MathExpr<'src>>,
        op: &'ctx AddOp,
        scope: &'ctx Scope<'ctx>
    ) -> BasicValueEnum<'ctx> {
        let left_value = self.codegen_math_expr(left, scope);
        let right_value = self.codegen_math_expr(right, scope);

        let expr_type = left_value.get_type();

        match expr_type {
            BasicTypeEnum::FloatType(_) => self.codegen_math_add_f64_expr(left_value, right_value, op),
            BasicTypeEnum::IntType(_) => self.codegen_math_add_i64_expr(left_value, right_value, op),
            _ => unimplemented!(),
        }
    }

    pub(crate) fn codegen_math_mul_expr(
        &self,
        left: &'ctx Box<MathExpr<'src>>,
        right: &'ctx Box<MathExpr<'src>>,
        op: &'ctx MulOp,
        scope: &'ctx Scope<'ctx>
    ) -> BasicValueEnum<'ctx> {
        let left_value = self.codegen_math_expr(left, scope);
        let right_value = self.codegen_math_expr(right, scope);

        let expr_type = left_value.get_type();

        match expr_type {
            BasicTypeEnum::FloatType(_) => self.codegen_math_mul_f64_expr(left_value, right_value, op),
            BasicTypeEnum::IntType(_) => self.codegen_math_mul_i64_expr(left_value, right_value, op),
            _ => unimplemented!(),
        }
    }

    pub(crate) fn codegen_math_power_expr(
        &self,
        base: &'ctx Box<MathExpr<'src>>,
        exp: &'ctx Box<MathExpr<'src>>,
        scope: &'ctx Scope<'ctx>
    ) -> BasicValueEnum<'ctx> {
        let left_value = self.codegen_math_expr(base, scope);
        let right_value = self.codegen_math_expr(exp, scope);

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
        op: &'ctx AddOp,
    ) -> BasicValueEnum<'ctx> {
        let left_int = left_value.into_int_value();
        let right_int = right_value.into_int_value();

        match op {
            AddOp::Add => self.builder.build_int_add(left_int, right_int, "addtmp").unwrap().into(),
            AddOp::Sub => self.builder.build_int_sub(left_int, right_int, "subtmp").unwrap().into(),
        }
    }

    fn codegen_math_add_f64_expr(
        &self,
        left_value: BasicValueEnum<'ctx>,
        right_value: BasicValueEnum<'ctx>,
        op: &'ctx AddOp,
    ) -> BasicValueEnum<'ctx> {
        let left_float = left_value.into_float_value();
        let right_float = right_value.into_float_value();

        match op {
            AddOp::Add => self.builder.build_float_add(left_float, right_float, "addtmp").unwrap().into(),
            AddOp::Sub => self.builder.build_float_sub(left_float, right_float, "subtmp").unwrap().into(),
        }
    }

    fn codegen_math_mul_i64_expr(
        &self,
        left_value: BasicValueEnum<'ctx>,
        right_value: BasicValueEnum<'ctx>,
        op: &'ctx MulOp,
    ) -> BasicValueEnum<'ctx> {
        let left_int = left_value.into_int_value();
        let right_int = right_value.into_int_value();

        match op {
            MulOp::Mul => self.builder.build_int_mul(left_int, right_int, "multmp").unwrap().into(),
            MulOp::Div => self.builder.build_int_signed_div(left_int, right_int, "divtmp").unwrap().into(),
            MulOp::Mod => self.builder.build_int_signed_rem(left_int, right_int, "modtmp").unwrap().into(),
            MulOp::DivFloor => {
                unimplemented!()
            }
        }
    }

    fn codegen_math_mul_f64_expr(
        &self,
        left_value: BasicValueEnum<'ctx>,
        right_value: BasicValueEnum<'ctx>,
        op: &'ctx MulOp,
    ) -> BasicValueEnum<'ctx> {
        let left_float = left_value.into_float_value();
        let right_float = right_value.into_float_value();

        match op {
            MulOp::Mul => self.builder.build_float_mul(left_float, right_float, "multmp").unwrap().into(),
            MulOp::Div => self.builder.build_float_div(left_float, right_float, "divtmp").unwrap().into(),
            MulOp::Mod => self.builder.build_float_rem(left_float, right_float, "modtmp").unwrap().into(),
            MulOp::DivFloor => {
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