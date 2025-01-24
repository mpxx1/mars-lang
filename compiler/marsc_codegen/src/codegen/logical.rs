use crate::codegen::codegen::Codegen;
use inkwell::values::{BasicValue, BasicValueEnum};
use inkwell::IntPredicate;
use mir::stages::s2::{MIRCmpOp, MIRLogicalExpr, MIRScope};

impl<'ctx, 'src> Codegen<'ctx, 'src>
where
    'src: 'ctx
{
    pub(crate) fn codegen_logical_expr(&self, logical_expr: &'ctx MIRLogicalExpr<'src>, scope: &'ctx MIRScope<'ctx>) -> BasicValueEnum<'ctx> {
        match logical_expr {
            MIRLogicalExpr::Not { inner, .. } => {
                let inner_value = self
                    .codegen_logical_expr(inner, scope)
                    .into_int_value();

                self.builder
                    .build_not(inner_value, "not")
                    .unwrap()
                    .as_basic_value_enum()
            },
            MIRLogicalExpr::Or { left, right, .. } => {
                let left_value = self
                    .codegen_logical_expr(left, scope)
                    .into_int_value();
                let right_value = self
                    .codegen_logical_expr(right, scope)
                    .into_int_value();

                self.builder
                    .build_or(left_value, right_value, "or")
                    .unwrap()
                    .as_basic_value_enum()
            },
            MIRLogicalExpr::And { left, right, .. } => {
                let left_value = self
                    .codegen_logical_expr(left, scope)
                    .into_int_value();
                let right_value = self
                    .codegen_logical_expr(right, scope)
                    .into_int_value();

                self.builder
                    .build_and(left_value, right_value, "or")
                    .unwrap()
                    .as_basic_value_enum()
            },
            MIRLogicalExpr::Comparison { left, right, op, .. } => {
                let left_value = self
                    .codegen_math_expr(left, scope)
                    .into_int_value();
                let right_value = self
                    .codegen_math_expr(right, scope)
                    .into_int_value();

                match op {
                    MIRCmpOp::Equal => {
                        self.builder.build_int_compare(IntPredicate::EQ, left_value, right_value, "eq").unwrap().into()
                    }
                    MIRCmpOp::NotEqual => {
                        self.builder.build_int_compare(IntPredicate::NE, left_value, right_value, "noteq").unwrap().into()
                    }
                    MIRCmpOp::More => {
                        self.builder.build_int_compare(IntPredicate::SGT, left_value, right_value, "more").unwrap().into()
                    }
                    MIRCmpOp::MoreEqual => {
                        self.builder.build_int_compare(IntPredicate::SGE, left_value, right_value, "moreeq").unwrap().into()
                    }
                    MIRCmpOp::Less => {
                        self.builder.build_int_compare(IntPredicate::SLT, left_value, right_value, "less").unwrap().into()
                    }
                    MIRCmpOp::LessEqual => {
                        self.builder.build_int_compare(IntPredicate::SLE, left_value, right_value, "lesseq").unwrap().into()
                    }
                }
            },
            MIRLogicalExpr::Primary(primary) => {
                self.codegen_expr(primary, scope)
            }
        }
    }
}