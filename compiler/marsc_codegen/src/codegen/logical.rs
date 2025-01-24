use crate::codegen::codegen::Codegen;
use inkwell::values::{BasicValue, BasicValueEnum};
use inkwell::IntPredicate;
use lir::{LIRCmpOp, LIRLogicalExpr};

impl<'ctx, 'src> Codegen<'ctx, 'src>
where
    'src: 'ctx
{
    pub(crate) fn codegen_logical_expr(&self, logical_expr: &'ctx LIRLogicalExpr) -> BasicValueEnum<'ctx> {
        match logical_expr {
            LIRLogicalExpr::Not { inner, .. } => {
                let inner_value = self
                    .codegen_logical_expr(inner)
                    .into_int_value();

                self.builder
                    .build_not(inner_value, "not")
                    .unwrap()
                    .as_basic_value_enum()
            },
            LIRLogicalExpr::Or { left, right, .. } => {
                let left_value = self
                    .codegen_logical_expr(left)
                    .into_int_value();
                let right_value = self
                    .codegen_logical_expr(right)
                    .into_int_value();

                self.builder
                    .build_or(left_value, right_value, "or")
                    .unwrap()
                    .as_basic_value_enum()
            },
            LIRLogicalExpr::And { left, right, .. } => {
                let left_value = self
                    .codegen_logical_expr(left)
                    .into_int_value();
                let right_value = self
                    .codegen_logical_expr(right)
                    .into_int_value();

                self.builder
                    .build_and(left_value, right_value, "or")
                    .unwrap()
                    .as_basic_value_enum()
            },
            LIRLogicalExpr::Comparison { left, right, op, .. } => {
                let left_value = self
                    .codegen_math_expr(left)
                    .into_int_value();
                let right_value = self
                    .codegen_math_expr(right)
                    .into_int_value();

                match op {
                    LIRCmpOp::Equal => {
                        self.builder.build_int_compare(IntPredicate::EQ, left_value, right_value, "eq").unwrap().into()
                    }
                    LIRCmpOp::NotEqual => {
                        self.builder.build_int_compare(IntPredicate::NE, left_value, right_value, "noteq").unwrap().into()
                    }
                    LIRCmpOp::More => {
                        self.builder.build_int_compare(IntPredicate::SGT, left_value, right_value, "more").unwrap().into()
                    }
                    LIRCmpOp::MoreEqual => {
                        self.builder.build_int_compare(IntPredicate::SGE, left_value, right_value, "moreeq").unwrap().into()
                    }
                    LIRCmpOp::Less => {
                        self.builder.build_int_compare(IntPredicate::SLT, left_value, right_value, "less").unwrap().into()
                    }
                    LIRCmpOp::LessEqual => {
                        self.builder.build_int_compare(IntPredicate::SLE, left_value, right_value, "lesseq").unwrap().into()
                    }
                }
            },
            LIRLogicalExpr::Primary(primary) => {
                self.codegen_expr(primary)
            }
        }
    }
}