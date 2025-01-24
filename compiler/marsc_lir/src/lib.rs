use std::collections::HashMap;

pub trait ToLir<'src> {
    fn compile_lir(self) -> Result<Lir<'src>, err::CompileError<'src>>;
}

impl<'src> ToLir<'src> for mir::Mir<'src> {
    fn compile_lir(self) -> Result<Lir<'src>, err::CompileError<'src>> {
        Ok(self.into())
    }
}

#[derive(Debug)]
pub struct Lir<'src> {
    pub code: &'src str,
    pub structs: HashMap<String, LIRStruct>,
    pub functions: HashMap<String, LIRFunc>,
    pub blocks: HashMap<usize, Vec<LIRInstruction>>,
    pub sys_funs: Vec<String>,
}

#[derive(Debug)]
pub struct LIRStruct {
    pub name: String,
    pub fields: Vec<(String, LIRType)>, // order as in decl
}

#[derive(Debug)]
pub struct LIRFunc {
    pub name: String,
    pub args: Vec<(String, LIRType)>,
    pub return_type: LIRType,
    pub block_id: usize,
}

#[derive(Debug, Clone)]
pub enum LIRInstruction {
    Return {
        expr: Option<LIRExpr>,
    },
    Break,
    Assignment {
        ident: String,
        ty: LIRType,
        expr: LIRExpr,
    },
    Assign {
        lhs: LIRExpr,
        rhs: LIRExpr,
    },
    FuncCall(LIRFuncCall),
    GoToBlock {
        block_id: usize,
    },
    GoToIfCond {
        cond: Box<LIRExpr>,
        then_block: usize,
        else_block: Option<usize>,
    },
    GoToWhile {
        cond: Box<LIRExpr>,
        loop_block: usize,
    },
}

#[derive(Debug, Clone)]
pub struct LIRFuncCall {
    pub function: String,
    pub args: Vec<LIRExpr>,
}

#[derive(Debug, Clone)]
pub enum LIRExpr {
    Identifier(String),
    FuncCall(LIRFuncCall),
    Array(Vec<LIRExpr>),
    MemLookup {
        base: String,
        indices: Vec<LIRExpr>,
        span_start: usize,
        span_end: usize,
    },
    StructFieldCall {
        struct_name: String,
        field_index: usize,
    },
    StructInit {
        struct_name: String,
        fields: Vec<LIRExpr>,
    },
    Cast {
        ty: Box<LIRType>,
        expr: Box<LIRExpr>,
    },
    Dereference {
        refer: Box<LIRExpr>,
        span_start: usize,
        span_end: usize,
    },
    Reference(Box<LIRExpr>),
    Logical(LIRLogicalExpr),
    Math(LIRMathExpr),
    Literal(LIRLiteral),
}

#[derive(Debug, Clone)]
pub enum LIRLogicalExpr {
    Not {
        inner: Box<LIRLogicalExpr>,
    },
    Or {
        left: Box<LIRLogicalExpr>,
        right: Box<LIRLogicalExpr>,
    },
    And {
        left: Box<LIRLogicalExpr>,
        right: Box<LIRLogicalExpr>,
    },
    Comparison {
        left: Box<LIRMathExpr>,
        right: Box<LIRMathExpr>,
        op: LIRCmpOp,
    },
    Primary(Box<LIRExpr>),
}

#[derive(Debug, Clone)]
pub enum LIRMathExpr {
    Additive {
        left: Box<LIRMathExpr>,
        right: Box<LIRMathExpr>,
        op: LIRAddOp,
    },
    Multiplicative {
        left: Box<LIRMathExpr>,
        right: Box<LIRMathExpr>,
        op: LIRMulOp,
    },
    Power {
        base: Box<LIRMathExpr>,
        exp: Box<LIRMathExpr>,
    },
    Primary(Box<LIRExpr>),
}

#[derive(Debug, Clone)]
pub enum LIRLiteral {
    Int(i64),
    Float(f64),
    Str(String),
    Char(char),
    Bool(bool),
    NullRef,
}

// #[derive(Debug, Clone)]
// pub struct LIRStructType {
//     // pub decl_scope_id: usize,
//     // pub struct_id: usize,
//     pub ident: String,
// }

// impl PartialEq for LIRStructType {
//     fn eq(&self, other: &Self) -> bool {
//         // self.decl_scope_id == other.decl_scope_id &&
//         // self.struct_id == self.struct_id
//         self.ident == other.ident
//     }
// }

#[derive(Debug, Clone, PartialEq)]
pub enum LIRType {
    I64,
    F64,
    Str,
    Char,
    Bool,
    Void,
    StructType(String),
    Array(Box<LIRType>, usize),
    Vec(Box<LIRType>),
    Ref(Box<LIRType>),
    Any,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LIRCmpOp {
    Equal,
    NotEqual,
    More,
    MoreEqual,
    Less,
    LessEqual,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LIRAddOp {
    Add,
    Sub,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LIRMulOp {
    Mul,
    Div,
    Mod,
    DivFloor,
}

use mir::*;
use stages::s2::*;

fn global_name(ident: String, id: usize) -> String {
    format!("{ident}_{id}")
}

impl<'src> From<Mir<'src>> for Lir<'src> {
    fn from(mir: Mir<'src>) -> Self {
        let mut structs = HashMap::new();
        let mut functions = HashMap::new();
        let mut tmp = HashMap::new();
        let mut blocks = HashMap::new();

        for (id, scope) in mir.scopes {
            for (ident, struct_proto) in scope.structs {
                let id = struct_proto.node_id;
                structs.insert(global_name(ident, id), struct_proto.into());
            }

            for (ident, fun) in scope.funs {
                if !mir.sys_funs.contains(&ident) && ident != "main" {
                    let name = global_name(ident, fun.node_id);
                    functions.insert(name, fun.into());
                } else {
                    functions.insert(ident, to_lir_func(fun, false));
                }
            }

            tmp.insert(id, scope.instrs);
        }

        for (id, instrs) in tmp {
            blocks.insert(
                id,
                instrs
                    .into_iter()
                    .map(|x| proceed_instr(x, &structs, &mir.sys_funs))
                    .collect(),
            );
        }

        Self {
            code: mir.code,
            structs,
            functions,
            blocks,
            sys_funs: mir.sys_funs,
        }
    }
}

fn proceed_instr(
    instr: MIRInstruction,
    structs: &HashMap<String, LIRStruct>,
    sys_funs: &Vec<String>,
) -> LIRInstruction {
    match instr {
        MIRInstruction::Return { expr, .. } => LIRInstruction::Return {
            expr: expr.map(|expr| proceed_expr(expr, structs, sys_funs)),
        },

        MIRInstruction::Break { .. } => LIRInstruction::Break,

        MIRInstruction::Assignment {
            ident, ty, expr, ..
        } => LIRInstruction::Assignment {
            ident,
            ty: ty.into(),
            expr: proceed_expr(expr, structs, sys_funs),
        },

        MIRInstruction::Assign { lhs, rhs, .. } => LIRInstruction::Assign {
            lhs: proceed_expr(lhs, structs, sys_funs),
            rhs: proceed_expr(rhs, structs, sys_funs),
        },

        MIRInstruction::FuncCall(fc) => LIRInstruction::FuncCall(LIRFuncCall {
            function: if !sys_funs.contains(&fc.ident) {
                global_name(fc.ident, fc.fn_id)
            } else {
                fc.ident
            },
            args: fc
                .args
                .into_iter()
                .map(|x| proceed_expr(x, structs, sys_funs))
                .collect(),
        }),

        MIRInstruction::GoToBlock { block_id } => LIRInstruction::GoToBlock { block_id },

        MIRInstruction::GoToIfCond {
            cond,
            then_block_id,
            else_block_id,
        } => LIRInstruction::GoToIfCond {
            cond: Box::new(proceed_expr(*cond, structs, sys_funs)),
            then_block: then_block_id,
            else_block: else_block_id,
        },

        MIRInstruction::GoToWhile { cond, loop_id } => LIRInstruction::GoToWhile {
            cond: Box::new(proceed_expr(*cond, structs, sys_funs)),
            loop_block: loop_id,
        },
    }
}

fn proceed_expr(
    expr: MIRExpr<'_>,
    structs: &HashMap<String, LIRStruct>,
    sys_funs: &Vec<String>,
) -> LIRExpr {
    match expr {
        MIRExpr::Identifier { ident, .. } => LIRExpr::Identifier(ident),
        MIRExpr::FuncCall(x) => LIRExpr::FuncCall(LIRFuncCall {
            function: if !sys_funs.contains(&x.ident) {
                global_name(x.ident, x.fn_id)
            } else {
                x.ident
            },
            args: x
                .args
                .into_iter()
                .map(|expr| proceed_expr(expr, structs, sys_funs))
                .collect(),
        }),
        MIRExpr::ArrayDecl { list, .. } => LIRExpr::Array(
            list.into_iter()
                .map(|expr| proceed_expr(expr, structs, sys_funs))
                .collect(),
        ),
        MIRExpr::MemLookup {
            ident,
            indices,
            span,
        } => LIRExpr::MemLookup {
            base: ident,
            indices: indices
                .into_iter()
                .map(|expr| proceed_expr(expr, structs, sys_funs))
                .collect(),
            span_start: span.start(),
            span_end: span.end(),
        },
        MIRExpr::StructFieldCall {
            struct_id,
            ident,
            field,
            ..
        } => {
            // let glob = global_name(ident, struct_id);

            LIRExpr::StructFieldCall {
                struct_name: ident.clone(),
                field_index: {
                    let struct_entry = structs
                        .iter()
                        .find(|&(_, x)| x.name.ends_with(&format!("_{struct_id}")))
                        .unwrap_or_else(|| panic!("Struct {} not found in LIR structs", ident))
                        .1;
                    // let struct_entry = structs.get(&ident)
                    //     .unwrap_or_else(|| panic!("Struct {} not found in LIR structs", ident));

                    struct_entry
                        .fields
                        .iter()
                        .position(|(id, _)| *id == field)
                        .unwrap_or_else(|| panic!("Field {} not found in struct {}", field, ident))
                },
            }
        }

        MIRExpr::StructInit {
            ident,
            fields,
            struct_id,
            ..
        } => LIRExpr::StructInit {
            struct_name: global_name(ident, struct_id),
            fields: fields
                .into_iter()
                .map(|(_, e)| proceed_expr(e, structs, sys_funs))
                .collect(),
        },

        MIRExpr::CastType { cast_to, expr, .. } => LIRExpr::Cast {
            ty: Box::new((*cast_to).into()),
            expr: Box::new(proceed_expr(*expr, structs, sys_funs)),
        },

        MIRExpr::Dereference { inner, span } => LIRExpr::Dereference {
            refer: Box::new(proceed_expr(*inner, structs, sys_funs)),
            span_start: span.start(),
            span_end: span.end(),
        },

        MIRExpr::Reference { inner, .. } => {
            LIRExpr::Reference(Box::new(proceed_expr(*inner, structs, sys_funs)))
        }

        MIRExpr::Literal(l) => LIRExpr::Literal(l.into()),

        MIRExpr::LogicalExpr(le) => LIRExpr::Logical(proceed_logical(le, structs, sys_funs)),

        MIRExpr::MathExpr(me) => LIRExpr::Math(proceed_math(me, structs, sys_funs)),
    }
}

fn proceed_logical(
    expr: MIRLogicalExpr<'_>,
    structs: &HashMap<String, LIRStruct>,
    sys_funs: &Vec<String>,
) -> LIRLogicalExpr {
    match expr {
        MIRLogicalExpr::Not { inner, .. } => LIRLogicalExpr::Not {
            inner: Box::new(proceed_logical(*inner, structs, sys_funs)),
        },

        MIRLogicalExpr::Or { left, right, .. } => LIRLogicalExpr::Or {
            left: Box::new(proceed_logical(*left, structs, sys_funs)),
            right: Box::new(proceed_logical(*right, structs, sys_funs)),
        },

        MIRLogicalExpr::And { left, right, .. } => LIRLogicalExpr::And {
            left: Box::new(proceed_logical(*left, structs, sys_funs)),
            right: Box::new(proceed_logical(*right, structs, sys_funs)),
        },

        MIRLogicalExpr::Comparison {
            left, right, op, ..
        } => LIRLogicalExpr::Comparison {
            left: Box::new(proceed_math(*left, structs, sys_funs)),
            right: Box::new(proceed_math(*right, structs, sys_funs)),
            op: op.into(),
        },

        MIRLogicalExpr::Primary(p) => {
            LIRLogicalExpr::Primary(Box::new(proceed_expr(*p, structs, sys_funs)))
        }
    }
}

fn proceed_math(
    expr: MIRMathExpr<'_>,
    structs: &HashMap<String, LIRStruct>,
    sys_funs: &Vec<String>,
) -> LIRMathExpr {
    match expr {
        MIRMathExpr::Additive {
            left, right, op, ..
        } => LIRMathExpr::Additive {
            left: Box::new(proceed_math(*left, structs, sys_funs)),
            right: Box::new(proceed_math(*right, structs, sys_funs)),
            op: op.into(),
        },

        MIRMathExpr::Multiplicative {
            left, right, op, ..
        } => LIRMathExpr::Multiplicative {
            left: Box::new(proceed_math(*left, structs, sys_funs)),
            right: Box::new(proceed_math(*right, structs, sys_funs)),
            op: op.into(),
        },

        MIRMathExpr::Power { base, exp, .. } => LIRMathExpr::Power {
            base: Box::new(proceed_math(*base, structs, sys_funs)),
            exp: Box::new(proceed_math(*exp, structs, sys_funs)),
        },

        MIRMathExpr::Primary(p) => {
            LIRMathExpr::Primary(Box::new(proceed_expr(*p, structs, sys_funs)))
        }
    }
}

impl<'src> From<MIRLiteral<'src>> for LIRLiteral {
    fn from(lit: MIRLiteral<'src>) -> Self {
        match lit {
            MIRLiteral::Int { lit, .. } => LIRLiteral::Int(lit),
            MIRLiteral::Float { lit, .. } => LIRLiteral::Float(lit),
            MIRLiteral::Str { lit, .. } => LIRLiteral::Str(lit),
            MIRLiteral::Char { lit, .. } => LIRLiteral::Char(lit),
            MIRLiteral::Bool { lit, .. } => LIRLiteral::Bool(lit),
            MIRLiteral::NullRef { .. } => LIRLiteral::NullRef,
        }
    }
}

impl From<MIRCmpOp> for LIRCmpOp {
    fn from(op: MIRCmpOp) -> Self {
        match op {
            MIRCmpOp::Equal => LIRCmpOp::Equal,
            MIRCmpOp::NotEqual => LIRCmpOp::NotEqual,
            MIRCmpOp::More => LIRCmpOp::More,
            MIRCmpOp::MoreEqual => LIRCmpOp::MoreEqual,
            MIRCmpOp::Less => LIRCmpOp::Less,
            MIRCmpOp::LessEqual => LIRCmpOp::LessEqual,
        }
    }
}

impl From<MIRAddOp> for LIRAddOp {
    fn from(op: MIRAddOp) -> Self {
        match op {
            MIRAddOp::Add => LIRAddOp::Add,
            MIRAddOp::Sub => LIRAddOp::Sub,
        }
    }
}

impl From<MIRMulOp> for LIRMulOp {
    fn from(op: MIRMulOp) -> Self {
        match op {
            MIRMulOp::Mul => LIRMulOp::Mul,
            MIRMulOp::Div => LIRMulOp::Div,
            MIRMulOp::Mod => LIRMulOp::Mod,
            MIRMulOp::DivFloor => LIRMulOp::DivFloor,
        }
    }
}

impl From<MIRFunc<'_>> for LIRFunc {
    fn from(fun: MIRFunc) -> Self {
        Self {
            name: global_name(fun.ident, fun.node_id),
            args: fun.args.into_iter().map(|x| (x.ident, x.ty.into())).collect(),
            return_type: fun.return_type.into(),
            block_id: fun.node_id,
        }
    }
}

fn to_lir_func(fun: MIRFunc<'_>, generate_name: bool) -> LIRFunc {
    LIRFunc {
        name: if generate_name { global_name(fun.ident, fun.node_id) } else { fun.ident },
        args: fun.args.into_iter().map(|x| (x.ident, x.ty.into())).collect(),
        return_type: fun.return_type.into(),
        block_id: fun.node_id,
    }
}

impl From<MIRStruct<'_>> for LIRStruct {
    fn from(s: MIRStruct<'_>) -> Self {
        let glob = global_name(s.ident, s.node_id);
        Self {
            name: glob.clone(),
            fields: s
                .fields
                .into_iter()
                .map(|x| (x.ident, x.ty.into()))
                .collect(),
        }
    }
}

impl From<MIRType> for LIRType {
    fn from(ty: MIRType) -> Self {
        match ty {
            MIRType::I64 => LIRType::I64,
            MIRType::F64 => LIRType::F64,
            MIRType::Char => LIRType::Char,
            MIRType::Bool => LIRType::Bool,
            MIRType::Void => LIRType::Void,
            MIRType::Str => LIRType::Str,
            MIRType::Any => LIRType::Any,

            MIRType::StructType(mir_struct) => {
                LIRType::StructType(global_name(mir_struct.ident, mir_struct.struct_id))
            }

            MIRType::Array(inner, size) => LIRType::Array(Box::new((*inner).into()), size),

            MIRType::Vec(inner) => LIRType::Vec(Box::new((*inner).into())),

            MIRType::Ref(inner) => LIRType::Ref(Box::new((*inner).into())),
        }
    }
}

#[test]
fn resolving_parent_ids<'src>() -> Result<(), err::CompileError<'src>> {
    let inp = r#"
        struct A { a: i64 }

        fn hello(a: i64) -> i64 {
            println("10");
            println("{a}");
            return a;
        }

        fn main() -> void {
            struct B { a: f64, b: i64 }

            var bgg = B { b: 60, a: 0.0 };

            hello(bgg.b);
            var lala = bgg.a;

            return;
        }
    "#;

    let hir = hir::compile_hir(&inp)?;
    let mir = hir.compile_mir()?;
    let lir = mir.compile_lir()?;

    println!("{lir:#?}");

    Ok(())
}

#[test]
fn sys_funs_test<'src>() -> Result<(), err::CompileError<'src>> {
    let inp = r#"
        fn main() -> void {
            var vec: Vec<i64> = [];
            var ref = &vec;
            var cap = capacity(&vec);
            var x = capacity(ref);
            return;
        }
    "#;

    let hir = hir::compile_hir(&inp)?;
    let mir = hir.compile_mir()?;
    let lir = mir.compile_lir()?;

    println!("{lir:#?}");

    Ok(())
}

#[test]
fn q_sort<'src>() -> Result<(), err::CompileError<'src>> {
    // var pivot = *( arr[0] );        /*  todo  ->  (*arr)[0]  */
    let inp = r#"
    fn quick_sort(arr: &Vec<i64>) -> Vec<i64> {
        var len = len(arr);
        if len <= 1 {
            return *arr;
        }

        var pivot = *arr[0];   /* here */
        var left: Vec<i64> = [];
        var right: Vec<i64> = [];

        var i: i64 = 1;
        while i < l {
            if arr[i] < pivot {
                push(left, arr[i]);
            } else {
                push(right, arr[i]);
            }
        }

        var result = quick_sort(&left);
        push(result, pivot);
        var rightRes = quick_sort(&right);

        i = 0;
        while i < len(rightRes) {
            push(result, rightRes[i]);
        }

        return result;
    }

    fn main() -> void {
        var arr: Vec<i64> = [3, 6, 8, 10, 1, 2, 1];
        var sorted = quick_sort(&arr);
    }
    "#;

    let hir = hir::compile_hir(&inp)?;
    let mir = hir.compile_mir()?;
    // let lir = mir.compile_lir()?;

    // println!("{:#?}", lir);
    println!("{:#?}", mir);
    // println!("{:#?}", hir.ast.program);

    Ok(())
}
