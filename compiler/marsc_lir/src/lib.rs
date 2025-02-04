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

fn get_var_decl_id(scopes: &HashMap<usize, MIRScope>, scope_id: &usize, ident: &String) -> usize {
    
    // dbg!(&scopes);
    
    let mut scope = scopes.get(scope_id).unwrap();

    loop {
        if let Some(x) = scope.vars.get(ident) {
            break;
        }
        
        if scope.node_id == scope.parent_id {
            panic!("Something went wrong");
        }

        scope = scopes.get(&scope.parent_id).unwrap();
    }

    scope.node_id
}

fn make_uniq_names(mir: Mir) -> Mir {
    
    let scopes_copy = mir.scopes.clone();  // terrible
    
    let scopes = mir
        .scopes
        .into_iter()
        .map(|(i, x)| {
            let instrs = x
                .instrs
                .into_iter()
                .map(|mut instr| {
                    instr_idents_uniq(&scopes_copy, &mut instr, &mir.sys_funs, &x.node_id, &x.parent_id);
                    instr
                })
                .collect();

            (
                i,
                MIRScope {
                    parent_id: i,
                    node_id: x.node_id,
                    structs: x.structs,
                    funs: x.funs,
                    vars: x.vars,
                    instrs,
                    scope_type: x.scope_type,
                },
            )
        })
        .collect();

    MirS2 {
        code: mir.code,
        scopes,
        sys_funs: mir.sys_funs,
    }
}

fn instr_idents_uniq(
    scopes: &HashMap<usize, MIRScope>,
    instr: &mut MIRInstruction,
    sys_funs: &Vec<String>,
    id: &usize,
    parent_id: &usize,
) {
    match instr {
        MIRInstruction::Assign { lhs, rhs, .. } => {
            expr_idents_uniq(scopes, lhs, id, sys_funs);
            expr_idents_uniq(scopes, rhs, id, sys_funs);
        }

        MIRInstruction::Assignment { ident, expr, .. } => {
            let left = ident;
            let right = expr;

            if let MIRExpr::Identifier { ident, .. } = right {
                if left == ident {
                    *left = global_name(left.clone(), *id);
                    expr_idents_uniq(scopes, right, parent_id, sys_funs);
                    return;
                }
            }

            *left = global_name(left.clone(), *id);
            expr_idents_uniq(scopes, right, id, sys_funs);
        }

        MIRInstruction::Return { expr, .. } => {
            if let Some(expr) = expr {
                expr_idents_uniq(scopes, expr, id, sys_funs);
            }
        }

        MIRInstruction::Break { .. } => {}

        MIRInstruction::FuncCall(x) => {
            // if x.ident != "main" && !sys_funs.contains(&x.ident) {
            //     x.ident.push_str(format!("_{id}").as_str());
            // }
            x.args
                .iter_mut()
                .for_each(|x| expr_idents_uniq(scopes, x, id, sys_funs));
        }

        MIRInstruction::GoToBlock { .. } => {}

        MIRInstruction::GoToIfCond { cond, .. } => expr_idents_uniq(scopes, cond, id, sys_funs),

        MIRInstruction::GoToWhile { cond, .. } => expr_idents_uniq(scopes, cond, id, sys_funs),
    }
}

fn expr_idents_uniq(
    scopes: &HashMap<usize, MIRScope>,
    expr: &mut MIRExpr,
    id: &usize,
    sys_funs: &Vec<String>
) {
    match expr {
        MIRExpr::LogicalExpr(x) => {
            make_uniq_logical_expr(scopes, x, id, sys_funs);
        }

        MIRExpr::MathExpr(x) => {
            make_uniq_math_expr(scopes, x, id, sys_funs);
        }

        MIRExpr::Identifier { ident, .. } => {
            let id = get_var_decl_id(scopes, id, ident);
            ident.push_str(format!("_{id}").as_str());
        }

        MIRExpr::Reference { inner, .. } => {
            expr_idents_uniq(scopes, inner.as_mut(), id, sys_funs);
        }

        MIRExpr::FuncCall(x) => {
            // if x.ident != "main" && !sys_funs.contains(&x.ident) {
            //     x.ident.push_str(format!("_{id}").as_str());
            // }
            x.args
                .iter_mut()
                .for_each(|x| expr_idents_uniq(scopes, x, id, sys_funs));
        }

        MIRExpr::StructInit { .. } => {
            // ident.push_str(format!("_{id}").as_str());
        }

        MIRExpr::CastType { expr, .. } => expr_idents_uniq(scopes, expr, id, sys_funs),

        MIRExpr::ArrayDecl { list, .. } => {
            list.iter_mut()
                .for_each(|x| expr_idents_uniq(scopes, x, id, sys_funs));
        }

        MIRExpr::Dereference { inner, .. } => {
            expr_idents_uniq(scopes, inner, id, sys_funs);
        }

        MIRExpr::StructFieldCall { ident, .. } => {
            let id = get_var_decl_id(scopes, id, ident);
            ident.push_str(format!("_{id}").as_str());
        }

        MIRExpr::MemLookup { ident, indices, .. } => {
            // todo reimpl
            
            let first_id = &id;
            let id = get_var_decl_id(scopes, id, ident);    // would it broke arrays?
            ident.push_str(format!("_{id}").as_str());
            indices
                .iter_mut()
                .for_each(|x| expr_idents_uniq(scopes, x, &first_id, sys_funs));
        }

        MIRExpr::Literal(_) => {}
    }
}

fn make_uniq_logical_expr(
    scopes: &HashMap<usize, MIRScope>,
    expr: &mut MIRLogicalExpr, 
    id: &usize, 
    sys_funs: &Vec<String>
) {
    match expr {
        MIRLogicalExpr::Comparison { left, right, .. } => {
            make_uniq_math_expr(scopes, left.as_mut(), id, sys_funs);
            make_uniq_math_expr(scopes, right.as_mut(), id, sys_funs);
        }

        MIRLogicalExpr::Or { left, right, .. } => {
            make_uniq_logical_expr(scopes, left.as_mut(), id, sys_funs);
            make_uniq_logical_expr(scopes, right.as_mut(), id, sys_funs);
        }

        MIRLogicalExpr::And { left, right, .. } => {
            make_uniq_logical_expr(scopes, left.as_mut(), id, sys_funs);
            make_uniq_logical_expr(scopes, right.as_mut(), id, sys_funs);
        }

        MIRLogicalExpr::Not { inner, .. } => {
            make_uniq_logical_expr(scopes, inner.as_mut(), id, sys_funs);
        }

        MIRLogicalExpr::Primary(x) => expr_idents_uniq(scopes, x.as_mut(), id, sys_funs),
    }
}

fn make_uniq_math_expr(
    scopes: &HashMap<usize, MIRScope>,
    math: &mut MIRMathExpr,
    id: &usize,
    sys_funs: &Vec<String>
) {
    match math {
        MIRMathExpr::Additive { left, right, .. } => {
            make_uniq_math_expr(scopes, left.as_mut(), id, sys_funs);
            make_uniq_math_expr(scopes, right.as_mut(), id, sys_funs);
        }

        MIRMathExpr::Multiplicative { left, right, .. } => {
            make_uniq_math_expr(scopes, left.as_mut(), id, sys_funs);
            make_uniq_math_expr(scopes, right.as_mut(), id, sys_funs);
        }

        MIRMathExpr::Power { base, exp, .. } => {
            make_uniq_math_expr(scopes, base.as_mut(), id, sys_funs);
            make_uniq_math_expr(scopes, exp.as_mut(), id, sys_funs);
        }

        MIRMathExpr::Primary(x) => expr_idents_uniq(scopes, x, id, sys_funs),
    }
}

impl<'src> From<Mir<'src>> for Lir<'src> {
    fn from(mir: Mir<'src>) -> Self {
        let mir = make_uniq_names(mir);

        let mut structs = HashMap::new();
        let mut functions = HashMap::new();
        let mut tmp = HashMap::new();
        let mut blocks = HashMap::new();

        for (id, scope) in mir.scopes {
            for (ident, struct_proto) in scope.structs {
                let id = struct_proto.node_id;
                structs.insert(global_name(ident, id), struct_proto.into());
            }

            let mut _fn_name = "".to_owned();
            for (ident, fun) in scope.funs {
                _fn_name = if mir.sys_funs.contains(&ident) || ident == "main" {
                    ident
                } else {
                    global_name(ident, fun.node_id)
                };

                functions.insert(_fn_name, proceed_fn(fun, &mir.sys_funs));
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
        } => LIRExpr::StructFieldCall {
            struct_name: ident.clone(),
            field_index: {
                let struct_entry = structs
                    .iter()
                    .find(|&(_, x)| x.name.ends_with(&format!("_{struct_id}")))
                    .unwrap_or_else(|| panic!("Struct {} not found in LIR structs", ident))
                    .1;

                struct_entry
                    .fields
                    .iter()
                    .position(|(id, _)| *id == field)
                    .unwrap_or_else(|| panic!("Field {} not found in struct {}", field, ident))
            },
        },

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

fn proceed_fn(fun: MIRFunc, sys_funs: &Vec<String>) -> LIRFunc {
    fn get_fn_name(s: String, id: usize, sys_funs: &Vec<String>) -> String {
        if sys_funs.contains(&s) || s == "main" {
            s
        } else {
            global_name(s, id)
        }
    }

    let args = fun
        .args
        .into_iter()
        .map(|x| (format!("{}_{}", x.ident, fun.node_id), x.ty.into()))
        .collect();

    LIRFunc {
        name: get_fn_name(fun.ident.clone(), fun.node_id, sys_funs),
        args,
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
        fn factorial(n: i64) -> i64 {
            if n <= 1 {
                return 1;
            }
        
            var prev: i64 = factorial(n - 1);
            return n * prev;
        }
        
        
        fn main() -> i64 {
            var x: i64 = factorial(10);
            println("");
        
            return 0;
        }
    "#;

    let hir = hir::compile_hir(&inp)?;
    let mir = hir.compile_mir()?;
    let lir = mir.compile_lir()?;

    println!("{lir:#?}");

    Ok(())
}

#[test]
fn inner_block_test<'src>() -> Result<(), err::CompileError<'src>> {
    let inp = r#"
            fn main() -> i64 { 
            
            var a = 10;
            {
                a += 10;
            }
            
            return 0;
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
        fn main() -> i64 {
            var a = 10;
            while a < 30 {
                a += 5;
            }
            
            println("{a}");
            return 0;
        }
    "#;

    let hir = hir::compile_hir(&inp)?;
    let mir = hir.compile_mir()?;
    let lir = mir.compile_lir()?;

    println!("{lir:#?}");

    Ok(())
}

#[test]
fn if_test<'src>() -> Result<(), err::CompileError<'src>> { 
    
    let inp = r#""#;
    
    Ok(())
}


#[test]
fn bench_3<'src>() -> Result<(), err::CompileError<'src>> {
    let inp = r#"
        fn sieveOfEratosthenes(n: i64) -> Vec<i64> {
            var primes: Vec<i64> = [];
            var i = 0;
            while i <= n {
                vec_push(&primes, 1);
                i += 1;
            }
        
            primes[0] = 0;
            primes[1] = 0;
        
            i = 2;
            while i * i <= n {
                if primes[i] == 1 {
                    var j = i * i;
                    while j <= n {
                        var k = j + 1;
                        primes[j] = 0;
                        j += i;
                    }
                }
                i += 1;
            }
        
            var result: Vec<i64> = [];
            var k: i64 = 0;
            while k <= n {
                if primes[k] == 1 {
                    vec_push(&result, k);
                }
                k += 1;
            }
        
            return result;
        }
        
        fn main() -> i64 {
            var primes: Vec<i64> = sieveOfEratosthenes(50);
        
            var i = 0;
            while i < len(&primes) {
                var x = primes[i];
                
                println("{x}");
                i += 1;
            }
        
            return 0;
        }
    "#;
    
    let hir = hir::compile_hir(&inp)?;
    let mir = hir.compile_mir()?;
    let lir = mir.compile_lir()?;
    
    println!("{lir:#?}");
    
    
    Ok(())
}
