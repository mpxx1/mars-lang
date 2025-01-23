use std::collections::HashMap;

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
    pub args: Vec<LIRType>,
    pub return_type: LIRType,
    pub block_id: usize,
}

#[derive(Debug, Clone)]
pub enum LIRInstruction {
    Return { expr: Option<LIRExpr> },
    Break,
    Assignment { ident: String, ty: LIRType, expr: LIRExpr },
    Assign { lhs: LIRExpr, rhs: LIRExpr },
    FuncCall(LIRFuncCall),
    GoToBlock { block_id: usize },
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
    MemLookup { base: String, indices: Vec<LIRExpr>, span_start: usize, span_end: usize, },
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
    Dereference { refer: Box<LIRExpr>, span_start: usize, span_end: usize, },
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

#[derive(Debug, Clone)]
pub struct LIRStructType {
    pub decl_scope_id: usize,
    pub struct_id: usize,
    pub ident: String,
}

impl PartialEq for LIRStructType {
    fn eq(&self, other: &Self) -> bool {
        self.decl_scope_id == other.decl_scope_id &&
        self.struct_id == self.struct_id
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LIRType {
    I64,
    F64,
    Str,
    Char,
    Bool,
    Void,
    StructType(LIRStructType),
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
        let mut blocks = HashMap::new();
        
        for (id, scope) in mir.scopes {
            for (ident, struct_proto) in scope.structs {
                let id = struct_proto.node_id;
                structs.insert(global_name(ident, id), struct_proto.into());
            }
            
            for (ident, fun) in scope.funs {
                let id = fun.node_id;
                functions.insert(global_name(ident, id), fun.into());
            }
            
            blocks.insert(id, vec![]);
            // blocks.insert(id, scope.instrs.into_iter().map(Into::into).collect());
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

fn proceed_expr(expr: MIRExpr<'_>, structs: &HashMap<String, LIRStruct>) -> LIRExpr {
    if let MIRExpr::StructFieldCall { decl_scope_id: _, struct_id, ident, field: _, span: _ } = 
        expr {
            let glob = global_name(ident.clone(), struct_id);
            let indx = structs
                .get(&glob)
                .unwrap()
                .fields
                .iter()
                .enumerate()
                .find(|(_, (id, _))| id == &ident)
                .unwrap().0;
            
            LIRExpr::StructFieldCall { struct_name: glob, field_index: indx }
        } else {
            expr.into()
        }
}

impl<'src> From<MIRExpr<'src>> for LIRExpr {
    fn from(expr: MIRExpr<'src>) -> Self {
        match expr {
            MIRExpr::Identifier { ident, .. } => LIRExpr::Identifier(ident),
            
            MIRExpr::FuncCall(fc) => LIRExpr::FuncCall(fc.into()),
            
            MIRExpr::ArrayDecl { list, .. } => {
                LIRExpr::Array(list.into_iter().map(|e| e.into()).collect())
            }
            
            MIRExpr::MemLookup { ident, indices, span } => LIRExpr::MemLookup {
                base: ident,
                indices: indices.into_iter().map(|e| e.into()).collect(),
                span_start: span.start(),
                span_end: span.end(),
            },
            
            MIRExpr::StructFieldCall { .. } => unreachable!(), 
            
            MIRExpr::StructInit { ident, fields, .. } => LIRExpr::StructInit {
                struct_name: ident,
                fields: fields.into_iter().map(|(_, e)| e.into()).collect(),
            },
            
            MIRExpr::CastType { cast_to, expr, .. } => LIRExpr::Cast {
                ty: Box::new((*cast_to).into()),
                expr: Box::new((*expr).into()),
            },
            
            MIRExpr::Dereference { inner, span } => LIRExpr::Dereference {
                refer: Box::new((*inner).into()),
                span_start: span.start(),
                span_end: span.end(),
            },
            
            MIRExpr::Reference { inner, .. } => LIRExpr::Reference(Box::new((*inner).into())),
            
            MIRExpr::LogicalExpr(le) => LIRExpr::Logical(le.into()),
            
            MIRExpr::MathExpr(me) => LIRExpr::Math(me.into()),
            
            MIRExpr::Literal(l) => LIRExpr::Literal(l.into()),
        }
    }
}

impl<'src> From<MIRFuncCall<'_>> for LIRFuncCall {
    fn from(fc: MIRFuncCall<'_>) -> Self {
        LIRFuncCall {
            function: global_name(fc.ident, fc.fn_id),
            args: fc.args.into_iter().map(|arg| arg.into()).collect(),
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

impl<'src> From<MIRLogicalExpr<'src>> for LIRLogicalExpr {
    fn from(expr: MIRLogicalExpr<'src>) -> Self {
        match expr {
            MIRLogicalExpr::Not { inner, .. } => LIRLogicalExpr::Not {
                inner: Box::new((*inner).into()),
            },
            
            MIRLogicalExpr::Or { left, right, .. } => LIRLogicalExpr::Or {
                left: Box::new((*left).into()),
                right: Box::new((*right).into()),
            },
            
            MIRLogicalExpr::And { left, right, .. } => LIRLogicalExpr::And {
                left: Box::new((*left).into()),
                right: Box::new((*right).into()),
            },
            
            MIRLogicalExpr::Comparison { left, right, op, .. } => LIRLogicalExpr::Comparison {
                left: Box::new((*left).into()),
                right: Box::new((*right).into()),
                op: op.into(),
            },
            
            MIRLogicalExpr::Primary(p) => LIRLogicalExpr::Primary(Box::new((*p).into())),
        }
    }
}

impl<'src> From<MIRMathExpr<'src>> for LIRMathExpr {
    fn from(expr: MIRMathExpr<'src>) -> Self {
        match expr {
            MIRMathExpr::Additive { left, right, op, .. } => LIRMathExpr::Additive {
                left: Box::new((*left).into()),
                right: Box::new((*right).into()),
                op: op.into(),
            },
            
            MIRMathExpr::Multiplicative { left, right, op, .. } => LIRMathExpr::Multiplicative {
                left: Box::new((*left).into()),
                right: Box::new((*right).into()),
                op: op.into(),
            },
            
            MIRMathExpr::Power { base, exp, .. } => LIRMathExpr::Power {
                base: Box::new((*base).into()),
                exp: Box::new((*exp).into()),
            },
            
            MIRMathExpr::Primary(p) => LIRMathExpr::Primary(Box::new((*p).into())),
        }
    }
}

// Преобразования для операторов
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

impl<'src> From<MIRFunc<'_>> for LIRFunc {
    fn from(fun: MIRFunc<'_>) -> Self {
        Self {
            name: global_name(fun.ident, fun.node_id),
            args: fun.args.into_iter().map(|x| x.ty.into()).collect(),
            return_type: fun.return_type.into(),
            block_id: fun.node_id,
        }
    }
}

impl From<MIRStruct<'_>> for LIRStruct {
    fn from(s: MIRStruct<'_>) -> Self {
        let glob = global_name(s.ident, s.node_id);
        Self {
            name: glob.clone(), 
            fields: s.fields.into_iter().map(|x| (x.ident, x.ty.into())).collect(),
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
            
            MIRType::StructType(mir_struct) => LIRType::StructType(LIRStructType {
                decl_scope_id: mir_struct.decl_scope_id,
                struct_id: mir_struct.struct_id,
                ident: global_name(mir_struct.ident, mir_struct.struct_id),
            }),
            
            MIRType::Array(inner, size) => {
                LIRType::Array(Box::new((*inner).into()), size)
            }
            
            MIRType::Vec(inner) => {
                LIRType::Vec(Box::new((*inner).into()))
            }
            
            MIRType::Ref(inner) => {
                LIRType::Ref(Box::new((*inner).into()))
            }
        }
    }
}
