use std::collections::HashMap;

#[derive(Debug)]
pub struct Lir {
    pub structs: HashMap<String, LIRStruct>,
    pub functions: HashMap<String, LIRFunction>,
    pub blocks: HashMap<usize, Vec<LIRInstruction>>,
    pub sys_funs: Vec<String>,
}

#[derive(Debug)]
pub struct LIRStruct {
    pub name: String,
    pub fields: Vec<LIRType>, // order as in decl
}

#[derive(Debug)]
pub struct LIRFunction {
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
    MemLookup { base: String, indices: Vec<LIRExpr> },
    StructField {
        struct_expr: Box<LIRExpr>,
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
    Dereference(Box<LIRExpr>),
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
    Custom(String),
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
use stages::s2::{MIRStruct, MIRType};

fn global_name(ident: String, id: usize) -> String {
    format!("{ident}_{id}")
}

impl From<Mir<'_>> for Lir {
    fn from(mir: Mir) -> Self {
        let mut structs = HashMap::new();
        let mut functions = HashMap::new();
        let mut blocks = HashMap::new();
        
        for (id, scope) in mir.scopes {
            for (ident, struct_proto) in scope.structs {
                let id = struct_proto.node_id;
                structs.insert(global_name(ident, id), struct_proto.into());
            }
        }
        
        Self {
            structs,
            functions,
            blocks,
            sys_funs: mir.sys_funs,
        }
    }
}

impl From<MIRStruct<'_>> for LIRStruct {
    fn from(s: MIRStruct<'_>) -> Self {
        let glob = global_name(s.ident, s.node_id);
        Self {
            name: glob.clone(), 
            fields: s.fields.into_iter().map(|x| x.ty.into).collect(),
        }
    }
}

