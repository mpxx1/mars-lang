pub mod new_block_var_decl;

use std::collections::HashMap;

use new_block_var_decl::block_var_decl;
use ast::*;
use super::s1;

use pest::Span;

pub fn compile_mir_s2(mir: MirS2) -> Result<MirS2, err::CompileError> {
    let mir = block_var_decl(mir)?;
    // todo - check usages
    // todo - check references

    Ok(mir)
}

#[derive(Debug)]
pub struct MirS2<'src> {
    pub code: &'src str,
    pub scopes: HashMap<usize, MIRScope<'src>>,
    pub sys_funs: Vec<String>,
}

#[derive(Debug)]
pub struct MIRScope<'src> {
    pub parent_id: usize,
    pub node_id: usize,
    pub structs: HashMap<String, MIRStruct<'src>>,
    pub funs: HashMap<String, MIRFunc<'src>>,
    pub vars: HashMap<String, MIRVariable>,
    pub instrs: Vec<MIRInstruction<'src>>,
    pub scope_type: MIRScopeType,
}

#[repr(u8)]
#[derive(Debug, Clone)]
pub enum MIRScopeType {
    Global, 
    Block, 
    Function,
}

#[derive(Debug, Clone)]
pub struct MIRStruct<'src> {
    pub node_id: usize,
    pub parent_id: usize,
    pub ident: String,
    pub fields: Vec<MIRArgDecl<'src>>,
    pub span: Span<'src>,
}

#[derive(Debug, Clone)]
pub struct MIRFunc<'src> {
    pub node_id: usize,
    pub parent_id: usize,
    pub ident: String,
    pub args: Vec<MIRArgDecl<'src>>,
    pub return_type: MIRType,
    pub span: Span<'src>,
}

#[derive(Debug, Clone)]
pub struct MIRVariable {
    pub ident: String,
    pub ty: MIRType,
}

#[derive(Debug, Clone)]
pub enum MIRInstruction<'src> {
    Return {
        expr: Option<MIRExpr<'src>>,
        span: Span<'src>,
    },

    Break {
        span: Span<'src>,
    },

    Assignment {
        ident: String,
        ty: MIRType,
        expr: MIRExpr<'src>,
        span: Span<'src>,
    },

    Assign {
        lhs: MIRExpr<'src>, // ident, deref, mem
        rhs: MIRExpr<'src>,
        span: Span<'src>,
    },

    FuncCall(MIRFuncCall<'src>),

    GoToBlock {
        block_id: usize,
    },

    GoToIfCond {
        cond: Box<MIRExpr<'src>>,
        then_block_id: usize,
        else_block_id: Option<usize>,
    },

    GoToWhile {
        cond: Box<MIRExpr<'src>>,
        loop_id: usize,
    },
}

#[derive(Debug, Clone)]
pub struct MIRArgDecl<'src> {
    pub ident: String,
    pub ty: MIRType,
    pub span: Span<'src>,
}

#[derive(Debug, Clone)]
pub enum MIRExpr<'src> {
    
    Identifier { 
        ident: String,
        span: Span<'src>, 
    },
    
    FuncCall(MIRFuncCall<'src>),

    ArrayDecl {
        list: Vec<MIRExpr<'src>>,
        span: Span<'src>,
    },

    MemLookup {
        ident: String,
        indices: Vec<MIRExpr<'src>>,
        span: Span<'src>,
    },

    StructFieldCall {
        decl_scope_id: usize,
        struct_id: usize,
        ident: String,
        field: String,
        span: Span<'src>,
    },

    StructInit {
        decl_scope_id: usize,
        struct_id: usize,
        ident: String,
        fields: Vec<(String, MIRExpr<'src>)>,
        span: Span<'src>,
    },

    CastType {
        cast_to: Box<MIRType>,
        expr: Box<MIRExpr<'src>>,
        span: Span<'src>,
    },

    Dereference {
        inner: Box<MIRExpr<'src>>,
        span: Span<'src>,
    },

    Reference {
        inner: Box<MIRExpr<'src>>,
        span: Span<'src>,
    },

    LogicalExpr(MIRLogicalExpr<'src>),
    MathExpr(MIRMathExpr<'src>),

    Literal(MIRLiteral<'src>),
}

#[derive(Debug, Clone)]
pub enum MIRLogicalExpr<'src> {
    Not {
        inner: Box<MIRLogicalExpr<'src>>,
        span: Span<'src>,
    },

    Or {
        left: Box<MIRLogicalExpr<'src>>,
        right: Box<MIRLogicalExpr<'src>>,
        span: Span<'src>,
    },

    And {
        left: Box<MIRLogicalExpr<'src>>,
        right: Box<MIRLogicalExpr<'src>>,
        span: Span<'src>,
    },

    Comparison {
        left: Box<MIRMathExpr<'src>>,
        right: Box<MIRMathExpr<'src>>,
        op: MIRCmpOp,
        span: Span<'src>,
    },

    Primary(Box<MIRExpr<'src>>),
}

#[derive(Debug, Clone)]
pub enum MIRMathExpr<'src> {
    Additive {
        left: Box<MIRMathExpr<'src>>,
        right: Box<MIRMathExpr<'src>>,
        op: MIRAddOp,
        span: Span<'src>,
    },

    Multiplicative {
        left: Box<MIRMathExpr<'src>>,
        right: Box<MIRMathExpr<'src>>,
        op: MIRMulOp,
        span: Span<'src>,
    },

    Power {
        base: Box<MIRMathExpr<'src>>,
        exp: Box<MIRMathExpr<'src>>,
        span: Span<'src>,
    },

    Primary(Box<MIRExpr<'src>>),
}

#[derive(Debug, Clone)]
pub enum MIRCmpOp {
    Equal,
    NotEqual,
    More,
    MoreEqual,
    Less,
    LessEqual,
}

#[derive(Debug, Clone)]
pub enum MIRAddOp {
    Add,
    Sub,
}

#[derive(Debug, Clone)]
pub enum MIRMulOp {
    Mul,
    Div,
    Mod,
    DivFloor,
}

#[derive(Debug, Clone)]
pub enum MIRLiteral<'src> {
    Int {
        lit: i64,
        span: Span<'src>,
    },

    Float {
        lit: f64,
        span: Span<'src>,
    },

    Str {
        lit: String,
        span: Span<'src>,
    },

    Char {
        lit: char,
        span: Span<'src>,
    },

    Bool {
        lit: bool,
        span: Span<'src>,
    },

    NullRef {
        span: Span<'src>,
    },
}

#[derive(Debug, Clone)]
pub struct MIRFuncCall<'src> {
    decl_scope_id: usize,
    fn_id: usize,
    ident: String,
    args: Vec<MIRExpr<'src>>,
    span: Span<'src>,
}

#[derive(Debug, Clone)]
pub struct MIRStructType {
    pub decl_scope_id: usize,
    pub struct_id: usize,
    pub ident: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MIRType {
    I64,
    F64,
    Str,
    Char,
    Bool,
    Void,
    StructType(MIRStructType),
    // Custom(String),
    Array(Box<MIRType>, usize),
    Vec(Box<MIRType>),
    Ref(Box<MIRType>),
    Any,
}

impl PartialEq for MIRStructType {
    fn eq(&self, other: &Self) -> bool {
        self.decl_scope_id == other.decl_scope_id &&
        self.struct_id == self.struct_id
    }
}

impl From<Type<'_>> for MIRType {
    fn from(ty: Type) -> Self {
        match ty {
            Type::I64 => MIRType::I64,
            Type::F64 => MIRType::F64,
            Type::Str => MIRType::Str,
            Type::Char => MIRType::Char,
            Type::Bool => MIRType::Bool,
            Type::Void => MIRType::Void,
            Type::StructType(x) => MIRType::StructType(x.into()),
            // Type::Custom(id) => MIRType::Custom(id.ident.to_string()),
            Type::Array(inner, size) => MIRType::Array(Box::new((*inner).into()), size),
            Type::Vec(inner) => MIRType::Vec(Box::new((*inner).into())),
            Type::Ref(inner) => MIRType::Ref(Box::new((*inner).into())),
            Type::Any => MIRType::Any,
            _ => panic!("Unsupported type conversion"),
        }
    }
}

impl<'src> From<StructType<'_>> for MIRStructType {
    fn from(s: StructType<'_>) -> Self {
        Self {
            decl_scope_id: s.decl_scope_id,
            struct_id: s.struct_id,
            ident: s.ident.to_owned(),
        }
    }
} 

impl<'src> From<s1::StructProto<'src>> for MIRStruct<'src> {
    fn from(proto: s1::StructProto<'src>) -> Self {
        Self {
            parent_id: proto.parent_id,
            node_id: proto.node_id,
            ident: proto.ident.to_owned(),
            fields: proto.fields.into_iter().map(|x| x.into()).collect(),
            span: proto.span,
        }
    }
}

impl<'src> From<s1::FuncProto<'src>> for MIRFunc<'src> {
    fn from(proto: s1::FuncProto<'src>) -> Self {
        Self {
            parent_id: proto.parent_id,
            node_id: proto.node_id,
            ident: proto.ident.to_owned(),
            args: proto.args.into_iter().map(Into::into).collect(),
            return_type: proto.return_type.into(),
            span: proto.span,
        }
    }
}

impl<'src> From<s1::Variable<'src>> for MIRVariable {
    fn from(v: s1::Variable<'src>) -> Self {
        Self {
            ident: v.ident.to_owned(), 
            ty: v.ty.into(),
        }
    }
}

impl<'src> From<ArgDecl<'src>> for MIRArgDecl<'src> {
    fn from(arg: ArgDecl<'src>) -> Self {
        Self {
            ident: arg.ident.to_string(),
            ty: arg.ty.clone().into(),
            span: arg.span,
        }
    }
}

impl<'src> From<Literal<'src>> for MIRLiteral<'src> {
    fn from(lit: Literal<'src>) -> Self {
        match lit {
            Literal::Int { lit, span, .. } => Self::Int { lit, span },
            Literal::Float { lit, span, .. } => Self::Float { lit, span },
            Literal::Str { lit, span, .. } => Self::Str { lit, span },
            Literal::Char { lit, span, .. } => Self::Char { lit, span },
            Literal::Bool { lit, span, .. } => Self::Bool { lit, span },
            Literal::NullRef { span, .. } => Self::NullRef { span },
        }
    }
}

impl<'src> From<FuncCall<'src>> for MIRFuncCall<'src> {
    fn from(fc: FuncCall<'src>) -> Self {
        Self {
            decl_scope_id: fc.decl_scope_id,
            fn_id: fc.fn_id,
            ident: fc.ident.ident.to_string(),
            args: fc.args.into_iter().map(Into::into).collect(),
            span: fc.span,
        }
    }
}

impl<'src> From<Expr<'src>> for MIRExpr<'src> {
    fn from(expr: Expr<'src>) -> Self {
        match expr {
            Expr::Identifier(id) => Self::Identifier { ident: id.ident.to_string(), span: id.span },
            Expr::FuncCall(fc) => Self::FuncCall(fc.into()),
            
            Expr::ArrayDecl { list, span, .. } => Self::ArrayDecl {
                list: list.into_iter().map(Into::into).collect(),
                span,
            },
            
            Expr::MemLookup { ident, indices, span, .. } => Self::MemLookup {
                ident: ident.ident.to_string(),
                indices: indices.into_iter().map(Into::into).collect(),
                span,
            },
            
            Expr::StructFieldCall { ident, field, span, decl_scope_id, struct_id, .. } => Self::StructFieldCall {
                decl_scope_id,
                struct_id,
                ident: ident.ident.to_string(),
                field: field.ident.to_string(),
                span,
            },
            
            Expr::StructInit { ident, fields, span, decl_scope_id, struct_id, .. } => Self::StructInit {
                decl_scope_id,
                struct_id,
                ident: ident.ident.to_string(),
                fields: fields.into_iter().map(|f| (f.ident.ident.to_owned(), f.expr.into())).collect(),
                span,
            },
            
            Expr::CastType { cast_to, expr, span, .. } => Self::CastType {
                cast_to: Box::new((*cast_to).into()),
                expr: Box::new((*expr).into()),
                span,
            },
            
            Expr::Dereference { inner, span, .. } => Self::Dereference {
                inner: Box::new((*inner).into()),
                span,
            },
            
            Expr::Reference { inner, span, .. } => Self::Reference {
                inner: Box::new((*inner).into()),
                span,
            },
            
            Expr::LogicalExpr(le) => Self::LogicalExpr(le.into()),
            Expr::MathExpr(me) => Self::MathExpr(me.into()),
            Expr::Literal(lit) => Self::Literal(lit.into()),
        }
    }
}

impl<'src> From<LogicalExpr<'src>> for MIRLogicalExpr<'src> {
    fn from(expr: LogicalExpr<'src>) -> Self {
        match expr {
            LogicalExpr::Not { inner, span, .. } => Self::Not {
                inner: Box::new((*inner).into()),
                span,
            },
            
            LogicalExpr::Or { left, right, span, .. } => Self::Or {
                left: Box::new((*left).into()),
                right: Box::new((*right).into()),
                span,
            },
            
            LogicalExpr::And { left, right, span, .. } => Self::And {
                left: Box::new((*left).into()),
                right: Box::new((*right).into()),
                span,
            },
            
            LogicalExpr::Comparison { left, right, op, span, .. } => Self::Comparison {
                left: Box::new((*left).into()),
                right: Box::new((*right).into()),
                op: op.into(),
                span,
            },
            
            LogicalExpr::Primary(expr) => Self::Primary(Box::new((*expr).into())),
        }
    }
}

impl From<CmpOp> for MIRCmpOp {
    fn from(op: CmpOp) -> Self {
        match op {
            CmpOp::Equal => Self::Equal,
            CmpOp::NotEqual => Self::NotEqual,
            CmpOp::More => Self::More,
            CmpOp::MoreEqual => Self::MoreEqual,
            CmpOp::Less => Self::Less,
            CmpOp::LessEqual => Self::LessEqual,
        }
    }
}

impl<'src> From<MathExpr<'src>> for MIRMathExpr<'src> {
    fn from(expr: MathExpr<'src>) -> Self {
        match expr {
            MathExpr::Additive { left, right, op, span, .. } => Self::Additive {
                left: Box::new((*left).into()),
                right: Box::new((*right).into()),
                op: op.into(),
                span,
            },
            
            MathExpr::Multiplicative { left, right, op, span, .. } => Self::Multiplicative {
                left: Box::new((*left).into()),
                right: Box::new((*right).into()),
                op: op.into(),
                span,
            },
            
            MathExpr::Power { base, exp, span, .. } => Self::Power {
                base: Box::new((*base).into()),
                exp: Box::new((*exp).into()),
                span,
            },
            
            MathExpr::Primary(expr) => Self::Primary(Box::new((*expr).into())),
        }
    }
}

impl From<AddOp> for MIRAddOp {
    fn from(op: AddOp) -> Self {
        match op {
            AddOp::Add => Self::Add,
            AddOp::Sub => Self::Sub,
        }
    }
}

impl From<MulOp> for MIRMulOp {
    fn from(op: MulOp) -> Self {
        match op {
            MulOp::Mul => Self::Mul,
            MulOp::Div => Self::Div,
            MulOp::Mod => Self::Mod,
            MulOp::DivFloor => Self::DivFloor,
        }
    }
}

impl From<s1::ScopeType> for MIRScopeType {
    fn from(ty: s1::ScopeType) -> Self {
        match ty {
            s1::ScopeType::Function => MIRScopeType::Function,
            s1::ScopeType::Block => MIRScopeType::Block, 
            s1::ScopeType::Global => MIRScopeType::Global,
        }
    }
}

impl<'src> From<s1::MirS1<'src>> for MirS2<'src> {
    fn from(m: s1::MirS1<'src>) -> Self {
        Self {
            code: m.code,
            scopes: m.scopes.into_iter().map(|(i, s)| (i, s.into())).collect(),
            sys_funs: m.sys_funs.into_iter().map(ToOwned::to_owned).collect(),
        }
    }
}

impl<'src> From<s1::Scope<'src>> for MIRScope<'src> {
    fn from(s: s1::Scope<'src>) -> Self {
        Self {
            parent_id: s.parent_id,
            node_id: s.node_id,
            structs: s.structs.into_iter().map(
                |(i, x)|
                    (i.to_owned(), x.into())
                ).collect(),
            funs: s.funs.into_iter().map(|(i, x)| 
                    (i.to_owned(), x.into())
                ).collect(),
            vars: s.vars.into_iter().map(|(i, x)|
                    (i.to_owned(), x.into())
                ).collect(),
            instrs: s.instrs.into_iter().map(Into::into).collect(),
            scope_type: s.scope_type.into(),
        }
    }
}

impl<'src> From<Stmt<'src>> for MIRInstruction<'src> {
    fn from(stmt: Stmt<'src>) -> Self {
        match stmt {
            Stmt::Return { expr, span, .. } => Self::Return {
                expr: expr.map(Into::into),
                span,
            },
            
            Stmt::Break { span, .. } => Self::Break { span },
            
            Stmt::Assignment { ident, ty, expr, span, .. } => Self::Assignment {
                ident: ident.to_string(),
                ty: ty.into(),
                expr: expr.into(),
                span,
            },
            
            Stmt::Assign { lhs, rhs, span, .. } => Self::Assign {
                lhs: lhs.into(),
                rhs: rhs.into(),
                span,
            },
            
            Stmt::FuncCall(fc) => Self::FuncCall(fc.into()),
            
            Stmt::GoToBlock { node_id } => Self::GoToBlock { block_id: node_id },
            
            Stmt::GoToIfCond { cond, then_block_id, else_block_id } => Self::GoToIfCond {
                cond: Box::new((*cond).into()),
                then_block_id,
                else_block_id,
            },
            
            Stmt::GoToWhile { cond, loop_id } => Self::GoToWhile {
                cond: Box::new((*cond).into()),
                loop_id,
            },
            
            _ => panic!("Unsupported statement conversion"),
        }
    }
}