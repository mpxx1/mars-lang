use pest::Span;

#[derive(Debug, Clone)]
pub enum MIRInstruction<'src> {
    Return {
        expr: Option<MIRExpr<'src>>,
        span: Span<'src>,
    },

    Break {
        span: Span<'src>,
    },

    StructDecl {
        node_id: usize,
        ident: String,
        fields: Vec<MIRArgDecl<'src>>,
        span: Span<'src>,
    },
    
    SysFuncDecl {
        node_id: usize,
        ident: String,
        args: Vec<MIRArgDecl<'src>>,
        return_type: MIRType,
    },

    FuncDecl {
        node_id: usize,
        ident: String,
        args: Vec<MIRArgDecl<'src>>,
        return_type: MIRType,
        body: usize,
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
    Identifier(String),
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
        ident: String,
        field: String,
        span: Span<'src>,
    },

    StructInit {
        ident: String,
        fields: Vec<MIRExpr<'src>>,
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
    ident: String,
    args: Vec<MIRExpr<'src>>,
    span: Span<'src>,
}


#[derive(Debug, Clone, PartialEq)]
pub enum MIRType {
    I64,
    F64,
    Str,
    Char,
    Bool,
    Void,
    Custom(String),
    Array(Box<MIRType>, usize),
    Vec(Box<MIRType>),
    Ref(Box<MIRType>),
    Any,
    Unresolved,
}