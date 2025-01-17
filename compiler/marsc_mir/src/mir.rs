use pest::Span;

#[derive(Debug, Copy, Clone, Default)]
pub struct NodeId(pub u32);

#[derive(Debug, Copy, Clone, Default)]
pub struct DefId(pub u32);

static mut GLOBAL_DEF_ID_COUNTER: u32 = 1000;

pub fn gen_def_id() -> DefId {
    unsafe {
        GLOBAL_DEF_ID_COUNTER += 1;
        DefId(GLOBAL_DEF_ID_COUNTER)
    }
}

#[derive(Debug, Clone, Default)]
pub struct MIR<'a> {
    pub program: Vec<ProgStmt<'a>>,
}

#[derive(Debug, Clone)]
pub enum ProgStmt<'a> {
    StructDecl(StructDecl<'a>),
    FuncDecl(FuncDecl<'a>),
}

#[derive(Debug, Clone)]
pub struct StructDecl<'a> {
    pub def_id: DefId,
    pub node_id: NodeId,
    pub ident: &'a str,
    pub fields: Vec<ArgDecl<'a>>,
    pub span: Span<'a>,
}

#[derive(Debug, Clone)]
pub struct FuncDecl<'a> {
    pub def_id: DefId,
    pub node_id: NodeId,
    pub ident: &'a str,
    pub args: Vec<ArgDecl<'a>>,
    pub return_type: Type<'a>,
    pub body: Block<'a>,
    pub span: Span<'a>
}

#[derive(Debug, Clone)]
pub struct ArgDecl<'a> {
    pub def_id: DefId,
    pub node_id: NodeId,
    pub ident: &'a str,
    pub ty: Type<'a>,
    pub span: Span<'a>,
}

#[derive(Debug, Clone)]
pub enum Type<'a> {
    I64,
    F64,
    Str,
    Char,
    Bool,
    Void,
    Custom(Identifier<'a>),
    Array(Box<Type<'a>>, usize),
    Vec(Box<Type<'a>>),
    Ref(Box<Type<'a>>),
}

#[derive(Debug, Clone)]
pub struct Block<'a> {
    pub node_id: NodeId,
    pub stmts: Vec<Stmt<'a>>,
    pub span: Span<'a>,
}

#[derive(Debug, Clone)]
pub enum Stmt<'a> {
    Block(Block<'a>),

    Return {
        node_id: NodeId,
        expr: Option<Expr<'a>>,
        span: Span<'a>
    },

    Break {
        node_id: NodeId,
        span: Span<'a>,
    },

    StructDecl(StructDecl<'a>),

    FuncDecl(FuncDecl<'a>),

    Assignment {
        def_id: DefId,
        node_id: NodeId,
        ident: &'a str,
        ty: Option<Type<'a>>,
        expr: Expr<'a>,
        span: Span<'a>,
    },

    Assign {
        node_id: NodeId,
        lhs: Expr<'a>, // ident, deref, mem
        rhs: Expr<'a>,
        span: Span<'a>,
    },

    FuncCall(FuncCall<'a>),

    IfElse {
        node_id: NodeId,
        cond: Box<Expr<'a>>,
        then_block: Block<'a>,
        else_block: Option<Block<'a>>,
        span: Span<'a>,
    },

    WhileLoop {
        node_id: NodeId,
        cond: Box<Expr<'a>>,
        body: Block<'a>,
        span: Span<'a>,
    },
}

#[derive(Debug, Clone)]
pub enum Expr<'a> {
    Identifier(Identifier<'a>),
    FuncCall(FuncCall<'a>),

    ArrayDecl {
        node_id: NodeId,
        list: Vec<Expr<'a>>,
        span: Span<'a>,
    },

    MemLookup {
        node_id: NodeId,
        ident: Identifier<'a>,
        indices: Vec<Expr<'a>>,
        span: Span<'a>,
    },

    StructFieldCall {
        node_id: NodeId,
        ident: Identifier<'a>,
        field: Identifier<'a>,
        span: Span<'a>,
    },

    StructInit {
        node_id: NodeId,
        ident: Identifier<'a>,
        fields: Vec<StructFieldDecl<'a>>,
        span: Span<'a>,
    },

    CastType {
        node_id: NodeId,
        cast_to: Box<Type<'a>>,
        expr: Box<Expr<'a>>,
        span: Span<'a>,
    },

    Dereference {
        node_id: NodeId,
        inner: Box<Expr<'a>>,
        span: Span<'a>,
    },

    Reference {
        node_id: NodeId,
        inner: Box<Expr<'a>>,
        span: Span<'a>,
    },

    LogicalExpr(LogicalExpr<'a>),
    MathExpr(MathExpr<'a>),

    Literal(Literal<'a>),
}

#[derive(Debug, Clone)]
pub struct Identifier<'a> {
    pub def_id: DefId,
    pub node_id: NodeId,
    pub ident: &'a str,
    pub span: Span<'a>,
}

#[derive(Debug, Clone)]
pub enum Literal<'a> {
    Int {
        node_id: NodeId,
        lit: i64,
        span: Span<'a>
    },

    Float {
        node_id: NodeId,
        lit: f64,
        span: Span<'a>
    },

    Str {
        node_id: NodeId,
        lit: String,
        span: Span<'a>
    },

    Char {
        node_id: NodeId,
        lit: char,
        span: Span<'a>
    },

    Bool {
        node_id: NodeId,
        lit: bool,
        span: Span<'a>
    },
}

#[derive(Debug, Clone)]
pub struct FuncCall<'a> {
    pub node_id: NodeId,
    pub ident: Identifier<'a>,
    pub args: Vec<Expr<'a>>,
    pub span: Span<'a>,
}

#[derive(Debug, Clone)]
pub struct StructFieldDecl<'a> {
    pub node_id: NodeId,
    pub ident: Identifier<'a>,
    pub expr: Expr<'a>,
    pub span: Span<'a>,
}

#[derive(Debug, Clone)]
pub enum LogicalExpr<'a> {
    Not {
        node_id: NodeId,
        inner: Box<LogicalExpr<'a>>,
        span: Span<'a>
    },

    Or {
        node_id: NodeId,
        left: Box<LogicalExpr<'a>>,
        right: Box<LogicalExpr<'a>>,
        span: Span<'a>,
    },

    And {
        node_id: NodeId,
        left: Box<LogicalExpr<'a>>,
        right: Box<LogicalExpr<'a>>,
        span: Span<'a>,
    },

    Comparison {
        node_id: NodeId,
        left: Box<MathExpr<'a>>,
        right: Box<MathExpr<'a>>,
        op: CmpOp,
        span: Span<'a>,
    },

    Primary(Box<Expr<'a>>),
}

#[derive(Debug, Clone)]
pub enum MathExpr<'a> {
    Additive {
        node_id: NodeId,
        left: Box<MathExpr<'a>>,
        right: Box<MathExpr<'a>>,
        op: AddOp,
        span: Span<'a>,
    },

    Multiplicative {
        node_id: NodeId,
        left: Box<MathExpr<'a>>,
        right: Box<MathExpr<'a>>,
        op: MulOp,
        span: Span<'a>,
    },

    Power {
        node_id: NodeId,
        base: Box<MathExpr<'a>>,
        exp: Box<MathExpr<'a>>,
        span: Span<'a>,
    },

    Primary(Box<Expr<'a>>),
}

#[derive(Debug, Clone)]
pub enum CmpOp {
    Equal,
    NotEqual,
    More,
    MoreEqual,
    Less,
    LessEqual,
}

#[derive(Debug, Clone)]
pub enum AddOp {
    Add,
    Sub,
}

#[derive(Debug, Clone)]
pub enum MulOp {
    Mul,
    Div,
    Mod,
    DivFloor,
}
