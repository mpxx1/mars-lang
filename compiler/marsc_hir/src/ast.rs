use pest::Span;

#[derive(Debug, Copy, Clone, Default)]
pub struct NodeId(pub u32);

#[derive(Debug, Clone, Default)]
pub struct AST<'ast> {
    pub program: Vec<ProgStmt<'ast>>,
}

#[derive(Debug, Clone)]
pub enum ProgStmt<'ast> {
    StructDecl(StructDecl<'ast>),
    FuncDecl(FuncDecl<'ast>),
}

#[derive(Debug, Clone)]
pub struct StructDecl<'ast> {
    pub node_id: NodeId,
    pub ident: &'ast str,
    pub fields: Vec<ArgDecl<'ast>>,
    pub span: Span<'ast>,
}

#[derive(Debug, Clone)]
pub struct FuncDecl<'ast> {
    pub node_id: NodeId,
    pub ident: &'ast str,
    pub args: Vec<ArgDecl<'ast>>,
    pub return_type: Type<'ast>,
    pub body: Block<'ast>,
    pub span: Span<'ast>
}

#[derive(Debug, Clone)]
pub struct ArgDecl<'ast> {
    pub node_id: NodeId,
    pub ident: &'ast str,
    pub ty: Type<'ast>,
    pub span: Span<'ast>,
}

#[derive(Debug, Clone)]
pub enum Type<'ast> {
    I64,
    F64,
    Str,
    Char,
    Bool,
    Void,
    Custom(Identifier<'ast>),
    Array(Box<Type<'ast>>, usize),
    Vec(Box<Type<'ast>>),
    Ref(Box<Type<'ast>>),
}

#[derive(Debug, Clone)]
pub struct Block<'ast> {
    pub node_id: NodeId,
    pub stmts: Vec<Stmt<'ast>>,
    pub span: Span<'ast>,
}

#[derive(Debug, Clone)]
pub enum Stmt<'ast> {
    Block(Block<'ast>),

    Return {
        node_id: NodeId,
        expr: Option<Expr<'ast>>,
        span: Span<'ast>
    },

    Break {
        node_id: NodeId,
        span: Span<'ast>,
    },

    StructDecl(StructDecl<'ast>),

    FuncDecl(FuncDecl<'ast>),

    Assignment {
        node_id: NodeId,
        ident: &'ast str,
        ty: Option<Type<'ast>>,
        expr: Expr<'ast>,
        span: Span<'ast>,
    },

    Assign {
        node_id: NodeId,
        lhs: Expr<'ast>, // ident, deref, mem
        rhs: Expr<'ast>,
        span: Span<'ast>,
    },

    FuncCall(FuncCall<'ast>),

    IfElse {
        node_id: NodeId,
        cond: Box<Expr<'ast>>,
        then_block: Block<'ast>,
        else_block: Option<Block<'ast>>,
        span: Span<'ast>,
    },

    WhileLoop {
        node_id: NodeId,
        cond: Box<Expr<'ast>>,
        body: Block<'ast>,
        span: Span<'ast>,
    },
}

#[derive(Debug, Clone)]
pub enum Expr<'ast> {
    Identifier(Identifier<'ast>),
    FuncCall(FuncCall<'ast>),

    ArrayDecl {
        node_id: NodeId,
        list: Vec<Expr<'ast>>,
        span: Span<'ast>,
    },

    MemLookup {
        node_id: NodeId,
        ident: Identifier<'ast>,
        indices: Vec<Expr<'ast>>,
        span: Span<'ast>,
    },

    StructFieldCall {
        node_id: NodeId,
        ident: Identifier<'ast>,
        field: Identifier<'ast>,
        span: Span<'ast>,
    },

    StructInit {
        node_id: NodeId,
        ident: Identifier<'ast>,
        fields: Vec<StructFieldDecl<'ast>>,
        span: Span<'ast>,
    },

    CastType {
        node_id: NodeId,
        cast_to: Box<Type<'ast>>,
        expr: Box<Expr<'ast>>,
        span: Span<'ast>,
    },

    Dereference {
        node_id: NodeId,
        inner: Box<Expr<'ast>>,
        span: Span<'ast>,
    },

    Reference {
        node_id: NodeId,
        inner: Box<Expr<'ast>>,
        span: Span<'ast>,
    },

    LogicalExpr(LogicalExpr<'ast>),
    MathExpr(MathExpr<'ast>),

    Literal(Literal<'ast>),
}

#[derive(Debug, Clone)]
pub struct Identifier<'ast> {
    pub node_id: NodeId,
    pub ident: &'ast str,
    pub span: Span<'ast>,
}

#[derive(Debug, Clone)]
pub enum Literal<'ast> {
    Int {
        node_id: NodeId,
        lit: i64,
        span: Span<'ast>
    },

    Float {
        node_id: NodeId,
        lit: f64,
        span: Span<'ast>
    },

    Str {
        node_id: NodeId,
        lit: String,
        span: Span<'ast>
    },

    Char {
        node_id: NodeId,
        lit: char,
        span: Span<'ast>
    },

    Bool {
        node_id: NodeId,
        lit: bool,
        span: Span<'ast>
    },
}

#[derive(Debug, Clone)]
pub struct FuncCall<'ast> {
    pub node_id: NodeId,
    pub ident: Identifier<'ast>,
    pub args: Vec<Expr<'ast>>,
    pub span: Span<'ast>,
}

#[derive(Debug, Clone)]
pub struct StructFieldDecl<'ast> {
    pub node_id: NodeId,
    pub ident: Identifier<'ast>,
    pub expr: Expr<'ast>,
    pub span: Span<'ast>,
}

#[derive(Debug, Clone)]
pub enum LogicalExpr<'ast> {
    Not {
        node_id: NodeId,
        inner: Box<LogicalExpr<'ast>>,
        span: Span<'ast>
    },

    Or {
        node_id: NodeId,
        left: Box<LogicalExpr<'ast>>,
        right: Box<LogicalExpr<'ast>>,
        span: Span<'ast>,
    },

    And {
        node_id: NodeId,
        left: Box<LogicalExpr<'ast>>,
        right: Box<LogicalExpr<'ast>>,
        span: Span<'ast>,
    },

    Comparison {
        node_id: NodeId,
        left: Box<MathExpr<'ast>>,
        right: Box<MathExpr<'ast>>,
        op: CmpOp,
        span: Span<'ast>,
    },

    Primary(Box<Expr<'ast>>),
}

#[derive(Debug, Clone)]
pub enum MathExpr<'ast> {
    Additive {
        node_id: NodeId,
        left: Box<MathExpr<'ast>>,
        right: Box<MathExpr<'ast>>,
        op: AddOp,
        span: Span<'ast>,
    },

    Multiplicative {
        node_id: NodeId,
        left: Box<MathExpr<'ast>>,
        right: Box<MathExpr<'ast>>,
        op: MulOp,
        span: Span<'ast>,
    },

    Power {
        node_id: NodeId,
        base: Box<MathExpr<'ast>>,
        exp: Box<MathExpr<'ast>>,
        span: Span<'ast>,
    },

    Primary(Box<Expr<'ast>>),
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
