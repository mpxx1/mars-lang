use pest::Span;

#[derive(Debug, Clone, Default)]
pub struct Ast<'src> {
    pub program: Vec<ProgStmt<'src>>,
}

#[derive(Debug, Clone)]
pub enum ProgStmt<'src> {
    StructDecl(StructDecl<'src>),
    FuncDecl(FuncDecl<'src>),
}

#[derive(Debug, Clone)]
pub struct StructDecl<'src> {
    pub node_id: usize,
    pub ident: &'src str,
    pub fields: Vec<ArgDecl<'src>>,
    pub span: Span<'src>,
}

#[derive(Debug, Clone)]
pub struct FuncDecl<'src> {
    pub node_id: usize,
    pub ident: &'src str,
    pub args: Vec<ArgDecl<'src>>,
    pub return_type: Type<'src>,
    pub body: Block<'src>,
    pub span: Span<'src>,
}

#[derive(Debug, Clone)]
pub struct ArgDecl<'src> {
    pub node_id: usize, // excess
    pub ident: &'src str,
    pub ty: Type<'src>,
    pub span: Span<'src>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type<'src> {
    I64,
    F64,
    Str,
    Char,
    Bool,
    Void,
    Custom(Identifier<'src>),
    Array(Box<Type<'src>>, usize),
    Vec(Box<Type<'src>>),
    Ref(Box<Type<'src>>),
    
    // checker only
    Any,
    Unresolved,
    InnerBlock,
}

#[derive(Debug, Clone)]
pub struct Block<'src> {
    pub node_id: usize,
    pub stmts: Vec<Stmt<'src>>,
    pub span: Span<'src>,
}

#[derive(Debug, Clone)]
pub enum Stmt<'src> {
    Block(Block<'src>),

    Return {
        node_id: usize, // excess
        expr: Option<Expr<'src>>,
        span: Span<'src>,
    },

    Break {
        node_id: usize, // excess
        span: Span<'src>,
    },

    StructDecl(StructDecl<'src>),

    FuncDecl(FuncDecl<'src>),

    Assignment {
        node_id: usize,
        ident: &'src str,
        ty: Type<'src>,
        expr: Expr<'src>,
        span: Span<'src>,
    },

    Assign {
        node_id: usize,  // excess
        lhs: Expr<'src>, // ident, deref, mem
        rhs: Expr<'src>,
        span: Span<'src>,
    },

    FuncCall(FuncCall<'src>),

    IfElse {
        // pub parent_id: usize,
        node_id: usize,
        cond: Box<Expr<'src>>,
        then_block: Block<'src>,
        else_block: Option<Block<'src>>,
        span: Span<'src>,
    },

    WhileLoop {
        // pub parent_id: usize,
        node_id: usize,
        cond: Box<Expr<'src>>,
        body: Block<'src>,
        span: Span<'src>,
    },
}

#[derive(Debug, Clone)]
pub enum Expr<'src> {
    Identifier(Identifier<'src>),
    FuncCall(FuncCall<'src>),

    ArrayDecl {
        node_id: usize,
        list: Vec<Expr<'src>>,
        span: Span<'src>,
    },

    MemLookup {
        node_id: usize,
        ident: Identifier<'src>,
        indices: Vec<Expr<'src>>,
        span: Span<'src>,
    },

    StructFieldCall {
        node_id: usize,
        ident: Identifier<'src>,
        field: Identifier<'src>,
        span: Span<'src>,
    },

    StructInit {
        node_id: usize,
        ident: Identifier<'src>,
        fields: Vec<StructFieldDecl<'src>>,
        span: Span<'src>,
    },

    CastType {
        node_id: usize,
        cast_to: Box<Type<'src>>,
        expr: Box<Expr<'src>>,
        span: Span<'src>,
    },

    Dereference {
        node_id: usize,
        inner: Box<Expr<'src>>,
        span: Span<'src>,
    },

    Reference {
        node_id: usize,
        inner: Box<Expr<'src>>,
        span: Span<'src>,
    },

    LogicalExpr(LogicalExpr<'src>),
    MathExpr(MathExpr<'src>),

    Literal(Literal<'src>),
}

#[derive(Debug, Clone)]
pub struct Identifier<'src> {
    pub node_id: usize,
    pub ident: &'src str,
    pub span: Span<'src>,
}

#[derive(Debug, Clone)]
pub enum Literal<'src> {
    Int {
        node_id: usize,
        lit: i64,
        span: Span<'src>,
    },

    Float {
        node_id: usize,
        lit: f64,
        span: Span<'src>,
    },

    Str {
        node_id: usize,
        lit: String,
        span: Span<'src>,
    },

    Char {
        node_id: usize,
        lit: char,
        span: Span<'src>,
    },

    Bool {
        node_id: usize,
        lit: bool,
        span: Span<'src>,
    },
    
    NullRef {
        node_id: usize,
        span: Span<'src>,
    },
}

#[derive(Debug, Clone)]
pub struct FuncCall<'src> {
    pub node_id: usize,
    pub ident: Identifier<'src>,
    pub args: Vec<Expr<'src>>,
    pub span: Span<'src>,
}

#[derive(Debug, Clone)]
pub struct StructFieldDecl<'src> {
    pub node_id: usize,
    pub ident: Identifier<'src>,
    pub expr: Expr<'src>,
    pub span: Span<'src>,
}

#[derive(Debug, Clone)]
pub enum LogicalExpr<'src> {
    Not {
        node_id: usize,
        inner: Box<LogicalExpr<'src>>,
        span: Span<'src>,
    },

    Or {
        node_id: usize,
        left: Box<LogicalExpr<'src>>,
        right: Box<LogicalExpr<'src>>,
        span: Span<'src>,
    },

    And {
        node_id: usize,
        left: Box<LogicalExpr<'src>>,
        right: Box<LogicalExpr<'src>>,
        span: Span<'src>,
    },

    Comparison {
        node_id: usize,
        left: Box<MathExpr<'src>>,
        right: Box<MathExpr<'src>>,
        op: CmpOp,
        span: Span<'src>,
    },

    Primary(Box<Expr<'src>>),
}

#[derive(Debug, Clone)]
pub enum MathExpr<'src> {
    Additive {
        node_id: usize,
        left: Box<MathExpr<'src>>,
        right: Box<MathExpr<'src>>,
        op: AddOp,
        span: Span<'src>,
    },

    Multiplicative {
        node_id: usize,
        left: Box<MathExpr<'src>>,
        right: Box<MathExpr<'src>>,
        op: MulOp,
        span: Span<'src>,
    },

    Power {
        node_id: usize,
        base: Box<MathExpr<'src>>,
        exp: Box<MathExpr<'src>>,
        span: Span<'src>,
    },

    Primary(Box<Expr<'src>>),
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

impl<'src> PartialEq for Identifier<'src> {
    fn eq(&self, other: &Self) -> bool {
        self.ident == other.ident
    }
}