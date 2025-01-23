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
    pub node_id: usize, // excess ?
    pub ident: &'src str,
    pub ty: Type<'src>,
    pub span: Span<'src>,
}

#[derive(Debug, Clone)]
pub struct StructType<'src> {
    pub decl_scope_id: usize,
    pub struct_id: usize,
    pub ident: &'src str,
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

    // mir only
    StructType(StructType<'src>),
    Any,
    Unresolved,
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
        node_id: usize, // excess ?
        expr: Option<Expr<'src>>,
        span: Span<'src>,
    },

    Break {
        node_id: usize, // excess ?
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
        node_id: usize,  // excess ?
        lhs: Expr<'src>, // ident, deref, mem
        rhs: Expr<'src>,
        span: Span<'src>,
    },

    FuncCall(FuncCall<'src>),

    IfElse {
        node_id: usize,
        else_id: Option<usize>,
        cond: Box<Expr<'src>>,
        then_block: Block<'src>,
        else_block: Option<Block<'src>>,
        span: Span<'src>,
    },

    WhileLoop {
        node_id: usize,
        cond: Box<Expr<'src>>,
        body: Block<'src>,
        span: Span<'src>,
    },

    // type checker
    GoToBlock {
        node_id: usize,
        // fn return type
    },

    GoToIfCond {
        cond: Box<Expr<'src>>,
        then_block_id: usize,
        else_block_id: Option<usize>,
    },

    GoToWhile {
        cond: Box<Expr<'src>>,
        loop_id: usize,
    },
}

#[derive(Debug, Clone)]
pub enum Expr<'src> {
    Identifier(Identifier<'src>),
    FuncCall(FuncCall<'src>),

    ArrayDecl {
        node_id: usize, // excess ?
        list: Vec<Expr<'src>>,
        span: Span<'src>,
    },

    MemLookup {
        node_id: usize, // excess ?
        ident: Identifier<'src>,    
        indices: Vec<Expr<'src>>,
        span: Span<'src>,
    },

    StructFieldCall {
        decl_scope_id: usize,
        struct_id: usize,
        node_id: usize,             // excess ?
        ident: Identifier<'src>,    
        field: Identifier<'src>,    
        span: Span<'src>,
    },

    StructInit {
        decl_scope_id: usize,
        struct_id: usize,
        node_id: usize, // excess ?
        ident: Identifier<'src>,
        fields: Vec<StructFieldDecl<'src>>,
        span: Span<'src>,
    },

    CastType {
        node_id: usize, // excess ?
        cast_to: Box<Type<'src>>,
        expr: Box<Expr<'src>>,
        span: Span<'src>,
    },

    Dereference {
        node_id: usize, // excess ?
        inner: Box<Expr<'src>>,
        span: Span<'src>,
    },

    Reference {
        node_id: usize, // excess ?
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
    pub decl_scope_id: usize,
    pub fn_id: usize,
    pub node_id: usize, // excess ?
    pub ident: Identifier<'src>,    
    pub args: Vec<Expr<'src>>,
    pub span: Span<'src>,
}

#[derive(Debug, Clone)]
pub struct StructFieldDecl<'src> {
    pub node_id: usize, // excess ?
    pub ident: Identifier<'src>,    
    pub expr: Expr<'src>,
    pub span: Span<'src>,
}

#[derive(Debug, Clone)]
pub enum LogicalExpr<'src> {
    Not {
        node_id: usize, // excess ?
        inner: Box<LogicalExpr<'src>>,
        span: Span<'src>,
    },

    Or {
        node_id: usize, // excess ?
        left: Box<LogicalExpr<'src>>,
        right: Box<LogicalExpr<'src>>,
        span: Span<'src>,
    },

    And {
        node_id: usize, // excess ?
        left: Box<LogicalExpr<'src>>,
        right: Box<LogicalExpr<'src>>,
        span: Span<'src>,
    },

    Comparison {
        node_id: usize, // excess ?
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
        node_id: usize, // excess ?
        left: Box<MathExpr<'src>>,
        right: Box<MathExpr<'src>>,
        op: AddOp,
        span: Span<'src>,
    },

    Multiplicative {
        node_id: usize, // excess ?
        left: Box<MathExpr<'src>>,
        right: Box<MathExpr<'src>>,
        op: MulOp,
        span: Span<'src>,
    },

    Power {
        node_id: usize, // excess ?
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

impl PartialEq for Identifier<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.ident == other.ident
    }
}

impl<'src> PartialEq for StructType<'src> {
    fn eq(&self, other: &Self) -> bool {
        self.ident == other.ident
    }
}
