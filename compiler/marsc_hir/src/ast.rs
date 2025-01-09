use pest::Span;

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
    pub ident: String,
    pub fields: Vec<ArgDecl>,
    pub span: Span<'ast>,
}

#[derive(Debug, Clone)]
pub struct FuncDecl<'ast> {
    pub ident: String,
    pub args: Vec<ArgDecl>,
    pub return_type: Type,
    pub body: Block<'ast>,
}

#[derive(Debug, Clone)]
pub struct ArgDecl {
    pub ident: String,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub enum Type {
    I64,
    F64,
    Str,
    Char,
    Bool,
    Void,
    Custom(String),
    Array(Box<Type>, usize),
    Vec(Box<Type>),
    Ref(Box<Type>),
}

#[derive(Debug, Clone)]
pub struct Block<'ast> {
    pub stmts: Vec<Stmt<'ast>>,
}

#[derive(Debug, Clone)]
pub enum Stmt<'ast> {
    Block(Block<'ast>),
    Return(Option<Expr<'ast>>),
    Break,
    StructDecl(StructDecl<'ast>),
    FuncDecl(FuncDecl<'ast>),
    Assignment(Assignment<'ast>),
    Assign(Assign<'ast>),
    FuncCall(FuncCall<'ast>),
    IfElse(IfElse<'ast>),
    Loop(WhileLoop<'ast>),
}

#[derive(Debug, Clone)]
pub struct Assignment<'ast> {
    pub ident: String,
    pub typ: Option<Type>,
    pub expr: Expr<'ast>,
}

#[derive(Debug, Clone)]
pub struct Assign<'ast> {
    pub lhs: Expr<'ast>, // ident, deref, mem
    pub rhs: Expr<'ast>,
}

#[derive(Debug, Clone)]
pub struct MemLookup<'ast> {
    pub ident: String,
    pub indices: Vec<Expr<'ast>>,
}

#[derive(Debug, Clone)]
pub enum Expr<'ast> {
    Identifier(String),
    FuncCall(FuncCall<'ast>),
    ArrayDecl(Vec<Expr<'ast>>),
    MemLookup(MemLookup<'ast>),
    StructFieldCall(StructFieldCall),
    StructInit(StructInit<'ast>),
    IfElse(IfElse<'ast>),
    Loop(WhileLoop<'ast>),
    CastType(CastType<'ast>),
    Dereference(Box<Expr<'ast>>),
    Reference(Box<Expr<'ast>>),

    LogicalExpr(LogicalExpr<'ast>),
    MathExpr(MathExpr<'ast>),

    Literal(Literal),
}

#[derive(Debug, Clone)]
pub struct StructFieldCall {
    pub ident: String,
    pub field: String,
}

#[derive(Debug, Clone)]
pub struct CastType<'ast> {
    pub cast_to: Box<Type>,
    pub expr: Box<Expr<'ast>>,
}

#[derive(Debug, Clone)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Str(String),
    Char(char),
    Bool(bool),
}

#[derive(Debug, Clone)]
pub struct FuncCall<'ast> {
    pub ident: String,
    pub args: Vec<Expr<'ast>>,
}

#[derive(Debug, Clone)]
pub struct StructInit<'ast> {
    pub ident: String,
    pub fields: Vec<(String, Expr<'ast>)>,
}

#[derive(Debug, Clone)]
pub struct IfElse<'ast> {
    pub condition: Box<Expr<'ast>>,
    pub then_block: Block<'ast>,
    pub else_block: Option<Block<'ast>>,
}

#[derive(Debug, Clone)]
pub struct WhileLoop<'ast> {
    pub condition: Box<Expr<'ast>>,
    pub body: Block<'ast>,
}

#[derive(Debug, Clone)]
pub enum LogicalExpr<'ast> {
    Not(Box<LogicalExpr<'ast>>),
    Or(Box<LogicalExpr<'ast>>, Box<LogicalExpr<'ast>>),
    And(Box<LogicalExpr<'ast>>, Box<LogicalExpr<'ast>>),
    Comparison(Box<MathExpr<'ast>>, CmpOp, Box<MathExpr<'ast>>),
    Primary(Box<Expr<'ast>>),
}

#[derive(Debug, Clone)]
pub enum MathExpr<'ast> {
    Additive(Box<MathExpr<'ast>>, AddOp, Box<MathExpr<'ast>>),
    Multiplicative(Box<MathExpr<'ast>>, MulOp, Box<MathExpr<'ast>>),
    Power(Box<MathExpr<'ast>>, Box<MathExpr<'ast>>),
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
