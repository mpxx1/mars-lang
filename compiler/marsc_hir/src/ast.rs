#[derive(Debug, Clone, Default)]
pub struct AST {
    pub program: Vec<ProgStmt>,
}

#[derive(Debug, Clone)]
pub enum ProgStmt {
    StructDecl(StructDecl),
    FuncDecl(FuncDecl),
}

#[derive(Debug, Clone, Default)]
pub struct StructDecl {
    pub name: String,
    pub fields: Vec<ArgDecl>,
}

#[derive(Debug, Clone)]
pub struct FuncDecl {
    pub name: String,
    pub args: Vec<ArgDecl>,
    pub return_type: Type,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub struct ArgDecl {
    pub name: String,
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
pub struct Block {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Block(Block),
    Return(Option<Expr>),
    Break,
    StructDecl(StructDecl),
    FuncDecl(FuncDecl),
    Assignment(Assignment),
    Assign(Assign),
    FuncCall(FuncCall),
    IfElse(IfElse),
    Loop(WhileLoop),
}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub var_name: String,
    pub typ: Option<Type>,
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub struct Assign {
    pub lhs: Expr,      // ident, deref, mem
    pub rhs: Expr,
}

#[derive(Debug, Clone)]
pub struct MemLookup {
    pub identifier: String,
    pub indices: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Identifier(String),
    FuncCall(FuncCall),
    ArrayDecl(Vec<Expr>),
    MemLookup(MemLookup),
    StructFieldCall(StructFieldCall),
    StructInit(StructInit),
    IfElse(IfElse),
    Loop(WhileLoop),
    CastType(CastType),
    Dereference(Box<Expr>),
    Reference(Box<Expr>),

    LogicalExpr(LogicalExpr),
    MathExpr(MathExpr),

    Literal(Literal),
}

#[derive(Debug, Clone)]
pub struct StructFieldCall {
    pub name: String,
    pub field: String,
}

#[derive(Debug, Clone)]
pub struct CastType {
    pub cast_to: Box<Type>,
    pub expr: Box<Expr>
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
pub struct FuncCall {
    pub name: String,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct StructInit {
    pub name: String,
    pub fields: Vec<(String, Expr)>,
}

#[derive(Debug, Clone)]
pub struct IfElse {
    pub condition: Box<Expr>,
    pub then_block: Block,
    pub else_block: Option<Block>,
}

#[derive(Debug, Clone)]
pub struct WhileLoop {
    pub condition: Box<Expr>,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub enum LogicalExpr {
    Not(Box<LogicalExpr>),
    Or(Box<LogicalExpr>, Box<LogicalExpr>),
    And(Box<LogicalExpr>, Box<LogicalExpr>),
    Comparison(Box<MathExpr>, CmpOp, Box<MathExpr>),
    Primary(Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum MathExpr {
    Additive(Box<MathExpr>, AddOp, Box<MathExpr>),
    Multiplicative(Box<MathExpr>, MulOp, Box<MathExpr>),
    Power(Box<MathExpr>, Box<MathExpr>),
    Primary(Box<Expr>),
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
