use crate::expr::Expr;
use m_lex::token::Token;

#[derive(Debug, Clone)]
pub enum Stmt {
    Expression {
        expression: Expr,
    },
    Print {
        expression: Expr,
    },
    Var {
        name: Token,
        initializer: Expr,
    },
    Block {
        statements: Vec<Box<Stmt>>,
    },
    Struct {
        name: Token,
        methods: Vec<Box<Stmt>>,
        superclass: Option<Expr>,
    },
    IfStmt {
        predicate: Expr,
        then: Box<Stmt>,
        els: Option<Box<Stmt>>,
    },
    WhileStmt {
        condition: Expr,
        body: Box<Stmt>,
    },
    Function {
        name: Token,
        params: Vec<Token>,
        body: Vec<Box<Stmt>>,
    },
    CmdFunction {
        name: Token,
        cmd: String,
    },
    ReturnStmt {
        keyword: Token,
        value: Option<Expr>,
    },
}

