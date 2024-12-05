use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use m_lex::token::Token;
use crate::stmt::Stmt;


#[derive(Debug, Clone)]
pub enum CallableImpl {
    LoxFunction(LoxFunctionImpl),
    NativeFunction(NativeFunctionImpl),
}

#[derive(Debug, Clone)]
pub struct LoxFunctionImpl {
    pub name: String,
    pub arity: usize,
    // pub parent_env: Environment,
    pub params: Vec<Token>,
    pub body: Vec<Box<Stmt>>,
}

#[derive(Debug, Clone)]
pub struct NativeFunctionImpl {
    pub name: String,
    pub arity: usize,
    // pub fun: Rc<dyn Fn(&Vec<LiteralValue>) -> LiteralValue>,
}

#[derive(Debug, Clone)]
pub enum LiteralValue {
    Float63(f64),
    Int63(i64),
    StringValue(String),
    True,
    False,
    Null,
    Callable(CallableImpl),
    LoxStruct {
        name: String,
        methods: HashMap<String, LoxFunctionImpl>,
        //methods: Vec<(String, LiteralValue)>, // TODO Could also add static fields?
    },
    LoxInstance {
        class: Box<LiteralValue>,
        fields: Rc<RefCell<Vec<(String, LiteralValue)>>>,
    },
}

#[derive(Debug, Clone)]
pub enum Expr {
    AnonFunction {
        id: usize,
        paren: Token,
        arguments: Vec<Token>,
        body: Vec<Box<Stmt>>,
    },
    Assign {
        id: usize,
        name: Token,
        value: Box<Expr>,
    },
    Binary {
        id: usize,
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    // 2 + 2 |> f
    Call {
        // x |> f -> Call { id, f, paren (pipe), arguments: [x]}
        id: usize,
        callee: Box<Expr>,
        paren: Token,
        arguments: Vec<Expr>,
    },
    Get {
        id: usize,
        object: Box<Expr>,
        name: Token,
    },
    Grouping {
        id: usize,
        expression: Box<Expr>,
    },
    Literal {
        id: usize,
        value: LiteralValue,
    },
    Logical {
        id: usize,
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Set {
        id: usize,
        object: Box<Expr>,
        name: Token,
        value: Box<Expr>,
    },
    This {
        id: usize,
        keyword: Token,
    },
    Super {
        id: usize,
        keyword: Token,
        method: Token,
    },
    Unary {
        id: usize,
        operator: Token,
        right: Box<Expr>,
    },
    Variable {
        id: usize,
        name: Token,
    },
}
