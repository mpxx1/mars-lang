use crate::ast::*;
use anyhow::{anyhow, Result};
use pest::{iterators::Pair, Parser};
use pest_derive::Parser;

static mut GLOBAL_COUNTER: u32 = 1000;

// Глобальная функция
pub fn gen_id() -> u32 {
    unsafe {
        GLOBAL_COUNTER += 1;
        GLOBAL_COUNTER
    }
}

#[derive(Parser)]
#[grammar = "mars_grammar.pest"]
struct MarsLangParser;

pub fn build_ast(source_code: &str) -> Result<AST> {
    let prog = MarsLangParser::parse(Rule::program, source_code)?;

    let mut ast = AST::default();

    for pair in prog {
        ast.program.push(match pair.as_rule() {
            Rule::struct_decl => ProgStmt::StructDecl(parse_struct_decl(pair)?),
            Rule::func_decl => ProgStmt::FuncDecl(parse_func_decl(pair)?),
            Rule::EOI => {
                break;
            }
            _ => {
                return Err(
                    anyhow!("Failed to parse element '{:?}'", pair.as_rule()), // todo span
                );
            }
        })
    }

    Ok(ast)
}

fn parse_struct_decl(pair: Pair<Rule>) -> Result<StructDecl> {
    let span = pair.as_span();
    let mut decl_iter = pair.into_inner();

    Ok(StructDecl {
        node_id: gen_id(),
        ident: parse_name(decl_iter.next().unwrap())?,
        fields: parse_args_decl(decl_iter.next().unwrap())?,
        span,
    })
}

fn parse_args_decl(pairs: Pair<Rule>) -> Result<Vec<ArgDecl>> {
    pairs
        .into_inner()
        .map(|pair| {
            match pair.as_rule() {
                Rule::arg_decl => parse_arg_decl(pair),
                _ => {
                    return Err(anyhow!("Failed to parse arg decl"));
                } // todo span
            }
        })
        .collect::<Result<Vec<_>>>()
}

fn parse_arg_decl(pair: Pair<Rule>) -> Result<ArgDecl> {
    let span = pair.as_span();
    let mut inner_iter = pair.into_inner();

    Ok(ArgDecl {
        node_id: gen_id(),
        ident: parse_name(inner_iter.next().unwrap())?,
        ty: parse_type(inner_iter.next().unwrap())?,
        span,
    })
}

fn parse_func_decl(pair: Pair<Rule>) -> Result<FuncDecl> {
    let span = pair.as_span();
    let mut decl_iter = pair.into_inner();

    Ok(FuncDecl {
        node_id: gen_id(),
        ident: parse_name(decl_iter.next().unwrap())?,
        args: parse_args_decl(decl_iter.next().unwrap())?,
        return_type: parse_type(decl_iter.next().unwrap().into_inner().next().unwrap())?,
        body: parse_block(decl_iter.next().unwrap())?,
        span,
    })
}

fn parse_name(pair: Pair<Rule>) -> Result<&str> {
    Ok(pair.as_span().as_str())
}

fn parse_ident(pair: Pair<Rule>) -> Result<Identifier> {
    let span = pair.as_span();
    Ok(Identifier { node_id: gen_id(), ident: pair.as_span().as_str(), span, })
}

fn parse_type(pair: Pair<Rule>) -> Result<Type> {
    Ok(match pair.as_rule() {
        Rule::str_type => Type::Str,
        Rule::i64_type => Type::I64,
        Rule::f64_type => Type::F64,
        Rule::bool_type => Type::Bool,
        Rule::char_type => Type::Char,
        Rule::void_type => Type::Void,
        Rule::custom_type => Type::Custom(parse_ident(pair)?),
        Rule::array_type => {
            let mut p_iter = pair.into_inner();
            Type::Array(
                Box::new(parse_type(p_iter.next().unwrap())?),
                p_iter.next().unwrap().as_str().parse::<usize>()?,
            )
        }
        Rule::ref_type => Type::Ref(Box::new(parse_type(pair.into_inner().next().unwrap())?)),
        Rule::vec_type => Type::Vec(Box::new(parse_type(pair.into_inner().next().unwrap())?)),
        _ => return Err(anyhow!("Failed to parse type '{:?}'", pair.as_rule())), // todo span
    })
}

fn parse_block(pair: Pair<Rule>) -> Result<Block> {
    let span = pair.as_span();
    Ok(Block {
        node_id: gen_id(),
        stmts: pair
            .into_inner()
            .map(|p| parse_stmt(p))
            .collect::<Result<Vec<_>>>()?,
        span,
    })
}

fn parse_stmt(pair: Pair<Rule>) -> Result<Stmt> {
    let span = pair.as_span();
    match pair.as_rule() {
        Rule::block => parse_block(pair).map(Stmt::Block),
        Rule::r#break => Ok(Stmt::Break { node_id: gen_id(), span, }),
        Rule::struct_decl => parse_struct_decl(pair).map(Stmt::StructDecl),
        Rule::func_decl => parse_func_decl(pair).map(Stmt::FuncDecl),
        Rule::r#return => parse_return(pair),
        Rule::assignment => parse_assignment(pair),
        Rule::assign => parse_assign(pair),
        Rule::func_call => parse_func_call(pair).map(Stmt::FuncCall),
        Rule::if_else => parse_if_else(pair),
        Rule::while_loop => parse_while_loop(pair),
        _ => Err(anyhow::anyhow!(
            "Failed to parse block stmt '{:?}'", // todo scope
            pair.as_rule()
        )),
    }
}

fn parse_return(pair: Pair<Rule>) -> Result<Stmt> {
    let span = pair.as_span();
    Ok(Stmt::Return {
        node_id: gen_id(),
        expr: parse_return_body(pair.into_inner().next().unwrap())?,
        span,
    })
}

fn parse_return_body(pair: Pair<Rule>) -> Result<Option<Expr>> {
    if pair.as_str().is_empty() {
        return Ok(None);
    }
    // dbg!(&pair);
    Ok(Some(parse_expr(pair.into_inner().next().unwrap())?))
}

fn parse_assignment(pair: Pair<Rule>) -> Result<Stmt> {
    let span = pair.as_span();
    let mut inner_iter = pair.into_inner();
    let ident = parse_name(inner_iter.next().unwrap())?;
    let sth = inner_iter.next().unwrap();
    let (ty, expr) = match sth.as_rule() {
        Rule::str_type
        | Rule::vec_type
        | Rule::ref_type
        | Rule::array_type
        | Rule::custom_type
        | Rule::char_type
        | Rule::bool_type
        | Rule::i64_type
        | Rule::f64_type => (
            Some(parse_type(sth)?),
            parse_expr(inner_iter.next().unwrap())?,
        ),
        _ => (None, parse_expr(sth)?),
    };

    Ok(Stmt::Assignment {
        node_id: gen_id(),
        ident,
        ty,
        expr,
        span,
    })
}

fn parse_assign(pair: Pair<Rule>) -> Result<Stmt> {
    let span = pair.as_span();
    let mut inner_iter = pair.into_inner();
    let lhs = parse_expr(inner_iter.next().unwrap())?;
    let op = inner_iter.next().unwrap().into_inner().next().unwrap();
    let rhs = parse_expr(inner_iter.next().unwrap())?;
    let node_id = gen_id();

    Ok(match op.as_rule() {
        Rule::assign_base => Stmt::Assign {
            node_id,
            lhs,
            rhs,
            span,
        },
        Rule::assign_add => Stmt::Assign {
            node_id,
            lhs: lhs.clone(),
            rhs: Expr::MathExpr(MathExpr::Additive {
                node_id: gen_id(),
                left: Box::new(MathExpr::Primary(Box::new(lhs))),
                right: Box::new(MathExpr::Primary(Box::new(rhs))),
                op: AddOp::Add,
                span,
            }),
            span,
        },
        Rule::assign_sub => Stmt::Assign {
            node_id,
            lhs: lhs.clone(),
            rhs: Expr::MathExpr(MathExpr::Additive {
                node_id: gen_id(),
                left: Box::new(MathExpr::Primary(Box::new(lhs))),
                right: Box::new(MathExpr::Primary(Box::new(rhs))),
                op: AddOp::Sub,
                span,
            }),
            span,
        },
        Rule::assign_mul => Stmt::Assign {
            node_id,
            lhs: lhs.clone(),
            rhs: Expr::MathExpr(MathExpr::Multiplicative {
                node_id: gen_id(),
                left: Box::new(MathExpr::Primary(Box::new(lhs))),
                right: Box::new(MathExpr::Primary(Box::new(rhs))),
                op: MulOp::Mul,
                span,
            }),
            span,
        },
        Rule::assign_div => Stmt::Assign {
            node_id,
            lhs: lhs.clone(),
            rhs: Expr::MathExpr(MathExpr::Multiplicative {
                node_id: gen_id(),
                left: Box::new(MathExpr::Primary(Box::new(lhs))),
                right: Box::new(MathExpr::Primary(Box::new(rhs))),
                op: MulOp::Div,
                span,
            }),
            span,
        },
        Rule::assign_div_floor => Stmt::Assign {
            node_id,
            lhs: lhs.clone(),
            rhs: Expr::MathExpr(MathExpr::Multiplicative {
                node_id: gen_id(),
                left: Box::new(MathExpr::Primary(Box::new(lhs))),
                right: Box::new(MathExpr::Primary(Box::new(rhs))),
                op: MulOp::DivFloor,
                span,
            }),
            span,
        },
        Rule::assign_mod => Stmt::Assign {
            node_id,
            lhs: lhs.clone(),
            rhs: Expr::MathExpr(MathExpr::Multiplicative {
                node_id: gen_id(),
                left: Box::new(MathExpr::Primary(Box::new(lhs))),
                right: Box::new(MathExpr::Primary(Box::new(rhs))),
                op: MulOp::Mod,
                span,
            }),
            span,
        },
        Rule::assign_pow => Stmt::Assign {
            node_id,
            lhs: lhs.clone(),
            rhs: Expr::MathExpr(MathExpr::Power {
                node_id: gen_id(),
                base: Box::new(MathExpr::Primary(Box::new(lhs))),
                exp: Box::new(MathExpr::Primary(Box::new(rhs))),
                span,
            }),
            span,
        },
        _ => panic!("Failed to parse assignment rule"), // todo scope
    })
}

fn parse_func_call(pair: Pair<Rule>) -> Result<FuncCall> {
    let span = pair.as_span();
    let mut inner_iter = pair.into_inner();

    Ok(FuncCall {
        node_id: gen_id(),
        ident: parse_ident(inner_iter.next().unwrap())?,
        args: parse_func_args_to_call(inner_iter.next().unwrap())?,
        span,
    })
}

fn parse_func_args_to_call(pair: Pair<Rule>) -> Result<Vec<Expr>> {
    Ok(pair
        .into_inner()
        .map(|p| parse_expr(p))
        .collect::<Result<Vec<_>>>()?)
}

fn parse_expr(pair: Pair<Rule>) -> Result<Expr> {
    // dbg!(&pair);
    match pair.as_rule() {
        Rule::cast_type => parse_cast_type(pair),
        Rule::reference => Ok(parse_reference(pair)?),
        Rule::dereference => Ok(parse_deref(pair)?),
        Rule::func_call => parse_func_call(pair).map(Expr::FuncCall),
        Rule::array_decl => parse_arr_decl(pair),
        Rule::mem_lookup => parse_mem_look(pair),
        Rule::struct_init => parse_struct_init(pair),
        Rule::struct_field_call => parse_struct_field_call(pair),
        Rule::logical_expr => parse_logical_expr(pair).map(Expr::LogicalExpr),
        Rule::math_expr => parse_math_expr(pair).map(Expr::MathExpr),
        Rule::identifier => parse_ident(pair).map(Expr::Identifier),
        _ => Err(anyhow!("Failed to parse expr rule: {:?}", pair.as_rule())),
    }
}

fn parse_struct_field_call(pair: Pair<Rule>) -> Result<Expr> {
    let span = pair.as_span();
    let mut inner_iter = pair.into_inner();

    Ok(Expr::StructFieldCall {
        node_id: gen_id(),
        ident: parse_ident(inner_iter.next().unwrap())?,
        field: parse_ident(inner_iter.into_iter().next().unwrap())?,
        span,
    })
}

fn parse_mem_look(pair: Pair<Rule>) -> Result<Expr> {
    let span = pair.as_span();
    let mut inner_iter = pair.into_inner();
    let ident = parse_ident(inner_iter.next().unwrap())?;
    let indices = inner_iter
        .map(|p| parse_expr(p))
        .collect::<Result<Vec<_>>>()?;
    Ok(Expr::MemLookup {
        node_id: gen_id(),
        ident,
        indices,
        span,
    })
}

fn parse_arr_decl(pair: Pair<Rule>) -> Result<Expr> {
    let span = pair.as_span();
    Ok(Expr::ArrayDecl {
        node_id: gen_id(),
        list: pair
            .into_inner()
            .map(|p| parse_expr(p))
            .collect::<Result<Vec<_>>>()?,
        span,
    })
}

fn parse_deref(pair: Pair<Rule>) -> Result<Expr> {
    let span = pair.as_span();
    Ok(Expr::Dereference {
        node_id: gen_id(),
        inner: Box::new(parse_expr(
        pair.into_inner().next().unwrap(),
        )?),
        span,
    })
}

fn parse_reference(pair: Pair<Rule>) -> Result<Expr> {
    let span = pair.as_span();
    Ok(Expr::Reference {
        node_id: gen_id(),
        inner: Box::new(parse_expr(
        pair.into_inner().next().unwrap(),
        )?),
        span,
    })
}

fn parse_cast_type(pair: Pair<Rule>) -> Result<Expr> {
    let span = pair.as_span();
    let mut inner_iter = pair.into_inner();
    Ok(Expr::CastType {
        node_id: gen_id(),
        cast_to: Box::new(parse_type(inner_iter.next().unwrap())?),
        expr: Box::new(parse_expr(inner_iter.next().unwrap())?),
        span,
    })
}

fn parse_logical_expr(pair: Pair<Rule>) -> Result<LogicalExpr> {
    parse_logical_or_expr(pair.into_inner().next().unwrap())
}

fn parse_logical_or_expr(pair: Pair<Rule>) -> Result<LogicalExpr> {
    let span = pair.as_span();
    let mut inner_iter = pair.into_inner();
    if inner_iter.len() == 1 {
        return parse_logical_and_expr(inner_iter.next().unwrap());
    }
    let mut exprs = vec![];

    for p in inner_iter {
        match p.as_rule() {
            Rule::or_op => continue,
            Rule::logical_and_expr => exprs.push(parse_logical_and_expr(p)?),
            _ => panic!("Failed to parse logical_or expr"),
        }
    }

    exprs.reverse();

    let mut left = exprs.pop().unwrap();
    let mut right = exprs.pop().unwrap();
    let mut res = LogicalExpr::Or {
        node_id: gen_id(),
        left: Box::new(left),
        right: Box::new(right),
        span,
    };

    while !exprs.is_empty() {
        left = res;
        right = exprs.pop().unwrap();
        res = LogicalExpr::Or {
            node_id: gen_id(),
            left: Box::new(left),
            right: Box::new(right),
            span,
        };
    }

    Ok(res)
}

fn parse_logical_and_expr(pair: Pair<Rule>) -> Result<LogicalExpr> {
    let span = pair.as_span();
    let mut inner_iter = pair.into_inner();
    if inner_iter.len() == 1 {
        return parse_logical_not_expr(inner_iter.next().unwrap());
    }
    let mut exprs = vec![];

    for p in inner_iter {
        match p.as_rule() {
            Rule::and_op => continue,
            Rule::logical_not_expr => exprs.push(parse_logical_not_expr(p)?),
            _ => panic!("Failed to parse logical_or expr"),
        }
    }

    exprs.reverse();

    let mut left = exprs.pop().unwrap();
    let mut right = exprs.pop().unwrap();
    let mut res = LogicalExpr::And {
        node_id: gen_id(),
        left: Box::new(left),
        right: Box::new(right),
        span,
    };

    while !exprs.is_empty() {
        left = res;
        right = exprs.pop().unwrap();
        res = LogicalExpr::And {
            node_id: gen_id(),
            left: Box::new(left),
            right: Box::new(right),
            span,
        };
    }

    Ok(res)
}

fn parse_logical_not_expr(pair: Pair<Rule>) -> Result<LogicalExpr> {
    let span = pair.as_span();
    let mut inner_iter = pair.into_inner();
    if inner_iter.len() == 1 {
        return parse_primary_logical_expr(inner_iter.next().unwrap());
    }
    inner_iter.next().unwrap();

    Ok(LogicalExpr::Not {
        node_id: gen_id(),
        inner: Box::new(parse_primary_logical_expr(
        inner_iter.next().unwrap(),
        )?),
        span,
    })
}

fn parse_primary_logical_expr(pair: Pair<Rule>) -> Result<LogicalExpr> {
    let inner = pair.into_inner().next().unwrap();
    Ok(LogicalExpr::Primary(match inner.as_rule() {
        Rule::comparison_expr => Box::new(Expr::LogicalExpr(parse_cmp_logical_expr(inner)?)),
        Rule::math_expr => Box::new(Expr::MathExpr(parse_math_expr(inner)?)),
        Rule::logical_expr => Box::new(Expr::LogicalExpr(parse_logical_expr(inner)?)),
        _ => panic!("Failed to parse primary_logical_expr"),
    }))
}

fn parse_cmp_logical_expr(pair: Pair<Rule>) -> Result<LogicalExpr> {
    let span = pair.as_span();
    let inner_iter = pair.into_inner();

    let mut operation = vec![];
    let mut nums = vec![];

    for p in inner_iter {
        match p.as_rule() {
            Rule::more_eq_op => operation.push(CmpOp::MoreEqual),
            Rule::less_eq_op => operation.push(CmpOp::LessEqual),
            Rule::equal_op => operation.push(CmpOp::Equal),
            Rule::not_equal_op => operation.push(CmpOp::NotEqual),
            Rule::more_op => operation.push(CmpOp::More),
            Rule::less_op => operation.push(CmpOp::Less),
            Rule::math_expr => nums.push(parse_math_expr(p)?),
            _ => panic!("Failed to parse additive expr"),
        }
    }

    let sec = nums.pop().unwrap();
    let fir = nums.pop().unwrap();
    let op = operation.pop().unwrap();

    Ok(LogicalExpr::Comparison {
        node_id: gen_id(),
        left: Box::new(fir),
        right: Box::new(sec),
        op,
        span,
    })
}

fn parse_math_expr(pair: Pair<Rule>) -> Result<MathExpr> {
    parse_additive_expr(pair.into_inner().next().unwrap())
}

fn parse_additive_expr(pair: Pair<Rule>) -> Result<MathExpr> {
    let span = pair.as_span();
    let mut inner_iter = pair.into_inner();
    if inner_iter.len() == 1 {
        return parse_multiplicative_expr(inner_iter.next().unwrap());
    }
    let mut operations = vec![];
    let mut numbers = vec![];

    for p in inner_iter {
        match p.as_rule() {
            Rule::add_op => operations.push(AddOp::Add),
            Rule::sub_op => operations.push(AddOp::Sub),
            Rule::multiplicative_expr => numbers.push(parse_multiplicative_expr(p)?),
            _ => panic!("Failed to parse additive expr"),
        }
    }

    operations.reverse();
    numbers.reverse();

    let mut left = numbers.pop().unwrap();
    let mut right = numbers.pop().unwrap();
    let mut op = operations.pop().unwrap();
    let mut res = MathExpr::Additive {
        node_id: gen_id(),
        left: Box::new(left),
        right: Box::new(right),
        op,
        span,
    };

    while !operations.is_empty() {
        left = res;
        right = numbers.pop().unwrap();
        op = operations.pop().unwrap();
        res = MathExpr::Additive {
            node_id: gen_id(),
            left: Box::new(left),
            right: Box::new(right),
            op,
            span,
        };
    }

    Ok(res)
}

fn parse_multiplicative_expr(pair: Pair<Rule>) -> Result<MathExpr> {
    let span = pair.as_span();
    let mut inner_iter = pair.into_inner();
    if inner_iter.len() == 1 {
        return parse_power_expr(inner_iter.next().unwrap());
    }

    let mut operations = vec![];
    let mut numbers = vec![];

    for p in inner_iter {
        match p.as_rule() {
            Rule::mul_op => operations.push(MulOp::Mul),
            Rule::div_op => operations.push(MulOp::Div),
            Rule::div_floor_op => operations.push(MulOp::DivFloor),
            Rule::mod_op => operations.push(MulOp::Mod),
            Rule::power_expr => numbers.push(parse_power_expr(p)?),
            _ => panic!("Failed to parse multiplicative expr"),
        }
    }

    operations.reverse();
    numbers.reverse();

    let mut left = numbers.pop().unwrap();
    let mut right = numbers.pop().unwrap();
    let mut op = operations.pop().unwrap();
    let mut res = MathExpr::Multiplicative {
        node_id: gen_id(),
        left: Box::new(left),
        right: Box::new(right),
        op,
        span,
    };

    while !operations.is_empty() {
        left = res;
        right = numbers.pop().unwrap();
        op = operations.pop().unwrap();
        res = MathExpr::Multiplicative {
            node_id: gen_id(),
            left: Box::new(left),
            right: Box::new(right),
            op,
            span,
        };
    }

    Ok(res)
}

fn parse_power_expr(pair: Pair<Rule>) -> Result<MathExpr> {
    let span = pair.as_span();
    let mut inner_iter = pair.into_inner();
    if inner_iter.len() == 1 {
        return parse_primary_expr(inner_iter.next().unwrap());
    }

    let mut numbers = vec![];

    for p in inner_iter {
        match p.as_rule() {
            Rule::pow_op => continue,
            Rule::primary_math_expr => numbers.push(parse_primary_expr(p)?),
            _ => panic!("Failed to parse power expr"),
        }
    }

    numbers.reverse();

    let mut base = numbers.pop().unwrap();
    let mut exp = numbers.pop().unwrap();
    let mut res = MathExpr::Power {
        node_id: gen_id(),
        base: Box::new(base),
        exp: Box::new(exp),
        span,
    };

    while !numbers.is_empty() {
        base = res;
        exp = numbers.pop().unwrap();
        res = MathExpr::Power {
            node_id: gen_id(),
            base: Box::new(base),
            exp: Box::new(exp),
            span,
        };
    }

    Ok(res)
}

fn parse_primary_expr(pair: Pair<Rule>) -> Result<MathExpr> {
    let inner = pair.into_inner().next().unwrap();
    Ok(match inner.as_rule() {
        Rule::literal => MathExpr::Primary(Box::new(Expr::Literal(parse_literal(inner)?))),
        Rule::identifier => MathExpr::Primary(Box::new(Expr::Identifier(parse_ident(inner)?))),
        Rule::struct_field_call => MathExpr::Primary(Box::new(
            parse_struct_field_call(inner)?,
        )),
        Rule::dereference => MathExpr::Primary(Box::new(parse_deref(inner)?)),
        Rule::math_expr => parse_math_expr(inner)?,
        Rule::cast_type => MathExpr::Primary(Box::new(parse_cast_type(inner)?)),
        Rule::func_call => MathExpr::Primary(Box::new(Expr::FuncCall(
            parse_func_call(inner)?
        ))),
        Rule::mem_lookup => MathExpr::Primary(Box::new(parse_mem_look(inner)?)),
        _ => panic!("Failed to parse primary expr"),
    })
}

fn parse_while_loop(pair: Pair<Rule>) -> Result<Stmt> {
    let span = pair.as_span();
    let mut inner = pair.into_inner();
    Ok(Stmt::WhileLoop {
        node_id: gen_id(),
        cond: Box::new(parse_expr(inner.next().unwrap())?),
        body: parse_block(inner.next().unwrap())?,
        span,
    })
}

fn parse_if_else(pair: Pair<Rule>) -> Result<Stmt> {
    let span = pair.as_span();
    let mut inner = pair.into_inner();
    let cond = Box::new(parse_expr(inner.next().unwrap())?);
    let then_block = parse_block(inner.next().unwrap())?;
    let mut else_block = None;
    if let Some(block) = inner.next() {
        else_block = Some(parse_block(block)?);
    }

    Ok(Stmt::IfElse {
        node_id: gen_id(),
        cond,
        then_block,
        else_block,
        span,
    })
}

fn parse_literal(pair: Pair<Rule>) -> Result<Literal> {
    let span = pair.as_span();
    let inner = pair.into_inner().next().unwrap();
    Ok(match inner.as_rule() {
        Rule::int_decl => Literal::Int {
            node_id: gen_id(),
            lit: inner.as_str().parse::<i64>()?,
            span,
        },

        Rule::flt_decl => Literal::Float {
            node_id: gen_id(),
            lit: inner.as_str().parse::<f64>()?,
            span,
        },

        Rule::bool_decl => Literal::Bool {
            node_id: gen_id(),
            lit: inner.as_str().parse::<bool>()?,
            span,
        },

        Rule::str_decl => Literal::Str {
            node_id: gen_id(),
            lit: inner.as_str().replace("\"", "").to_string(),
            span,
        },

        Rule::char_decl => Literal::Char {
            node_id: gen_id(),
            lit: inner.as_str().replace("'", "").parse::<char>()?,
            span,
        },

        _ => return Err(anyhow!("Failed to parse literal")), // todo impossible exception
    })
}

fn parse_struct_init(pair: Pair<Rule>) -> Result<Expr> {
    let span = pair.as_span();
    let mut inner_iter = pair.into_inner();

    Ok(Expr::StructInit {
        node_id: gen_id(),
        ident: parse_ident(inner_iter.next().unwrap())?,
        fields: parse_struct_init_args(inner_iter.next().unwrap())?,
        span,
    })
}

fn parse_struct_init_args(pair: Pair<Rule>) -> Result<Vec<StructFieldDecl>> {
    Ok(pair
        .into_inner()
        .map(|p| parse_struct_init_arg(p).unwrap())
        .collect())
}

fn parse_struct_init_arg(pair: Pair<Rule>) -> Result<StructFieldDecl> {
    let span = pair.as_span();
    let mut inner_iter = pair.into_inner();
    let ident = parse_ident(inner_iter.next().unwrap())?;
    let expr = parse_expr(inner_iter.next().unwrap())?;
    Ok(StructFieldDecl { node_id: gen_id(), ident, expr, span })
}

#[test]
fn liter_test() -> Result<()> {
    let inp = "fn main() -> void {

        var a = 22;

    }";
    let out = build_ast(inp)?;
    println!("{out:#?}");

    Ok(())
}

#[test]
fn test() -> Result<()> {
    let inp = r#"struct Hello {
        a: helo,
        b: str
    }

    struct Ola {
        a: helo,
        b: str
    }
    "#;

    let out = build_ast(inp)?;
    println!("{out:#?}");

    Ok(())
}


#[test]
fn scopes_test() -> Result<()> {
    let inp = r#"struct Foo {}

    fn main() -> i64 {
        var a = 10 + b;
        {
            {}
            {
                {}
                var hello = hello();
                {
                    a += hello(halo(10, 30 + 4));
                }
            }
        }

        {
            { a += 10; }
        }

        { var a = 10; }

        return 0;
    }
    "#;

    let out = build_ast(inp)?;
    println!("{out:#?}");

    Ok(())
}
