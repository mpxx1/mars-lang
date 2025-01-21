use crate::Hir;
use ast::*;
use err::CompileError;
use pest::{iterators::Pair, Parser};
use pest_derive::Parser;

static mut GLOBAL_COUNTER: usize = 1_000_000;

pub(crate) fn gen_id() -> usize {
    unsafe {
        GLOBAL_COUNTER += 1;
        GLOBAL_COUNTER
    }
}

#[derive(Parser)]
#[grammar = "mars_grammar.pest"]
struct MarsLangParser;

pub(crate) fn parse<'src>(source_code: &'src str) -> Result<Hir<'src>, CompileError<'src>> {
    let mut hir = Hir {
        last_id: 0,
        ast: Ast::default(),
        code: source_code,
    };
    let prog = MarsLangParser::parse(Rule::program, hir.code).unwrap_or_else(|e| {
        println!("{e}");
        std::process::exit(1)
    });

    for pair in prog {
        hir.ast.program.push(match pair.as_rule() {
            Rule::struct_decl => ProgStmt::StructDecl(parse_struct_decl(pair)?),
            Rule::func_decl => ProgStmt::FuncDecl(parse_func_decl(pair)?),
            Rule::EOI => {
                break;
            }
            _ => {
                return Err(CompileError::new(
                    pair.as_span(),
                    "Unexpected rule".to_owned(),
                ));
            }
        })
    }

    hir.last_id = gen_id();
    Ok(hir)
}

fn parse_struct_decl<'src>(pair: Pair<'src, Rule>) -> Result<StructDecl<'src>, CompileError<'src>> {
    let span = pair.as_span();
    let mut decl_iter = pair.into_inner();

    Ok(StructDecl {
        node_id: gen_id(),
        ident: parse_name(decl_iter.next().unwrap())?,
        fields: parse_args_decl(decl_iter.next().unwrap())?,
        span,
    })
}

fn parse_args_decl<'src>(
    pairs: Pair<'src, Rule>,
) -> Result<Vec<ArgDecl<'src>>, CompileError<'src>> {
    pairs
        .into_inner()
        .map(|pair| match pair.as_rule() {
            Rule::arg_decl => parse_arg_decl(pair),
            _ => {
                return Err(CompileError::new(
                    pair.as_span(),
                    "Failed to parse arg decl".to_owned(),
                ));
            }
        })
        .collect::<Result<Vec<_>, CompileError<'src>>>()
}

fn parse_arg_decl<'src>(pair: Pair<'src, Rule>) -> Result<ArgDecl<'src>, CompileError<'src>> {
    let span = pair.as_span();
    let mut inner_iter = pair.into_inner();

    Ok(ArgDecl {
        node_id: gen_id(),
        ident: parse_name(inner_iter.next().unwrap())?,
        ty: parse_type(inner_iter.next().unwrap())?,
        span,
    })
}

fn parse_func_decl<'src>(pair: Pair<'src, Rule>) -> Result<FuncDecl<'src>, CompileError<'src>> {
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

fn parse_name<'src>(pair: Pair<Rule>) -> Result<&str, CompileError<'src>> {
    Ok(pair.as_span().as_str())
}

fn parse_ident<'src>(pair: Pair<Rule>) -> Result<Identifier, CompileError<'src>> {
    let span = pair.as_span();
    Ok(Identifier {
        node_id: gen_id(),
        ident: pair.as_span().as_str(),
        span,
    })
}

fn parse_type<'src>(pair: Pair<'src, Rule>) -> Result<Type<'src>, CompileError<'src>> {
    let span = pair.as_span();
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
                p_iter
                    .next()
                    .unwrap()
                    .as_str()
                    .parse::<usize>()
                    .map_err(|_| {
                        CompileError::new(span, "Can not parse length of array".to_owned())
                    })?,
            )
        }
        Rule::ref_type => Type::Ref(Box::new(parse_type(pair.into_inner().next().unwrap())?)),
        Rule::vec_type => Type::Vec(Box::new(parse_type(pair.into_inner().next().unwrap())?)),
        _ => {
            return Err(CompileError::new(
                span,
                format!("Failed to parse inner type: {:?}", pair.as_rule()),
            ))
        }
    })
}

fn parse_block<'src>(pair: Pair<'src, Rule>) -> Result<Block<'src>, CompileError<'src>> {
    let span = pair.as_span();
    Ok(Block {
        node_id: gen_id(),
        stmts: pair
            .into_inner()
            .map(|p| parse_stmt(p))
            .collect::<Result<Vec<_>, CompileError<'src>>>()?,
        span,
    })
}

fn parse_stmt<'src>(pair: Pair<'src, Rule>) -> Result<Stmt<'src>, CompileError<'src>> {
    let span = pair.as_span();
    match pair.as_rule() {
        Rule::block => parse_block(pair).map(Stmt::Block),
        Rule::r#break => Ok(Stmt::Break {
            node_id: gen_id(),
            span,
        }),
        Rule::struct_decl => parse_struct_decl(pair).map(Stmt::StructDecl),
        Rule::func_decl => parse_func_decl(pair).map(Stmt::FuncDecl),
        Rule::r#return => parse_return(pair),
        Rule::assignment => parse_assignment(pair),
        Rule::assign => parse_assign(pair),
        Rule::func_call => parse_func_call(pair).map(Stmt::FuncCall),
        Rule::if_else => parse_if_else(pair),
        Rule::while_loop => parse_while_loop(pair),
        _ => Err(CompileError::new(
            span,
            "Faild to parse stmt block".to_owned(),
        )),
    }
}

fn parse_return<'src>(pair: Pair<'src, Rule>) -> Result<Stmt<'src>, CompileError<'src>> {
    let span = pair.as_span();
    Ok(Stmt::Return {
        node_id: gen_id(),
        expr: parse_return_body(pair.into_inner().next().unwrap())?,
        span,
    })
}

fn parse_return_body<'src>(
    pair: Pair<'src, Rule>,
) -> Result<Option<Expr<'src>>, CompileError<'src>> {
    if pair.as_str().is_empty() {
        return Ok(None);
    }
    // dbg!(&pair);
    Ok(Some(parse_expr(pair.into_inner().next().unwrap())?))
}

fn parse_assignment<'src>(pair: Pair<'src, Rule>) -> Result<Stmt<'src>, CompileError<'src>> {
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

    let ty = if ty.is_none() {
        Type::Unresolved
    } else {
        ty.unwrap()
    };
    Ok(Stmt::Assignment {
        node_id: gen_id(),
        ident,
        ty,
        expr,
        span,
    })
}

fn parse_assign<'src>(pair: Pair<'src, Rule>) -> Result<Stmt<'src>, CompileError<'src>> {
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

        _ => {
            return Err(CompileError::new(
                span,
                "Failed to parse assignment rule".to_owned(),
            ))
        }
    })
}

fn parse_func_call<'src>(pair: Pair<'src, Rule>) -> Result<FuncCall<'src>, CompileError<'src>> {
    let span = pair.as_span();
    let mut inner_iter = pair.into_inner();

    Ok(FuncCall {
        decl_scope_id: None,
        node_id: gen_id(),
        ident: parse_ident(inner_iter.next().unwrap())?,
        args: parse_func_args_to_call(inner_iter.next().unwrap())?,
        span,
    })
}

fn parse_func_args_to_call<'src>(
    pair: Pair<'src, Rule>,
) -> Result<Vec<Expr<'src>>, CompileError<'src>> {
    Ok(pair
        .into_inner()
        .map(|p| parse_expr(p))
        .collect::<Result<Vec<_>, CompileError<'src>>>()?)
}

fn parse_expr<'src>(pair: Pair<'src, Rule>) -> Result<Expr<'src>, CompileError<'src>> {
    // dbg!(&pair);
    let span = pair.as_span();
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
        _ => return Err(CompileError::new(span, "Failed to parse expr".to_owned())),
    }
}

fn parse_struct_field_call<'src>(pair: Pair<Rule>) -> Result<Expr, CompileError<'src>> {
    let span = pair.as_span();
    let mut inner_iter = pair.into_inner();

    Ok(Expr::StructFieldCall {
        node_id: gen_id(),
        ident: parse_ident(inner_iter.next().unwrap())?,
        field: parse_ident(inner_iter.into_iter().next().unwrap())?,
        span,
    })
}

fn parse_mem_look<'src>(pair: Pair<'src, Rule>) -> Result<Expr<'src>, CompileError<'src>> {
    let span = pair.as_span();
    let mut inner_iter = pair.into_inner();
    let ident = parse_ident(inner_iter.next().unwrap())?;
    let indices = inner_iter
        .map(|p| parse_expr(p))
        .collect::<Result<Vec<_>, CompileError<'src>>>()?;
    Ok(Expr::MemLookup {
        node_id: gen_id(),
        ident,
        indices,
        span,
    })
}

fn parse_arr_decl<'src>(pair: Pair<'src, Rule>) -> Result<Expr<'src>, CompileError<'src>> {
    let span = pair.as_span();
    Ok(Expr::ArrayDecl {
        node_id: gen_id(),
        list: pair
            .into_inner()
            .map(|p| parse_expr(p))
            .collect::<Result<Vec<_>, CompileError<'src>>>()?,
        span,
    })
}

fn parse_deref<'src>(pair: Pair<'src, Rule>) -> Result<Expr<'src>, CompileError<'src>> {
    let span = pair.as_span();
    Ok(Expr::Dereference {
        node_id: gen_id(),
        inner: Box::new(parse_expr(pair.into_inner().next().unwrap())?),
        span,
    })
}

fn parse_reference<'src>(pair: Pair<'src, Rule>) -> Result<Expr<'src>, CompileError<'src>> {
    let span = pair.as_span();
    Ok(Expr::Reference {
        node_id: gen_id(),
        inner: Box::new(parse_expr(pair.into_inner().next().unwrap())?),
        span,
    })
}

fn parse_cast_type<'src>(pair: Pair<'src, Rule>) -> Result<Expr<'src>, CompileError<'src>> {
    let span = pair.as_span();
    let mut inner_iter = pair.into_inner();
    Ok(Expr::CastType {
        node_id: gen_id(),
        cast_to: Box::new(parse_type(inner_iter.next().unwrap())?),
        expr: Box::new(parse_expr(inner_iter.next().unwrap())?),
        span,
    })
}

fn parse_logical_expr<'src>(
    pair: Pair<'src, Rule>,
) -> Result<LogicalExpr<'src>, CompileError<'src>> {
    parse_logical_or_expr(pair.into_inner().next().unwrap())
}

fn parse_logical_or_expr<'src>(
    pair: Pair<'src, Rule>,
) -> Result<LogicalExpr<'src>, CompileError<'src>> {
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
            _ => {
                return Err(CompileError::new(
                    p.as_span(),
                    "failed to parse logical_or expr".to_owned(),
                ))
            }
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

fn parse_logical_and_expr<'src>(
    pair: Pair<'src, Rule>,
) -> Result<LogicalExpr<'src>, CompileError<'src>> {
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
            _ => {
                return Err(CompileError::new(
                    p.as_span(),
                    "Failed to parse logical_amd expr".to_owned(),
                ))
            }
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

fn parse_logical_not_expr<'src>(
    pair: Pair<'src, Rule>,
) -> Result<LogicalExpr<'src>, CompileError<'src>> {
    let span = pair.as_span();
    let mut inner_iter = pair.into_inner();
    if inner_iter.len() == 1 {
        return parse_primary_logical_expr(inner_iter.next().unwrap());
    }
    inner_iter.next().unwrap();

    Ok(LogicalExpr::Not {
        node_id: gen_id(),
        inner: Box::new(parse_primary_logical_expr(inner_iter.next().unwrap())?),
        span,
    })
}

fn parse_primary_logical_expr<'src>(
    pair: Pair<'src, Rule>,
) -> Result<LogicalExpr<'src>, CompileError<'src>> {
    let inner = pair.into_inner().next().unwrap();
    Ok(LogicalExpr::Primary(match inner.as_rule() {
        Rule::comparison_expr => Box::new(Expr::LogicalExpr(parse_cmp_logical_expr(inner)?)),
        Rule::math_expr => Box::new(Expr::MathExpr(parse_math_expr(inner)?)),
        Rule::logical_expr => Box::new(Expr::LogicalExpr(parse_logical_expr(inner)?)),
        _ => {
            return Err(CompileError::new(
                inner.as_span(),
                "Failed to parse logical_primary expr".to_owned(),
            ))
        }
    }))
}

fn parse_cmp_logical_expr<'src>(
    pair: Pair<'src, Rule>,
) -> Result<LogicalExpr<'src>, CompileError<'src>> {
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
            _ => {
                return Err(CompileError::new(
                    p.as_span(),
                    "Failed to parse logical_cmp expr".to_owned(),
                ))
            }
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

fn parse_math_expr<'src>(pair: Pair<'src, Rule>) -> Result<MathExpr<'src>, CompileError<'src>> {
    parse_additive_expr(pair.into_inner().next().unwrap())
}

fn parse_additive_expr<'src>(pair: Pair<'src, Rule>) -> Result<MathExpr<'src>, CompileError<'src>> {
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
            _ => {
                return Err(CompileError::new(
                    p.as_span(),
                    "Failed to parse additive expr".to_owned(),
                ))
            }
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

fn parse_multiplicative_expr<'src>(
    pair: Pair<'src, Rule>,
) -> Result<MathExpr<'src>, CompileError<'src>> {
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
            _ => {
                return Err(CompileError::new(
                    p.as_span(),
                    "Failed to parse multiplicative expr".to_owned(),
                ))
            }
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

fn parse_power_expr<'src>(pair: Pair<'src, Rule>) -> Result<MathExpr<'src>, CompileError<'src>> {
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
            _ => {
                return Err(CompileError::new(
                    p.as_span(),
                    "Failed to parse power expr".to_owned(),
                ))
            }
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

fn parse_primary_expr<'src>(pair: Pair<'src, Rule>) -> Result<MathExpr<'src>, CompileError<'src>> {
    let inner = pair.into_inner().next().unwrap();
    Ok(match inner.as_rule() {
        Rule::literal => MathExpr::Primary(Box::new(Expr::Literal(parse_literal(inner)?))),
        Rule::identifier => MathExpr::Primary(Box::new(Expr::Identifier(parse_ident(inner)?))),
        Rule::struct_field_call => MathExpr::Primary(Box::new(parse_struct_field_call(inner)?)),
        Rule::dereference => MathExpr::Primary(Box::new(parse_deref(inner)?)),
        Rule::math_expr => parse_math_expr(inner)?,
        Rule::cast_type => MathExpr::Primary(Box::new(parse_cast_type(inner)?)),
        Rule::func_call => MathExpr::Primary(Box::new(Expr::FuncCall(parse_func_call(inner)?))),
        Rule::mem_lookup => MathExpr::Primary(Box::new(parse_mem_look(inner)?)),
        _ => {
            return Err(CompileError::new(
                inner.as_span(),
                "Failed to parse primary expr".to_owned(),
            ))
        }
    })
}

fn parse_while_loop<'src>(pair: Pair<'src, Rule>) -> Result<Stmt<'src>, CompileError<'src>> {
    let span = pair.as_span();
    let mut inner = pair.into_inner();
    Ok(Stmt::WhileLoop {
        node_id: gen_id(),
        cond: Box::new(parse_expr(inner.next().unwrap())?),
        body: parse_block(inner.next().unwrap())?,
        span,
    })
}

fn parse_if_else<'src>(pair: Pair<'src, Rule>) -> Result<Stmt<'src>, CompileError<'src>> {
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
        else_id: if else_block.is_some() {
            Some(gen_id())
        } else {
            None
        },
        cond,
        then_block,
        else_block,
        span,
    })
}

fn parse_literal<'src>(pair: Pair<'src, Rule>) -> Result<Literal<'src>, CompileError<'src>> {
    let span = pair.as_span();
    let inner = pair.into_inner().next().unwrap();
    Ok(match inner.as_rule() {
        Rule::int_decl => Literal::Int {
            node_id: gen_id(),
            lit: inner.as_str().parse::<i64>().map_err(|_| {
                CompileError::new(inner.as_span(), "faild to parse i64 literal".to_owned())
            })?,
            span,
        },

        Rule::flt_decl => Literal::Float {
            node_id: gen_id(),
            lit: inner.as_str().parse::<f64>().map_err(|_| {
                CompileError::new(inner.as_span(), "faild to parse f64 literal".to_owned())
            })?,
            span,
        },

        Rule::bool_decl => Literal::Bool {
            node_id: gen_id(),
            lit: inner.as_str().parse::<bool>().map_err(|_| {
                CompileError::new(inner.as_span(), "faild to parse bool literal".to_owned())
            })?,
            span,
        },

        Rule::str_decl => Literal::Str {
            node_id: gen_id(),
            lit: inner.as_str().replace("\"", "").to_string(),
            span,
        },

        Rule::char_decl => Literal::Char {
            node_id: gen_id(),
            lit: inner
                .as_str()
                .replace("'", "")
                .parse::<char>()
                .map_err(|_| {
                    CompileError::new(inner.as_span(), "faild to parse char literal".to_owned())
                })?,
            span,
        },

        Rule::null_decl => Literal::NullRef {
            node_id: gen_id(),
            span,
        },

        _ => {
            return Err(CompileError::new(
                inner.as_span(),
                format!("Unknown literal rule: {:?}", inner.as_rule()),
            ))
        }
    })
}

fn parse_struct_init<'src>(pair: Pair<Rule>) -> Result<Expr, CompileError<'src>> {
    let span = pair.as_span();
    let mut inner_iter = pair.into_inner();

    Ok(Expr::StructInit {
        node_id: gen_id(),
        ident: parse_ident(inner_iter.next().unwrap())?,
        fields: parse_struct_init_args(inner_iter.next().unwrap())?,
        span,
    })
}

fn parse_struct_init_args<'src>(
    pair: Pair<Rule>,
) -> Result<Vec<StructFieldDecl>, CompileError<'src>> {
    Ok(pair
        .into_inner()
        .map(|p| parse_struct_init_arg(p).unwrap())
        .collect())
}

fn parse_struct_init_arg<'src>(
    pair: Pair<'src, Rule>,
) -> Result<StructFieldDecl<'src>, CompileError<'src>> {
    let span = pair.as_span();
    let mut inner_iter = pair.into_inner();
    let ident = parse_ident(inner_iter.next().unwrap())?;
    let expr = parse_expr(inner_iter.next().unwrap())?;
    Ok(StructFieldDecl {
        node_id: gen_id(),
        ident,
        expr,
        span,
    })
}

#[test]
fn liter_test<'src>() -> Result<(), CompileError<'src>> {
    let inp = "fn main() -> void {

        var a = 22;

    }";
    let out = crate::compile_hir(inp)?;
    println!("{:#?}", out.ast);

    Ok(())
}

#[test]
fn test_if_else<'src>() -> Result<(), CompileError<'src>> {
    let inp = r#"
    fn main() -> void {
        var a = 10;
        print(a);
        
        if hello {
            fn hello() -> void {}
        }

        return;
    }
    "#;

    match crate::compile_hir(inp) {
        Ok(out) => {
            println!("{:#?}", out.ast);
            Ok(())
        }
        Err(err) => Err(err),
    }
}

#[test]
fn test_structs<'src>() -> Result<(), CompileError<'src>> {
    let inp = r#"struct Hello {
        a: helo,
    }

    fn main() -> void {
        var a = 10;
        print(a);

        return;
    }
    "#;

    match crate::compile_hir(inp) {
        Ok(out) => {
            println!("{:#?}", out.ast);
            Ok(())
        }
        Err(err) => Err(err),
    }
}

#[test]
fn scopes_test<'src>() -> Result<(), CompileError<'src>> {
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

    let out = crate::compile_hir(inp)?;
    println!("{:#?}", out.ast);

    Ok(())
}

#[test]
fn third_test<'src>() -> Result<(), CompileError<'src>> {
    #[allow(unused)]
    struct MirTest<'src> {
        ast: Ast<'src>,
        code: &'src str,
        example: i32,
    }

    fn translate<'src>(hir: Hir<'src>) -> Result<MirTest<'src>, CompileError<'src>> {
        Ok(MirTest {
            ast: hir.ast,
            code: hir.code,
            example: 11,
        })
    }

    let inp = r#"fn hello() -> void {
      var a = null;
      return;
    }
    "#;

    let hir = crate::compile_hir(inp)?;
    let mir = translate(hir)?;

    println!("{:#?}", mir.ast.program);

    Ok(())
}
