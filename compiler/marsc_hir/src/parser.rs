use crate::ast::*;
use anyhow::{anyhow, Context, Result};
use pest::{ Parser, iterators::Pair };
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "mars_grammar.pest"]
struct MarsLangParser;

pub fn build_ast(source_code: &str) -> Result<AST> {
    let prog = MarsLangParser::parse(Rule::program, source_code)
        .with_context(|| format!("Failed to parse source from '{}'", source_code))?;  // todo span

    let mut ast = AST::default();

    for pair in prog {
        ast.program.push(
            pair.as_rule().match {
                Rule::struct_decl => { ProgStmt::StructDecl(parse_struct_decl(pair)?) }
                Rule::func_decl   => { ProgStmt::FuncDecl(parse_func_decl(pair)?)   }
                // Rule::comment     => { continue }
                Rule::EOI         => { break; }
                _                 => { return Err(
                    anyhow!("Failed to parse element '{:?}'", pair.as_rule()) // todo span
                ); }
            }
        )
    }

    Ok(ast)
}

fn parse_struct_decl(pair: Pair<Rule>) -> Result<StructDecl> {
    let mut decl_iter = pair.into_inner();
    let name = parse_ident(decl_iter.next().unwrap())?;
    let args_pair = decl_iter.next().unwrap();
    let fields = parse_args_decl(args_pair)?;

    Ok( StructDecl { name, fields } )
}

fn parse_args_decl(pairs: Pair<Rule>) -> Result<Vec<ArgDecl>> {
    let mut args = vec![];

    for pair in pairs.into_inner() {
        args.push(
            pair.as_rule().match {
                Rule::arg_decl => { parse_arg_decl(pair)? }
                _ => { return Err(anyhow!("Failed to parse arg decl")); }  // todo span
            }
        )
    }

    Ok(args)
}

fn parse_arg_decl(pair: Pair<Rule>) -> Result<ArgDecl> {
    let mut inner_iter = pair.into_inner();
    let name = parse_ident(inner_iter.next().unwrap())?;
    let typ = parse_type(inner_iter.next().unwrap())?;

    Ok(ArgDecl { name, typ })
}

fn parse_func_decl(pair: Pair<Rule>) -> Result<FuncDecl> {
    let mut decl_iter = pair.into_inner();
    let name = parse_ident(decl_iter.next().unwrap())?;
    let args = parse_args_decl(decl_iter.next().unwrap())?;

    let opt = decl_iter.next().unwrap();
    let (return_type, body) = opt.as_rule().match {
        Rule::return_type => (
            Some(parse_type(opt.into_inner().next().unwrap())?),
            parse_block(decl_iter.next().unwrap())?
        ),
        Rule::block => (
            None,
            parse_block(opt)?
        ),
        _ => { return Err(anyhow!("Failed to parse function decl:\n{:#?}", opt.as_span())); } // todo span
    };

    Ok( FuncDecl { name, args, return_type, body } )
}

fn parse_ident(pair: Pair<Rule>) -> Result<String> {
    Ok(pair.as_span().as_str().to_string())
}

fn parse_type(pair: Pair<Rule>) -> Result<Type> {
    // dbg!(&pair);
    // let rule = pair.as_rule();
    Ok(pair.as_rule().match {
        Rule::i64_type => Type::I64,
        Rule::f64_type => Type::F64,
        Rule::bool_type => Type::Bool,
        Rule::char_type => Type::Char,
        Rule::custom_type => Type::Custom(parse_ident(
            pair.into_inner().next().unwrap()
        )?),
        Rule::array_type => {
            let mut p_iter = pair.into_inner();
            Type::Array(Box::new(
            parse_type(p_iter.next().unwrap())?),
              p_iter.next().unwrap().as_str().parse::<usize>()?
            )
        },
        Rule::ref_type => Type::Ref(Box::new(
            parse_type(
                pair.into_inner().next().unwrap()
            )?
        )),
        Rule::vec_type  => Type::Vec(Box::new(
            parse_type(
                pair.into_inner().next().unwrap()
            )?
        )),
        _ => { return Err(anyhow!("Failed to parse type '{:?}'", pair.as_rule())) } // todo span
    })
}

fn parse_block(pair: Pair<Rule>) -> Result<Block> {
    let stmts = pair
        .into_inner()
        .map(|p| {
            p.as_rule().match {
                Rule::r#break => Ok(Stmt::Break),
                Rule::struct_decl => parse_struct_decl(p).map(Stmt::StructDecl),
                Rule::func_decl => parse_func_decl(p).map(Stmt::FuncDecl),
                Rule::assigment => unimplemented!(),
                Rule::assign => unimplemented!(),
                Rule::stmt_expr => unimplemented!(),
                _ => Err(anyhow::anyhow!(
                    "Failed to parse block stmt '{:?}'",    // todo scope
                    p.as_rule()
                )),
            }
        })
        .collect::<Result<Vec<_>>>()?;

    Ok(Block { stmts })
}


// fn check_return(last_pair: &Pair<Rule>) -> Result<()> {
//     match last_pair.as_rule() {
//         Rule::r#return => {
//             let ret_body = last_pair.clone().into_inner().next().unwrap();
//             if ret_body.into_inner().next().is_none() {
//                 return Err(anyhow!("Return body can not be empty is empty")) // todo span
//             }
//
//             Ok(())
//         },
//         _ => { Err(anyhow!("Failed to parse block")) }  // todo span
//     }
// }

fn parse_expr(_pair: Pair<Rule>) -> Result<Expr> {
    unimplemented!()
}

fn parse_additive_expr(pair: Pair<Rule>) -> Result<MathExpr> {
    let mut inner_iter = pair.into_inner();
    if inner_iter.len() == 1 {
        return Ok(parse_multiplicative_expr(inner_iter.next().unwrap())?);
    }
    let mut operations = vec![];
    let mut numbers = vec![];

    for p in inner_iter {
        p.as_rule().match {
            Rule::add_op => operations.push(AddOp::Add),
            Rule::sub_op => operations.push(AddOp::Sub),
            Rule::multiplicative_expr => numbers.push(parse_multiplicative_expr(p)?),
            _ => panic!("Failed to parse additive expr"),
        }
    }

    operations.reverse();
    numbers.reverse();

    let mut first = numbers.pop().unwrap();
    let mut second = numbers.pop().unwrap();
    let mut op = operations.pop().unwrap();
    let mut tmp = MathExpr::Additive(Box::new(first), op, Box::new(second));

    while !operations.is_empty() {
        first = tmp;
        second = numbers.pop().unwrap();
        op = operations.pop().unwrap();
        tmp = MathExpr::Additive(Box::new(first), op, Box::new(second));
    }

    Ok(tmp)
}

fn parse_multiplicative_expr(pair: Pair<Rule>) -> Result<MathExpr> {
    let mut inner_iter = pair.into_inner();
    if inner_iter.len() == 1 {
        return Ok(parse_power_expr(inner_iter.next().unwrap())?);
    }

    let mut operations = vec![];
    let mut numbers = vec![];

    for p in inner_iter {
        p.as_rule().match {
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

    let mut first = numbers.pop().unwrap();
    let mut second = numbers.pop().unwrap();
    let mut op = operations.pop().unwrap();
    let mut tmp = MathExpr::Multiplicative(Box::new(first), op, Box::new(second));

    while !operations.is_empty() {
        first = tmp;
        second = numbers.pop().unwrap();
        op = operations.pop().unwrap();
        tmp = MathExpr::Multiplicative(Box::new(first), op, Box::new(second));
    }

    Ok(tmp)
}

fn parse_power_expr(pair: Pair<Rule>) -> Result<MathExpr> {
    let mut inner_iter = pair.into_inner();
    if inner_iter.len() == 1 {
        return Ok(parse_primary_expr(inner_iter.next().unwrap())?);
    }

    let mut numbers = vec![];

    for p in inner_iter {
        p.as_rule().match {
            Rule::pow_op => continue,
            Rule::multiplicative_expr => numbers.push(parse_primary_expr(p)?),
            _ => panic!("Failed to parse power expr"),
        }
    }

    numbers.reverse();

    let mut exp = numbers.pop().unwrap();
    let mut base = numbers.pop().unwrap();
    let mut res = MathExpr::Power(Box::new(base), Box::new(exp));

    while !numbers.is_empty() {
        exp = res;
        base = numbers.pop().unwrap();
        res = MathExpr::Power(Box::new(base), Box::new(exp));
    }

    Ok(res)
}

fn parse_primary_expr(pair: Pair<Rule>) -> Result<MathExpr> {
    let inner = pair.into_inner().next().unwrap();
    Ok(inner.as_rule().match {
        Rule::literal => MathExpr::Primary(Box::new(
            Expr::Literal(parse_literal(inner)?)
        )),
        Rule::identifier => MathExpr::Primary(Box::new(
            Expr::Identifier(parse_ident(inner)?),
        )),
        Rule::math_expr => parse_math_expr(inner)?,  // additive,
        _ => panic!("Failed to parse primary expr"),
    })
}

fn parse_while_loop(pair: Pair<Rule>) -> Result<WhileLoop> {
    let mut inner = pair.into_inner();
    Ok(WhileLoop {
        condition: Box::new(parse_expr(inner.next().unwrap())?),
        body: parse_block(inner.next().unwrap())?
    })
}

fn parse_if_else(pair: Pair<Rule>) -> Result<IfElse> {
    let mut inner = pair.into_inner();
    let condition = Box::new(parse_expr(inner.next().unwrap())?);
    let then_block = parse_block(inner.next().unwrap())?;
    let mut else_block = None;
    if let Some(block) = inner.next() {
        else_block = Some(parse_block(block)?);
    }

    Ok(IfElse { condition, then_block, else_block })
}

fn parse_literal(pair: Pair<Rule>) -> Result<Literal> {
    let inner = pair.into_inner().next().unwrap();
    Ok(inner.as_rule().match {
        Rule::int_decl => { Literal::Int(inner.as_str().parse::<i64>()?) }
        Rule::flt_decl => { Literal::Float(inner.as_str().parse::<f64>()?) }
        Rule::bool_decl => { Literal::Bool(inner.as_str().parse::<bool>()?) }
        Rule::str_decl => { Literal::Str(inner.as_str().replace("\"", "").to_string()) }
        Rule::char_decl => { Literal::Char(inner.as_str().replace("'", "").parse::<char>()?) }
        _ => { return Err(anyhow!("Failed to parse literal")) }  // todo impossible exception
    })
}

fn parse_struct_init(pair: Pair<Rule>) -> Result<StructInit> {
    let mut inner_iter = pair.into_inner();

    let name = parse_ident(inner_iter.next().unwrap())?;
    let fields = parse_struct_init_args(inner_iter.next().unwrap())?;

    Ok(StructInit { name, fields })
}

fn parse_struct_init_args(pair: Pair<Rule>) -> Result<Vec<(String, Expr)>> {
    Ok(pair
        .into_inner()
        .map(|p| parse_struct_init_arg(p).unwrap())
        .collect())
}

fn parse_struct_init_arg(pair: Pair<Rule>) -> Result<(String, Expr)> {
    let mut inner_iter = pair.into_inner();
    let name = parse_ident(inner_iter.next().unwrap())?;
    let expr = parse_expr(inner_iter.next().unwrap())?;
    Ok((name, expr))
}

#[test]
fn liter_test() {
    let string = "fn main() { var a = Hello {
          a: alpha,
          b: 42,
          c: true,
        } }";
    let mut it = MarsLangParser::parse(Rule::program, string).unwrap();
    let mut prog = it.next().unwrap().into_inner();
    prog.next().unwrap();
    prog.next().unwrap();
    let mut blk = prog.next().unwrap().into_inner();
    // let mut ass = blk.next().unwrap().into_inner();
    // ass.next().unwrap();
    let mut dst = blk.next().unwrap().into_inner();
    dst.next().unwrap();
    let dst = dst.next().unwrap();

    dbg!(&dst);
    let tst = parse_struct_init(dst).unwrap();
    dbg!(&tst);
}


#[test]
fn test() {
    // let binding = std::fs::read_to_string("notes").unwrap();
    // let f = binding.as_str();
    // let inp = r#"struct Hello { a: i64, b: Hello }"#;
    let inp = r#"fn hello() -> hello {
      struct Hello {
        a: [i64; 14],
      }

      return Hello { a: 123 }
    }"#;
    let out = build_ast(inp);

    dbg!(out);
}
