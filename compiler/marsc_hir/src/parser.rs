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

    for pair in prog.into_iter() {
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
    let mut decl_iter = pair.into_inner().into_iter();
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
    let mut inner_iter = pair.into_inner().into_iter();
    let name = parse_ident(inner_iter.next().unwrap())?;
    let typ = parse_type(inner_iter.next().unwrap())?;

    Ok(ArgDecl { name, typ })
}

fn parse_func_decl(pair: Pair<Rule>) -> Result<FuncDecl> {
    let mut decl_iter = pair.into_inner().into_iter();
    let name = parse_ident(decl_iter.next().unwrap())?;
    let args = parse_args_decl(decl_iter.next().unwrap())?;

    let opt = decl_iter.next().unwrap();
    let (return_type, body) = opt.as_rule().match {
        Rule::return_type => (
            Some(parse_type(opt.into_inner().into_iter().next().unwrap())?),
            parse_block(decl_iter.next().unwrap(), true)?
        ),
        Rule::block => (
            None,
            parse_block(opt, false)?
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
            pair.into_inner().into_iter().next().unwrap()
        )?),
        Rule::array_type => {
            let mut p_iter = pair.into_inner().into_iter();
            Type::Array(Box::new(
            parse_type(p_iter.next().unwrap())?),
              p_iter.next().unwrap().as_str().parse::<usize>()?
            )
        },
        Rule::func_type => unimplemented!(),
        Rule::ref_type => Type::Ref(Box::new(
            parse_type(
                pair.into_inner().into_iter().next().unwrap()
            )?
        )),
        Rule::size_type => Type::Size,
        Rule::u8_type   => Type::U8,
        Rule::vec_type  => Type::Vec(Box::new(
            parse_type(
                pair.into_inner().into_iter().next().unwrap()
            )?
        )),
        _ => { return Err(anyhow!("Failed to parse type '{:?}'", pair.as_rule())) } // todo span
    })
}

fn parse_block(pair: Pair<Rule>, must_return: bool) -> Result<Block> {
    // dbg!(pair, must_return);
    let mut block = Block { stmts: vec![] };
    let mut inner_iter = pair.into_inner().peekable();

    // collect
    while let Some(pair) = inner_iter.next() {
        if inner_iter.peek().is_none() {
            // if must return, then return_body must not be empty
            // else if must not return, then return body can be empty or, there can
            // be no return body at all
            check_return(&pair)?
            // stmt_expr (return)
        } else {
            block.stmts.push(
                pair.as_rule().match {
                    Rule::r#break => { Stmt::Break }
                    Rule::struct_decl => { Stmt::StructDecl(parse_struct_decl(pair)?) }
                    Rule::func_decl   => { Stmt::FuncDecl(parse_func_decl(pair)?) }
                    Rule::assigment => unimplemented!(),
                    Rule::assign => unimplemented!(),
                    Rule::stmt_expr => unimplemented!(),
                    _ => return Err(anyhow!("Failed to parse block stmt '{:?}'", pair.as_rule())), // todo span
                }
            )
        }
    }

    Ok(block)
}

fn check_return(last_pair: &Pair<Rule>) -> Result<()> {
    match last_pair.as_rule() {
        Rule::r#return => {
            let ret_body = last_pair.clone().into_inner().into_iter().next().unwrap();
            if ret_body.into_inner().into_iter().next().is_none() {
                return Err(anyhow!("Return body can not be empty is empty")) // todo span
            }

            Ok(())
        },
        _ => { Err(anyhow!("Failed to parse block")) }  // todo span
    }
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
