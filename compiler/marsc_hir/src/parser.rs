use crate::ast::Type::{Bool, Char, Custom, Size, F64, I64, U8};
use crate::ast::*;
use anyhow::{anyhow, Context, Result};
use pest::{iterators::Pair, Parser};
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "mars_grammar.pest"]
struct MarsLangParser;

pub fn build_ast(source_code: &str) -> Result<AST> {
    let prog = MarsLangParser::parse(Rule::program, source_code)
        .with_context(|| format!("Failed to parse source from '{}'", source_code))?;

    let mut ast = AST::default();

    for pair in prog.into_iter() {
        ast.program.push(pair.as_rule().match {
            Rule::struct_decl => parse_struct_decl(pair)?,
            Rule::func_decl => parse_func_decl(pair)?,
            Rule::comment => continue,
            Rule::EOI => {
                break;
            }
            _ => {
                return Err(anyhow!("Failed to parse element '{:?}'", pair.as_rule()));
            }
        })
    }

    Ok(ast)
}

fn parse_struct_decl(pair: Pair<Rule>) -> Result<ProgStmt> {
    let mut decl_iter = pair.into_inner().into_iter();
    let name = parse_ident(decl_iter.next().unwrap())?;
    let args_pair = decl_iter.next().unwrap();
    let fields = parse_args_decl(args_pair)?;

    Ok(ProgStmt::StructDecl(StructDecl { name, fields }))
}

fn parse_args_decl(pairs: Pair<Rule>) -> Result<Vec<ArgDecl>> {
    let mut args = vec![];

    for pair in pairs.into_inner() {
        args.push(pair.as_rule().match {
            Rule::arg_decl => parse_arg_decl(pair)?,
            _ => {
                return Err(anyhow!("Failed to parse arg decl"));
            }
        })
    }

    Ok(args)
}

fn parse_arg_decl(pair: Pair<Rule>) -> Result<ArgDecl> {
    let mut inner_iter = pair.into_inner().into_iter();
    let name = parse_ident(inner_iter.next().unwrap())?;
    let typ = parse_type(inner_iter.next().unwrap())?;

    Ok(ArgDecl { name, typ })
}

fn parse_func_decl(pair: Pair<Rule>) -> Result<ProgStmt> {
    let mut decl_iter = pair.into_inner().into_iter();
    let name = parse_ident(decl_iter.next().unwrap())?;
    let args = parse_args_decl(decl_iter.next().unwrap())?;

    let opt = decl_iter.next().unwrap();
    let (return_type, body) = opt.as_rule().match {
        Rule::return_type => (
            Some(parse_type(opt.into_inner().into_iter().next().unwrap())?),
            parse_block(decl_iter.next().unwrap(), true)?,
        ),
        Rule::block => (None, parse_block(opt, false)?),
        _ => {
            return Err(anyhow!(
                "Failed to parse function decl:\n{:#?}",
                opt.as_span()
            ));
        }
    };

    Ok(ProgStmt::FuncDecl(FuncDecl {
        name,
        args,
        return_type,
        body,
    }))
}

fn parse_ident(pair: Pair<Rule>) -> Result<String> {
    Ok(pair.as_span().as_str().to_string())
}

fn parse_type(pair: Pair<Rule>) -> Result<Type> {
    // dbg!(&pair);
    // let rule = pair.as_rule();
    Ok(pair.as_rule().match {
        Rule::i64_type => I64,
        Rule::f64_type => F64,
        Rule::bool_type => Bool,
        Rule::char_type => Char,
        Rule::custom_type => Custom(parse_ident(pair.into_inner().into_iter().next().unwrap())?),
        Rule::array_type => unimplemented!(),
        Rule::func_type => unimplemented!(),
        Rule::ref_type => unimplemented!(),
        Rule::size_type => Size,
        Rule::u8_type => U8,
        Rule::vec_type => unimplemented!(),
        _ => return Err(anyhow!("Failed to parse type '{:?}'", pair.as_rule())),
    })
}

fn parse_block(pair: Pair<Rule>, must_return: bool) -> Result<Block> {
    dbg!(pair, must_return);
    unimplemented!();
}

#[test]
fn test() {
    // let binding = std::fs::read_to_string("notes").unwrap();
    // let f = binding.as_str();
    // let inp = r#"struct Hello { a: i64, b: Hello }"#;
    let inp = r#"fn hello() {  }"#;
    let out = build_ast(inp);

    dbg!(out);
}
