use anyhow::Result;
use pest::Parser as PestParser;
use pest_derive::Parser as PestParser;


#[derive(PestParser)]
#[grammar = "mars_grammar.pest"]
struct Parser;