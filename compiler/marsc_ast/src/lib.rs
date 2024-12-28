use anyhow::Result;
use pest::Parser;
use pest_derive::Parser;


#[derive(Parser)]
#[grammar = "mars_grammar.pest"]
struct MarsLangParser;