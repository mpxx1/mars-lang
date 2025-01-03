use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "mars_grammar.pest"]
struct MarsLangParser;

fn build_ast(source_code: &str) {
    let out = MarsLangParser::parse(Rule::program, source_code);
    let out = out.unwrap();
    println!("{:#?}", out);
}


#[test]
fn test() {
    let binding = std::fs::read_to_string("notes").unwrap();
    let f = binding.as_str();
    build_ast(f)
}