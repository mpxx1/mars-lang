use clap::Parser;

#[derive(Parser, Debug)]
#[command(name = "mars-lang compiler")]
#[command(version = "1.0")]
#[command(author = "mars-lang team")]
#[command(about = "A simple compiler for demonstration purposes", long_about = None)]
pub struct Args {
    #[arg(value_hint = ValueHint::FilePath)]
    pub input: String,

    #[arg(short, long)]
    pub output: String,
}
