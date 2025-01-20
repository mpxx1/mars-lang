use clap::{Parser, ArgGroup, Subcommand};
use hir::ToHir;
use mir::ToMir;
use std::process::exit;
use std::fs;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand, Debug)]
enum Command {
    #[command(aliases = ["b"])]
    Build(BuildArgs),
}

#[derive(Debug, Parser)]
#[command(group(
    ArgGroup::new("flags")
        .required(false)
        .args(&["hir", "mir", "lir"]),
))]
struct BuildArgs {
    #[arg(short = 'O', long)]
    optimize: bool,
    
    #[arg(long)]
    hir: bool,

    #[arg(long)]
    mir: bool,
    
    #[arg(long)]
    lir: bool,

    #[arg(short = 'o', long)]
    output: Option<String>,

    input: String,
}


fn main() {    
    let Command::Build(args) = Cli::parse().command;
    
    if !fs::metadata(&args.input).is_ok() {
        println!("File '{}' not found", args.input);
        exit(1);
    }
    
    if !args.input.ends_with(".mars") {
        println!("Can parse .mars files only");
        exit(1);
    }
    
    let inp = fs::read_to_string(&args.input)
        .unwrap_or_else(|_| {
            println!("Faild to open file {}", &args.input);
            exit(1);
        });
    
    let hir = inp.trim().compile_hir().unwrap_or_else(|e| {
        println!("{e:?}");
        exit(1);
    });
    
    if args.hir {
        if args.output.is_none() {
            println!("{:?}", hir.ast.program);
            exit(0);
        }
        
        let output = args.output.unwrap();
        if !fs::metadata(output).is_ok() {
            println!("File '{}' not found", args.input);
            exit(1);
        }
        
        // save to output file
        exit(0);
    }
    
    let mir = hir.compile_mir().unwrap_or_else(|e| {
        println!("{e:?}");
        exit(1);
    });
    
    if args.mir {
        if args.output.is_none() {
            println!("{:?}", mir.scopes);
            exit(0);
        }
        
        let output = args.output.unwrap();
        if !fs::metadata(output).is_ok() {
            println!("File '{}' not found", args.input);
            exit(1);
        }
        
        // save to output file
        exit(0);
    }
    
    let _output = if let Some(x) = args.output { 
        x
    } else { 
        args.input[..args.input.len() - 5].to_owned()
    };
}

