mod log;

use std::fmt::format;
use clap::{ArgGroup, Parser, Subcommand};
use hir::ToHir;
use mir::ToMir;
use std::fs;
use std::process::exit;
use std::time::Instant;
use lir::ToLir;
use marsc_codegen::codegen::codegen::codegen;
use crate::log::{log_progress, log_success};

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
        .args(&["hir", "mir", "lir", "release", "llvm_ir"]),
))]
struct BuildArgs {
    #[arg(short, long)]
    release: bool,

    #[arg(long)]
    hir: bool,

    #[arg(long)]
    mir: bool,

    #[arg(long)]
    lir: bool,

    #[arg(long)]
    llvm_ir: bool,

    #[arg(short = 'o', long)]
    output: Option<String>,

    input: String,
}

pub fn main() {
    let start = Instant::now();
    
    let Command::Build(args) = Cli::parse().command;

    if fs::metadata(&args.input).is_err() {
        println!("File '{}' not found", args.input);
        exit(1);
    }

    if !args.input.ends_with(".mars") {
        println!("Can parse .mars files only");
        exit(1);
    }

    let inp = fs::read_to_string(&args.input).unwrap_or_else(|_| {
        println!("Faild to open file {}", &args.input);
        exit(1);
    });

    let hir = log_progress("Parsing", || {
        inp.trim().compile_hir().unwrap_or_else(|e| {
            println!("{e:?}");
            exit(1);
        })
    });

    if args.hir {
        if args.output.is_none() {
            println!("{:#?}", hir.ast.program);
            exit(0);
        }

        let _output = args.output.unwrap();
        // todo - try to create file
        // if !fs::metadata(output).is_ok() {
        //     println!("File '{}' not found", args.input);
        //     exit(1);
        // }

        // todo save to output file
        exit(0);
    }

    let mir = log_progress("MIR", || {
        hir.compile_mir().unwrap_or_else(|e| {
            println!("{e:?}");
            exit(1);
        })
    });

    if args.mir {
        if args.output.is_none() {
            println!("{:#?}", mir.scopes);
            exit(0);
        }

        let _output = args.output.unwrap();
        // todo - try to create file
        // if !fs::metadata(output).is_ok() {
        //     println!("File '{}' not found", args.input);
        //     exit(1);
        // }

        // todo - save to output file
        exit(0);
    }

    let lir = log_progress("LIR", || {
        mir.compile_lir().unwrap_or_else(|e| {
            println!("{e:?}");
            exit(1);
        })
    });

    if args.lir {
        if args.output.is_none() {
            println!("{:#?}", lir);
            exit(0);
        }

        let _output = args.output.unwrap();
        // todo - try to create file
        // if !fs::metadata(output).is_ok() {
        //     println!("File '{}' not found", args.input);
        //     exit(1);
        // }

        // todo - save to output file
        exit(0);
    }

    let output = if let Some(x) = args.output {
        x
    } else {
        args.input[..args.input.len() - 5].to_owned()
    };
    
    // println!("{:#?}", lir);

    let ir = log_progress("Codegen", || {
        codegen(&lir, output.as_str())
    });
    
    // println!("{}", ir);
    
    let build_message = format!("Built in {:?}", start.elapsed());
    log_success(build_message.as_str());
}
