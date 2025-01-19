extern crate marsc_proc_macro;

mod args;
mod config;
mod log;

use crate::args::Args;
use crate::config::build_config;
use clap::Parser;
use marsc_interface::linker::Linker;
use marsc_interface::passes::create_and_enter_global_context;
use marsc_interface::{interface, passes};
use std::process;
use std::time::Instant;
use anyhow::Result;
use marsc_codegen::codegen::codegen;
use marsc_mir::provider::TypeContextProviders;
use marsc_mir::stages::TypeCheckerProvider;
use crate::log::{log_progress, log_success};

pub struct RunCompiler<'a> {
    args: &'a Args,
    // file_loader: Option<Box<dyn FileLoader + Send + Sync>>,
}

impl<'a> RunCompiler<'a> {
    pub fn new(args: &'a Args) -> Self {
        RunCompiler { args }
    }

    pub fn run(&self) -> Result<()> {
        run_compiler(self.args)
    }
}

fn run_compiler(args: &Args) -> Result<()> {
    let config = build_config(args);

    interface::run_compiler(config, |compiler| {
        let session = &compiler.session;
        // let codegen_backend = &*compiler.codegen_backend;

        let hir = log_progress("Parsing", || {
            passes::parse(session)
        });
        
        let linker = create_and_enter_global_context(compiler, |type_context| {
            let mir = log_progress("Checking", || {
                type_context.providers().check_types(hir).unwrap()
            });
            
            let generated = log_progress("Generating", || {
                codegen(&mir, &session.io.output_file)
            });
            
            println!("{}", generated);
            
            Some(Linker {})
        });

        if let Some(_linker) = linker {
            // linker.link(session, codegen_backend) TODO
        }

        Ok(())
    })
}

pub fn main() -> ! {
    let start_time = Instant::now();
    
    let args = Args::parse();

    RunCompiler::new(&args).run().expect(""); // TODO


    let built_message = format!("Built in {:?}", start_time.elapsed());
    log_success(built_message.as_str());

    process::exit(0); // TODO
}