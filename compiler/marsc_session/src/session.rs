use std::path::PathBuf;

#[derive(Debug)]
pub struct CompilerIO {
    pub input_file: PathBuf,
    pub output_file: PathBuf,
    /*pub input: Input,
    pub output_dir: Option<PathBuf>,
    pub output_file: Option<OutFileName>,
    pub temporary_dir: Option<PathBuf>*/
}

#[derive(Debug)]
pub struct Session {
    pub io: CompilerIO,
    /*pub target: Target,
    pub host: Target,
    pub options: config::Options,*/
}
