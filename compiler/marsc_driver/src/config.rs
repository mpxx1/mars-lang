use crate::args::Args;
use marsc_interface::interface::Config;

pub fn build_config(args: &Args) -> Config {
    Config {
        input: args.input.to_string(),
        output: args.output.to_string(),
    }
}
