use std::collections::HashSet;
use std::fmt;
use std::fs;
use std::path::PathBuf;

use clap::Parser;
use clap_verbosity_flag::Verbosity;
use human_panic::setup_panic;

mod error;
mod mappings;

use error::Result;

use crate::mappings::day05;

fn main() -> Result<()> {
    setup_panic!();
    let args = Cli::parse();
    env_logger::Builder::new()
        .filter_level(args.verbose.log_level_filter())
        .init();

    let input = fs::read_to_string(args.input)?;

    if args.day == 5 {
        day05(&input)?;
    } else if args.day == 8 {
    }

    Ok(())
}

fn _dump_set<D: fmt::Debug>(set: &HashSet<D>) {
    print!("{{");
    for item in set {
        print!("{:?}, ", item);
    }
    print!("}}");
    println!();
}

#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(flatten)]
    verbose: Verbosity,

    /// The day to pull.
    #[arg(short, long)]
    day: usize,

    /// The input file to process.
    #[arg(short, long)]
    input: PathBuf,
}
