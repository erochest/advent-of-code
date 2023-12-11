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

use crate::mappings::get_minimum_location;

fn main() -> Result<()> {
    setup_panic!();
    let args = Cli::parse();
    env_logger::Builder::new()
        .filter_level(args.verbose.log_level_filter())
        .init();

    let input = fs::read_to_string(args.input)?;

    let location = get_minimum_location(input)?;

    if let Some(location) = location {
        println!("location: {}", location);
    } else {
        println!("no minimum");
    }

    Ok(())
}

// TODO: disjoint range application
// TODO: embedded range application
// TODO: reverse embeddded range application
// TODO: overlapping high range application
// TODO: overlapping low range application

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

    /// The input file to process.
    #[arg(short, long)]
    input: PathBuf,
}
