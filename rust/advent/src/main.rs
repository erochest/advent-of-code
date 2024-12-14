use std::fmt;
use std::fs;
use std::path::PathBuf;
use std::str::FromStr;

use clap::Parser;
use clap_verbosity_flag::Verbosity;
use human_panic::setup_panic;

mod error;
mod mappings;
mod wasteland;
mod y2024;

use error::Result;

fn main() -> Result<()> {
    setup_panic!();
    let args = Cli::parse();
    env_logger::Builder::new()
        .filter_level(args.verbose.log_level_filter())
        .init();

    let filename = if let Some(input) = args.input {
        input
    } else {
        // TODO: pull this into another function
        let input_source = args.source.unwrap_or_default();
        let suffix = if input_source == InputSource::Sample {
            "-0"
        } else {
            ""
        };
        let ext = if input_source == InputSource::Sample {
            "fixture"
        } else {
            "data"
        };
        let filename = format!(
            "../../{}/{}/day{:02}{}.{}",
            input_source, args.year, args.day, suffix, ext
        );
        PathBuf::from(filename)
    };
    let input = fs::read_to_string(filename)?;

    if args.year == 2023 && args.day == 5 {
        mappings::day05(&input)?;
    } else if args.year == 2023 && args.day == 8 {
        wasteland::day08(&input)?;
    } else if args.year == 2024 && args.day == 6 {
        y2024::day06(&input)?;
    }

    Ok(())
}

#[derive(Debug, Clone, PartialEq, Default)]
enum InputSource {
    Data,
    #[default]
    Sample,
}

impl fmt::Display for InputSource {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InputSource::Data => write!(f, "data"),
            InputSource::Sample => write!(f, "sample"),
        }
    }
}

impl FromStr for InputSource {
    type Err = error::Error;
    fn from_str(s: &str) -> Result<Self> {
        match s {
            "data" => Ok(InputSource::Data),
            "sample" => Ok(InputSource::Sample),
            _ => Err(error::Error::InvalidInputSource(s.to_string())),
        }
    }
}

// TODO: More documentation about how all of these interact
// TODO: have year and day default to current
#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(flatten)]
    verbose: Verbosity,

    /// The year to run.
    #[arg(short, long)]
    year: usize,

    /// The day to run.
    #[arg(short, long)]
    day: usize,

    /// The input file to process.
    #[arg(short, long)]
    input: Option<PathBuf>,

    /// The type of input, "data" or "sample". Defaults to "sample".
    #[arg(short, long)]
    source: Option<InputSource>,
}
