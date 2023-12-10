use std::{collections::HashSet, convert::TryFrom, fmt, fs, path::PathBuf, result, str::FromStr};

use clap::Parser;
use clap_verbosity_flag::Verbosity;
use human_panic::setup_panic;
use itertools::Itertools;

mod error;

use error::{Error, Result};

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

fn get_minimum_location(input: String) -> Result<Option<i128>> {
    let mut seeds: Option<HashSet<i128>> = None;
    for (key, paragraph) in &input.lines().group_by(|line| line.is_empty()) {
        if !key {
            let paragraph: Vec<_> = paragraph.collect();
            if let Some(ref current) = seeds {
                // println!("{}", paragraph[0]);
                // print!("current: ");
                // dump_set(current);

                let mappings = Mappings::try_from(paragraph)?;
                let next = next_values(current, &mappings);
                seeds = Some(next)
            } else {
                let line = find_first_line(&paragraph);
                let parsed = parse_seeds_line(&line)?;
                seeds = Some(parsed);
            }
        }
    }
    Ok(seeds.and_then(|s| s.into_iter().min()))
}

fn split_paragraphs(input: &str) -> impl Iterator<Item = Vec<&str>> {
    input
        .lines()
        .group_by(|line| line.is_empty())
        .filter(|(key, _)| !key)
        .map(|(_, lines)| lines.collect::<Vec<_>>())
}

fn next_values(current: &HashSet<i128>, mappings: &Mappings) -> HashSet<i128> {
    let mut next: HashSet<i128> = HashSet::new();
    for seed in current {
        next.insert(mappings.get(*seed));
    }
    next
}

fn parse_seeds_line(line: &str) -> Result<HashSet<i128>> {
    let parsed: result::Result<HashSet<i128>, _> =
        line.split(' ').skip(1).map(|n| n.parse()).collect();
    let parsed = parsed?;
    Ok(parsed)
}

fn find_first_line(lines: &[&str]) -> String {
    lines.iter().find(|p| !p.is_empty()).unwrap().to_string()
}

fn dump_set<D: fmt::Debug>(set: &HashSet<D>) {
    print!("{{");
    for item in set {
        print!("{:?}, ", item);
    }
    print!("}}");
    println!();
}

struct Mappings {
    mappings: Vec<MappingRange>,
}

impl Mappings {
    fn new() -> Self {
        Mappings { mappings: vec![] }
    }

    fn get(&self, x: i128) -> i128 {
        self.mappings.iter().find_map(|m| m.get(x)).unwrap_or(x)
    }
}

impl TryFrom<Vec<&str>> for Mappings {
    type Error = Error;

    fn try_from(value: Vec<&str>) -> result::Result<Self, Self::Error> {
        let mut mappings = Mappings::new();
        for line in &value[1..] {
            let range = line.parse::<MappingRange>()?;
            mappings.mappings.push(range);
        }
        Ok(mappings)
    }
}

struct MappingRange {
    destination: i128,
    source: i128,
    extent: i128,
}

impl MappingRange {
    fn new(destination: i128, source: i128, extent: i128) -> Self {
        MappingRange {
            destination,
            source,
            extent,
        }
    }

    fn contains(&self, x: i128) -> bool {
        x >= self.source && x < (self.source + self.extent)
    }

    fn get(&self, x: i128) -> Option<i128> {
        if self.contains(x) {
            Some(self.destination - self.source + x)
        } else {
            None
        }
    }
}

impl FromStr for MappingRange {
    type Err = Error;

    fn from_str(s: &str) -> result::Result<Self, Self::Err> {
        let values = s
            .split(' ')
            .map(|n| {
                n.parse::<i128>()
                    .map_err(|e| Error::RangeParseError(e.to_string()))
            })
            .take(3)
            .collect::<result::Result<Vec<_>, _>>()?;

        if values.len() < 3 {
            return Err(Error::RangeParseError(format!(
                "Invalid mapping range: {:?}",
                s
            )));
        }

        Ok(MappingRange::new(values[0], values[1], values[2]))
    }
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
