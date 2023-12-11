use std::collections::HashSet;
use std::convert::TryFrom;
use std::result;
use std::str::FromStr;

use itertools::Itertools;

use crate::error::{Error, Result};

pub fn get_minimum_location(input: String) -> Result<Option<i128>> {
    let mut seeds: Option<HashSet<i128>> = None;
    let paragraphs = split_paragraphs(&input);
    for paragraph in paragraphs {
        if let Some(ref current) = seeds {
            let mappings = Mappings::try_from(paragraph)?;
            let next = next_values(current, &mappings);
            seeds = Some(next)
        } else {
            let line = find_first_line(&paragraph);
            let parsed = parse_seeds_line(&line)?;
            seeds = Some(parsed);
        }
    }
    Ok(seeds.and_then(|s| s.into_iter().min()))
}

fn split_paragraphs(input: &str) -> Vec<Vec<&str>> {
    let mut accum = Vec::new();
    let groups = input.lines().group_by(|line| line.is_empty());

    for (key, lines) in &groups {
        if !key {
            accum.push(lines.collect::<Vec<_>>());
        }
    }

    accum
}

fn next_values(current: &HashSet<i128>, mappings: &Mappings) -> HashSet<i128> {
    let mut next: HashSet<i128> = HashSet::new();
    for seed in current {
        next.insert(mappings.get(*seed));
    }
    next
}

fn find_first_line(lines: &[&str]) -> String {
    lines.iter().find(|p| !p.is_empty()).unwrap().to_string()
}

fn parse_seeds_line(line: &str) -> Result<HashSet<i128>> {
    let parsed: result::Result<HashSet<i128>, _> =
        line.split(' ').skip(1).map(|n| n.parse()).collect();
    let parsed = parsed?;
    Ok(parsed)
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

    fn apply(&self, input: InputRange) -> (Vec<InputRange>, Option<InputRange>) {
        let end = self.source + self.extent;
        if input.start >= self.source && input.end <= end {
            let offset = self.destination - self.source;
            let output = input.shift_by(offset);
            (vec![output], None)
        } else {
            (vec![], Some(input))
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

struct InputRange {
    start: i128,
    end: i128,
    extent: i128,
}

impl InputRange {
    fn new(start: i128, extent: i128) -> Self {
        let end = start + extent;
        InputRange { start, end, extent }
    }

    fn shift_by(&self, offset: i128) -> Self {
        Self::new(self.start + offset, self.extent)
    }
}

#[cfg(test)]
mod tests;

