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

    /// This applies an input range to this mapping range, going with the
    /// function perspective.
    ///
    /// It returns a tuple with two items:
    /// - a vector containing any parts of the input that were not defined
    ///   for this mapping and
    /// - an optional range that was defined and processed by this mapping.
    fn apply(&self, input: InputRange) -> (Vec<InputRange>, Option<InputRange>) {
        let end = self.source + self.extent;
        let offset = self.destination - self.source;
        if input.start < self.source && input.end > end {
            // reverse embedded (the mapping is embedded in the input range)
            let output = InputRange::new_shifted(self.source, self.extent, offset);
            let disjoint_low = InputRange::new(input.start, self.source - input.start);
            let disjoint_high = InputRange::new(end, input.end - end);
            (vec![disjoint_low, disjoint_high], Some(output))
        } else if input.start < self.source && input.end >= self.source && input.end <= end {
            // overlapping low
            let output = InputRange::new_shifted(self.source, input.end - self.source, offset);
            let disjoint = InputRange::new(input.start, self.source - input.start);
            (vec![disjoint], Some(output))
        } else if input.start >= self.source && input.start < end && input.end > end {
            // overlapping high
            let output = InputRange::new_shifted(input.start, end - input.start, offset);
            let disjoint = InputRange::new(end, input.end - end);
            (vec![disjoint], Some(output))
        } else if input.start >= self.source && input.end <= end {
            // embedded
            let output = input.shift_by(offset);
            (vec![], Some(output))
        } else {
            // disjoint
            (vec![input], None)
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

    fn new_shifted(start: i128, extent: i128, offset: i128) -> Self {
        Self::new(start + offset, extent)
    }

    fn shift_by(&self, offset: i128) -> Self {
        Self::new(self.start + offset, self.extent)
    }
}

#[cfg(test)]
mod tests;

