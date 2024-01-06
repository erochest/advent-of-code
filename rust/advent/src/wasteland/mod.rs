use std::collections::HashMap;

use crate::error::{Error, Result};

pub fn day08(input: &str) -> Result<()> {
    unimplemented!()
}

#[derive(Debug, Default)]
struct MapNetwork {
    path: String,
    network: HashMap<String, Fork>,
}

impl MapNetwork {
    fn new(path: &str, network: HashMap<String, Fork>) -> Self {
        let path = path.to_string();
        MapNetwork { path, network }
    }

    fn get(&self, node: &str) -> Option<&Fork> {
        self.network.get(node)
    }
}

#[derive(Debug, Default, PartialEq, Eq, Clone)]
struct Fork {
    left: String,
    right: String,
}

impl Fork {
    fn new(left: &str, right: &str) -> Self {
        let left = left.to_string();
        let right = right.to_string();
        Fork { left, right }
    }
}

fn unable_to_parse_line(line: &str) -> Error {
    Error::MapNetworkParse(format!("unable to parse line: {:?}", line))
}

fn parse_pair(line: &str) -> Result<(String, Fork)> {
    let mut words = line.split(' ');
    let key = words
        .next()
        .ok_or_else(|| unable_to_parse_line(line))?
        .to_string();

    let _ = words.next().ok_or_else(|| unable_to_parse_line(line))?;

    let left = words.next().ok_or_else(|| unable_to_parse_line(line))?;
    let left = left[1..left.len() - 1].to_string();

    let right = words.next().ok_or_else(|| unable_to_parse_line(line))?;
    let right = right[..right.len() - 1].to_string();

    Ok((key, Fork::new(&left, &right)))
}

fn parse_input(input: &str) -> Result<MapNetwork> {
    let mut lines = input.lines();
    if let Some(path) = lines.next() {
        let network: Result<HashMap<_, _>> =
            lines.filter(|l| !l.is_empty()).map(parse_pair).collect();
        let network = network?;

        Ok(MapNetwork::new(path, network))
    } else {
        Err(Error::MapNetworkParse("unable to parse input".to_string()))
    }
}

#[cfg(test)]
mod test;

