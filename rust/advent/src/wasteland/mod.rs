use std::collections::HashMap;

use crate::error::{Error, Result};

pub fn day08(input: &str) -> Result<()> {
    let map_network = parse_input(input)?;

    let steps = map_network.steps_to_end("AAA");
    println!("{}", steps);

    let steps = map_network.ghost_steps_to_end();
    println!("{}", steps);

    Ok(())
}

#[derive(Debug, Default)]
struct MapNetwork {
    path: Vec<u8>,
    network: HashMap<String, Fork>,
}

impl MapNetwork {
    fn new(path: &str, network: HashMap<String, Fork>) -> Self {
        let path = path.as_bytes().to_vec();
        MapNetwork { path, network }
    }

    fn get(&self, node: &str) -> Option<&Fork> {
        self.network.get(node)
    }

    fn make_turn(&self, current: &str, turn: u8) -> &String {
        let fork = self.network.get(current).unwrap();
        if turn == 76u8 {
            // L
            &fork.left
        } else if turn == 82u8 {
            // R
            &fork.right
        } else {
            panic!("Wrong turn")
        }
    }

    fn steps_to_end(&self, start_node: &str) -> usize {
        let mut steps = 0;
        let mut current = start_node;
        let path_len = self.path.len();

        while !is_end(current) {
            let index: usize = steps % path_len;
            let turn = self.path.get(index).unwrap();
            current = self.make_turn(current, *turn);
            steps += 1;
        }

        steps
    }

    fn ghost_steps_to_end(&self) -> usize {
        let mut steps = 0;
        let mut current = self
            .network
            .keys()
            .filter(|k| is_start(k))
            .collect::<Vec<_>>();
        let path_len = self.path.len();

        while !current.iter().all(|c| is_end(c)) {
            let index: usize = steps % path_len;
            let turn = self.path.get(index).unwrap();
            for item in current.iter_mut() {
                *item = self.make_turn(item, *turn);
            }
            steps += 1;
        }

        steps
    }
}

fn is_start(node_name: &str) -> bool {
    node_name.ends_with('A')
}

fn is_end(node_name: &str) -> bool {
    node_name.ends_with('Z')
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
mod tests;

