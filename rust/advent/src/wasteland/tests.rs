use std::fs;

use pretty_assertions::assert_eq;

use crate::wasteland::{parse_input, Fork};

#[test]
fn parse_input_path() {
    let input_file = "./../../sample/2023/day08-1.fixture";
    let contents = fs::read_to_string(input_file).unwrap();
    let result = parse_input(&contents);
    assert!(result.is_ok());
    let mapping = result.unwrap();
    assert_eq!(b"RL".to_vec(), mapping.path);
}

#[test]
fn parse_input_network() {
    let input_file = "./../../sample/2023/day08-1.fixture";
    let contents = fs::read_to_string(input_file).unwrap();
    let result = parse_input(&contents);
    assert!(result.is_ok());
    let mapping = result.unwrap();
    assert!(mapping.get("MMM").is_none());
    assert_eq!(Some(&Fork::new("ZZZ", "GGG")), mapping.get("CCC"));
}

#[test]
fn path_to_end_single() {
    let input_file = "./../../sample/2023/day08-1.fixture";
    let contents = fs::read_to_string(input_file).unwrap();
    let result = parse_input(&contents);
    assert!(result.is_ok());
    let mapping = result.unwrap();
    let steps = mapping.steps_to_end("AAA");
    assert_eq!(2, steps);
}

#[test]
fn path_to_end_loop() {
    let input_file = "./../../sample/2023/day08-2.fixture";
    let contents = fs::read_to_string(input_file).unwrap();
    let result = parse_input(&contents);
    assert!(result.is_ok());
    let mapping = result.unwrap();
    let steps = mapping.steps_to_end("AAA");
    assert_eq!(6, steps);
}

#[test]
fn ghost_steps_to_end() {
    let input_file = "./../../sample/2023/day08-3.fixture";
    let contents = fs::read_to_string(input_file).unwrap();
    let result = parse_input(&contents);
    assert!(result.is_ok());
    let mapping = result.unwrap();
    let steps = mapping.ghost_steps_to_end();
    assert_eq!(6, steps);
}
