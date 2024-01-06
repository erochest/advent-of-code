use pretty_assertions::assert_eq;

use crate::wasteland::{parse_input, Fork};

static INPUT1: &str = "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)";

static INPUT2: &str = "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)";

#[test]
fn parse_input_path() {
    let result = parse_input(INPUT1);
    assert!(result.is_ok());
    let mapping = result.unwrap();
    assert_eq!(b"RL".to_vec(), mapping.path);
}

#[test]
fn parse_input_network() {
    let result = parse_input(INPUT1);
    assert!(result.is_ok());
    let mapping = result.unwrap();
    assert!(mapping.get("MMM").is_none());
    assert_eq!(Some(&Fork::new("ZZZ", "GGG")), mapping.get("CCC"));
}

#[test]
fn path_to_end_single() {
    let result = parse_input(INPUT1);
    assert!(result.is_ok());
    let mapping = result.unwrap();
    let steps = mapping.steps_to_end("AAA");
    assert_eq!(2, steps);
}

#[test]
fn path_to_end_loop() {
    let result = parse_input(INPUT2);
    assert!(result.is_ok());
    let mapping = result.unwrap();
    let steps = mapping.steps_to_end("AAA");
    assert_eq!(6, steps);
}

