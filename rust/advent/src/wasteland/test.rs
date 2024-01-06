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

#[test]
fn parse_input_path() {
    let result = parse_input(INPUT1);
    assert!(result.is_ok());
    let mapping = result.unwrap();
    assert_eq!("RL".to_string(), mapping.path);
}

#[test]
fn parse_input_network() {
    let result = parse_input(INPUT1);
    assert!(result.is_ok());
    let mapping = result.unwrap();
    assert!(mapping.get("MMM").is_none());
    assert_eq!(Some(&Fork::new("ZZZ", "GGG")), mapping.get("CCC"));
}

