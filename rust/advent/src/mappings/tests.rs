use std::fs;

use pretty_assertions::assert_eq;

use super::{get_minimum_location, get_minimum_location_range, InputRange, MappingRange};

#[test]
fn get_minimum_location_returns_correct_response() {
    let input_file = "./../../sample/2023/day05.fixture";
    let input = fs::read_to_string(input_file).unwrap();
    let actual = get_minimum_location(&input);

    assert!(actual.is_ok());
    let actual = actual.unwrap();
    assert!(actual.is_some());
    let actual = actual.unwrap();
    assert_eq!(actual, 35);
}

#[test]
fn get_minimum_location_range_returns_correct_response() {
    let input_file = "./../../sample/2023/day05.fixture";
    let input = fs::read_to_string(input_file).unwrap();
    let actual = get_minimum_location_range(&input);

    assert!(actual.is_ok());
    let actual = actual.unwrap();
    assert!(actual.is_some());
    let actual = actual.unwrap();
    assert_eq!(actual, 46);
}

#[test]
fn disjoint_range_application_changes_nothing() {
    let range_fn = MappingRange::new(50, 30, 20);
    let input = InputRange::new(10, 10);

    let (unmatched, output) = range_fn.apply(&input);

    assert_eq!(unmatched.len(), 1);
    assert_eq!(unmatched[0].start, 10);
    assert_eq!(unmatched[0].extent, 10);
    assert_eq!(unmatched[0].end, 20);

    assert!(output.is_none());
}

#[test]
fn embedded_range_application_matches_everything() {
    let range_fn = MappingRange::new(50, 30, 20);
    let input = InputRange::new(35, 10);

    let (unmatched, output) = range_fn.apply(&input);

    assert_eq!(unmatched.len(), 0);

    assert!(output.is_some());
    let output = output.unwrap();
    assert_eq!(output.start, 55);
    assert_eq!(output.extent, 10);
    assert_eq!(output.end, 65);
}

#[test]
fn overlapping_high_application_is_split() {
    let range_fn = MappingRange::new(50, 30, 20);
    let input = InputRange::new(40, 25);

    let (unmatched, output) = range_fn.apply(&input);

    assert_eq!(unmatched.len(), 1);
    assert_eq!(unmatched[0].start, 50);
    assert_eq!(unmatched[0].extent, 15);
    assert_eq!(unmatched[0].end, 65);

    assert!(output.is_some());
    let output = output.unwrap();
    assert_eq!(output.start, 60);
    assert_eq!(output.extent, 10);
    assert_eq!(output.end, 70);
}

#[test]
fn overlapping_low_application_is_split() {
    let range_fn = MappingRange::new(50, 30, 20);
    let input = InputRange::new(20, 20);

    let (unmatched, output) = range_fn.apply(&input);

    assert_eq!(unmatched.len(), 1);
    assert_eq!(unmatched[0].start, 20);
    assert_eq!(unmatched[0].end, 30);
    assert_eq!(unmatched[0].extent, 10);

    assert!(output.is_some());
    let output = output.unwrap();
    assert_eq!(output.start, 50);
    assert_eq!(output.end, 60);
    assert_eq!(output.extent, 10);
}

#[test]
fn reverse_embedded_application_is_split_three_ways() {
    let range_fn = MappingRange::new(50, 30, 20);
    let input = InputRange::new(20, 50);

    let (unmatched, output) = range_fn.apply(&input);

    assert_eq!(unmatched.len(), 2);
    assert_eq!(unmatched[0].start, 20);
    assert_eq!(unmatched[0].end, 30);
    assert_eq!(unmatched[0].extent, 10);
    assert_eq!(unmatched[1].start, 50);
    assert_eq!(unmatched[1].end, 70);
    assert_eq!(unmatched[1].extent, 20);

    assert!(output.is_some());
    let output = output.unwrap();
    assert_eq!(output.start, 50);
    assert_eq!(output.end, 70);
    assert_eq!(output.extent, 20);
}

