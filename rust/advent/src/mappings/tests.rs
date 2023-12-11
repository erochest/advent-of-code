use pretty_assertions::assert_eq;

use super::{InputRange, MappingRange};

#[test]
fn disjoint_range_application_changes_nothing() {
    let range_fn = MappingRange::new(50, 30, 20);
    let input = InputRange::new(10, 10);

    let (output, unmatched) = range_fn.apply(input);

    assert_eq!(output.len(), 0);
    assert!(unmatched.is_some());
    let unmatched = unmatched.unwrap();
    assert_eq!(unmatched.start, 10);
    assert_eq!(unmatched.extent, 10);
}

#[test]
fn embedded_range_application_matches_everything() {
    let range_fn = MappingRange::new(50, 30, 20);
    let input = InputRange::new(35, 10);

    let (output, unmatched) = range_fn.apply(input);

    assert_eq!(output.len(), 1);
    assert!(unmatched.is_none());
    assert_eq!(output[0].start, 55);
    assert_eq!(output[0].extent, 10);
    assert_eq!(output[0].end, 65);
}

