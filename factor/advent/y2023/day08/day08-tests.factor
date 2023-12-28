! Copyright (C) 2023 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: accessors advent.io advent.y2023.day08 assocs hashtables
io.encodings.utf8 io.files namespaces sequences tools.test ;
IN: advent.y2023.day08.tests

SYMBOLS: input-1 input-2 ;
"RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)" input-1 set-global

"LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)" input-2 set-global

{ 2 } [
    input-1 get-global
    parse-input
    follow-path-to-end
    length
] unit-test

{ 6 } [
    input-2 get-global
    parse-input
    follow-path-to-end
    length
] unit-test

{ { R L } } [
    input-1 get-global parse-input path>>
] unit-test

{ { "BBB" "CCC" } } [
    input-1 get-global parse-input network>> "AAA" of
] unit-test
{ { "ZZZ" "GGG" } } [
    input-1 get-global parse-input network>> "CCC" of
] unit-test

{ { "CCC" "ZZZ" } } [
    input-1 get-global parse-input follow-path-to-end
] unit-test

{ 13939 } [
    2023 8 data-file utf8 file-contents
    parse-input follow-path-to-end length
] unit-test

