! Copyright (C) 2023 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: accessors advent.io advent.y2023.day08 assocs hashtables
io.encodings.utf8 io.files namespaces sequences tools.test ;
IN: advent.y2023.day08.tests

SYMBOLS: input-1 input-2 input-3 ;
2023 8 1 read-fixture+ input-1 set-global
2023 8 2 read-fixture+ input-2 set-global
2023 8 3 read-fixture+ input-3 set-global

{ 2 } [
    input-1 get-global
    parse-input
    follow-path-to-end
] unit-test

{ 6 } [
    input-2 get-global
    parse-input
    follow-path-to-end
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
{ { "22C" "22C" } } [
    input-3 get-global parse-input network>> "22B" of
] unit-test

{ 2 } [
    input-1 get-global parse-input follow-path-to-end
] unit-test

{ { t t t f f f } } [
    { "AAA" "BBA" "A" "BBB" "ABB" "Z" } [ start? ] map
] unit-test

{ { t t t f f f } } [
    { "ZZZ" "AAZ" "Z" "AAA" "ZAA" "Y" } [ end? ] map
] unit-test

{ 13939 } [
    2023 8 data-file utf8 file-contents
    parse-input follow-path-to-end
] unit-test

{ 6 } [
    input-3 get-global parse-input
    follow-path-to-ghost-end
] unit-test

{ 8906539031197 } [
    2023 8 data-file utf8 file-contents parse-input
    follow-path-to-ghost-end
] unit-test
