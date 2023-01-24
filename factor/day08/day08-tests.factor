! Copyright (C) 2022 Eric Rochester.
! See http://factorcode.org/license.txt for BSD license.
USING: advent2021.day08 kernel math.combinatorics sequences
tools.test ;
IN: advent2021.day08.tests

{ T{
    signal-output
    f
    {
        0b1111111 0b0111110 0b1101101 0b0101111 0b0001011
        0b0111111 0b1111110 0b0110011 0b1011111 0b0000011
    }
    { 0b0111110 0b0101111 0b0111110 0b0101111 }
} }
[
    "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
    parse-line
] unit-test

{ {
    0b1110111 0b0100100 0b1011101 0b1101101 0b0101110
    0b1101011 0b1111011 0b0100101 0b1111111 0b1101111
} }
[
    {
        "beagcf" "cf" "eadgc" "adgcf" "bdcf"
        "bgdaf" "fgdaeb" "fca" "gdabfce" "adgcbf"
    }
    [ parse-pattern ] map
] unit-test

{ 10 }
[ "vocab:advent2021/day08/example.txt" read-patterns length ]
unit-test
{ T{
    signal-output
    f
    {
        0b0010010 0b1111111 0b1111110 0b1111101 0b1010110
        0b1111100 0b1111011 0b0111110 0b0101111 0b0011010
    }
    { 0b1111111 0b0111110 0b1111110 0b1010110 }
} }
[ "vocab:advent2021/day08/example.txt" read-patterns first ]
unit-test

{ 2 }
[ { 0b1111111 0b0111110 0b1111110 0b1010110 } count-unique-patterns ]
unit-test
{ 26 } [ "vocab:advent2021/day08/example.txt" day08a ] unit-test
{ 512 } [ "vocab:advent2021/day08/data.txt" day08a ] unit-test

{ V{ 0 1 2 4 5 6 } } [ 0b1110111 bits ] unit-test
{ V{ 2 5 } } [ 0b0100100 bits ] unit-test

{ 0b1111101 } [ "gafbecd" 0 >pattern ] unit-test
{ 0b0100100 } [ "gafbecd" 1 >pattern ] unit-test

{ {
    0b1111101 0b0100100 0b1111010 0b1101110 0b0100111
    0b1001111 0b1011111 0b1100100 0b1111111 0b1101111
} } [ "gafbecd" all-patterns ] unit-test

{ t }
[
    "deafgbc" all-patterns
    "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab"
    parse-pattern-seq
    connections-match-input?
] unit-test

{ f }
[
    "deafgbc" all-patterns
    "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg"
    parse-pattern-seq
    connections-match-input?
] unit-test

{ 5 }
[
    "cdfeb" parse-pattern "deafgbc" all-patterns identify-pattern
] unit-test

{ { 5 3 5 3 } }
[
    "deafgbc" all-patterns
    "cdfeb fcadb cdfeb cdbaf" parse-pattern-seq
    identify-patterns    
] unit-test

{ 5353 } [ { 5 3 5 3 } join>number ] unit-test
{ 7429 } [ { 7 4 2 9 } join>number ] unit-test 

{ 5353 }
[
    "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf" 
    parse-line
    "abcdefg" <permutations> [ all-patterns ] map
    solve-output
] unit-test
{ 8394 }
[
    "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
    parse-line
    "abcdefg" <permutations> [ all-patterns ] map
    solve-output
] unit-test

{ 61229 } [ "vocab:advent2021/day08/example.txt" day08b ] unit-test
{ 1091165 } [ "vocab:advent2021/day08/data.txt" day08b ] unit-test