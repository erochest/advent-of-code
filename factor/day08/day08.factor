! Copyright (C) 2022 Eric Rochester.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs advent2021.io bit-arrays
fry hashtables kernel locals math math.bits math.bitwise
math.combinatorics math.functions math.parser math.ranges
namespaces sequences sequences.extras sorting splitting
vectors ;
IN: advent2021.day08

TUPLE: signal-output input-patterns output-values ;
C: <signal-output> signal-output

CONSTANT: item-offset 97
: parse-item ( item -- n ) item-offset - 2 swap ^ ;
: parse-pattern ( pattern-input -- n )
    [ parse-item ] map sum ;
: split-items ( seq -- seq ) " " split ;
: parse-pattern-seq ( input -- output )
    split-items [ parse-pattern ] map ;
: trim-whitespace ( seq -- seq )
    [ " \t\n\r" member? ] trim ;
: split-fields ( seq -- seq ) "|" split ;
: parse-line ( line -- signal-output )
    split-fields
    [ trim-whitespace parse-pattern-seq ] map
    first2 <signal-output> ;

:: index-n ( n hashtable -- )
    n bit-count :> bits
    bits hashtable at [
        n swap push
    ] [
        n 1vector
        bits hashtable set-at
    ] if* ;

!  aaaa
! b    c
! b    c
!  dddd
! e    f
! e    f
!  gggg
SYMBOL: signal-patterns
{
    0b1110111 0b0100100 0b1011101 0b1101101 0b0101110
    0b1101011 0b1111011 0b0100101 0b1111111 0b1101111
} signal-patterns set-global
SYMBOL: signal-pattern-index
H{ } clone
signal-patterns get-global [ over index-n ] each
signal-pattern-index set-global

: read-patterns ( path -- patterns )
    (file-lines) [ parse-line ] map ;
: count-unique-patterns ( pattern-seq -- x )
    signal-pattern-index get-global swap
    [ bit-count over at length 1 = ] count
    nip ;
: day08a ( path -- n )
    read-patterns
    [ output-values>> count-unique-patterns ] map
    sum ;

: bits ( pattern -- seq )
    integer>bit-array <enumerated>
    [ second ] [ first ] filter-map ;
: >pattern ( connections n -- pattern )
    signal-patterns get-global nth bits
    [ over nth ] map
    parse-pattern
    nip ;
: all-patterns ( connections -- seq )
    9 [0,b] [ dupd >pattern ] map nip ;

: connections-match-input? ( search-patterns input-seq -- ? )
    [ natural-sort ] bi@ = ;

: identify-pattern ( pattern search-pattern -- n/f )
    swap [ = ] curry find drop ;

: identify-patterns ( search-pattern pattern-seq -- seq )
    swap [ identify-pattern ] curry map ;

: join>number ( seq -- n )
    0 swap <reversed> [
        10 swap ^ * +
    ] each-index ;

: solve-output ( pattern search-patterns -- n )
    [
        over input-patterns>> connections-match-input?
    ] find
    nip
    swap output-values>> identify-patterns
    join>number ;

: day08b ( path -- n )
    read-patterns
    "abcdefg" <permutations> [ all-patterns ] map
    swap [ over solve-output ] map
    sum nip ;
