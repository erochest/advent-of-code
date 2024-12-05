! Copyright (C) 2024 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: advent.io combinators kernel math regexp sequences
       splitting ;
IN: advent.y2024.day03

: extract-pair ( s -- s' ) 4 swap [ length 1 - ] keep subseq ;
: split-pair ( s -- s-pair ) "," split ;
: parse-mul-op ( s -- pair )
    extract-pair
    split-pair
    seq>numbers ;

: find-ops ( s -- pair-seq )
    R/ mul\((\d+),(\d+)\)/ all-matching-subseqs
    [ parse-mul-op ] map ;

: checksum-ops ( s -- n )
    find-ops
    [ product ] map
    sum ;

: scan-ops ( s -- s-seq )
    R/ mul\(\d+,\d+\)|do\(\)|don't\(\)/ all-matching-subseqs ;

: (inc-sum) ( pair n -- pair' )
    [ dup second ] dip
    +
    over set-second ;
: inc-sum ( pair n -- pair' ) 
    over first
    [ (inc-sum) ] [ drop ] if ;

: do-op ( p -- p' ) t over set-first ;
: don't-op ( p -- p' ) f over set-first ;

: interpret-op ( state op -- state' )
    {
        { [ dup "mul(" starts-with? ] [ parse-mul-op product inc-sum ] }
        { [ dup "do()" = ] [ drop do-op ] }
        { [ dup "don't()" = ] [ drop don't-op ] }
        [ drop ]
    } cond
     ;

: interpret-ops ( s -- n )
    scan-ops
    { t 0 } [ interpret-op ] reduce
    second ;

: part2 ( -- n )
    2024 3 data-file
    (read-file-contents)
    interpret-ops ;
