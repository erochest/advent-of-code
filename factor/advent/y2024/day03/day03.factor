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
    [ first2 * ] map
    sum ;
