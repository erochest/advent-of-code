! Copyright (C) 2024 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: advent.io hash-sets io kernel math math.combinatorics
       math.functions math.parser prettyprint sequences sets
       splitting ;
IN: advent.y2024.day05

: todo ( -- ) t f assert ;

: split-paragraphs ( line-seq -- paragraph-seq )
    [ empty? ] split-when ;

: parse-order-rules ( paragraph -- order-rules )
    [ "|" split [ string>number ] map ] map ;

: parse-updates-pages ( paragraph -- updates-pages )
    [ split-fields seq>numbers ] map ;

: parse-input ( lines -- order-rules updates-pages )
    split-paragraphs
    first2
    [ parse-order-rules ] [ parse-updates-pages ] bi* ;

! This assums that every combination of items update-page-order
! is represented in order-rules.
: update-in-order? ( update-page-order order-rules -- ? )
    [ 2 all-combinations >hash-set ] [ >hash-set ] bi* subset? ;

: middle-item ( seq -- item )
    [ length 2 / round 1 - ] keep nth ;

: (ordered-update-checksum) ( lines -- n )
    parse-input 
    [ over update-in-order? ] filter nip
    [ middle-item ] map
    sum ;

: ordered-update-checksum ( path -- n )
    (file-lines) (ordered-update-checksum) ;
