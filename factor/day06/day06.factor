! Copyright (C) 2022 Eric Rochester.
! See http://factorcode.org/license.txt for BSD license.

USING: advent2021.io arrays assocs hashtables io.encodings.utf8
io.files kernel math math.parser math.ranges prettyprint
sequences splitting vectors ;
IN: advent2021.day06

: inc ( n seq -- ) [ nth 1 + ] 2keep set-nth ;
: inc-by ( offset n seq -- ) [ nth + ] 2keep set-nth ;
: move-to ( seq value n -- seq ) pick inc-by ;
: tuck-school ( x -- seq x ) 9 0 <array> swap ;
: >school ( seq -- school-seq ) tuck-school [ over inc ] each ;
: spawn ( seq value -- seq ) [ 8 move-to ] keep 6 move-to ;
: age ( seq value n -- seq ) 1 - move-to ;
: school-day ( school -- new-school )
    tuck-school [
        [ spawn ] [ age ] if-zero
    ] each-index ;
: school-term ( start-school days -- school )
    [1,b] [ drop school-day ] each ;
: checksum ( school -- n ) sum ;
: run-for ( period path -- n )
    read>numbers >school swap school-term checksum ;
: day06a ( path -- n ) 80 swap run-for ;
: day06b ( path -- n ) 256 swap run-for ;
