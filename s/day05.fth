s" files.fth" included
s" debug.fth" included
s" stack.fth" included

0 7 2constant column-bounds
0 127 2constant row-bounds
8 constant row-width

: focus-upper ( from to -- from to )
    \ B R
    2dup swap - 2/ 1+ rot + swap ;

: focus-lower ( from to -- from to )
    \ F L
    2dup swap - 2/ 1+ - ;

: focus-row ( from to char -- from to )
    dup [char] F = if drop focus-lower
    else dup [char] B = if drop focus-upper
    else drop
    then then
    ;

: focus-column ( from to char -- from to )
    dup [char] R = if drop focus-upper
    else dup [char] L = if drop focus-lower
    else drop
    then then
    ;

: find-column-row ( a u -- column row )
    2dup 7 /string column-bounds 2swap bounds u+do
        i c@ focus-column
    loop
    drop -rot
    3 - row-bounds 2swap bounds u+do
        i c@ focus-row
    loop
    drop
    ;

: find-seat ( column row -- seat ) row-width * + ;

: (day05a) ( a u -- n )
    open-input
    -1 begin
        read-input-line while
        line-buffer swap
        find-column-row
        find-seat
        max
    repeat
    drop
    close-input
    ;

: day05a ( -- ) s" d/day05.txt" (day05a) . cr ;

\ This is an array of singles, but maybe it should be byte arrays.
column-bounds nip row-bounds nip find-seat 1+ constant seat-set-bounds
create seat-set seat-set-bounds cells allot
seat-set seat-set-bounds erase

: seat-addr ( u -- a ) cells seat-set + ;
: seat-set-addr ( column row -- a ) find-seat seat-addr ;
: seat-set@ ( column row -- ) seat-set-addr @ ;
: seat-set! ( f column row -- ) seat-set-addr ! ;
: seat-set-on ( column row -- ) seat-set-addr on ;
: seat-set-off ( column row -- ) seat-set-addr off ;

: is-front? ( row -- f ) row-bounds drop = ;
: is-back? ( row -- f ) row-bounds nip = ;

: read-seats ( -- )
    s" d/day05.txt" open-input
    cr ." code, row, column, seat "
    begin
        read-input-line while
        line-buffer swap
        cr 2dup type 3 spaces
        find-column-row
        2dup ." , " . ." , " .
        find-seat
        ." , " .
    repeat
    drop
    close-input
    ;

: read-seat-set ( a u -- )
    open-input
    begin
        read-input-line while
        line-buffer swap
        find-column-row
        seat-set-on
    repeat
    drop
    close-input
    ;

: is-empty? ( column row -- f ) seat-set@ invert ;
: has-neighbors? ( column row -- f )
    find-seat
    dup 1+ seat-addr @
    swap 1- seat-addr @
    and ;

: (day05b) ( a u -- row column )
    read-seat-set
    row-bounds 1+ swap u+do
        column-bounds 1+ swap u+do
            i j is-empty? if
                i j has-neighbors? if
                    i j i j find-seat
                    unloop unloop exit
                then
            then
        loop
    loop
    0 0 0
    ;

: day05b ( -- )
    s" d/day05.txt" (day05b)
    ." seat: " .
    ." row: " .
    ." column: " .
    cr
    ;
