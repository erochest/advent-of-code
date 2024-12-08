! Copyright (C) 2024 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: advent.io advent.map arrays hash-sets io kernel math
       namespaces prettyprint regexp sequences sets ;
IN: advent.y2024.day06

SYMBOLS: DIRECTIONS MOVEMENTS ;
{ CHAR: ^ CHAR: > CHAR: v CHAR: < } DIRECTIONS set-global
{ [ up ] [ right ] [ down ] [ left ] } MOVEMENTS set-global

: char>direction ( c -- d/f ) DIRECTIONS get-global index ;
: direction>move ( d -- m/f ) MOVEMENTS get-global ?nth ;

: find-guard-row ( map -- n row )
    [ R/ [<^>v]/ re-contains? ] find ;
: find-guard-col ( row -- n direction )
    [ DIRECTIONS get-global in? ] find ;
: find-guard ( map -- point direction )
    find-guard-row find-guard-col
    [ 2array ] dip
    char>direction ;

: on-map? ( pos map -- ? )
    [ first2 swap ] dip ?nth ?nth >boolean ;

: blocked? ( pos map -- ? )
    char-at? CHAR: # = ;

: make-move ( pair -- pos )
    first2
    [ clone ] [ direction>move ] bi*
    map-move ;

: change-direction ( pair -- pair' )
    [ 1 + DIRECTIONS get-global length mod ] mod-x ;

! This also returns the last, out of bounds step.
: next-step ( pair map -- pair' )
    over make-move
    2dup swap blocked?
    [ 2drop change-direction [ make-move ] keep ]
    [ nip swap ] if
    second 2array ;

: (count-visited-spaces) ( map-matrix -- n )
    dup find-guard 2array
    [ dup first pick on-map? ]
    [ over next-step dup first ] produce
    [ 2drop ] dip
    >hash-set members length ;

: count-visited-spaces ( path -- n )
    (file-lines) (count-visited-spaces) ;
