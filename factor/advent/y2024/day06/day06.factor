! Copyright (C) 2024 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: advent.io advent.map arrays combinators hash-sets io 
       kernel math namespaces prettyprint regexp sequences
       sets ;
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

: turn ( pair -- pair' )
    [ 1 + DIRECTIONS get-global length mod ] mod-x ;

! This also returns the last, out of bounds step.
: next-step ( pair map -- pair' )
    over make-move
    2dup swap blocked?
    [ 2drop turn [ make-move ] keep ]
    [ nip swap ] if
    second 2array ;

: (walk/init) ( map -- map guard-pair ) dup find-guard 2array ;
: (walk/on-map?) ( map pair -- map pair ? )
    dup first pick on-map? ;
: (walk/next-step) ( map pair -- map pair ) over next-step ;
: (walk/clean-up) ( map pair seq -- seq ) 2nip ;

: walk-map ( map -- pair-seq )
    (walk/init)
    [ (walk/on-map?) ]
    [ (walk/next-step) dup ] produce
    (walk/clean-up) ;


: uniq ( seq -- seq' ) >hash-set members ;
: count-uniq ( seq -- n ) uniq length ;

: (count-visited-spaces) ( map-matrix -- n )
    walk-map
    [ first ] map
    count-uniq ;

: count-visited-spaces ( path -- n )
    (file-lines) (count-visited-spaces) ;

: log ( tag -- ) print .s nl ;

: initialize-state ( map -- loops visited map guard )
    0
    HS{ } clone
    rot
    dup find-guard 2array ;

: mark-seen ( visited m pos -- visited m pos )
    pick dupd adjoin ;

: forward ( current -- current next )
    dup make-move over second 2array ;

: (on-map?) ( map c next -- map c next ? )
    dup first reach on-map? ;

: on-block? ( map c next -- map c next ? )
    dup first reach blocked? ;

: (turn) ( current n -- next ) drop turn ;

: seen? ( map current -- map current ? )
    [ swap in? ] 2keep rot ;

: true ( v m c -- ? ) 3drop t ;
: false ( v m c n -- ? ) 4drop f ;

: finalize-move ( current next -- next ) nip ;

: (is-guard-in-loop?) ( visited map current -- ? )
    seen? [ true ] [
        mark-seen
        forward
        (on-map?) [
            on-block? [ (turn) ] [ finalize-move ] if
            (is-guard-in-loop?)
        ] [ false ] if
    ] if ;

: is-guard-in-loop?
    ( visited map current n -- visited map current n ? )
    [
        3dup [ clone ] 2dip
        (is-guard-in-loop?)
    ] dip
    swap ;

: inc-loop-count ( loops v m c n -- loops v m c n )
    [ 1 + ] 4dip ;

: clean-up ( loops v m c n -- loops ) 4drop ;

! TODO: Also use more structs and methods to clean up
! and minimize stack juggling.
: (count-guard-loops) ( map -- n )
    initialize-state
    [
        mark-seen
        forward
        (on-map?)
    ] [
        on-block? [ (turn) ] [
            is-guard-in-loop? [ inc-loop-count ] when
            finalize-move
        ] if
    ] while
    clean-up ;

: count-loops ( path -- n )
    (file-lines) (count-guard-loops) ;
