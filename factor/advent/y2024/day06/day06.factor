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

: change-direction ( pair -- pair' )
    [ 1 + DIRECTIONS get-global length mod ] mod-x ;

! This also returns the last, out of bounds step.
: next-step ( pair map -- pair' )
    over make-move
    2dup swap blocked?
    [ 2drop change-direction [ make-move ] keep ]
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

: (can-loop/flag) ( pair map pair -- pair map pair n )
    {
        { [ dup first pick on-map? not ] [ 2 ] }
        { [ dup reach = ] [ 1 ] }
        [ 0 ]
    } cond ;

: (can-loop?) ( pair map -- ? )
    over clone change-direction
    (can-loop/flag)
    [ dup zero? ] [
        drop
        (walk/next-step)
        (can-loop/flag)
    ] while
    3nip
    1 = ;

: can-loop? ( pair map -- ? )
    over make-move
    over blocked?
    [ 2drop f ] [ (can-loop?) ] if ;

: count-true ( seq -- n ) [ ] count ;

: (count-loops) ( map-matrix -- n )
    dup walk-map uniq
    [ over can-loop? ] count
    nip ;

: count-loops ( path -- n )
    (file-lines) (count-loops) ;
