! Copyright (C) 2024 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: accessors advent.io advent.map arrays combinators
       hash-sets io kernel math namespaces prettyprint ranges
       regexp sequences sets ;
IN: advent.y2024.day06

! These consistently use coords of y, x :/

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

TUPLE: map-state grid x-bounds y-bounds start-pos ;
! reader>>
! writer<< ( value obj -- obj )
! >>setter ( obj value -- obj )
! change-changer ( obj quot -- obj )
C: <map-state> map-state

: initialize-state ( map-grid -- map-state )
    0 over ?nth length
    over length
    pick find-guard 2array
    <map-state> ;

: surrounding ( pos -- seq )
    { [ up ] [ down ] [ left ] [ right ] } cleave 4array ;

: get-matrix-coordinates ( map-state -- map-state seq )
    dup grid>> walk-map
    [ first ] map
    [ surrounding ] map concat
    uniq
    [ over grid>> on-map? ] filter ;

: clear? ( map-state pos -- map-state ? )
    over grid>> char-at? CHAR: . = ;

: set-char ( c pair grid -- ) 
    [ first2 swap ] dip
    nth
    set-nth ;

: set-block ( map-state pos -- map-state map-grid )
    over
    grid>> clone tuck
    CHAR: # -rot
    set-char ;

: seen? ( visited s current -- visited s current ? )
    pick dupd in? ;

: no-loop ( v s c -- f ) 3drop f ;
: is-loop ( v s c -- t ) 3drop t ;

: (on-map?) ( map-state current -- map-state current ? )
    2dup first swap grid>> on-map? ;

: mark-seen ( visited s current -- visited s current )
    [ pick adjoin ] keep ;

: (turn) ( current next -- turned ) drop turn ;

: (next-step) ( current -- current next )
    dup make-move over second 2array ;

: (blocked?) ( map-state c next -- map-state c next ? )
    dup first reach grid>> blocked? ;

: finish-step ( current next -- next ) nip ;

: (walk-path-loop?) ( visited map-state current -- ? )
    {
        { [ seen? ] [ is-loop ] }
        { [ (on-map?) not ] [ no-loop ] }
        [
            mark-seen
            (next-step)
            (blocked?) [
                (turn)
                (next-step)
            ] when
            finish-step
            (walk-path-loop?)
        ]
    } cond ;

: walk-path-loop? ( grid -- ? )
    initialize-state
    HS{ } clone swap
    dup start-pos>>
    dup .
    (walk-path-loop?) ;

! TODO: walk through once and only keep the matrix coordinates
! that touch the path.
! TODO: walk through the path and generate all the items around it;
! TODO: dedup them; and
! TODO: use that in place of `get-matrix-coordinates`.
: (count-guard-loops) ( map -- n )
    initialize-state
    get-matrix-coordinates
    [ clear? ] filter
    [ set-block walk-path-loop? ] count
    nip ;

: count-loops ( path -- n )
    (file-lines) (count-guard-loops) ;
