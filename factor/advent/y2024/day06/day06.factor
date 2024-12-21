! Copyright (C) 2024 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: accessors advent.io advent.map arrays combinators
       hash-sets io kernel math namespaces prettyprint ranges
       regexp sequences sets vectors ;
IN: advent.y2024.day06

! These consistently use coords of y, x :/

! TODO: test from bottom up
! TODO: use Nguard and Ncheck
! TODO: print map-state

SYMBOLS: DIRECTIONS MOVEMENTS ;
{ CHAR: ^ CHAR: > CHAR: v CHAR: < } DIRECTIONS set-global
{ [ up ] [ right ] [ down ] [ left ] } MOVEMENTS set-global

: find-blocks ( grid -- block-set )
    HS{ } clone [
        swap HS{ } clone [
            swap CHAR: # = [
                [ over ] dip 2array
                over adjoin
            ] [
                drop
            ] if
        ] reduce-index
        nip union!
    ] reduce-index ;

: char>direction ( c -- d/f ) DIRECTIONS get-global index ;
: direction>move ( d -- m/f ) MOVEMENTS get-global ?nth ;

! : find-guard-row ( map -- n row )
!     [ R/ [<^>v]/ re-contains? ] find ;
! : find-guard-col ( row -- n direction )
!     [ DIRECTIONS get-global in? ] find ;
: find-guard ( block-set -- point direction )
f ;
!     find-guard-row find-guard-col
!     [ 2array ] dip
!     char>direction ;
! 
! TUPLE: map-state block-set x-bounds y-bounds start-pos ;
! ! reader>>
! ! writer<< ( value obj -- obj )
! ! >>setter ( obj value -- obj )
! ! change-changer ( obj quot -- obj )
! : <map-state> ( grid -- map-state )
!     [ find-blocks ] keep
!     [ 0 tuck nth length ] keep
!     [ 0 tuck length ] keep
!     find-guard
!     map-state boa ;
! M: map-state clone
!     {
!         [ block-set>> clone ]
!         [ x-bounds>> ]
!         [ y-bounds>> ]
!         [ start-pos>> ]
! 
!     } cleave
!     map-state boa ;
! 
! : in-range ( i boundary -- ? )
!     [ [ 0 >= ] keep ] dip < and ;
! 
! : on-map? ( pos map-state -- ? )
!     [ first2 swap ] dip
!     [ y-bounds>> ] [ x-bounds>> ] bi
!     [ in-range ] dip
!     swapd in-range and ;
! 
! : read-char-at? ( pos map-state -- c )
!     block-set>> in? [ CHAR: # ] [ CHAR: . ] if ;
! 
! : blocked? ( pos map-state -- ? ) block-set>> in? ;
! 
! : make-move ( pair -- pos )
!     first2
!     [ clone ] [ direction>move ] bi*
!     map-move ;
! 
! : turn ( pair -- pair' )
!     [ 1 + DIRECTIONS get-global length mod ] mod-x ;
! 
! ! This also returns the last, out of bounds step.
! : next-step ( pair map-state -- pair' )
!     over make-move
!     2dup swap blocked?
!     [ 2drop turn [ make-move ] keep ]
!     [ nip swap ] if
!     second 2array ;
! 
! : (walk/init) ( map -- map guard-pair ) dup find-guard 2array ;
! : (walk/on-map?) ( map pair -- map pair ? )
!     dup first pick on-map? ;
! : (walk/next-step) ( map pair -- map pair ) over next-step ;
! : (walk/clean-up) ( map pair seq -- seq ) 2nip ;
! 
! : walk-map ( map-state -- pair-seq )
!     (walk/init)
!     [ (walk/on-map?) ]
!     [ (walk/next-step) dup ] produce
!     (walk/clean-up) ;
! 
! 
! : uniq ( seq -- seq' ) >hash-set members ;
! : count-uniq ( seq -- n ) uniq length ;
! 
! : (count-visited-spaces) ( map-matrix -- n )
!     walk-map
!     [ first ] map
!     count-uniq ;
! 
! : count-visited-spaces ( path -- n )
!     (file-lines) (count-visited-spaces) ;
! 
! : log ( tag -- ) print .s nl ;
! 
! : initialize-state ( map-grid -- map-state ) <map-state> ;
! 
! : surrounding ( pos -- seq )
!     { [ up ] [ down ] [ left ] [ right ] } cleave 4array ;
! 
! : get-matrix-coordinates ( map-state -- map-state seq )
!     dup walk-map
!     [ first ] map uniq
!     [ over on-map? ] filter ;
! 
! : clear? ( map-state pos -- map-state ? )
!     over blocked? not ;
! 
! : set-block ( map-state pos -- map-state map-state' )
!     [ dup clone ] dip
!     over [ adjoin ] change-block-set ;
! 
! : seen? ( visited s current -- visited s current ? )
!     [ nip swap in? ] 3check ;
! 
! : no-loop ( v s c -- f ) 3drop f ;
! : is-loop ( v s c -- t ) 3drop t ;
! 
! : (on-map?) ( map-state current -- map-state current ? )
!     2dup first swap on-map? ;
! 
! : mark-seen ( visited s current -- visited s current )
!     [ pick adjoin ] keep ;
! 
! : (turn) ( current next -- turned ) drop turn ;
! 
! : (next-step) ( current -- current next )
!     dup make-move over second 2array ;
! 
! : (blocked?) ( map-state c next -- map-state c next ? )
!     dup first reach block-set>> blocked? ;
! 
! : finish-step ( current next -- next ) nip ;
! 
! : (walk-path-loop?) ( visited map-state current -- ? )
!     {
!         { [ seen? ] [
!             pick "loop: " print [ pprint " " write ] each nl
!             is-loop
!         ] }
!         { [ (on-map?) not ] [ no-loop ] }
!         [
!             mark-seen
!             (next-step)
!             (blocked?) [
!                 (turn)
!                 (next-step)
!             ] when
!             finish-step
!             (walk-path-loop?)
!         ]
!     } cond ;
! 
! : walk-path-loop? ( map-state -- ? )
!     ! HS{ } clone swap
!     V{ } clone swap
!     dup start-pos>>
!     dup .
!     (walk-path-loop?) ;
! 
! : (count-guard-loops) ( map -- n )
!     initialize-state
!     get-matrix-coordinates
!     [ set-block walk-path-loop? ] count
!     nip ;
! 
! : count-loops ( path -- n )
!     (file-lines) (count-guard-loops) ;
