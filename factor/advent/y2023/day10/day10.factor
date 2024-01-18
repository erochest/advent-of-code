! Copyright (C) 2024 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: advent.io arrays assocs combinators hashtables io kernel
       math math.order namespaces sequences sequences.deep sets
       vectors ;
IN: advent.y2023.day10

<PRIVATE

: start? ( c -- ? ) 83 = ;
: swapw ( x y z -- z y x ) rot swapd ;
: make-pos ( x c y -- x pos c ) swap [ dupd 2array ] dip ;
: float-start ( hold x c y -- hold x )
    make-pos
    start? [ swapw ] when
    drop ;

PRIVATE>

: find-start ( grid -- position )
    f swap
    [
        swap [ float-start ] each-index
        drop
    ] each-index ;

: at-pos ( grid pos -- c/f )
    first2 [ swap ?nth ] bi@ ;

! <PRIVATE

SYMBOLS: N S E W ;
{ 124 76 74 } S set-global
{ 124 55 70 } N set-global
{  45 76 70 } W set-global
{  45 74 55 } E set-global

: change-pos ( pos quot -- pos )
    [ first2 ] dip call( x y -- x y ) 2array ;
: north ( pos -- pos ) [ [ 1 - ] dip ] change-pos ;
: south ( pos -- pos ) [ [ 1 + ] dip ] change-pos ;
: east ( pos -- pos ) [ 1 + ] change-pos ;
: west ( pos -- pos ) [ 1 - ] change-pos ;

! returns a hashmap from a position to characters it must
! contain to be valid next-steps.
: get-next-steps ( pos -- hashtable )
    dup north N get-global 2array
    swap dup south S get-global 2array
    swap dup east E get-global 2array
    swap west W get-global 2array
    4array
    >hashtable ;
: get-bounds ( grid -- xy )
    [ length ] [ first length ] bi 2array ;
: in-bounds? ( bounds pos -- ? )
    [ [ first ] bi@ [ > ] [ 0 >= ] bi and ]
    [ [ second ] bi@ [ > ] [ 0 >= ] bi and ] 2bi
    and ;
: current-distance ( distances pos -- d/f ) of ;
: filter-in-bounds ( next-steps bounds -- next-steps )
    [ nip swap in-bounds? ] curry assoc-filter ;
: filter-connector ( next-steps grid -- next-steps )
    [ rot at-pos swap in? ] curry assoc-filter ;
: calculate-next-distance
    ( next-distance value -- value' )
    {
        { [ dup not ] [ drop dup ] }
        { [ 2dup < ] [ drop dup ] }
        [ ]
    } cond
    nip ;
: update-distances
    ( next-pos-seq distances next-distance -- distances )
    [
        2over swap
        [ dupd calculate-next-distance ] change-at
        2drop
    ] curry reduce ;

! updates the distances and returns the next positions to
! process.
: get-links ( distances grid pos -- next-positions )
    ! TODO: this should start by looking at what's in the current
    ! position and only returning surrounding squares that are 
    ! connected to the current value. 'S' would look everywhere.
    dup get-next-steps
    pick get-bounds
    filter-in-bounds
    rot filter-connector
    keys 2over current-distance nipd 1 +
    swapd [ dup ] 2dip
    update-distances
    drop ;

! PRIVATE>

: 2dupd ( x y z -- x y x y z ) [ 2dup ] dip ;

: yn-prompt ( msg -- ? )
    write " " write readln first 121 = ;
: continue? ( -- ? ) "continue?" yn-prompt ;

: pop-current-pos ( pos-queue x y -- pos-queue' x y pos )
    [ dup pop ] 2dip rot ;

: append-next-pos ( pos-queue x y new-poses -- pos-queue' x y )
    -rot [ append ] 2dip ;

: (get-links)
    ( pos-queue distances grid -- pos-queue distances grid )
    pop-current-pos
    2dupd get-links
    append-next-pos ;

! 2,0->0
! 2,1->1
! 3,0->1
! 1,1->2
! 4,0->2
! 4,1->3
! 1,2->3
! 0,2->4
! 3,1->4
! 0,3->5
! 3,2->5
! 1,3->6
! 3,3->6
! 2,3->7
! 3,4->7
! 3,3->8
: find-farthest-distance ( grid -- distance )
    dup find-start
    [ 0 swap associate ] [ 1vector ] bi
    swapw
    [ pick empty? continue? not or ] [
        (get-links)
    ] until
    drop nip
    values
    0 [ max ] reduce ;
