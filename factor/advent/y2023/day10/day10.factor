! Copyright (C) 2024 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: advent.io arrays assocs assocs.extras combinators hashtables io kernel
       math math.order namespaces sequences sequences.deep sets
       vectors ;
IN: advent.y2023.day10

: swapw ( x y z -- z y x ) rot swapd ;
: at-pos ( grid pos -- c/f ) first2 [ swap ?nth ] bi@ ;
: 2dupd ( x y z -- x y x y z ) [ 2dup ] dip ;

<PRIVATE

: start? ( c -- ? ) 83 = ;
: make-pos ( x c y -- x pos c ) swap [ dupd 2array ] dip ;
: float-start ( hold x c y -- hold x )
    make-pos
    start? [ swapw ] when
    drop ;

: find-start ( grid -- position )
    f swap
    [
        swap [ float-start ] each-index
        drop
    ] each-index ;

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

: get-around ( pos -- ht )
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
: filter-in-bounds ( around bounds -- around )
    [ nip swap in-bounds? ] curry assoc-filter ;
: filter-connector ( around grid -- around )
    [ rot at-pos swap in? ] curry assoc-filter ;
: get-next-after-start ( grid pos -- x y )
    get-around
    over get-bounds
    filter-in-bounds
    swap filter-connector
    keys
    first2 ;
: (get-next-steps) ( grid pos c -- x/f y/f )
    {
        { 124 [ nip [ north ] [ south ] bi ] } ! |
        { 45 [ nip [ west ] [ east ] bi ] } ! -
        { 76 [ nip [ north ] [ east ] bi ] } ! L
        { 74 [ nip [ north ] [ west ] bi ] } ! J
        { 55 [ nip [ south ] [ west ] bi ] } ! 7
        { 70 [ nip [ east ] [ south ] bi ] } ! F
        [ start? [ get-next-after-start ] [ 2drop f f ] if ]
    } case ;
! returns only the two next steps from the current position.
: get-next-steps ( grid pos -- x y ) 2dup at-pos (get-next-steps) ;

PRIVATE>

: walk ( grid start next -- distances )
    H{ } clone -rot
    [ 1 swap reach set-at ] keep
    [ 0 swap pick set-at ] dip
    [ dup ] [
        pick swap get-next-steps
        pick [ dupd key? [ drop f ] when ] curry bi@ or
        dup [
            [
                [ dup assoc-size ] dip
                pick set-at
            ] keep
        ] when
    ] while
    drop nip ;

: merge-walks ( walk1 walk2 -- distances )
    [ min ] assoc-merge ;

: find-farthest-distance ( grid -- distance )
    dup find-start 
    2dup get-next-after-start
    [ 2dupd walk ] dip
    swap
    [ walk ] dip
    merge-walks
    values
    0 [ max ] reduce ;
