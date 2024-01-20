! Copyright (C) 2024 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: advent.io arrays assocs assocs.extras combinators
       grouping hash-sets hashtables io kernel math
       math.functions math.order namespaces prettyprint ranges
       sequences sequences.deep sets strings vectors ;
IN: advent.y2023.day10

: swapw ( x y z -- z y x ) rot swapd ;
: at-pos ( grid pos -- c/f ) first2 [ swap ?nth ] bi@ ;
: 2dupd ( x y z -- x y x y z ) [ 2dup ] dip ;
: 2vector ( x y -- v ) 2array >vector ;

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
: get-next-steps ( grid pos -- x y )
    2dup at-pos (get-next-steps) ;

PRIVATE>

: walk-seq ( grid start next -- pos-seq )
    [ 2vector ] keep
    [ dup ] [
        pick swap get-next-steps
        pick [ dupd in? [ drop f ] when ] curry bi@ or
        dup [
            [ over push ] keep
        ] when
    ] while
    drop nip ;

: walk ( grid start next -- distances )
    walk-seq
    <enumerated>
    [ first2 swap 2array ] map
    >hashtable ;

: merge-walks ( walk1 walk2 -- distances )
    [ min ] assoc-merge ;


: grid-next-steps ( grid -- start next1 next2 )
    dup find-start 
    tuck get-next-after-start ;

: find-pipes ( grid -- hash )
    dup grid-next-steps
    [ 2dupd walk ] dip
    swap
    [ walk ] dip
    merge-walks ;

: find-farthest-distance ( grid -- distance )
    find-pipes values
    0 [ max ] reduce ;

: all-points ( bounds -- seq )
    first2 [ [0..b) ] bi@ cartesian-product flatten1 ;

: cast-left ( point -- seq )
    [ second 0 [a..b] ] [ first ] bi
    [ swap 2array ] curry map ;

: cast-right ( point width -- seq )
    [ [ second ] dip [a..b] ] [ drop first ] 2bi
    [ swap 2array ] curry map ;

SYMBOLS: XOVER ;
"S|F7" >array XOVER set-global

: (enclosed?) ( bounds path-mapping point -- ? )
    cast-left
    [ over key? ] filter
    [ over at ] map
    [ XOVER get-global in? ] count
    odd?
    2nip ;

: enclosed? ( bounds path-set point -- ? )
    2dup swap key? [ 3drop f ] [ (enclosed?) ] if ;

: determinate ( p1 p2 -- n )
    [ [ first ] [ second ] bi* * ]
    [ [ second ] [ first ] bi* * ] 2bi
    - ;

! Shoelace formula
! https://en.wikipedia.org/wiki/Shoelace_formula
! from reddit tip
: pipe-area ( path -- area )
    2 circular-clump
    [ first2 determinate ] map-sum
    2 / abs ;

! Pick's theorem
! https://en.wikipedia.org/wiki/Pick%27s_theorem
! from reddit tip
: interior-points ( area exterior -- interior )
    2 / - 1 + ;

: count-enclosed ( grid -- count )
    dup grid-next-steps drop
    walk-seq
    [ pipe-area ] keep
    length interior-points ;
