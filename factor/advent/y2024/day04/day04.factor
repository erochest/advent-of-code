! Copyright (C) 2024 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: advent.io arrays io kernel math namespaces prettyprint
       sequences ;
IN: advent.y2024.day04

SYMBOLS: DIRECTIONS ;

: (row-coordinates) ( row-string row-n -- seq )
    swap [ nip dupd 2array ] { } map-index-as nip ;
: all-coordinates ( grid -- n )
    [ (row-coordinates) ] map-index concat ;

: char-at ( grid y-x -- char )
    first2
    [ swap nth ] dip
    swap nth ;
: char-at? ( grid y-x -- char/f )
    first2
    [ swap ?nth ] dip
    swap ?nth ;

: filter-char-positions ( grid coord-seq char -- coord-seq' )
    [ = ] curry [ dupd char-at ] prepose filter nip ;

: mod-y ( y-x quot: ( ... y -- ... y' ) -- y-x' )
    [ dup first ] dip call( y -- y ) over set-first ;
: mod-x ( y-x quot: ( ... x -- ... x' ) -- y-x' )
    [ dup second ] dip call( x -- x ) over set-second ;

: up ( y-x -- y-x' ) [ 1 - ] mod-y ;
: down ( y-x -- y-x' ) [ 1 + ] mod-y ;
: left ( y-x -- y-x' ) [ 1 - ] mod-x ;
: right ( y-x -- y-x' ) [ 1 + ] mod-x ;
: up-left ( y-x -- y-x' ) up left ;
: up-right ( y-x -- y-x' ) up right ;
: down-left ( y-x -- y-x' ) down left ;
: down-right ( y-x -- y-x' ) down right ;

{ [ up ] [ up-right ] [ right ] [ down-right ]
  [ down ] [ down-left ] [ left ] [ up-left ] }
DIRECTIONS set-global

: apply-direction ( dir pos -- a )
    over call( x -- x )
    2array ;

: initial-step ( pos -- assoc )
    DIRECTIONS get-global
    [ over clone apply-direction ] map
    nip ;

: pos-is-char? ( grid char pos -- ? ) swapd char-at? = ;

: extend-pairs ( array -- array )
    first2 clone apply-direction ;

: next-step ( grid directions char -- directions' )
    swap
    [ [ 2dup ] dip second pos-is-char? ] filter
    [ 2drop ] dip
    [ extend-pairs ] map ;

: find-all-coordinates ( grid string -- grid grid coords string )
    dupd [ [ all-coordinates ] keep swap ] dip ;

: find-initial-step ( grid grid coords string -- grid rest dirs )
    [ first filter-char-positions [ initial-step ] map concat ]
    [ rest-slice ] bi
    swap ;

: walk-remaining-steps ( grid rest dirs -- dirs )
    [ [ dup ] 2dip next-step ] reduce nip ;

: (find-words) ( grid s -- n )
    find-all-coordinates
    find-initial-step
    walk-remaining-steps
    length ;

: find-words ( file -- n ) (file-lines) "XMAS" (find-words) ;
