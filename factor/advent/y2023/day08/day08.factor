! Copyright (C) 2023 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: accessors advent.io arrays assocs combinators hashtables
       io kernel locals math math.functions prettyprint
       sequences splitting strings unicode ;
FROM: splitting => split-words ;
IN: advent.y2023.day08

TUPLE: map-network
    { path array }
    { network hashtable } ;
: <map-network> ( path network -- map-network )
    map-network boa ;

SYMBOLS: R L ;

: string>direction ( string -- direction )
    {
        { 82 [ R ] }
        { 76 [ L ] }
    } case ;

: alphanum? ( c -- ? ) [ alpha? ] [ digit? ] bi or ;

: string>network-node ( string -- tuple )
    split-words
    [ first ] [
        rest rest
        [ >array [ alphanum? ] filter >string ] map
    ] bi
    2array ;

: parse-input ( string -- map )
    split-lines [
        first >array [ string>direction ] map
    ] [
        rest rest [ string>network-node ] map >hashtable
    ] bi
    <map-network> ;

: left ( network current -- next ) of first ;
: right ( network current -- next ) of second ;

: next-node ( network current direction -- next )
    [ "next-node " write .s ] when-logging
    {
        { L [ left ] }
        { R [ right ] }
    } case ;

: dupdd ( x y z -- x x y z ) [ dup ] 2dip ;

: start? ( node-name -- ? ) last 65 = ;
: end? ( node-name -- ? ) last 90 = ;

: initial-positions ( map-network -- node-array )
    "initial-positions" trace
    network>> keys
    [ start? ] filter ;

! mn c sc'
: (follow-path-to-end) ( map-network start -- step-count )
    0
    [ over end? ] [
        [ dup [ network>> ] [ path>> ] bi ] 2dip
        [ [ dupdd next-node ] reduce nip ] dip
        1 +
    ] until
    rot path>> length *
    nip ;

: follow-path-to-end ( map-network -- step-count )
    "AAA" (follow-path-to-end) ;

: follow-path-to-ghost-end ( map-network -- step-count )
    dup initial-positions
    [ dupd (follow-path-to-end) ] map nip
    1 [ lcm ] reduce ;
