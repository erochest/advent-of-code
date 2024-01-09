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

: left ( network path -- network path' )
    2dup last of first suffix ;

: right ( network path -- network path' )
    2dup last of second suffix ;

: next-node ( network path direction -- network path )
    [ "next-node " write .s ] when-logging
    {
        { L [ left ] }
        { R [ right ] }
    } case ;

: start? ( node-name -- ? ) last 65 = ;
: end? ( node-name -- ? ) last 90 = ;

: initial-positions ( map-network -- node-array )
    "initial-positions" trace
    network>> keys
    [ start? ] filter ;

: (follow-path-to-end) ( map-network start -- step-sequence )
    1array
    [ dup last end? ] [
        [ dup [ network>> ] [ path>> ] bi ] dip
        [ next-node ] reduce
        nip
    ] until
    nip rest ;

: follow-path-to-end ( map-network -- step-sequence )
    "AAA" (follow-path-to-end) ;

: follow-path-to-ghost-end ( map-network -- step-count )
    dup initial-positions
    [ dupd (follow-path-to-end) length ] map nip
    1 [ lcm ] reduce ;
