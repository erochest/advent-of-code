! Copyright (C) 2023 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: accessors advent.io arrays assocs combinators hashtables
       kernel locals sequences splitting strings unicode ;
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

: string>network-node ( string -- tuple )
    split-words
    [ first ] [
        rest rest
        [ >array [ Letter? ] filter >string ] map
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
    {
        { L [ left ] }
        { R [ right ] }
    } case ;

:: follow-path-to-end ( network -- step-sequence )
    { "AAA" }
    [ dup last "ZZZ" = ] [
        network network>>
        network path>> rot
        [ next-node ] reduce
        nip
    ] until
    rest ;
