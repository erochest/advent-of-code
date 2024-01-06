! Copyright (C) 2023 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: accessors advent.io arrays assocs combinators hashtables
       io kernel locals math prettyprint sequences splitting
       strings unicode ;
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

:: follow-path-to-end ( network -- step-sequence )
    { "AAA" }
    [ dup last "ZZZ" = ] [
        network network>>
        network path>> rot
        [ next-node ] reduce
        nip
    ] until
    rest ;

TUPLE: working-state { current array }  { steps integer } ;
: <working-state> ( current -- working-state )
    1 working-state boa ;

: start? ( node-name -- ? ) last 65 = ;
: end? ( node-name -- ? ) last 90 = ;

: all-at-end? ( working-state -- working-state ? )
    dup current>> [ end? ] all? ;

<PRIVATE

: trace ( name -- )
    [ write .s ] curry when-logging ;

: initial-positions ( map-network -- working-state )
    "initial-positions" trace
    network>> keys
    [ start? ] filter
    <working-state> ;

: (arrange-reduce-stack)
    ( working-state map-network -- network direction-list working-state )
    "(arrange-reduce-stack)" trace
    [ network>> ] [ path>> ] bi rot ;

: (get-next-direction)
    ( working-state map-network -- map-network working-state direction )
    [ path>> over steps>> over length mod swap nth ] keep -rot ;

: (get-next-direction-shuffleless)
    ( map-network working-state -- direction )
    [ path>> dup length ] [ steps>> swap mod ] bi*
    swap nth ;

: left-state ( network working-state -- network working-state )
    [
        [ dupd [ network>> ] dip of first ] map
    ] change-current
    [ 1 + ] change-steps ;

: right-state ( network working-state -- network working-state )
    [
        [ dupd [ network>> ] dip of second ] map
    ] change-current
    [ 1 + ] change-steps ;

: (extend-path-state)
    ( network working-state direction -- network working-state )
    [ "(extend-path-state)" write .s ] when-logging
    {
        { L [ left-state ] }
        { R [ right-state ] }
    } case ;

: (clean-up) ( network working-state -- working-state )
    "(clean-up)" trace
    nip ;

: (compute-path-length) ( working-state -- path-length )
    "(compute-path-length)" trace
    steps>> 1 - ;

PRIVATE>

: follow-path-to-ghost-end-stack ( network -- step-count )
    dup initial-positions
    [ all-at-end? ] [
        over 2dup (get-next-direction-shuffleless)
        (extend-path-state)
        (clean-up)
        ! dup steps>> 1000000 mod [ dup steps>> . flush 0 ] when-zero drop
        ! [ "step " write dup . nl ] when-logging
    ] until
    nip (compute-path-length) ;

:: follow-path-to-ghost-end-locals ( network -- step-count )
    network initial-positions
    [ all-at-end? ] [
        network (get-next-direction)
        (extend-path-state)
        (clean-up)
        ! dup steps>> 1000000 mod [ dup steps>> . flush 0 ] when-zero drop
        ! [ "step " write dup . nl ] when-logging
    ] until
    (compute-path-length) ;
