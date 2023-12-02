! Copyright (C) 2023 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: accessors advent.io arrays assocs hashtables kernel math
       math.parser namespaces sequences splitting strings
       unicode ;
IN: advent.y2023.day02

SYMBOL: max-grab-target
H{ { "red" 12 } { "green" 13 } { "blue" 14 } }
max-grab-target set-global

TUPLE: color-grab { count integer } { color string } ;
: <color-grab> ( count color -- color-grab ) color-grab boa ;

TUPLE: game { id integer } { grabs array } ;
: <game> ( id grabs -- game ) game boa ;

: trim-ws ( string -- string ) [ blank? ] trim ;

: >color-grab ( string -- f/color-grab )
    " " split1
    [ [ string>number ] dip <color-grab> ] [ drop f ] if*
    ;

: >color-grab-list ( string -- array )
    "," split [ trim-ws >color-grab ] map ;

: >id ( string -- id ) " " split1 nip string>number ;
: >game-grabs ( string -- array )
    trim-ws
    ";" split [ trim-ws >color-grab-list ] map
    ;

: >game ( string -- game )
    ":" split1
    [ >id ] [ >game-grabs ] bi*
    <game> ;

: (possible-given?) ( hash color-grab -- hash ? )
    dupd
    [ color>> ] [ count>> ] bi
    [ swap at ] dip
    swap
    [ <= ] [ drop f ] if*
    ;

: possible-given? ( given-grab-hash grab-list -- ? )
    [ (possible-given?) ] all? nip ;

! mth game-ary
! mth mth grab-ary
! mth f
: sum-possible-ids ( max-target-hash path -- n )
    (file-lines)
    [ >game ] map
    [ grabs>> [ dupd possible-given? ] all? ] filter
    [ id>> ] map-sum
    nip
    ;

: debug-possible-ids ( path -- arg )
    (file-lines) [ dup >game 2array ] map ;
