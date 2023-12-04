! Copyright (C) 2023 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: accessors advent.io arrays assocs assocs.extras
       hashtables io kernel math math.order math.parser
       namespaces prettyprint sequences splitting strings
       unicode ;
IN: advent.y2023.day02

SYMBOL: max-grab-target
H{ { "red" 12 } { "green" 13 } { "blue" 14 } }
max-grab-target set-global

TUPLE: color-grab { count integer } { color string } ;
: <color-grab> ( count color -- color-grab ) color-grab boa ;

TUPLE: game { id integer } { grabs array } ;
: <game> ( id grabs -- game ) game boa ;

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

: (read-games) ( path -- game-array ) (file-lines) [ >game ] map ;

: (sum-possible-ids) ( max-target-hash game-array -- n )
    [ grabs>> [ dupd possible-given? ] all? ] filter
    [ id>> ] map-sum
    nip ;

: sum-possible-ids ( path -- n )
    max-grab-target get-global swap
    (read-games)
    (sum-possible-ids)
    ;

: debug-possible-ids ( path -- arg )
    (file-lines) [ dup >game 2array ] map ;

: merge-count ( counts grab -- counts )
    [ color>> ] [ count>> ] bi 
    ! copied/changed from the definition of of+
    [ [ 0 or ] ] dip [ max ] curry compose change-of
    ;

: collect-max-counts ( grab-list count-array -- counts )
    [ merge-count ] reduce ;

: collect-game-max-counts ( grabs -- counts )
    H{ } clone
    [ swap collect-max-counts ] reduce ;

: power-cubes ( grab-counts -- power ) values product ;

: (sum-power-cubes) ( game-array -- sum )
    [
        grabs>>
        collect-game-max-counts
        power-cubes
    ] map-sum ;
: sum-power-cubes ( path -- sum ) (read-games) (sum-power-cubes) ;
