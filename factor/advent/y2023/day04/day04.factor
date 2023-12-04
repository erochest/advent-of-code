! Copyright (C) 2023 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: accessors advent.io arrays kernel math
       math.functions math.parser sequences sets splitting ;
IN: advent.y2023.day04

TUPLE: card
       { n integer }
       { winning array }
       { card-numbers array } ;
: <card> ( n winning card-numbers -- card ) card boa ;

: split-words ( string -- array ) 
    " " split [ empty? not ] filter ;

: >card ( string -- card )
    ":" split1
    [ split-words second string>number ]
    [
        "|" split1
        [
            trim-ws split-words [ string>number ] map
        ] bi@
    ] bi*
    <card> ;

: score ( count -- score ) [ 0 ] [ 1 - 2 swap ^ ] if-zero ;

: score-card ( card -- score )
    [ winning>> ] [ card-numbers>> ] bi
    intersect length score ;

: sum-card-scores ( path -- n )
    (file-lines) [ >card score-card ] map-sum ;
