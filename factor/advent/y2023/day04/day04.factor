! Copyright (C) 2023 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: accessors advent.io arrays assocs assocs.extras kernel 
       locals math math.functions math.parser ranges sequences
       sets splitting ;
IN: advent.y2023.day04

TUPLE: card
       { n integer }
       { winning array }
       { card-numbers array } ;
: <card> ( n winning card-numbers -- card ) card boa ;

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

: count-matches ( card -- count )
    [ winning>> ] [ card-numbers>> ] bi
    intersect length ;

: score ( count -- score ) [ 0 ] [ 1 - 2 swap ^ ] if-zero ;

: score-card ( card -- score ) count-matches score ;

: sum-card-scores ( path -- n )
    (file-lines) [ >card score-card ] map-sum ;

:: sum-cascading-card ( accum card -- hash-map )
    card n>> :> n
    accum n inc-of :> accum
    n accum at :> current
    n card count-matches dupd + (a..b]
    accum
    current [ of+ ] curry
    reduce
    ;

: (sum-total-cards) ( card-array -- sum )
    H{ } clone [ sum-cascading-card ] reduce
    values sum
    ;

: sum-total-cards ( path -- n )
    (file-lines) [ >card ] map (sum-total-cards) ;
