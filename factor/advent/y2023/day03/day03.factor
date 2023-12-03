! Copyright (C) 2023 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: accessors advent.io arrays hash-sets kernel locals math
       math.order math.parser namespaces ranges regexp
       sequences sequences.deep sets strings ;
IN: advent.y2023.day03

SYMBOLS: number-re fixture-data puzzle-data ;

R/ \d+/ number-re set-global
2023 3 fixture (file-lines) fixture-data set-global

TUPLE: number-region { number integer } { points array } ;
: <number-region> ( n array -- number-region )
    number-region boa ;

: find-numbers ( lines -- array )
    number-re get-global swap
    [
        -rot over
        all-matching-slices
        [
            [ >string string>number ]
            [
                [ from>> ] [ to>> ] bi [a..b)
                reach [ 2array ] curry map
            ] bi
            <number-region>
        ] map
        [ drop ] 2dip
    ] map-index
    flatten nip
    ;

: min-bounding ( x y -- z )
    [ [ first  ] bi@ min ]
    [ [ second ] bi@ min ] 2bi
    2array ;

: max-bounding ( x y -- z )
    [ [ first  ] bi@ max ]
    [ [ second ] bi@ max ] 2bi
    2array ;

: append-point! ( vector x y -- )
    2array suffix! drop
    ;

:: (rectangle-seq) ( y accum from to -- )
    from to [ first ] bi@ 1 + [
        accum swap y append-point!
    ] each-integer-from
    ;

:: rectangle-seq ( from to -- array )
    V{ } clone :> accum
    from to [ second ] bi@ 1 + [
        accum from to (rectangle-seq)
    ] each-integer-from
    accum
    ;

: in-bounds? ( point -- ? ) [ 0 >= ] all? ;

: get-surrounding ( region -- point-array )
    points>>
    dup
    [ { 100000 100000 } [ min-bounding ] reduce [ 1 - ] map ]
    [ {      0      0 } [ max-bounding ] reduce [ 1 + ] map ] bi
    rectangle-seq
    [ in-bounds? ] filter
    swap diff
    ;

! ---

: get-surrounding-coords ( grid region -- array )
    2drop { } clone ;

: get-number-coords ( grid -- array ) drop { } clone ;

: get-part-numbers ( grid coord -- array ) 2drop { } clone ;

: (sum-part-numbers) ( grid -- n ) ;
    ! dup length [
    ! ] each-integer

: sum-part-numbers ( path -- n ) ;
