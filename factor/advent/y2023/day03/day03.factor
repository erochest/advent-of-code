! Copyright (C) 2023 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: accessors advent.io arrays kernel math math.parser
       namespaces ranges regexp sequences sequences.deep
       strings ;
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

: get-surrounding-coords ( grid region -- array )
    2drop { } clone ;

: get-number-coords ( grid -- array ) 2drop { } clone ;

: get-part-numbers ( grid coord -- array ) 2drop { } clone ;

: (sum-part-numbers) ( grid -- n ) ;
    ! dup length [
    ! ] each-integer

: sum-part-numbers ( path -- n ) ;
