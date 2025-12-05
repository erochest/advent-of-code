! Copyright (C) 2025 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: accessors arrays hash-sets io.encodings.utf8 io.files
kernel math math.parser multiline peg.ebnf ranges sequences ;
IN: advent.y2025.day05

TUPLE: parse-accum fresh in-stock state ;

: <parse-accum> ( -- parse-accum )
    8 <hash-set> { } clone 0 parse-accum boa ;

EBNF: ingredient-file [=[
    number         = ([0-9])+             => [[ string>number ]]
    range          = (number "-" number)  => [[ [ first ] [ third ] bi [a..b] ]]
    fresh          = range+
    empty          = "\n"
    instock        = number+
    ingredients    = fresh empty instock  => [[ [ first ] [ third ] bi 2array ]]
]=]

: parse-input ( str -- fresh in-stock )
    drop <parse-accum>
    [ ] reduce
    [ fresh>> ] [ in-stock>> ] bi ;
