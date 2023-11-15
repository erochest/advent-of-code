! Copyright (C) 2023 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.

USING: accessors arrays kernel math namespaces
       sequences vectors ;
IN: advent.y2015.day01

SYMBOL: first-code
20151125 first-code set-global
SYMBOL: multiplier
252533 multiplier set-global
SYMBOL: divisor
33554393 divisor set-global

TUPLE: fgenerator current nextf ;

: <fgenerator> ( n f -- generator ) fgenerator boa ;

: next-code ( prev mult div -- next ) [ * ] dip /mod nip ;

: code-generator ( -- generator )
    first-code get-global
    multiplier get-global 
    divisor get-global [ next-code ] curry curry
    <fgenerator> ;

! skip the next value and move ahead.
GENERIC: skip ( gen -- )

! return the current value without moving ahead.
GENERIC: peek ( gen -- v )

! return the current value and move ahead.
GENERIC: next ( gen -- v )

: next-and ( gen -- v gen ) [ next ] keep ;

M: fgenerator skip ( gen -- )
    dup
    [ current>> ] [ nextf>> ] bi call( n -- n )
    >>current
    drop ;

M: fgenerator peek ( gen -- v ) current>> ;

M: fgenerator next ( gen -- current ) [ current>> ] [ skip ] bi ;

: take>array ( generator n -- array )
    dup <vector>
    -rot
    [
        next-and
        [ suffix ] dip
    ] times
    drop >array ;

TUPLE: coords col row ;
: <coords> ( c r -- coords ) coords boa ;

: next-coord ( coord -- coord )
    [ col>> 1 + ] [ row>> 1 - ] bi
    [ 1 swap ] when-zero
    <coords> ;

: coord-generator ( -- generator )
    1 1 <coords>
    [ next-coord ] <fgenerator> ;

TUPLE: zip-generator gen1 gen2 ;

: zip ( gen1 gen2 -- zip-gen ) zip-generator boa ;

M: zip-generator skip ( zip-gen -- )
    [ gen1>> skip ] [ gen2>> skip ] bi ;

M: zip-generator peek ( zip-gen -- v )
    [ gen1>> peek ] [ gen2>> peek ] bi 2array ;

M: zip-generator next ( zip-gen -- v )
    [ peek ] [ skip ] bi ;

: code-at ( coord -- code )
    code-generator coord-generator zip
    [ 2dup gen2>> current>> = ] [
        dup skip
    ] until
    nip gen1>> current>> ;