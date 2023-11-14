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

TUPLE: generator current nextf ;

: <generator> ( n f -- generator ) generator boa ;

: next-code ( prev mult div -- next ) [ * ] dip /mod nip ;

: aoc-generator ( -- generator )
    first-code get-global
    multiplier get-global 
    divisor get-global [ next-code ] curry curry
    <generator> ;

: skip ( gen -- gen' )
    dup
    [ current>> ] [ nextf>> ] bi call( n -- n )
    >>current ;


: step ( gen -- current gen' ) [ current>> ] [ skip ] bi ;

: take>array ( generator n -- array )
    dup <vector>
    -rot
    [
        step
        [ suffix ] dip
    ] times
    drop >array ;

TUPLE: coords row col ;
: <coords> ( r c -- coords ) coords boa ;
