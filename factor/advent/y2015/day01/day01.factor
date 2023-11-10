! Copyright (C) 2023 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: arrays kernel math namespaces ;
IN: advent.y2015.day01

SYMBOL: first-code
20151125 first-code set-global
SYMBOL: multiplier
252533 multiplier set-global
SYMBOL: divisor
33554393 divisor set-global

: next-code ( div mult prev -- next ) * swap /mod nip ;

: take>array ( lazy-list n -- array )
    dup <vector>
    -rot
    [
        [ car ] [ cdr ] bi
        [ suffix ] dip
    ] times
    drop >array ;
