! Copyright (C) 2023 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: kernel math namespaces sequences ;
IN: advent.y2015.day02

: total-weight ( group -- weight ) sum ;

: quantum-entanglement ( group -- qe ) product ;

SYMBOL: balance-scale
SYMBOL: size-scale
SYMBOL: qe-scale

1000 balance-scale set-global
50 size-scale set-global
10000 qe-scale set-global

: (balance-score) ( groups -- score )
    [ total-weight ] map
        [ [ first  ] [ second ] bi - abs ]
        [ [ first  ] [ third  ] bi - abs ]
        [ [ second ] [ third  ] bi - abs ] tri
    + +
    balance-scale get-global / ;

: (size-score) ( groups -- score )
    first length size-scale get-global / ;

: (qe-score) ( groups -- score )
    first quantum-entanglement qe-scale get-global / ;

: balance-score ( groups -- score )
    [ (balance-score) ]
    [ (size-score) ]
    [ (qe-score) ] tri
    + + ;

