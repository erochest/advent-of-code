! Copyright (C) 2022 Eric Rochester.
! See http://factorcode.org/license.txt for BSD license.
USING: advent.io arrays kernel locals math math.ranges
sequences ;
IN: advent.year2021.day07

: domain ( seq -- seq ) [ infimum ] [ supremum ] bi [a,b] ;
: calculate-between-all ( positions pos distance-func -- fuel )
    curry map sum ; inline
:: calculate-fuel-costs ( distance-func positions -- seq )
    positions domain [
        positions distance-func swapd calculate-between-all
    ] map ; inline
: day07 ( path distance-func -- n )
    swap read>numbers
    calculate-fuel-costs
    infimum ; inline

: fuel ( from to -- fuel ) - abs ;
: parta ( path -- n ) [ fuel ] day07 ;

: sigma ( n -- n ) [1,b] sum ;
: crab-fuel ( from to -- fuel ) - abs sigma ;
: partb ( path -- n ) [ crab-fuel ] day07 ;
