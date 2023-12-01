! Copyright (C) 2023 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: advent.io arrays combinators kernel math math.parser 
       regexp sequences strings unicode ;
IN: advent.y2023.day01

: ascii>digit ( c -- str ) 48 - ;

: >calibration ( str -- n )
    [ digit? not ] trim
    [ first ascii>digit 10 * ] [ last ascii>digit ] bi + ;

: sum-calibrations ( path -- n ) 
    (file-lines) 
    [ >calibration ] map
    sum ;

: word>digit ( str -- n )
    {
        { "one" [ 1 ] }
        { "two" [ 2 ] }
        { "three" [ 3 ] }
        { "four" [ 4 ] }
        { "five" [ 5 ] }
        { "six" [ 6 ] }
        { "seven" [ 7 ] }
        { "eight" [ 8 ] }
        { "nine" [ 9 ] }
        [ first ascii>digit ]
    } case ;

: first-digit ( str -- n )
    R/ [0-9]|one|two|three|four|five|six|seven|eight|nine/
    first-match
    >string
    word>digit ;

: last-digit ( str -- n )
    R/ [0-9]|one|two|three|four|five|six|seven|eight|nine/r
    first-match
    >string
    word>digit ;

: >calibration-word ( str -- n )
    [ first-digit 10 * ] [ last-digit ] bi + ;

: sum-calibration-words ( path -- n )
    (file-lines)
    [ >calibration-word ] map
    sum ;

: debug-calibration-words ( path -- array )
    (file-lines)
    [ [ >calibration-word ] keep swap 2array ] map ;
