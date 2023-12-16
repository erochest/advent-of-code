! Copyright (C) 2023 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: advent.io arrays assocs kernel math math.parser ranges
       sequences splitting unicode ;
IN: advent.y2023.day06


: parse-data ( lines -- assoc-array )
    [ ":" split1 nip split-numbers ] map
    first2 zip ;

: parse-file ( path -- assoc-array ) (file-lines) parse-data ;

: race-distance ( race-length button-time -- travelled )
    [ - ] keep * ;

: count-winning-presses ( best race-length -- count )
    dup [0..b]
    [ dupd race-distance ] map nip
    [ dupd < ] filter nip
    length ;

: get-winning-count-product ( assoc-array -- product )
    [ first2 swap count-winning-presses ] map product ;

: fix-kerning ( string -- n )
    [ blank? ] trim
    " " split
    [ empty? not ] filter
    "" join
    string>number ;

: parse-kerned ( lines -- time distance )
    [ ":" split1 nip fix-kerning ] map
    first2 ;

: get-kerned-count-product ( time distance -- product )
    swap count-winning-presses ;
