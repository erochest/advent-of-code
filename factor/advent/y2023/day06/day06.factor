! Copyright (C) 2023 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: advent.io arrays assocs kernel math ranges sequences
       splitting ;
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
