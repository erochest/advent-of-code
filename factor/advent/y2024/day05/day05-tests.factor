! Copyright (C) 2024 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: advent.io advent.y2024.day05 namespaces sequences tools.test ;
IN: advent.y2024.day05.tests

SYMBOLS: FIXTURE ;

2024 5 0 fixture+ (file-lines) FIXTURE set-global

{
    {
        { "line 1" "line 2" "line 3" "line 4" }
        { "line 5" "line 6" }
        { "line 7" "line 8" }
    }
} [
    { "line 1" "line 2" "line 3" "line 4" ""
      "line 5" "line 6" "" "line 7" "line 8" }
    split-paragraphs
] unit-test

{ { { 53 29 } { 61 53 } { 97 53 } { 61 29 } { 47 13 } } }
[ { "53|29" "61|53" "97|53" "61|29" "47|13" } parse-order-rules ] unit-test

{ { { 75 29 13 } { 75 97 47 61 53 } } }
[ { "75,29,13" "75,97,47,61,53" } parse-updates-pages ]
unit-test

{ t }
[
    { 75 47 61 53 29 }
    { 
        { 47 53 } { 97 61 } { 97 47 } { 75 29 } { 61 13 } { 75 53 } { 29 13 }
        { 53 29 } { 61 53 } { 97 53 } { 61 29 } { 47 13 } { 97 75 }
        { 75 47 } { 47 61 } { 75 61 } { 47 29 }
    }
    update-in-order?
] unit-test

{ f }
[
    { 75 97 47 61 53 }
    { 
        { 47 53 } { 97 61 } { 97 47 } { 75 29 } { 61 13 } { 75 53 } { 29 13 }
        { 53 29 } { 61 53 } { 97 53 } { 61 29 } { 47 13 } { 97 75 }
        { 75 47 } { 47 61 } { 75 61 } { 47 29 }
    }
    update-in-order?
] unit-test

{ { 61 53 29 } } [
    {
        { 75 47 61 53 29 }
        { 97 61 53 29 13 }
        { 75 29 13 }
    }
    [ middle-item ] map
] unit-test

{ 143 } [ FIXTURE get-global (ordered-update-checksum) ] unit-test
