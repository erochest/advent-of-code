! Copyright (C) 2024 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: advent.io advent.y2024.day04 kernel math namespaces
       sequences tools.test ;
IN: advent.y2024.day04.tests


SYMBOLS: FIXTURE ;
2024 4 0 fixture+ (file-lines) FIXTURE set-global

{ { 
    { 0 0 } { 0 1 } { 0 2 } { 0 3 } { 0 4 }
    { 1 0 } { 1 1 } { 1 2 } { 1 3 } { 1 4 }
    { 2 0 } { 2 1 } { 2 2 } { 2 3 } { 2 4 }
    { 3 0 } { 3 1 } { 3 2 } { 3 3 } { 3 4 }
} } [
    { "abcde" "fghij" "klmno" "qrstu" } all-coordinates
] unit-test

{ { 100 102 111 115 } }
[
    { "abcde" "fghij" "klmno" "qrstu" }
    { { 0 3 } { 1 0 } { 2 4 } { 3 2 } }
    [ dupd char-at ] map
    nip
] unit-test

{ { { 1 2 } } } [ 
    { "abcde" "fghij" "klmno" "qrstu" }
    { 
        { 0 0 } { 0 1 } { 0 2 } { 0 3 } { 0 4 }
        { 1 0 } { 1 1 } { 1 2 } { 1 3 } { 1 4 }
        { 2 0 } { 2 1 } { 2 2 } { 2 3 } { 2 4 }
        { 3 0 } { 3 1 } { 3 2 } { 3 3 } { 3 4 }
    } 
    104
    filter-char-positions
] unit-test

{ { 4 2 } { 0 4 } { -1 4 } } [
    { 3 2 } [ 1 + ] mod-y
    { 1 4 } [ 1 - ] mod-y
    { 0 4 } [ 1 - ] mod-y
] unit-test

{ { 2 4 } { 4 0 } { 4 -1 } } [
    { 2 3 } [ 1 + ] mod-x
    { 4 1 } [ 1 - ] mod-x
    { 4 0 } [ 1 - ] mod-x
] unit-test

{ { 3 2 } } [ { 4 2 } up ] unit-test
{ { 5 2 } } [ { 4 2 } down ] unit-test
{ { 4 3 } } [ { 4 2 } right ] unit-test
{ { 4 1 } } [ { 4 2 } left ] unit-test
{ { 3 3 } } [ { 4 2 } up-right ] unit-test
{ { 3 1 } } [ { 4 2 } up-left ] unit-test
{ { 5 3 } } [ { 4 2 } down-right ] unit-test
{ { 5 1 } } [ { 4 2 } down-left ] unit-test

{
    {
        { [ up ] { 1 0 } }
        { [ up-right ] { 1 1 } }
        { [ right ] { 2 1 } }
        { [ down-right ] { 3 1 } }
        { [ down ] { 3 0 } }
        { [ down-left ] { 3 -1 } }
        { [ left ] { 2 -1 } }
        { [ up-left ] { 1 -1 } }
    }
} [
    { 2 0 } initial-step
] unit-test

{ CHAR: g } [
    { "abcde" "fghij" "klmno" "qrstu" }
    { 1 1 } char-at?
] unit-test
{ f } [
    { "abcde" "fghij" "klmno" "qrstu" }
    { -1 1 } char-at?
] unit-test
{ f } [
    { "abcde" "fghij" "klmno" "qrstu" }
    { 1 -1 } char-at?
] unit-test
{ f } [
    { "abcde" "fghij" "klmno" "qrstu" }
    { 1 7 } char-at?
] unit-test

{ f } [
    { "abcde" "fghij" "klmno" "qrstu" }
    CHAR: g
    { -1 1 } pos-is-char?
] unit-test
{ f } [
    { "abcde" "fghij" "klmno" "qrstu" }
    CHAR: g
    { 2 2 } pos-is-char?
] unit-test
{ t } [
    { "abcde" "fghij" "klmno" "qrstu" }
    CHAR: g
    { 1 1 } pos-is-char?
] unit-test

{ { [ up ] { 1 0 } } } [
    { [ up ] { 2 0 } } extend-pairs
] unit-test
{ { [ left ] { 2 -1 } } } [
    { [ left ] { 2 0 } } extend-pairs
] unit-test
{ { [ left ] { 2 1 } } } [
    { [ left ] { 2 2 } } extend-pairs
] unit-test

{ { { [ up-right ] { 0 2 } } } } [
    { "abcde" "fghij" "klmno" "qrstu" }
    {
        { [ up ] { 1 0 } }
        { [ up-right ] { 1 1 } }
        { [ right ] { 2 1 } }
        { [ down-right ] { 3 1 } }
        { [ down ] { 3 0 } }
        { [ down-left ] { 3 -1 } }
        { [ left ] { 2 -1 } }
        { [ up-left ] { 1 -1 } }
    }
    CHAR: g
    next-step
] unit-test

{ t } [ FIXTURE get-global { 1 2 } is-stroke-one? ] unit-test
{ f } [ FIXTURE get-global { 5 2 } is-stroke-one? ] unit-test
{ t } [ FIXTURE get-global { 1 2 } is-stroke-two? ] unit-test
{ f } [ FIXTURE get-global { 5 3 } is-stroke-two? ] unit-test

{ 18 } [ FIXTURE get-global "XMAS" (find-words) ] unit-test
{ 9 } [ FIXTURE get-global (find-x-mas) ] unit-test
