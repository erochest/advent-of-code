! Copyright (C) 2024 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: advent.io advent.y2023.day10 advent.y2023.day10.private
       arrays assocs hashtables kernel math namespaces
       prettyprint sequences sorting splitting tools.test ;
IN: advent.y2023.day10.tests

SYMBOLS: INPUT0 INPUT1 INPUT2 INPUT3 INPUT4 INPUT5 ;

2023 10 0 read-fixture+ split-lines INPUT0 set-global
2023 10 1 read-fixture+ split-lines INPUT1 set-global
2023 10 2 read-fixture+ split-lines INPUT2 set-global
2023 10 3 read-fixture+ split-lines INPUT3 set-global
2023 10 4 read-fixture+ split-lines INPUT4 set-global
2023 10 5 read-fixture+ split-lines INPUT5 set-global

{ f } [ INPUT0 get-global find-start ] unit-test
{ { 1 1 } } [ INPUT1 get-global find-start ] unit-test
{ { 2 0 } } [ INPUT2 get-global find-start ] unit-test

{ { 3 3 } } [
    "123
456
789" split-lines get-bounds
] unit-test

{ { 2 4 } } [
    "1234
5678" split-lines get-bounds
] unit-test

{ H{ { { 2 4 } 0 } { { 1 1 } 0 } } } [
    H{
        { { 2 4 } 0 } { { 1 1 } 0 } { { -1 2 } 0 }
        { { 4 7 } 0 }
    }
    { 5 5 } filter-in-bounds
] unit-test

{ { { 2 1 } { 3 0 } } } [
    INPUT2 get-global { 2 0 } get-next-steps
    2array sort
] unit-test

: test-get-next-steps-on-input0 ( x y from -- )
    [ 2array ] dip
    INPUT0 get-global swap
    [ get-next-steps ] 2curry
    unit-test ;

{ 1 1 } { 3 1 } { 2 1 } test-get-next-steps-on-input0 ! |
{ 1 1 } { 1 3 } { 1 2 } test-get-next-steps-on-input0 ! -
{ 2 1 } { 3 2 } { 3 1 } test-get-next-steps-on-input0 ! L
{ 2 3 } { 3 2 } { 3 3 } test-get-next-steps-on-input0 ! J
{ 2 3 } { 1 2 } { 1 3 } test-get-next-steps-on-input0 ! 7
{ 1 2 } { 2 1 } { 1 1 } test-get-next-steps-on-input0 ! F
f f { 2 2 } test-get-next-steps-on-input0 ! .

{ H{
    { { 1 1 } 0 } { { 1 2 } 1 } { { 1 3 } 2 }
    { { 2 3 } 3 } { { 3 3 } 4 } { { 3 2 } 5 }
    { { 3 1 } 6 } { { 2 1 } 7 }
} } [
    INPUT1 get-global { 1 1 } { 1 2 } walk
] unit-test

{ 8 } [ INPUT2 get-global find-farthest-distance ] unit-test
{ 6831 } [
    2023 10 data-file (file-lines) find-farthest-distance
] unit-test

{ 1 } [ INPUT1 get-global count-enclosed ] unit-test
{ 0 } [ INPUT2 get-global count-enclosed ] unit-test
{ 4 } [ INPUT3 get-global count-enclosed ] unit-test
{ 8 } [ INPUT4 get-global count-enclosed ] unit-test
{ 10 } [ INPUT5 get-global count-enclosed ] unit-test

{ 305 } [
    2023 10 data-file (file-lines) count-enclosed
] unit-test
