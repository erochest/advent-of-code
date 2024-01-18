! Copyright (C) 2024 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: advent.io advent.y2023.day10 assocs hashtables kernel
       math namespaces sequences sorting splitting tools.test ;
IN: advent.y2023.day10.tests

SYMBOLS: INPUT0 INPUT1 INPUT2 ;

".....
.F-7.
.|.|.
.L-J.
....." split-lines INPUT0 set-global

".....
.S-7.
.|.|.
.L-J.
....." split-lines INPUT1 set-global

"7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ" split-lines INPUT2 set-global

{ f } [ INPUT0 get-global find-start ] unit-test
{ { 1 1 } } [ INPUT1 get-global find-start ] unit-test
{ { 2 0 } } [ INPUT2 get-global find-start ] unit-test

{ 4 } [ { 1 1 } get-next-steps assoc-size ] unit-test
{ t } [
    { 1 1 } get-next-steps
    keys
    [ [ first ] map ] [ [ second ] map ] bi
    [
        [ [ 0 >= ] [ 2 <= ] bi and ] all?
    ] bi@
    and
] unit-test

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

{ { { 1 3 } { 3 3 } } } [
    { 2 3 } get-next-steps
    INPUT1 get-global
    get-bounds
    filter-in-bounds
    INPUT1 get-global
    filter-connector
    keys sort
] unit-test

{ { 3 2 3 } } [
    { f 2 4 } [ 3 swap calculate-next-distance ] map
] unit-test

{
    H{ { { 2 0 } 0 } { { 2 1 } 1 } { { 3 0 } 1 } }
    { { 2 1 } { 3 0 } }
} [
    H{ { { 2 0 } 0 } }
    dup
    INPUT2 get-global
    { 2 0 }
    get-links
] unit-test

! should not return ones we've been to before
{
    H{ 
        { { 2 0 } 0 } 
        { { 2 1 } 1 } { { 3 0 } 1 } 
        { { 1 1 } 2 } { { 4 0 } 2 }
        { { 4 1 } 3 }
    }
    { { 4 1 } }
} [
    H{ 
        { { 2 0 } 0 } 
        { { 2 1 } 1 } { { 3 0 } 1 } 
        { { 1 1 } 2 } { { 4 0 } 2 }
    }
    dup
    INPUT2 get-global
    { 4 0 }
    get-links
] unit-test

! should not return ones we've been to before, unless we've lowered the
! distance. 
{
    H{ 
        { { 2 0 } 0 } 
        { { 2 1 } 1 } { { 3 0 } 1 } 
        { { 1 1 } 2 } { { 4 0 } 2 }
        { { 4 1 } 3 }
    }
    { { 4 1 } }
} [
    H{ 
        { { 2 0 } 0 } 
        { { 2 1 } 1 } { { 3 0 } 1 } 
        { { 1 1 } 2 } { { 4 0 } 2 }
        { { 4 1 } 7 }
    }
    dup
    INPUT2 get-global
    { 4 0 }
    get-links
] unit-test

! queue nothing that we haven't seen before
{
    H{ 
        { { 2 0 } 0 } 
        { { 2 1 } 1 } { { 3 0 } 1 } 
        { { 1 1 } 2 } { { 4 0 } 2 }
        { { 4 1 } 3 }
    }
    { }
} [
    H{ 
        { { 2 0 } 0 } 
        { { 2 1 } 1 } { { 3 0 } 1 } 
        { { 1 1 } 2 } { { 4 0 } 2 }
        { { 4 1 } 3 }
    }
    dup
    INPUT2 get-global
    { 4 0 }
    get-links
] unit-test

! { 8 } [ INPUT2 get-global find-farthest-distance ] unit-test
