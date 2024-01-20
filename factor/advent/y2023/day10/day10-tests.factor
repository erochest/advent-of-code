! Copyright (C) 2024 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: advent.io advent.y2023.day10 advent.y2023.day10.private
       arrays assocs hashtables kernel math namespaces
       prettyprint sequences sorting splitting tools.test ;
IN: advent.y2023.day10.tests

SYMBOLS: INPUT0 INPUT1 INPUT2 INPUT3 INPUT4 INPUT5 ;

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

"...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
..........." split-lines INPUT3 set-global

".F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ..." split-lines INPUT4 set-global

"FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L" split-lines INPUT5 set-global

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
