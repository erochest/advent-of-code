! Copyright (C) 2024 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: advent.io advent.y2024.day01 kernel tools.test namespaces ;
IN: advent.y2024.day01.tests

SYMBOLS: INPUT ;

2024 01 0 fixture+ INPUT set-global

{ { { 3 4 } { 4 3 } { 2 5 } { 1 3 } { 3 9 } { 3 3 } } }
[ INPUT get-global read-data ] unit-test

{ { { 1 2 3 3 3 4 } { 3 3 3 4 5 9 } } }
[ INPUT get-global read-data sort-data ] unit-test

{ { 2 1 0 1 2 5 } }
[ { { 1 2 3 3 3 4 } { 3 3 3 4 5 9 } } compute-distances ]
unit-test

{ 11 }
[
    { { 3 4 } { 4 3 } { 2 5 } { 1 3 } { 3 9 } { 3 3 } }
    find-site-distances 
] unit-test

{ 31 }
[
    { { 3 4 } { 4 3 } { 2 5 } { 1 3 } { 3 9 } { 3 3 } }
    find-similarity-scores
] unit-test
