! Copyright (C) 2023 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: accessors advent.io advent.y2015.day01 kernel math 
       tools.test ;
IN: advent.y2015.day01.tests

{ 31916031 } [ 20151125 252533 33554393 next-code ] unit-test

{ t 4 } [
    0 [ 1 + ] <generator> 
    skip skip skip skip 
    [ generator? ] [ current>> ] bi 
] unit-test

{ 0 1 2 } [ 
    0 [ 1 + ] <generator> step step step drop
] unit-test

{ { 0 1 2 3 4 } } [ 0 [ 1 + ] <generator> 5 take>array ]
unit-test

{ { 20151125 31916031 18749137 16080970 21629792 17289845
    24592653 8057251 16929656 30943339 } }
[ aoc-generator 10 take>array ] unit-test

! generate lazy sequence of coordinates
! zip them together
! find the code at 
! To continue, please consult the code grid in the manual.  Enter the code at row 2978, column 3083.
