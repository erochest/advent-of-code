! Copyright (C) 2023 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: accessors advent.io advent.y2015.day01 kernel math 
       tools.test ;
IN: advent.y2015.day01.tests

{ 31916031 } [ 20151125 252533 33554393 next-code ] unit-test

{ t 4 } [
    0 [ 1 + ] <fgenerator> 
    4 [ [ skip ] keep ] times
    [ fgenerator? ] [ current>> ] bi 
] unit-test

{ 0 1 2 } [ 
    0 [ 1 + ] <fgenerator> 3 [ next-and ] times drop
] unit-test

{ { 0 1 2 3 4 } } [ 0 [ 1 + ] <fgenerator> 5 take>array ]
unit-test

{ { 20151125 31916031 18749137 16080970 21629792 17289845
    24592653 8057251 16929656 30943339 } }
[ code-generator 10 take>array ] unit-test

{ T{ coords f 1 1 } } [ 1 1 <coords> ] unit-test

{ T{ coords f 1 2 } } [ 1 1 <coords> next-coord ] unit-test
{ T{ coords f 2 1 } } [ 1 2 <coords> next-coord ] unit-test
{ T{ coords f 1 3 } } [ 2 1 <coords> next-coord ] unit-test
{ T{ coords f 2 2 } } [ 1 3 <coords> next-coord ] unit-test
{ T{ coords f 3 1 } } [ 2 2 <coords> next-coord ] unit-test
{ T{ coords f 1 4 } } [ 3 1 <coords> next-coord ] unit-test

{ { T{ coords f 1 1 } T{ coords f 1 2 } T{ coords f 2 1 } T{ coords f 1 3 } T{ coords f 2 2 } } }
[ coord-generator 5 take>array ] unit-test

! generate lazy sequence of coordinates
! zip them together
! find the code at 
! To continue, please consult the code grid in the manual.  Enter the code at row 2978, column 3083.
