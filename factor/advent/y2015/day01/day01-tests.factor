! Copyright (C) 2023 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: advent.io advent.y2015.day01 lists.lazy tools.test ;
IN: advent.y2015.day01.tests

{ 31916031 } [ 33554393 252533 20151125 next-code ] unit-test

{ { 0 1 2 3 4 } } [ 0 lfrom 5 take>array ] unit-test

! generate lazy sequence of codes
! generate lazy sequence of coordinates
! zip them together
! find the code at 
! To continue, please consult the code grid in the manual.  Enter the code at row 2978, column 3083.
