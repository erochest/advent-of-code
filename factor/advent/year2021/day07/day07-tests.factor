! Copyright (C) 2022 Eric Rochester.
! See http://factorcode.org/license.txt for BSD license.
USING: tools.test advent.io advent.year2021.day07 ;
IN: advent.year2021.day07.tests

{ 37 } [ 2021 7 fixture parta ] unit-test
{ 356922 } [ 2021 7 data-file parta ] unit-test

{ 1 } [ 1 sigma ] unit-test
{ 3 } [ 2 sigma ] unit-test
{ 66 } [ 11 sigma ] unit-test

{ 66 } [ 16 5 crab-fuel ] unit-test
{ 10 } [  1 5 crab-fuel ] unit-test
{  6 } [  2 5 crab-fuel ] unit-test
{ 15 } [  0 5 crab-fuel ] unit-test

{ 168 } [ 2021 7 fixture partb ] unit-test
{ 100347031 } [ 2021 7 data-file partb ] unit-test
