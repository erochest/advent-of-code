! Copyright (C) 2022 Eric Rochester.
! See http://factorcode.org/license.txt for BSD license.
USING: tools.test advent2021.day07 ;
IN: advent2021.day07.tests

{ 37 }
[ "vocab:advent2021/day07/example.txt" day07a ]
unit-test
{ 356922 }
[ "vocab:advent2021/day07/data.txt" day07a ]
unit-test

{ 1 } [ 1 sigma ] unit-test
{ 3 } [ 2 sigma ] unit-test
{ 66 } [ 11 sigma ] unit-test

{ 66 } [ 16 5 crab-fuel ] unit-test
{ 10 } [  1 5 crab-fuel ] unit-test
{  6 } [  2 5 crab-fuel ] unit-test
{ 15 } [  0 5 crab-fuel ] unit-test

{ 168 }
[ "vocab:advent2021/day07/example.txt" day07b ]
unit-test
{ 100347031 }
[ "vocab:advent2021/day07/data.txt" day07b ]
unit-test
