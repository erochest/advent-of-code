! Copyright (C) 2022 Your name.
! See http://factorcode.org/license.txt for BSD license.
USING: advent.year2021.day01 advent.io arrays math.ranges sequences tools.test ;
IN: advent.year2021.day01.tests

{ { { 1 2 } { 2 3 } { 3 4 } { 4 5 } { 5 6 } } }
[ 6 [1,b] >pairs [ >array ] map ] unit-test

{ { t f f t } }
[ { { 4 5 } { 5 4 } { 9 8 } { 9 10 } } [ ascending? ] map ] unit-test

{ 7 } [ 2021 1 fixture parta ] unit-test
{ 1709 } [ 2021 1 data-file parta ] unit-test

{ 5 } [ 2021 1 fixture partb ] unit-test
