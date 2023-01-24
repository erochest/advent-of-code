! Copyright (C) 2022 Eric Rochester.
! See http://factorcode.org/license.txt for BSD license.
USING: advent2021.day06 sorting tools.test ;
IN: advent2021.day06.tests

{ { 0 1 1 2 1 0 0 0 0 } } [ { 3 4 3 1 2 } >school ] unit-test

{ { 1 2 1 0 0 0 1 0 1 } } [ { 2 3 2 0 1 } >school school-day ]
unit-test

{ { 2 3 0 0 0 0 5 0 1 } }
[ { 1 2 3 0 0 0 0 4 0 } school-day ] unit-test

{ { 3 5 3 2 2 1 5 1 4 } }
[ { 3 4 3 1 2 } >school 18 school-term ] unit-test

{ 5934 } [ V{ 3 4 3 1 2 } >school 80 school-term checksum ]
unit-test

{ 5934 } [ "vocab:advent2021/day06/example.txt" day06a ]
unit-test
{ 389726 } [ "vocab:advent2021/day06/data.txt" day06a ]
unit-test

{ 26984457539 }
[ "vocab:advent2021/day06/example.txt" day06b ]
unit-test
{ 1743335992042 }
[ "vocab:advent2021/day06/data.txt" day06b ]
unit-test
