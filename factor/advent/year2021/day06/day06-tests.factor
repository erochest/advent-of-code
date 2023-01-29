! Copyright (C) 2022 Eric Rochester.
! See http://factorcode.org/license.txt for BSD license.
USING: advent.io advent.year2021.day06 sorting tools.test ;
IN: advent.year2021.day06.tests

{ { 0 1 1 2 1 0 0 0 0 } } [ { 3 4 3 1 2 } >school ] unit-test

{ { 1 2 1 0 0 0 1 0 1 } } [ { 2 3 2 0 1 } >school school-day ]
unit-test

{ { 2 3 0 0 0 0 5 0 1 } }
[ { 1 2 3 0 0 0 0 4 0 } school-day ] unit-test

{ { 3 5 3 2 2 1 5 1 4 } }
[ { 3 4 3 1 2 } >school 18 school-term ] unit-test

{ 5934 } [ V{ 3 4 3 1 2 } >school 80 school-term checksum ]
unit-test

{ 5934 } [ 2021 6 fixture parta ]
unit-test
{ 389726 } [ 2021 6 data-file parta ]
unit-test

{ 26984457539 }
[ 2021 6 fixture partb ]
unit-test
{ 1743335992042 }
[ 2021 6 data-file partb ]
unit-test
