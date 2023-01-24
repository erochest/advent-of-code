! Copyright (C) 2022 Your name.
! See http://factorcode.org/license.txt for BSD license.
USING: tools.test advent2021.day03 bit-vectors kernel ;
IN: advent2021.day03.tests

{ { 4 30 22 23 21 15 7 28 16 25 2 10 } }
[ "vocab:advent2021/day03/example.txt" read-data ] unit-test

{ 3 4 1 2 } [ 1 2 3 4 2swap ] unit-test

{ 16 } [ { 4 30 22 23 21 15 7 28 16 25 2 10 } left-bit ] unit-test
{ { f t t t t f f t t t f f } }
[ { 4 30 22 23 21 15 7 28 16 25 2 10 } 16 scan-mask ] unit-test

{ 32 } [ 16 next-mask ] unit-test
{ 16 } [ 8 next-mask ] unit-test
{ 8 } [ 4 next-mask ] unit-test

{ 7 5 } [ { f t t t t f f t t t f f } count-bools ] unit-test

{ ?V{ t } ?V{ f } } [ ?V{ } clone ?V{ } clone f accum-bits-epsilon ] unit-test

{ 22 9 }
[ { 4 30 22 23 21 15 7 28 16 25 2 10 } gamma-epsilon [ bit-vector>integer ] bi@ ]
unit-test

{ 22 } [ ?V{ f t t f t } bit-vector>integer ] unit-test

{ 198 } [ "vocab:advent2021/day03/example.txt" day03a ] unit-test
{ 4191876 } [ "vocab:advent2021/day03/data.txt" day03a ] unit-test

{ 23 } [ { 4 30 22 23 21 15 7 28 16 25 2 10 } oxygen-rating ] unit-test
{ 10 } [ { 4 30 22 23 21 15 7 28 16 25 2 10 } co2-scrubber ] unit-test 

{ 230 } [ "vocab:advent2021/day03/example.txt" day03b ] unit-test
{ 3414905 } [ "vocab:advent2021/day03/data.txt" day03b ] unit-test