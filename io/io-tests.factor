! Copyright (C) 2022 Eric Rochester.
! See http://factorcode.org/license.txt for BSD license.
USING: tools.test advent2021.io ;
IN: advent2021.io.tests

{ { 199 200 208 210 200 207 240 269 260 263 } }
[ "vocab:advent2021/day01/example.txt" read-lines>numbers ]
unit-test

{ { 3 4 3 1 2 } }
[ "vocab:advent2021/day06/example.txt" read>numbers ] unit-test
