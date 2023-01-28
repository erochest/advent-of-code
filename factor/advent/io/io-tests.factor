! Copyright (C) 2022 Eric Rochester.
! See http://factorcode.org/license.txt for BSD license.
USING: tools.test advent.io prettyprint ;
IN: advent.io.tests

{ { 199 200 208 210 200 207 240 269 260 263 } }
[ 2021 1 fixture read-lines>numbers ]
unit-test

{ { 3 4 3 1 2 } } [ 2021 6 fixture read>numbers ] unit-test
