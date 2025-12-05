! Copyright (C) 2025 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: advent.io advent.y2025.day05 hash-sets kernel tools.test
    vectors ;
IN: advent.y2025.day05.tests

{ 3 4 5 10 11 12 13 14 15 16 17 18 19 20 } >hash-set
1vector
[ 2025 5 0 read-fixture+ parse-input drop ] unit-test
