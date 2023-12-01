! Copyright (C) 2023 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: tools.test advent.io advent.y2023.day01 sequences ;
IN: advent.y2023.day01.tests

{ 0 } [ "0" first ascii>digit ] unit-test
{ 8 } [ "8" first ascii>digit ] unit-test
{ 12 } [ "1abc2" >calibration ] unit-test
{ 38 } [ "pqr3stu8vwx" >calibration ] unit-test
{ 15 } [ "a1b2c3d4e5f" >calibration ] unit-test
{ 77 } [ "treb7uchet" >calibration ] unit-test

{ 142 } [ 2023 1 fixture sum-calibrations ] unit-test
{ 56465 } [ 2023 1 data-file sum-calibrations ] unit-test

{ 0 } [ "0" word>digit ] unit-test
{ 1 } [ "one" word>digit ] unit-test
{ { 1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9 } }
[
    {
        "1" "2" "3" "4" "5" "6" "7" "8" "9"
        "one" "two" "three" "four" "five" "six" "seven"
        "eight" "nine"
    }
    [ word>digit ] map
] unit-test
{ 98 } [ "pcg91vqrfpxxzzzoneightzt" >calibration-word ] unit-test

{ 29 } [ "two1nine" >calibration-word ] unit-test
{ 281 } [ 2023 1 fixture-a sum-calibration-words ] unit-test
{ 55902 } [ 2023 1 data-file sum-calibration-words ] unit-test

