! Copyright (C) 2024 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: advent.y2024.day03 tools.test ;
IN: advent.y2024.day03.tests

{ { { 2 4 } { 5 5 } { 11 8 } { 8 5 } } }
[
    "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
    find-ops
] unit-test

{ 161 } [
    "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
    checksum-ops
] unit-test

{ { "mul(2,4)" "don't()" "mul(5,5)" "mul(11,8)" "do()" "mul(8,5)" } }
[
    "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
    scan-ops
] unit-test

{ { t 8 } } [ { t 0 } "mul(2,4)" interpret-op ] unit-test
{ { f 0 } } [ { f 0 } "mul(2,4)" interpret-op ] unit-test
{ { t 0 } } [ { f 0 } "do()" interpret-op ] unit-test
{ { t 0 } } [ { t 0 } "do()" interpret-op ] unit-test
{ { f 0 } } [ { t 0 } "don't()" interpret-op ] unit-test
{ { f 0 } } [ { f 0 } "don't()" interpret-op ] unit-test

{ 48 } [
    "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
    interpret-ops
] unit-test
