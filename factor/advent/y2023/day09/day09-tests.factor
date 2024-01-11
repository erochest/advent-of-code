! Copyright (C) 2024 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: advent.io advent.y2023.day09 arrays namespaces
       sequences tools.test ;
IN: advent.y2023.day09.tests

SYMBOL: FIXTURE
{
    { 0 3 6 9 12 15 }
    { 1 3 6 10 15 21 }
    { 10 13 16 21 30 45 }
} FIXTURE set-global

FIXTURE get-global 1array [
    2023 9 fixture read-readings
] unit-test

{ { 3 3 3 3 3 } } [ FIXTURE get-global first deltas ] unit-test
{ { 0 0 0 0 } } [ { 3 3 3 3 3 } deltas ] unit-test

{ f } [ { 0 3 6 9 12 15 } all-zeros? ] unit-test
{ f } [ { 3 3 3 3 3 } all-zeros? ] unit-test
{ t } [ { 0 0 0 0 } all-zeros? ] unit-test

{ 0 } [ f { 0 0 0 0 } (extrapolate) ] unit-test
{ 3 } [ 0 { 3 3 3 3 3 } (extrapolate) ] unit-test
{ 18 } [ 3 { 0 3 6 9 12 15 } (extrapolate) ] unit-test

{ V{
    { 1 3 6 10 15 21 }
    { 2 3 4 5 6 }
    { 1 1 1 1 }
    { 0 0 0 }
} } [
    { 1 3 6 10 15 21 } (derive-to-zero)
] unit-test

{ 18 } [ { 0 3 6 9 12 15 } extrapolate ] unit-test
{ 28 } [ { 1 3 6 10 15 21 } extrapolate ] unit-test
{ 68 } [ { 10 13 16 21 30 45 } extrapolate ] unit-test

{ 114 } [
    2023 9 fixture read-readings sum-extrapolations
] unit-test
{ 1921197370 } [
    2023 9 data-file read-readings sum-extrapolations
] unit-test

{ 2 } [
    2023 9 fixture read-readings sum-prepolations
] unit-test
{ 1124 } [
    2023 9 data-file read-readings sum-prepolations
] unit-test

