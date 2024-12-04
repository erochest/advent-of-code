! Copyright (C) 2024 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: advent.io advent.y2024.day02 namespaces tools.test ;
IN: advent.y2024.day02.tests

SYMBOLS: INPUT-FILENAME ;
2024 2 0 fixture+ INPUT-FILENAME set-global

{ {
    { 7 6 4 2 1 }
    { 1 2 7 8 9 }
    { 9 7 6 2 1 }
    { 1 3 2 4 5 }
    { 8 6 4 4 1 }
    { 1 3 6 7 9 }
  } } [
    INPUT-FILENAME get-global parse-input
  ] unit-test

{ { -1 -2 -2 -1 } } [ { 7 6 4 2 1 } pair-differences ] unit-test
{ { 1 5 1 1 } } [ { 1 2 7 8 9 } pair-differences ] unit-test

{ -1 } [ { -1 -2 -2 -1 } direction ] unit-test
{ 1 } [ { 1 5 1 1 } direction ] unit-test
{ 0 } [ { 1 5 -2 -2 1 } direction ] unit-test

{ t } [ { -1 -2 -2 -1 } scoped? ] unit-test
{ f } [ { 1 5 1 1 } scoped? ] unit-test
{ f } [ { 1 5 -2 -2 1 } scoped? ] unit-test

{ t } [ { -1 -2 -2 -1 } safe? ] unit-test
{ f } [ { 1 5 1 1 } safe? ] unit-test
{ f } [ { 1 5 -2 -2 1 } safe? ] unit-test

{ 2 } [
    INPUT-FILENAME get-global parse-input count-safe?
] unit-test

{ { 6 4 2 1 } } [ 0 { 7 6 4 2 1 } remove-level ] unit-test
{ { 1 2 8 9 } } [ 2 { 1 2 7 8 9 } remove-level ] unit-test
{ { 9 6 2 1 } } [ 1 { 9 7 6 2 1 } remove-level ] unit-test
{ { 1 3 2 4 } } [ 4 { 1 3 2 4 5 } remove-level ] unit-test

{ t } [ { 7 6 4 2 1 } dampner-safe? ] unit-test
{ f } [ { 1 2 7 8 9 } dampner-safe? ] unit-test
{ f } [ { 9 7 6 2 1 } dampner-safe? ] unit-test
{ t } [ { 1 3 2 4 5 } dampner-safe? ] unit-test
{ t } [ { 8 6 4 4 1 } dampner-safe? ] unit-test
{ t } [ { 1 3 6 7 9 } dampner-safe? ] unit-test

{ 4 } [
    INPUT-FILENAME get-global parse-input count-dampner-safe?
] unit-test

