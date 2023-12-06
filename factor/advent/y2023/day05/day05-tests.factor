! Copyright (C) 2023 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: assocs arrays sequences kernel tools.test
       advent.y2023.day05 advent.io sets ranges ;
IN: advent.y2023.day05.tests

{ f } [ 97 50 98 2 <mapping-range> in? ] unit-test
{ t } [ 98 50 98 2 <mapping-range> in? ] unit-test
{ t } [ 99 50 98 2 <mapping-range> in? ] unit-test
{ f } [ 100 50 98 2 <mapping-range> in? ] unit-test

{ f f } [ 97 50 98 2 <mapping-range> at* ] unit-test
{ 50 t } [ 98 50 98 2 <mapping-range> at* ] unit-test
{ 51 t } [ 99 50 98 2 <mapping-range> at* ] unit-test
{ f f } [ 100 50 98 2 <mapping-range> at* ] unit-test


{ { 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
   21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
   41 42 43 44 45 46 47 48 49 52 53 54 55 56 57 58 59 60 61 62
   63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82
   83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 50 51 100
   } } [
    0 100 (a..b] >array
    50 98 2 <mapping-range> 52 50 48 <mapping-range> 2array
    <mapping>
    [ at* drop ] curry map
] unit-test
{ t } [
    0 100 (a..b] >array
    50 98 2 <mapping-range> 52 50 48 <mapping-range> 2array
    <mapping>
    [ at* nip ] curry all?
] unit-test

{ 35 } [ 2023 5 fixture get-min-location ] unit-test
