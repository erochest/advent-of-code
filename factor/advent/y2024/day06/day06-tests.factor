! Copyright (C) 2024 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: advent.io advent.y2024.day06 arrays hash-sets kernel
       namespaces sequences tools.test ;
IN: advent.y2024.day06.tests

SYMBOLS: FIXTURE ;
2024 6 0 fixture+ (file-lines) FIXTURE set-global

: fixture ( -- seq ) FIXTURE get-global ;

{ t } [ { 0 0 } fixture on-map? ] unit-test
{ t } [ { 3 3 } fixture on-map? ] unit-test
{ f } [ { 0 -4 } fixture on-map? ] unit-test
{ f } [ { 12 4 } fixture on-map? ] unit-test

{ { 5 4 } } [ { { 6 4 } 0 } make-move ] unit-test
{ { 6 5 } } [ { { 6 4 } 1 } make-move ] unit-test
{ { 7 4 } } [ { { 6 4 } 2 } make-move ] unit-test
{ { 6 3 } } [ { { 6 4 } 3 } make-move ] unit-test
{ { 0 -1 } } [ { { 0 0 } 3 } make-move ] unit-test

{ { { 5 4 } 1 } } [ { { 5 4 } 0 } turn ] unit-test
{ { { 5 4 } 2 } } [ { { 5 4 } 1 } turn ] unit-test
{ { { 5 4 } 3 } } [ { { 5 4 } 2 } turn ] unit-test
{ { { 5 4 } 0 } } [ { { 5 4 } 3 } turn ] unit-test

{ { 6 4 } 0 } [ fixture find-guard ] unit-test

{ { { 0,1 } { 0,2 } { 0,3 } { 1,1 } { 1,2 }
    { 1,3 } { 2,1 } { 2,3 } { 3,2 } } }
[
    { "...." "...." "..^." "...." }
    initialize-state
    get-matrix-coordinates
    nip
] unit-test

{ 42 } [ fixture (count-visited-spaces) ] unit-test
{ 6 } [ fixture (count-guard-loops) ] unit-test
