! Copyright (C) 2024 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: advent.io advent.y2024.day06 arrays kernel namespaces
       sequences tools.test ;
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

{ { { 5 4 } 1 } } [ { { 5 4 } 0 } change-direction ] unit-test
{ { { 5 4 } 2 } } [ { { 5 4 } 1 } change-direction ] unit-test
{ { { 5 4 } 3 } } [ { { 5 4 } 2 } change-direction ] unit-test
{ { { 5 4 } 0 } } [ { { 5 4 } 3 } change-direction ] unit-test

{ { { 5 4 } 0 } }
[ { { 6 4 } 0 } fixture next-step ]
unit-test

{ { 6 4 } 0 } fixture { { 6 5 } 0 } 0 4array
[ { { 6 4 } 0 } fixture { { 6 5 } 0 } (can-loop/flag) ]
unit-test
{ { 6 4 } 0 } fixture { { 6 4 } 0 } 1 4array
[ { { 6 4 } 0 } fixture { { 6 4 } 0 } (can-loop/flag) ]
unit-test
{ { 6 4 } 0 } fixture { { 12 5 } 0 } 2 4array
[ { { 6 4 } 0 } fixture { { 12 5 } 0 } (can-loop/flag) ]
unit-test

{ f } [ { { 1 4 } 0 } fixture can-loop? ] unit-test
{ f } [ { { 6 5 } 3 } fixture can-loop? ] unit-test
{ t } [ { { 6 4 } 3 } fixture can-loop? ] unit-test
{ t } [ { { 8 4 } 3 } fixture can-loop? ] unit-test

{ { 6 4 } 0 } [ fixture find-guard ] unit-test

{ 42 } [ fixture (count-visited-spaces) ] unit-test
{ 6 } [ fixture (count-loops) ] unit-test
