! Copyright (C) 2024 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: advent.io advent.y2024.day06 kernel namespaces sequences
       tools.test ;
IN: advent.y2024.day06.tests

SYMBOLS: FIXTURE ;
2024 6 0 fixture+ (file-lines) FIXTURE set-global

{ t } [ { 0 0 } FIXTURE get-global on-map? ] unit-test
{ t } [ { 3 3 } FIXTURE get-global on-map? ] unit-test
{ f } [ { 0 -4 } FIXTURE get-global on-map? ] unit-test
{ f } [ { 12 4 } FIXTURE get-global on-map? ] unit-test

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
[ { { 6 4 } 0 } FIXTURE get-global next-step ]
unit-test

{ { 6 4 } 0 } [ FIXTURE get-global find-guard ] unit-test

{ 42 } [ FIXTURE get-global (count-visited-spaces) ] unit-test
