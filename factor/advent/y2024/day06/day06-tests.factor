! Copyright (C) 2024 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: advent.io advent.map advent.y2024.day06 arrays hash-sets
       kernel namespaces sequences tools.test ;
IN: advent.y2024.day06.tests

SYMBOLS: FIXTURE ;
2024 6 0 fixture+ (file-lines) FIXTURE set-global

: fixture ( -- seq ) FIXTURE get-global ;

{ HS{ } } [ { "" "" "" } find-blocks ] unit-test
{ HS{ { 1 1 } } }
[ { "..." ".#." "..." } find-blocks ] unit-test

{ 0 } [ CHAR: ^ char>direction ] unit-test
{ 2 } [ CHAR: v char>direction ] unit-test
{ f } [ CHAR: a char>direction ] unit-test

{ f } [ 7 direction>move ] unit-test
{ [ up ] } [ 0 direction>move ] unit-test
{ [ right ] } [ 1 direction>move ] unit-test
{ [ left ] } [ 3 direction>move ] unit-test

{ { f f } f } [ { "..." "..." "..." } find-guard ] unit-test
{ { 1 1 } 0 } [ { "..." ".^." "..." } find-guard ] unit-test
{ { 1 2 } 2 } [ { "..." "..v" "..." } find-guard ] unit-test

! { 42 } [ fixture (count-visited-spaces) ] unit-test
! { 6 } [ fixture (count-guard-loops) ] unit-test

