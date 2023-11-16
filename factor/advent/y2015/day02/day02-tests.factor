! Copyright (C) 2023 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: tools.test advent.y2015.day02 ;
IN: advent.y2015.day02.tests

{ 4 } [ { 1 1 2 } total-weight ] unit-test

{ 2 } [ { 1 1 2 } quantum-entanglement ] unit-test

{ 499/10000 } [ 
    { { 11 9 } { 10 8 2 } { 7 5 4 3 1 } }
    balance-score ] unit-test
