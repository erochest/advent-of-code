! Copyright (C) 2023 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: advent.io arrays tools.test advent.y2023.day04 kernel ;
IN: advent.y2023.day04.tests

1
{ 41 48 83 86 17 }
{ 83 86 6 31 17 9 48 53 }
<card>
1array
[ "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53" >card ]
unit-test

{ 8 }
[
    1
    { 41 48 83 86 17 }
    { 83 86 6 31 17 9 48 53 }
    <card>
    score-card
] unit-test

{ 0 }
[
    1
    { 41 49 84 87 18 }
    { 83 86 6 31 17 9 48 53 }
    <card>
    score-card
] unit-test

{ H{ { 1 1 } { 2 1 } { 3 1 } { 4 1 } { 5 1 } } }
[
    H{ } clone
    "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
    >card
    sum-cascading-card
] unit-test

{ 13 } [ 2023 4 fixture sum-card-scores ] unit-test
{ 24733 } [ 2023 4 data-file sum-card-scores ] unit-test

{ 30 } [ 2023 4 fixture sum-total-cards ] unit-test
{ 5422730 } [ 2023 4 data-file sum-total-cards ] unit-test
