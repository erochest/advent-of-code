! Copyright (C) 2023 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: arrays advent.io advent.y2023.day07 kernel math
       math.order namespaces prettyprint sequences sorting
       tools.test ;
IN: advent.y2023.day07.tests

{ T{ hand f { 2 3 4 5 6 } }  }
[ "23456" string>hand ] unit-test
{ T{ hand f { A Q K T J } }  }
[ "AQKTJ" string>hand ] unit-test

{ t } [ H: 33333 five-of-a-kind? ] unit-test
{ t } [ H: QQQQQ five-of-a-kind? ] unit-test
{ 6 } [ H: QQQQQ hand-value ] unit-test
{ f } [ H: KK2KK five-of-a-kind? ] unit-test

{ t } [ H: 333A3 four-of-a-kind? ] unit-test
{ t } [ H: KK2KK four-of-a-kind? ] unit-test
{ 5 } [ H: KK2KK hand-value ] unit-test
{ f } [ H: AAAAA four-of-a-kind? ] unit-test
{ f } [ H: 333AA four-of-a-kind? ] unit-test

{ t } [ H: 333AA full-house? ] unit-test
{ t } [ H: AA333 full-house? ] unit-test
{ t } [ H: 3A3A3 full-house? ] unit-test
{ 4 } [ H: 3A3A3 hand-value ] unit-test
{ f } [ H: KQKQJ full-house? ] unit-test

{ t } [ H: 333A2 three-of-a-kind? ] unit-test
{ t } [ H: A3334 three-of-a-kind? ] unit-test
{ t } [ H: 3A3K3 three-of-a-kind? ] unit-test
{ 3 } [ H: 3A3K3 hand-value ] unit-test
{ f } [ H: KQKQK three-of-a-kind? ] unit-test
{ f } [ H: KQKQJ three-of-a-kind? ] unit-test

{ t } [ H: 23432 two-pair? ] unit-test
{ 2 } [ H: 23432 hand-value ] unit-test
{ f } [ H: 23332 two-pair? ] unit-test

{ t } [ H: 22345 one-pair? ] unit-test
{ 1 } [ H: 22345 hand-value ] unit-test
{ f } [ H: 23432 one-pair? ] unit-test
{ f } [ H: 23A45 one-pair? ] unit-test
{ 0 } [ H: 23A45 hand-value ] unit-test

{ +gt+ } [ H: 33332 H: 2AAAA <=> ] unit-test
{ +gt+ } [ H: 77888 H: 77788 <=> ] unit-test
{ +lt+ } [ H: K333Q H: K333K <=> ] unit-test
{ +lt+ } [ H: 23579 H: 97325 <=> ] unit-test
{ +lt+ } [ H: 234QA H: 232J4 <=> ] unit-test

{ +gt+ } [ H: J2234 H: 92234 <=> ] unit-test
{ +lt+ } [ H: J2234 H: 92232 [ <=> ] with-jokers ] unit-test

: values-with-without-jokers ( hand -- value joker-values )
    [ hand-card-values ]
    [ [ hand-card-values ] with-jokers ] bi ;

{ t } [ H: 32T3K values-with-without-jokers = ] unit-test
{ t } [ H: KK677 values-with-without-jokers = ] unit-test

{ { 3 10 5 5 11 5 } { 5 10 5 5 1 5 } } [
    H: T55J5 values-with-without-jokers
] unit-test
{ { 2 13 10 11 11 10 } { 5 13 10 1 1 10 } } [
    H: KTJJT values-with-without-jokers
] unit-test
{ { 3 12 12 12 11 14 } { 5 12 12 12 1 14 } } [
    H: QQQJA values-with-without-jokers
] unit-test

: test-joker-hand-types ( expected hand -- )
    [ values-with-without-jokers [ first ] bi@ ] curry unit-test ;

{ 0 0 } H: 2354A test-joker-hand-types
{ 0 1 } H: 23J4A test-joker-hand-types
{ 1 1 } H: 2324A test-joker-hand-types
{ 1 3 } H: 2324J test-joker-hand-types
{ 1 3 } H: 2T8JJ test-joker-hand-types
{ 3 3 } H: 34393 test-joker-hand-types
{ 2 4 } H: 7373J test-joker-hand-types
{ 4 4 } H: 3A3A3 test-joker-hand-types
{ 3 5 } H: 7JJ3J test-joker-hand-types
{ 2 5 } H: 7J73J test-joker-hand-types
{ 3 5 } H: 343J3 test-joker-hand-types
{ 5 5 } H: 34333 test-joker-hand-types
{ 5 6 } H: 333J3 test-joker-hand-types
{ 4 6 } H: 3J3J3 test-joker-hand-types
{ 4 6 } H: 7J7JJ test-joker-hand-types
{ 5 6 } H: JJ7JJ test-joker-hand-types
{ 6 6 } H: JJJJJ test-joker-hand-types
{ 6 6 } H: 33333 test-joker-hand-types

{ 6440 } [ 2023 7 fixture read-hands total-winnings ] unit-test
{ 250951660 } [
    2023 7 data-file read-hands total-winnings
] unit-test

{ 5905 } [
    [ 2023 7 fixture read-hands total-winnings ] with-jokers
] unit-test

{ 251481660 } [
    [ 2023 7 data-file read-hands total-winnings ] with-jokers
] unit-test
