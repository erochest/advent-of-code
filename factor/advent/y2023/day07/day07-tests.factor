! Copyright (C) 2023 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: arrays advent.io advent.y2023.day07 kernel math
       math.order prettyprint sequences sorting tools.test ;
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

{ three-of-a-kind four-of-a-kind } [
    H: T55J5
    [ hand-type ] keep
    [ hand-type ] with-jokers
] unit-test

{ two-pair four-of-a-kind } [
    H: KTJJT
    [ hand-type ] keep
    [ hand-type ] with-jokers
] unit-test

{ three-of-a-kind four-of-a-kind } [
    H: QQQJA
    [ hand-type ] keep
    [ hand-type ] with-jokers
] unit-test

{ +lt+ } [ H: JKKK2 H: QQQQ2 [ <=> ] with-jokers ] unit-test

{ 6440 } [ 2023 7 fixture read-hands total-winnings ] unit-test
{ 250951660 } [
    2023 7 data-file read-hands total-winnings
] unit-test
