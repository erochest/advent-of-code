! Copyright (C) 2022 Your name.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays tools.test advent.year2021.day04 kernel
math.order sequences sorting advent.io ;
IN: advent.year2021.day04.tests

! !!!!!!!!!! inc-nth
{ { 0 1 } } [ 0 0 2array 1 over inc-nth ] unit-test
{ { 1 0 } } [ 0 0 2array 0 over inc-nth ] unit-test

! !!!!!!!! board>play-board
{ H{ {  0 { 0 4 } } {  1 { 4 0 } } {  2 { 1 1 } } {  3 { 3 2 } } {  4 { 1 3 } }
     {  5 { 3 4 } } {  6 { 3 0 } } {  7 { 2 4 } } {  8 { 1 0 } } {  9 { 2 1 } }
     { 10 { 3 1 } } { 11 { 0 3 } } { 12 { 4 1 } } { 13 { 0 1 } } { 14 { 2 2 } }
     { 15 { 4 3 } } { 16 { 2 3 } } { 17 { 0 2 } } { 18 { 3 3 } } { 19 { 4 4 } }
     { 20 { 4 2 } } { 21 { 2 0 } } { 22 { 0 0 } } { 23 { 1 2 } } { 24 { 1 4 } } } }
[ { { 22 13 17 11  0 }
    {  8  2 23  4 24 }
    { 21  9 14 16  7 }
    {  6 10  3 18  5 }
    {  1 12 20 15 19 } } board>play-board ] unit-test

! !!!!!!!! parse-moves
{ { 7 4 9 5 11 } } [ "7,4,9,5,11" parse-moves ] unit-test

! !!!!!!!! parse-board

{ { { 22 13 17 11  0 }
    {  8  2 23  4 24 }
    { 21  9 14 16  7 }
    {  6 10  3 18  5 }
    {  1 12 20 15 19 } } }
[ { "22 13 17 11  0"
    " 8  2 23  4 24" 
    "21  9 14 16  7" 
    " 6 10  3 18  5" 
    " 1 12 20 15 19" } parse-board ] unit-test

! !!!!!! read04a

{ { 7 4 9 5 11 17 23 2 0 14 21 24 10 16 13 6 15 25 12 22 18 20 8 19 3 26 1
 } }
[ 2021 4 fixture read04a moves-left>> ] unit-test

{ { { 22 13 17 11  0 }
    {  8  2 23  4 24 }
    { 21  9 14 16  7 }
    {  6 10  3 18  5 }
    {  1 12 20 15 19 } } }
[ 2021 4 fixture read04a boards>> first board>> ] unit-test
{ { {  3 15  0  2 22 }
    {  9 18 13 17  5 }
    { 19  8  7 25 23 }
    { 20 11 10 24  4 }
    { 14 21 16 12  6 } } }
[ 2021 4 fixture read04a boards>> second board>> ] unit-test
{ { { 14 21 17 24  4 }
    { 10 16 15  9 19 }
    { 18  8 23 26 20 }
    { 22 11 13  6  5 }
    {  2  0 12  3  7 } } }
[ 2021 4 fixture read04a boards>> third board>> ] unit-test

{ H{ {  0 { 0 4 } } {  1 { 4 0 } } {  2 { 1 1 } } {  3 { 3 2 } } {  4 { 1 3 } }
     {  5 { 3 4 } } {  6 { 3 0 } } {  7 { 2 4 } } {  8 { 1 0 } } {  9 { 2 1 } }
     { 10 { 3 1 } } { 11 { 0 3 } } { 12 { 4 1 } } { 13 { 0 1 } } { 14 { 2 2 } }
     { 15 { 4 3 } } { 16 { 2 3 } } { 17 { 0 2 } } { 18 { 3 3 } } { 19 { 4 4 } }
     { 20 { 4 2 } } { 21 { 2 0 } } { 22 { 0 0 } } { 23 { 1 2 } } { 24 { 1 4 } } } }
[ 2021 4 fixture read04a boards>> first play>> ] unit-test

{ f } [ f f { { 0 0 0 0 0 } { 0 0 0 0 0 } } bingo-board boa winning? ] unit-test
{ f } [ f f { { 0 1 0 3 0 } { 0 2 4 0 0 } } bingo-board boa winning? ] unit-test
{ t } [ f f { { 0 1 5 3 0 } { 0 2 4 0 0 } } bingo-board boa winning? ] unit-test
{ t } [ f f { { 0 1 0 3 0 } { 5 2 4 0 0 } } bingo-board boa winning? ] unit-test

: make-move-fixture ( -- bingo-game )
    { { { 22 13 17 11  0 }
        {  8  2 23  4 24 }
        { 21  9 14 16  7 }
        {  6 10  3 18  5 }
        {  1 12 20 15 19 } } }
    { 7 4 9 5 11 17 }
    <bingo-game> ;
! !!!!!!!! make-game-move
{ { 4 9 5 11 17 } } [ make-move-fixture dup make-game-move moves-left>> ] unit-test
{ 7 } [ make-move-fixture dup make-game-move last-move>> ] unit-test
{ H{ {  0 { 0 4 } } {  1 { 4 0 } } {  2 { 1 1 } } {  3 { 3 2 } } {  4 { 1 3 } }
     {  5 { 3 4 } } {  6 { 3 0 } }                {  8 { 1 0 } } {  9 { 2 1 } }
     { 10 { 3 1 } } { 11 { 0 3 } } { 12 { 4 1 } } { 13 { 0 1 } } { 14 { 2 2 } }
     { 15 { 4 3 } } { 16 { 2 3 } } { 17 { 0 2 } } { 18 { 3 3 } } { 19 { 4 4 } }
     { 20 { 4 2 } } { 21 { 2 0 } } { 22 { 0 0 } } { 23 { 1 2 } } { 24 { 1 4 } } } }
[ make-move-fixture dup make-game-move boards>> first play>> ] unit-test
{ { { 0 0 1 0 0 } { 0 0 0 0 1 } } }
[ make-move-fixture dup make-game-move boards>> first hits>> ] unit-test

! !!!!!!! make-game-move, move not found
: move-not-found-fixture ( -- bingo-game )
    make-move-fixture { 25 29 } >>moves-left ;
{ { 29 } } [ move-not-found-fixture dup make-game-move moves-left>> ] unit-test
{ 25 } [ move-not-found-fixture dup make-game-move last-move>> ] unit-test
{ H{ {  0 { 0 4 } } {  1 { 4 0 } } {  2 { 1 1 } } {  3 { 3 2 } } {  4 { 1 3 } }
     {  5 { 3 4 } } {  6 { 3 0 } } {  7 { 2 4 } } {  8 { 1 0 } } {  9 { 2 1 } }
     { 10 { 3 1 } } { 11 { 0 3 } } { 12 { 4 1 } } { 13 { 0 1 } } { 14 { 2 2 } }
     { 15 { 4 3 } } { 16 { 2 3 } } { 17 { 0 2 } } { 18 { 3 3 } } { 19 { 4 4 } }
     { 20 { 4 2 } } { 21 { 2 0 } } { 22 { 0 0 } } { 23 { 1 2 } } { 24 { 1 4 } } } }
[ move-not-found-fixture dup make-game-move boards>> first play>> ] unit-test
{ { { 0 0 0 0 0 } { 0 0 0 0 0 } } }
[ move-not-found-fixture dup make-game-move boards>> first hits>> ] unit-test


! !!!!!! play-to-win
{ 24 }
[ 2021 4 fixture read04a dup play-to-win last-move>> ] unit-test

! !!!!!! score-board
: score-board-fixture ( -- board )
    f
    H{ {  2 { 1 1 } } {  4 { 1 3 } } {  5 { 3 4 } } {  6 { 3 0 } }
       {  9 { 2 1 } } { 13 { 0 1 } } { 15 { 4 3 } } { 18 { 3 3 } } }
    V{ } clone bingo-board boa ;

{ 1728 } [ 24 score-board-fixture score-board ] unit-test

! !!!!!! get-unmarked-numbers
{ { 2 4 5 6 9 13 15 18 } } [ score-board-fixture get-unmarked-numbers [ <=> ] sort ] unit-test

! !!!! day04a
{ 4512 } [ 2021 4 fixture parta ] unit-test
{ 49686 } [ 2021 4 data-file parta ] unit-test

! !!!!!! play-to-lose
{ 13 }
[ 2021 4 fixture read04a dup play-to-lose last-move>> ] unit-test

! !!!! day04b
{ 1924 } [ 2021 4 fixture partb ] unit-test
{ 26878 } [ 2021 4 data-file partb ] unit-test
