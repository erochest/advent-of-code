! Copyright (C) 2023 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: accessors advent.io arrays assocs combinators io kernel
       lexer math math.parser math.order math.statistics
       namespaces prettyprint sequences sequences.deep sorting
       splitting ;
IN: advent.y2023.day07

SYMBOLS: jokers-wild ;
f jokers-wild set

: with-jokers ( quot -- )
    t jokers-wild rot with-variable ;

SYMBOLS: T J Q K A ;
: string>card ( string -- card-n )
    {
        { 50 [ 2 ] }
        { 51 [ 3 ] }
        { 52 [ 4 ] }
        { 53 [ 5 ] }
        { 54 [ 6 ] }
        { 55 [ 7 ] }
        { 56 [ 8 ] }
        { 57 [ 9 ] }
        { 84 [ T ] }
        { 74 [ J ] }
        { 81 [ Q ] }
        { 75 [ K ] }
        { 65 [ A ] }
    } case ;

: card-counts ( hand -- count ) cards>> histogram values ;

SYMBOLS: five-of-a-kind four-of-a-kind full-house
three-of-a-kind two-pair one-pair high-card ;

: five-of-a-kind? ( hand -- ? )
    cards>>
    [ rest ] [ first ] bi
    [ = ] curry
    all? ;

: four-of-a-kind? ( hand -- ? )
    card-counts
    [ 4 = ] any? ;

: full-house? ( hand -- ? )
    card-counts
    [ [ 3 = ] any? ] [ [ 2 = ] any? ] bi and ;

: three-of-a-kind? ( hand -- ? )
    card-counts
    [ [ 3 = ] any? ] [ [ 2 = ] none? ] bi and ;

: two-pair? ( hand -- ? )
    card-counts histogram 2 of 2 = ;

: one-pair? ( hand -- ? )
    card-counts histogram 2 of 1 = ;

TUPLE: hand { cards array } ; ! { type symbol } ;
: <hand> ( card-array -- hand )
    dup length 5 assert=
    ! dup determine-type
    hand boa ;

: string>hand ( string -- hand )
    >array
    [ string>card ] map
    <hand> ;

SYNTAX: H: scan-token string>hand suffix! ;

: card-value ( card -- value )
    {
        { A [ 14 ] }
        { K [ 13 ] }
        { Q [ 12 ] }
        { J [ 11 ] }
        { T [ 10 ] }
        [ ]
    } case ;

: hand-type ( hand -- symbol/f )
    {
        { [ dup five-of-a-kind? ] [ drop five-of-a-kind ] }
        { [ dup four-of-a-kind? ] [ drop four-of-a-kind ] }
        { [ dup full-house? ] [ drop full-house ] }
        { [ dup three-of-a-kind? ] [ drop three-of-a-kind ] }
        { [ dup two-pair? ] [ drop two-pair ] }
        { [ dup one-pair? ] [ drop one-pair ] }
        [ drop f ]
    } cond ;

6 five-of-a-kind set-global
5 four-of-a-kind set-global
4 full-house set-global
3 three-of-a-kind set-global
2 two-pair set-global
1 one-pair set-global

: hand-value ( hand -- value )
    hand-type
    [ get-global ] [ 0 ] if* ;

: card-values ( hand -- value-array )
    cards>> [ card-value ] map ;

M: hand <=> ( obj1 obj2 -- <=> )
    [
        [ card-values ] [ hand-value ] bi
        prefix
    ] bi@ <=> ;

: parse-hands ( lines -- hand-bid-array )
    [
        ! dup "input " write .
        " " split1
        [ string>hand ] [ string>number ] bi*
        2array
        ! dup "\tparsed " write .
    ] map ;

: read-hands ( path -- hand-bid-array )
    (file-lines) parse-hands ;

: total-winnings ( hand-bid-array -- winnings )
    [ first ] sort-by
    <enumerated>
    [
        ! dup "ranked" write .
        [ first 1 + ] [ second second ] bi *
    ] map-sum
    ;
