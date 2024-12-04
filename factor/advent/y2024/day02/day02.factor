! Copyright (C) 2024 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: advent.io arrays combinators grouping kernel math
       prettyprint ranges sequences ;
IN: advent.y2024.day02

: parse-input ( path -- seqs )
    (file-lines) [ split-numbers ] map ;

: pair-differences ( seq -- seq' )
    2 clump [ [ second ] [ first ] bi - ] map ;

: direction ( seq -- -1/0/1 )
    {
        { [ dup [ 0 > ] all? ] [ drop 1 ] }
        { [ dup [ 0 < ] all? ] [ drop -1 ] }
        [ drop 0 ]
    } cond ;

: scoped? ( seq -- ? )
    [ abs dup 1 >= swap 3 <= and ] all? ;

: safe? ( seq -- ? )
    [ direction zero? not ] [ scoped? ] bi
    and ;

: count-safe? ( seqs -- n )
    [ pair-differences ] map
    [ safe? ] count ;

: remove-level ( n seq -- seq ) remove-nth ;

: (dampner-safe?) ( n seq -- ? )
    remove-level pair-differences safe? ;
: dampner-safe? ( seq -- ? )
    dup pair-differences safe? 
    [ nip ]
    [
        0 over length [a..b)
        [ over clone (dampner-safe?) ] any? nip
    ] if* ;

: count-dampner-safe? ( seqs -- n )
    [ dampner-safe? ] count ;

: debug-dampner-safe? ( seqs -- )
    [
        dup pair-differences dup dampner-safe? 
        3array
        dup third
        [ drop ] [ . ] if
    ] each ;
