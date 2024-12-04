! Copyright (C) 2024 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: advent.io combinators grouping kernel math sequences ;
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
