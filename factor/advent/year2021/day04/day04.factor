! Copyright (C) 2022 Your name.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors assocs arrays io io.encodings.utf8 io.files kernel hashtables
locals math math.parser prettyprint sequences sequences.generalizations 
splitting combinators.short-circuit ;
IN: advent.year2021.day04

:: inc-nth ( n seq -- ) n seq nth 1 + n seq set-nth ;
: cursor-next-row ( pair -- ) 0 over inc-nth -1 1 rot set-nth ;
: cursor-next-col ( pair -- ) 1 swap inc-nth ;

! a play-board-seq indexes each item with an array of completion zones.
:: board>play-board ( board-seq -- play-board-hash-table )
    25 <hashtable> :> play-board-dict
    -1 -1 2array board-seq [
        [ dup cursor-next-row ] dip
        [
            [ dup cursor-next-col dup clone ] dip
            play-board-dict set-at
        ] each
    ] each
    drop
    play-board-dict
    ;

TUPLE: bingo-board board play hits ;
: <bingo-board> ( board -- bingo-board )
    dup board>play-board
    5 0 <array> 5 0 <array> 2array
    bingo-board boa ;

: winning? ( board -- ? )
    hits>> { [ first [ 5 = ] any? ] [ second [ 5 = ] any? ] } 1|| ;

: mark-square ( board move -- pair )
    [ play>> ] dip
    [ swap at ] 2keep
    swap delete-at
    ;
: incr-col-hit ( hits col -- )
    [ second ] dip 
    [ over nth ] keep
    [ 1 + ] dip
    rot set-nth ;
: incr-row-hit ( hits col -- )
    [ first ] dip
    [ over nth ] keep
    [ 1 + ] dip
    rot set-nth ;
: make-move ( board move -- )
    dupd mark-square
    dup [
        [ hits>> ] dip
        [ first ] [ second ] bi
        overd incr-col-hit
        incr-row-hit
    ] [
        2drop
    ] if 
    ;

TUPLE: bingo-game boards moves-left last-move winning ;
: <bingo-game> ( boards moves-left -- bingo-game )
    [ [ <bingo-board> ] map ] dip f V{ } clone bingo-game boa ;

: parse-moves ( string -- number-seq ) "," split [ string>number ] map ;
: parse-board ( string-seq -- bingo-board )
    [ " " split [ length 0 > ] filter [ string>number ] map ] map ;
: read04a ( path -- bingo-game )
    utf8 file-lines
    [ length zero? ] split-when
    unclip
       [ [ parse-board ] map ]
       [ first parse-moves ] bi*
    <bingo-game> ;

: update-moves ( game -- move )
    dup moves-left>>
    dup empty? [ 2drop f ] [
        unclip
        [ >>moves-left ] dip
        [ >>last-move ] keep
        nip
    ] if
    ;
: update-winning ( game -- )
    dup boards>> [ winning? ] partition
    pick boards<<
    [ winning>> ] dip append! drop ;
:: make-game-move ( game -- )
    game update-moves :> move
    game boards>> [ move make-move ] each
    game update-winning
    ;

: play-to-win ( bingo-game -- )
    [ dup winning>> empty? ] [
        dup make-game-move
    ] while drop ;

: get-unmarked-numbers ( bingo-board -- seq ) play>> keys ;
: score-board ( last-number bingo-board -- score )
    get-unmarked-numbers sum * ;

: parta ( path -- checksum )
    read04a
    dup play-to-win
       [ last-move>> ]
       [ winning>> first ] bi
    score-board
    ;

: play-to-lose ( bingo-game -- )
    [ dup boards>> empty? not ] [
        dup make-game-move
    ] while drop ;
: partb ( path -- checksum )
    read04a
    dup play-to-lose
        [ last-move>> ]
        [ winning>> last ] bi
    score-board
    ;
