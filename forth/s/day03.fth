
s" files.fth" included
s" debug.fth" included
s" stack.fth" included
s" bytes.fth" included

struct
    cell% field map-data
    cell% field map-height
    cell% field map-pattern-width
end-struct map%

: read-pattern ( a n -- p )
    \ Read the pattern in the input string into a bit array.
    0 0 2swap bounds u+do ( pattern iter )
        i c@ [char] # = if
            tuck set-position swap
        then
        1+
    loop
    drop ;

: update-pattern-width ( pw hc u -- u hc u )
    rot drop tuck ;

: read-map ( a n -- map-a )
    \ Read the map from a file and return a map structure.
    open-input
    here 0 0 begin ( data-a pattern-width height-counter )
        read-input-line ( data-a pattern-width height-counter u f )
    while ( da pw hc u )
        update-pattern-width ( da pw' hc u )
        line-buffer swap read-pattern , ( da pw hc )
        1+ ( da pw hc' )
    repeat
    drop close-input
    map% %allot ( da pw hc m )
    tuck map-height ! ( da pw m )
    tuck map-pattern-width ! ( da m )
    tuck map-data !
    ;

: allot-line ( height -- ) 2* cells allot ;
: rise-run-over ( rise run a -- rise run a rise run ) third third ;
: scale-rise-run ( rise run i -- rise' run' )
    swap over * -rot * swap ;
: rise-run! ( rise run a i -- )
    2* cells + 2! ;

: get-tree ( y x m -- f )
    \ Return true if position y, x has a tree.
    dup map-pattern-width @
    swap map-data @
    2swap swap
    cells rot + @
    swap rot mod
    1 swap lshift and
    0<> ;

: inc-counter ( c0 m mh rise run -- c1 m mh rise run )
    4 roll 1+ 0 ( m mh rise run c1 0 )
    2rot 2rot ( c1 0 m mh rise run )
    4 roll drop ( c1 m mh rise run )
    ;

: end-of-map? ( map-height rise run -- map-height rise run f )
    over 3 pick >= ;

: count-trees ( map-a rise run -- n )
    \ Take a map and a line and count the number of trees on the line.
    { rise run } ( m )
    0 swap ( c m )
    dup map-height @ ( c m mh )
    0 0 begin ( c m mh current-rise current-run )
        2dup ( c m mh rise run rise run )
        5 pick ( c m mh rise run rise run m )
        get-tree ( c m mh rise run f )
        if ( c m mh rise run )
            inc-counter ( c1 m mh rise run )
        then
        rise under+ ( c m mh rise' run )
        run + ( c m mh rise run' )
        end-of-map? ( c m mh current-rise current-run f )
    until ( c m mh rise run )
    2drop 2drop ( c ) ;

: day03a ( rise run a n -- n )
    read-map ( rise run map-a )
    -rot ( map-a rise run )
    count-trees ( n )
    ;

\ run-rise pairs
create slopes 1 , 1 , 3 , 1 , 5 , 1 , 7 , 1 , 1 , 2 ,
variable slope-count
5 slope-count !

: (day03b) ( slope-addr slope-count a n -- n )
    read-map 1 swap 2swap ( c m sa sc )
    2* cells over + swap u+do ( c m )
        dup ( c m m )
        i 2@ ( c m m rise run )
        count-trees ( c m n )
        rot * swap ( c' m )
    2 cells +loop ( c m )
    drop ( c ) ;

: day03b ( -- )
    slopes slope-count @ s" d/day03.txt" (day03b) .  ;

\ 7397677560 is TOO HIGH
