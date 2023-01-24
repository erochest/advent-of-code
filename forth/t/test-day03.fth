
: fixture ( -- map ) s" f/day03.txt" read-map ;

: test-read-pattern
    s" ...." read-pattern assert( 0= )
    s" #..." read-pattern assert( 1 = )
    s" ..#." read-pattern assert( 4 = )
    s" ####" read-pattern assert( 15 = )
    \ s" test-read-pattern" debug
    ;

: test-read-map
    fixture
    dup map-height @ assert( 11 = )
    dup map-pattern-width @ assert( 11 = )
    dup map-data @ 0 cells + @ assert( 12 = )
    dup map-data @ 1 cells + @ assert( 273 = )
    dup map-data @ 2 cells + @ assert( 578 = )
    dup map-data @ 3 cells + @ assert( 1300 = )
    map-data @ 4 cells + @ assert( 610 = )
    \ s" test-read-map" debug
    ;

: test-get-tree
    fixture
    0 0 third assert( get-tree invert )
    0 3 third assert( get-tree )
    0 7 third assert( get-tree invert )
    1 0 third assert( get-tree )
    3 4 third assert( get-tree )
    3 5 third assert( get-tree invert )
    3 14 third assert( get-tree invert )
    3 15 rot assert( get-tree )
    \ s" test-get-tree" debug
    ;

: test-count-trees
    fixture ( m )
    1 3 count-trees ( n )
    assert( 7 = )
    \ s" test-count-trees" debug
    ;

: test-day03a
    1 3 s" f/day03.txt" day03a
    assert( 7 = )
    \ s" test-day03a" debug
    ;

: test-day03a-prod
    1 3 s" d/day03.txt" day03a
    assert( 164 = )
;

: test-day03b
    1 1 s" f/day03.txt" day03a assert( 2 = )
    1 3 s" f/day03.txt" day03a assert( 7 = )
    1 5 s" f/day03.txt" day03a assert( 3 = )
    1 7 s" f/day03.txt" day03a assert( 4 = )
    2 1 s" f/day03.txt" day03a assert( 2 = )
    slopes slope-count @ s" f/day03.txt" (day03b)
    assert( 336 = )
    ;

: test-day03b-prod
    slopes slope-count @ s" d/day03.txt" (day03b)
    dup assert( 7397677560 < )
    assert( 5007658656 = )
    ;

test-read-pattern
test-read-map
test-get-tree
test-count-trees
test-day03a
test-day03a-prod
test-day03b
test-day03b-prod