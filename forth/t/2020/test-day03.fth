
: fixture ( -- map ) s" ../sample/2020/day03.txt" read-map ;

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

: test-parta
    1 3 s" ../sample/2020/day03.txt" parta
    assert( 7 = )
    \ s" test-parta" debug
    ;

: test-parta-prod
    1 3 s" ../data/2020/day03.txt" parta
    assert( 164 = )
;

: test-partb
    1 1 s" ../sample/2020/day03.txt" parta assert( 2 = )
    1 3 s" ../sample/2020/day03.txt" parta assert( 7 = )
    1 5 s" ../sample/2020/day03.txt" parta assert( 3 = )
    1 7 s" ../sample/2020/day03.txt" parta assert( 4 = )
    2 1 s" ../sample/2020/day03.txt" parta assert( 2 = )
    slopes slope-count @ s" ../sample/2020/day03.txt" (partb)
    assert( 336 = )
    ;

: test-partb-prod
    slopes slope-count @ s" ../data/2020/day03.txt" (partb)
    dup assert( 7397677560 < )
    assert( 5007658656 = )
    ;

test-read-pattern
test-read-map
test-get-tree
test-count-trees
test-parta
test-parta-prod
test-partb
test-partb-prod