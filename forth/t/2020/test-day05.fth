
: test-focus-upper
    0 63 focus-upper assert( 63 = ) assert( 32 = )
    32 47 focus-upper assert( 47 = ) assert( 40 = )
    40 47 focus-upper assert( 47 = ) assert( 44 = )
    44 45 focus-upper assert( 45 = ) assert( 45 = )
    \ s" test-focus-upper" debug
    ;

: test-focus-lower
    0 127 focus-lower assert( 63 = ) assert( 0= )
    32 63 focus-lower assert( 47 = ) assert( 32 = )
    44 47 focus-lower assert( 45 = ) assert( 44 = )
    44 45 focus-lower assert( 44 = ) assert( 44 = )
    \ s" test-focus-lower" debug
    ;

: test-focus-row
    0 127 [char] F focus-row assert( 63 = ) assert( 0= )
    0 63 [char] B focus-row assert( 63 = ) assert( 32 = )
    32 63 [char] F focus-row assert( 47 = ) assert( 32 = )
    32 47 [char] B focus-row assert( 47 = ) assert( 40 = )
    40 47 [char] B focus-row assert( 47 = ) assert( 44 = )
    44 47 [char] F focus-row assert( 45 = ) assert( 44 = )
    44 45 [char] F focus-row assert( 44 = ) assert( 44 = )
    \ s" test-focus-row" debug
    ;

: test-focus-column
    0 7 [char] R focus-column assert( 7 = ) assert( 4 = )
    4 7 [char] L focus-column assert( 5 = ) assert( 4 = )
    4 5 [char] R focus-column assert( 5 = ) assert( 5 = )
    \ s" test-focus-column" debug
    ;

: test-find-column-row
    s" FBFBBFFRLR" find-column-row assert( 44 = ) assert( 5 = )
    s" BFFFBBFRRR" find-column-row assert( 70 = ) assert( 7 = )
    s" FFFBBBFRRR" find-column-row assert( 14 = ) assert( 7 = )
    s" BBFFBBFRLL" find-column-row assert( 102 = ) assert( 4 = )
    \ s" find-column-row" debug
    ;

: test-find-seat
    5 44 find-seat assert( 357 = )
    7 70 find-seat assert( 567 = )
    7 14 find-seat assert( 119 = )
    4 102 find-seat assert( 820 = )
    \ s" find-seat" debug
    ;

: test-(parta) s" ../sample/2020/day05.txt" (parta) assert( 820 = ) ;
: test-(parta)-data s" ../data/2020/day05.txt" (parta) assert( 880 = ) ;

: test-is-front?
    0 assert( is-front? )
    127 assert( is-front? 0= )
    32 assert( is-front? 0= )
    \ s" test-is-front?" debug
    ;

: test-is-back?
    0 assert( is-back? 0= )
    127 assert( is-back? )
    32 assert( is-back? 0= )
    \ s" test-is-back?" debug
    ;

: test-all
    test-focus-upper
    test-focus-lower
    test-focus-row
    test-focus-column
    test-find-column-row
    test-find-seat
    test-(parta)
    test-(parta)-data
    test-is-front?
    test-is-back?
    ;