
: test-set-declarations
    s" abc " set-declarations assert( 1 2 4 + + = )
    s" a a a " set-declarations assert( 1 = )
    s" ab ac " set-declarations assert( 1 2 4 + + = )
    s" b" set-declarations assert( 2 = )
    ;

: test-count-declarations
    0 count-declarations assert( 0= )
    1 count-declarations assert( 1 = )
    4 count-declarations assert( 1 = )
    32 count-declarations assert( 1 = )
    1024 count-declarations assert( 1 = )
    7 count-declarations assert( 3 = )
    1 4 16 64 + + + count-declarations assert( 4 = )
    0 [char] z [char] a - set-position count-declarations assert( 1 = )
    ;

: test-(parta) s" ../sample/2020/day06.txt" (parta) assert( 11 = ) ;
: test-(parta)-data s" ../data/2020/day06.txt" (parta) assert( 6778 = ) ;

: test-(partb) s" ../sample/2020/day06.txt" (partb) assert( 6 = ) ;
: test-(partb)-data s" ../data/2020/day06.txt" (partb) assert( 3406 = ) ;

: test-all
    test-set-declarations
    test-count-declarations
    test-(parta)
    test-(parta)-data
    test-(partb)
    test-(partb)-data
    ;