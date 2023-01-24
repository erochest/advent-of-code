
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

: test-(day06a) s" f/day06.txt" (day06a) assert( 11 = ) ;
: test-(day06a)-data s" d/day06.txt" (day06a) assert( 6778 = ) ;

: test-(day06b) s" f/day06.txt" (day06b) assert( 6 = ) ;
: test-(day06b)-data s" d/day06.txt" (day06b) assert( 3406 = ) ;

: test-all
    test-set-declarations
    test-count-declarations
    test-(day06a)
    test-(day06a)-data
    test-(day06b)
    test-(day06b)-data
    ;