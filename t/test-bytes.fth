: test-create-mask
    0 create-mask assert( 1 = )
    1 create-mask assert( 3 = )
    2 create-mask assert( 7 = )
    3 create-mask assert( 15 = )
    \ s" test-create-mask" debug
    ;

: test-set-position
    0 0 set-position assert( 1 = )
    0 1 set-position assert( 2 = )
    0 2 set-position assert( 4 = )
    \ s" test-set-position" debug
    ;

: test-all
    test-create-mask
    test-set-position
    ;