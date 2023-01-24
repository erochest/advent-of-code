
: test-queue-set-new
    queue-set-new
    queue-set-size @ assert( 0= ) ;

: test-queue-set-add-unique
    queue-set-new
    dup 42 queue-set-add
    dup queue-set-size @ assert( 1 = )
    dup queue-set-addr @ 0 cells + @ assert( 42 = )
    drop ;

: fixture
    queue-set-new
    dup 13 queue-set-add
    dup 42 queue-set-add
    dup 24 queue-set-add ;

: test-queue-set-add-duplicate
    fixture
    dup 42 queue-set-add
    dup queue-set-size @ assert( 3 = )
    dup queue-set-addr @ 0 cells + @ assert( 13 = )
    dup queue-set-addr @ 1 cells + @ assert( 42 = )
    dup queue-set-addr @ 2 cells + @ assert( 24 = )
    drop ;

: test-queue-set@
    fixture
    dup 42 queue-set-add
    dup 0 queue-set@ assert( 13 = )
    dup 1 queue-set@ assert( 42 = )
    dup 2 queue-set@ assert( 24 = )
    drop ;

: test-queue-set-contains?
    fixture
    dup assert( 99 queue-set-contains? invert )
    dup assert( 13 queue-set-contains? )
    dup assert( 42 queue-set-contains? )
    dup assert( 24 queue-set-contains? )
    drop ;

: test-queue-set-add-all
    here 1 , 1 , 2 , 3 , 5 , 8 ,
    queue-set-new
    swap
    6
    queue-set-add-all
    dup queue-set-size @ assert( 5 = )
    dup 0 queue-set@ assert( 1 = )
    dup 1 queue-set@ assert( 2 = )
    dup 2 queue-set@ assert( 3 = )
    dup 3 queue-set@ assert( 5 = )
    dup 4 queue-set@ assert( 8 = )
    drop ;

: test-all
    test-queue-set-new
    \ s" test-queue-set-new" debug
    test-queue-set-add-unique
    \ s" test-queue-set-add-unique" debug
    test-queue-set-add-duplicate
    \ s" test-queue-set-add-duplicate" debug
    test-queue-set@
    \ s" test-queue-set@" debug
    test-queue-set-contains?
    \ s" test-queue-set-contains?" debug
    test-queue-set-add-all
    \ s" test-queue-set-add-all" debug
    s" test-all" debug ;