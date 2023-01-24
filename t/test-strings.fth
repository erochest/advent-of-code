: test-skip-space
    s" a   something" 1 /string
    dup assert( 12 = )
    skip-space
    dup assert( 9 = )
    assert( s" something" str= )

    s" else"
    skip-space
    assert( s" else" str= )

    \ s" test-skip-space" debug
    ;

: test-read-to
    s" somthing" 32 read-to
    dup assert( 0= )
    assert( s" " str= )

    s" something else" 32 read-to
    assert( s" else" str= )

    \ s" test-read-to" debug
    ;

: test-split-on
    s" key:value" [char] : split-on

    assert( s" value" str= )
    assert( s" key" str= )

    s" key-only" [char] : split-on

    assert( s" " str= )
    assert( s" key-only" str= )

    \ s" test-split-on" debug
    ;

: test-split-all
    s" light red bags contain 1 bright white bag, 2 muted yellow bags." 32 split-all
    dup assert( 12 = )
    over 0 cells + 2@ assert( s" light" str= )
    over 2 cells + 2@ assert( s" red" str= )
    over 4 cells + 2@ assert( s" bags" str= )
    over 6 cells + 2@ assert( s" contain" str= )
    over 8 cells + 2@ assert( s" 1" str= )
    over 10 cells + 2@ assert( s" bright" str= )
    over 12 cells + 2@ assert( s" white" str= )
    over 14 cells + 2@ assert( s" bag," str= )
    over 16 cells + 2@ assert( s" 2" str= )
    over 18 cells + 2@ assert( s" muted" str= )
    over 20 cells + 2@ assert( s" yellow" str= )
    over 22 cells + 2@ assert( s" bags." str= )
;

: test-all
    test-skip-space
    test-read-to
    test-split-on
    test-split-all
    s" test-all" debug ;
