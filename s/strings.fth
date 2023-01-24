
: is-space? ( c -- f ) 32 = ;
: get-char ( a n -- c ) + c@ ;

: skip-space ( a n -- a n )
    0 begin ( a n c c+a  )
        dup 3 pick get-char is-space?
        while
        1+
    repeat
    /string ;

: read-to ( a n c -- a n )
    \ Reads to the next instance of c and returns the string from the next character.
    \ If the character isn't in the string, return an empty string.
    { a n c }
    s" " ( a-empty n-empty )
    a n bounds u+do
        i c@ c = if ( a-empty n-empty )
            2drop ( )
            i 1+ ( a1 )
            n ( a1 n0 )
            i 1+ a - ( a1 n0 n-diff )
            -  ( a1 n1 )
            leave
        then
    loop ;

: offset-length ( a0 n0 a2 n2 -- a0 n1' a2 n2 )
    2dup 2rot rot - ( a2 n2 a2 a0 n1' )
    rot drop 2swap ;

: split-on ( a0 n0 c -- a0 n1 a2 n2 )
    { delimiter }
    2dup delimiter read-to ( a0 n0 a2 n2 )
    offset-length ( a0 n1' a2 n2 )
    dup 0<> if
        2swap 1- 2swap ( a0 n1 a2 n2 )
    then ;

\ Returns the beginning of an array of strings
: split-all ( a0 n0 c -- a1 n1 )
    { delimiter }
    here 0 2swap ( addr n a0 n0 )
    begin
        dup 0> while
        delimiter split-on ( addr n a1 n1 a2 n2 )
        2swap 2, ( addr n a2 n2 )
        2swap 1+ 2swap ( addr n' a2 n2 )
    repeat ( addr n a n )
    2drop ( addr n )
    ;
