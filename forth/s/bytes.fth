: create-mask ( n -- n )
    \ Create a mask in which the first n bits are set.
    1 swap 0 u+do
        dup 2* or
    loop ;

: set-position ( n-bits-1 n -- n-bits-2 )
    \ Set the position at n to 1.
    1 swap lshift or ;

: bit-set? ( bits n -- f ) 1 swap lshift and ;
