
\ I copied this section from the gforth tutorial.
0 value fd-in
0 value fd-out
1024 constant max-line
create line-buffer max-line 2 + allot

: open-input ( addr n -- )
  \ Opens an input file passed in as a string file name and assign it to fd-in.
  r/o open-file throw to fd-in ;

: close-input ( -- )
  \ Close the file handle in fd-in.
  fd-in close-file throw ;

: read-input-line ( -- u flag )
  \ Reads a line into line-buffer.
  \ Returns the characters read and a flag, which will be 0 on EOF.
  line-buffer max-line fd-in read-line throw ;

: write-output-line ( addr u -- ) fd-out write-line throw ;
: write-cr ( -- ) 13 fd-out emit-file throw ;

: end-of-file? ( n -- flag )
  \ Reads the flag left by read-input-line and returns true if EOF has been reached.
  0= ;

: open-output ( addr u -- )  w/o create-file throw to fd-out ;
: close-output ( -- )  fd-out close-file throw ;

: over-lines ( runtime: addr u -- addr u )
  postpone open-input
  postpone begin
  postpone read-input-line
  postpone while
  postpone line-buffer postpone swap
  ; immediate compile-only

: done-lines ( runtime: u -- )
  postpone repeat
  postpone close-input
  postpone drop
  ; immediate compile-only
