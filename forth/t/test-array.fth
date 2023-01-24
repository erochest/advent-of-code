
: test-array-from-capacity
  16 array-from-capacity
  array-capacity @ assert( 16 = ) ;

: test-array-default-capacity
  array-default-capacity
  array-capacity @ assert( 1024 = ) ;

: test-array->address-pair
  array-default-capacity dup array->address-pair
  assert( 0= )
  swap array-addr @ assert( = ) ;

: third ( a b c -- a b c a ) 2 pick ;
: fourth ( a b c d -- a b c d a ) 3 pick ;

: setup
  array-default-capacity
  4 over array-length !
  7 0 third array[] !
  14 1 third array[] !
  21 2 third array[] !
  28 3 third array[] ! ;

: dsetup
  array-default-capacity
  8 over array-length !
  7.0 0 fourth darray[] 2!
  14.0 1 fourth darray[] 2!
  21.0 2 fourth darray[] 2!
  28.0 3 fourth darray[] 2! ;

: test-array[]
  setup
  42 over array[] assert( 0= )
  0 over array[] @ assert( 7 = )
  1 over array[] @ assert( 14 = )
  2 over array[] @ assert( 21 = )
  3 over array[] @ assert( 28 = )
  drop ;

: test-darray[]
  dsetup
  128 over darray[] assert( 0= )
  0 over darray[] 2@ assert( 7.0 d= )
  1 over darray[] 2@ assert( 14.0 d= )
  2 over darray[] 2@ assert( 21.0 d= )
  3 over darray[] 2@ assert( 28.0 d= )
  drop ;

: test-loop-array
  0 setup over-array
    dup case 
      0 of i @ assert( 7 = ) endof
      1 of i @ assert( 14 = ) endof
      2 of i @ assert( 21 = ) endof
      3 of i @ assert( 28 = ) endof
      4 of i @ assert( false ) endof
    endcase
    1+
  loop-array
  drop ;

: test-loop-darray
  0 dsetup over-array
    dup case 
      0 of i 2@ assert( 7.0 d= ) endof
      1 of i 2@ assert( 14.0 d= ) endof
      2 of i 2@ assert( 21.0 d= ) endof
      3 of i 2@ assert( 28.0 d= ) endof
      4 of i 2@ assert( false ) endof
    endcase
    1+
  loop-darray
  drop ;

: test-array,
  array-default-capacity
  dup 6 array,
  dup 18 array,
  dup 27 array,
  dup 54 array,
  dup array-length @ assert( 4 = )
  0 over array[] @ assert( 6 = )
  1 over array[] @ assert( 18 = )
  2 over array[] @ assert( 27 = )
  3 over array[] @ assert( 54 = )
  drop ;

: test-darray,
  array-default-capacity
  dup 6. darray,
  dup 18. darray,
  dup 27. darray,
  dup 54. darray,
  dup array-length @ assert( 8 = )
  0 over darray[] 2@ assert( 6. d= )
  1 over darray[] 2@ assert( 18. d= )
  2 over darray[] 2@ assert( 27. d= )
  3 over darray[] 2@ assert( 54. d= )
  drop ;

: test-all
  test-array-from-capacity
  test-array-default-capacity
  test-array->address-pair
  test-array[]
  test-darray[]
  test-loop-array
  test-loop-darray
  test-array,
  test-darray,
  s" array test-all" debug
  ;
