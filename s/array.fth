
\ array stuff

struct
  \ char% field array-type
  cell% field array-addr
  cell% field array-length
  cell% field array-capacity
end-struct array%

: array-from-capacity ( capacity -- array% )
  here over cells allot 0
  array% %allot
  tuck array-length !
  tuck array-addr !
  tuck array-capacity ! ;

: array-default-capacity ( -- array% ) 1024 array-from-capacity ;

: array->address-pair ( array% -- addr n )
  dup array-addr @
  swap array-length @ ;

: array[] ( i array% -- addr|0 )
  2dup array-length @ < if
    array-addr @ swap cells +
  else 2drop 0
  then ;

: darray[] ( i array% -- addr|0 ) over under+ array[] ;

: array-bounds ( array% -- limit start )
  dup array-addr @ dup
  rot array-length @ cells under+ ;

: over-array ( runtime: array% -- )
  ]] array-bounds u+do [[ ; immediate compile-only

: loop-array ( -- ) ]] cell +loop [[ ; immediate compile-only
: loop-darray ( -- ) ]] 2 cells +loop [[ ; immediate compile-only

: array-assert-capacity ( array% -- )
  dup array-length @
  swap array-capacity @
  assert( < ) ;

: array, ( array% n -- )
  swap
  dup array-assert-capacity
  dup array-length @ cells
  swap 1 over array-length +!
  array-addr @ + ! ;

: darray, ( array% d -- )
  rot
  dup array-assert-capacity
  dup array-length @ cells
  swap 2 over array-length +!
  array-addr @ + 2!  ;

