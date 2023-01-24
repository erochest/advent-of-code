
s" array.fth" included

\ A queue-set is an address to an array of singles with a size indicating how many
\ items are in the array.

: queue-set-addr array-addr ;
: queue-set-size array-length ;

: queue-set-2size ( queue-set -- n )
  queue-set-size @ 2/ ;

: queue-set-dump ( queue-set -- )
    dup ." queue-set dump " . cr
    dup queue-set-addr @ dup ." address " . cr
    swap queue-set-size @ dup ." size " . cr
    cells dump ;

: queue-set-2dump ( queue-set -- )
  dup ." queue-set dump " . cr
  dup queue-set-addr @ dup ." address " . cr
  swap queue-set-2size dup ." size " . cr
  2* cells dump ;

\ Initializes a new queue set.
: queue-set-new ( -- queue-set )
    array-default-capacity
    here over queue-set-addr !
    0 over queue-set-size ! ;

: queue-set-bounds ( queue-set -- u u )
    dup
    queue-set-addr @ dup
    rot queue-set-size @ cells
    under+ ;

: queue-set-size-1+ ( queue-set -- )
    1 swap queue-set-size +! ;

: queue-set-size-2+ ( queue-set -- )
    2 swap queue-set-size +! ;

\ Tests whether n is already in the queue-set.
: queue-set-contains? ( queue-set n -- flag )
    { target }
    false swap
    queue-set-bounds u+do
        target i @ = if
            invert
            leave
        then 
    cell +loop ;

\ Tests whether n is already in the queue-set.
: queue-set-2contains? ( queue-set d -- flag )
    false -rot 3 roll
    queue-set-bounds u+do
        2dup i 2@ d= if
            rot invert -rot
            leave
        then 
    2 cells +loop
    2drop ;

\ Adds n to the queue-set if it's not already there.
\ TODO: return flag on whether it's actually added, maybe?
: queue-set-add ( queue-set n -- )
    2dup queue-set-contains? if
        2drop
    else
        array,
    then ;

\ Adds n to the queue-set if it's not already there.
\ TODO: return flag on whether it's actually added, maybe?
: queue-set-2add ( queue-set d -- )
    2 pick 2 pick 2 pick
    queue-set-2contains? if
        2drop drop
    else
        darray,
    then ;

\ Returns item n, 0-indexed
: queue-set@ ( queue-set index -- n )
    swap array[] @ ;

\ Returns item n, 0-indexed
: queue-set2@ ( queue-set index -- d )
    swap darray[] 2@ ;

\ Adds all the items in the second array to the queue-set.
: queue-set-add-all ( queue-set addr n -- queue-set )
    cells bounds u+do
        dup i @ queue-set-add
    1 cells +loop ;
