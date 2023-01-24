
: test-open-input
  s" f/day01.txt" open-input
  assert( fd-in 0<> )
  close-input ;

: test-read-input-line
  s" f/day01.txt" open-input
  read-input-line
  assert( 0<> )
  assert( 4 = )
  assert( s" 1721" line-buffer 4 compare 0= )
  close-input
  ;

: test-end-of-file?
  s" f/one-line.txt" open-input
  read-input-line
  assert( end-of-file? invert )
  read-input-line
  assert( end-of-file? )
  close-file
  2drop
  ;

: test-line-buffer>n
  s" f/day01.txt" open-input
  read-input-line
  close-input
  drop
  line-buffer>n
  assert( 1721 = )
  ;

: test-over-addr
  1 2 3 over-addr
  assert( 2 = )
  assert( 1 = )
  assert( 3 = )
  assert( 2 = )
  assert( 1 = )
  ;

: test-offset!
  here dup 42 , 13 ,
  99 over 0 swap offset!
  assert( dup @ 99 = )
  51 over 1 swap offset!
  assert( 1 cells + @ 51 = )
  -2 allot
  drop
  ;

: test-read-counter+
  1 2 read-counter+
  assert( 2 = )
  assert( 2 = )
  ;

: test-read-input 
  s" f/day01.txt" read-input
  assert( 6 = )
  dup assert( @ 1721 = )
  dup assert( 1 cells + @ 979 = )
  dup assert( 2 cells + @ 366 = )
  dup assert( 3 cells + @ 299 = )
  dup assert( 4 cells + @ 675 = )
  dup assert( 5 cells + @ 1456 = )
  ;

create pair-totalling-fixture 4 , 2 , 6 , 20 ,
: test-find-pair-totalling
  pair-totalling-fixture 4 8 find-pair-totalling
  2dup assert( + 8 = )
  \ These are trying to capture that the top 2 aren't getting the same ID.
  2dup assert( 4 <> ) assert( 4 <> )
  \ These are trying to capture that I don't care about the order
  assert( dup 2 = swap 6 = or )
  assert( dup 2 = swap 6 = or )
  ;

: test-find-triplet-totalling
  s" f/day01.txt" read-input
  2020 find-triplet-totalling
  assert( dup dup 979 = rot 366 = rot 675 = or or )
  assert( dup dup 979 = rot 366 = rot 675 = or or )
  assert( dup dup 979 = rot 366 = rot 675 = or or )
  ;

: tests
  test-open-input
  test-read-input-line
  test-end-of-file?
  test-over-addr
  test-offset!
  test-read-counter+
  test-line-buffer>n
  test-read-input
  test-find-pair-totalling
  test-find-triplet-totalling
  ;

tests

