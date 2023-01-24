
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

: test-all
  test-open-input
  test-read-input-line
  test-end-of-file?
  ;

: dump-file
  s" dump-file" debug
  s" f/day01.txt" 2dup debug over-lines
    s" over-lines" debug
    ." >>>" type cr
    s" type" debug
  done-lines
  s" done-lines" debug
  ;

dump-file