
: test-bag
  faded olive bag bag@ assert( 1 7 lshift 1 19 lshift d= ) ;

: test-sqrt
  1 sqrt assert( 0= )
  2 sqrt assert( 1 = )
  4 sqrt assert( 2 = )
  8 sqrt assert( 3 = )
  1024 sqrt assert( 10 = )
  33554432 sqrt assert( 25 = ) ;

: test-(color)
  other (color) assert( s" other" str= )
  red (color) assert( s" red" str= )
  purple (color) assert( s" purple" str= )
  yellow (color) assert( s" yellow" str= )
  ;

: test-(modifier)
  no (modifier) assert( s" no" str= )
  bright (modifier) assert( s" bright" str= )
  mirrored (modifier) assert( s" mirrored" str= )
  wavy (modifier) assert( s" wavy" str= )
  ;

: test-contain
  rules-reset
  faded olive bags contain
  dup rule-bag @ bag@ assert( faded olive d= )
  dup rule-count @ assert( 0= )
  drop ;

: test-rule-body@
  faded olive bag contain 1 muted red bag,
  dup rule-bag @ bag@ assert( faded olive d= )
  dup rule-body@ dup bag@ assert( muted red d= )
  2drop ;

: test-directly-contains?
  muted plum bag contain
  3 light white bag,
  4 dotted black bag,
  1 shiny blue bag,
  dup assert( muted red directly-contains? 0= )
  dup assert( dotted black directly-contains? )
  drop
  dark plum bag contain
  1 shiny red bag,
  3 faded olive bag,
  dup assert( faded olive directly-contains? )
  drop ;

: load-rules ( -- addr n )
  rules-reset
  muted plum bag contain 2 light white bags, 4 dotted black bags, 1 shiny blue bag, drop
  bright orange bag contain no other bag, drop
  faded olive bag contain 1 muted red bag, 3 vibrant gold bag, drop
  dotted black bag contain 7 vibrant gold bag, drop
  vibrant gold bag contain 4 dark plum bag, 1 bright orange bag, drop
  dark plum bag contain 1 shiny red bag, 1 faded olive bag, drop

  \ ." RULE ARRAY " cr
  \ dump-rule-array

    rule-array @ dup array-length @ ;

: array-contains? ( addr u n -- f )
    { target }
    false -rot
    cells bounds u+do
        target i @ = if
            invert
            leave
        then
    1 cells +loop ;

: test-array-contains?
    here 3
    42 , 13 , 99 ,
    assert( 2dup 12 array-contains? invert )
    assert( 2dup 13 array-contains? )
    assert( 2dup 42 array-contains? )
    assert( 2dup 99 array-contains? )
    2drop ;

: test-all-can-contain?
s" : test-all-can-contain?" debug
  load-rules ( addr n )
  s" load-rules ( addr n )" debug
  muted red bag all-can-contain?
  s" muted red bag all-can-contain?" debug
  dup ." dump-bags" cr queue-set-dump-bags
  dup muted red assert( queue-set-2contains? )
  s" dup muted red assert( queue-set-2contains? )" debug
  dup faded olive assert( queue-set-2contains? )
  s" dup faded olive assert( queue-set-2contains? )" debug
  dup dark plum assert( queue-set-2contains? )
  s" dup dark plum assert( queue-set-2contains? )" debug
  dup bright orange assert( queue-set-2contains? invert )
  s" dup bright orange assert( queue-set-2contains? invert )" debug
  drop ;

: test-parse-rule
  s" light red bags contain 1 bright white bag, 2 muted yellow bags." parse-rule
  dup assert( rule-bag @ bag@ light red d= )
  dup assert( rule-count @ 2 = )
  dup rule-body@ bag@ assert( bright white d= )
  dup rule-body@ next-bag + bag@ assert( muted yellow d= )
  drop

  s" bright white bags contain 1 shiny gold bag." parse-rule
  dup assert( rule-bag @ bag@ bright white d= )
  dup assert( rule-count @ 1 = )
  dup rule-body@ bag@ assert( shiny gold d= )
  drop

  s" faded blue bags contain no other bags." parse-rule
  dup assert( rule-bag @ bag@ faded blue d= )
  dup assert( rule-count @ 0= )
  drop ;

: test-rule!
  rules-reset
  s" light red bags contain 1 bright white bag, 2 muted yellow bags." parse-rule
  \ rule! is called as part of "contain"
  assert( array-length @ 1 = )
  rule-array @ 0 swap array[] @
  dup assert( rule-bag @ bag@ light red d= )
  dup assert( rule-count @ 2 = )
  dup rule-body@ bag@ assert( bright white d= )
  dup rule-body@ next-bag + bag@ assert( muted yellow d= )
  2drop
  ;

: test-find-modifiers
  s" light red bags contain 1 bright white bag, 2 muted yellow bags." find-modifiers
  assert( 3 = )
  dup 0 cells + 2@ assert( s" light" str= )
  dup 2 cells + 2@ assert( s" bright" str= )
  dup 4 cells + 2@ assert( s" muted" str= )
  drop
  ;

: test-find-colors
  s" light red bags contain 1 bright white bag, 2 muted yellow bags." find-colors
  assert( 3 = )
  dup 0 cells + 2@ assert( s" red" str= )
  dup 2 cells + 2@ assert( s" white" str= )
  dup 4 cells + 2@ assert( s" yellow" str= )
  drop
  ;

: test-#bags-contained
  rules-reset
  s" f/day07.txt" over-lines
    parse-rule drop
  done-lines
  shiny gold bag #bags-contained
  s" shiny gold bag #bags-contained" debug
  assert( 32 = ) ;

: test-(day07a)
  rules-reset
  shiny gold bag s" f/day07.txt" (day07a)
  assert( 4 = ) ;
: test-(day07a)-data
  rules-reset
  shiny gold bag s" d/day07.txt" (day07a)
  assert( 124 = ) ;

: test-(day07b) shiny gold bag s" f/day07b.txt" (day07b) assert( 126 = ) ;
: test-(day07b)-data shiny gold bag s" d/day07.txt" (day07b) assert( 0<> ) ;

: test-all
    test-bag
    test-sqrt
    test-(color)
    test-(modifier)
    test-contain
    test-rule-body@
    test-directly-contains?
    test-array-contains?
    test-all-can-contain?
    test-parse-rule
    test-rule!
    test-find-modifiers
    test-find-colors
    test-#bags-contained
    test-(day07a)
    test-(day07a)-data
    test-(day07b)
    \ test-(day07b)-data
    s" test-all" debug
    ;
