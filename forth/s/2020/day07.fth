s" ../files.fth" included
s" ../debug.fth" included
\ s" ../array.fth" included \ included in queue-set.fth
s" ../stack.fth" included
s" ../queue-set.fth" included
s" ../strings.fth" included

: no 0 ;
: bright [ 1 0 lshift ] literal ;
: clear [ 1 1 lshift ] literal ;
: dark [ 1 2 lshift ] literal ;
: dim [ 1 3 lshift ] literal ;
: dotted [ 1 4 lshift ] literal ;
: drab [ 1 5 lshift ] literal ;
: dull [ 1 6 lshift ] literal ;
: faded [ 1 7 lshift ] literal ;
: light [ 1 8 lshift ] literal ;
: mirrored [ 1 9 lshift ] literal ;
: muted [ 1 10 lshift ] literal ;
: pale [ 1 11 lshift ] literal ;
: plaid [ 1 12 lshift ] literal ;
: posh [ 1 13 lshift ] literal ;
: shiny [ 1 14 lshift ] literal ;
: striped [ 1 15 lshift ] literal ;
: vibrant [ 1 16 lshift ] literal ;
: wavy [ 1 17 lshift ] literal ;

: other 0 ;
: aqua [ 1 0 lshift ] literal ;
: beige [ 1 1 lshift ] literal ;
: black [ 1 2 lshift ] literal ;
: blue [ 1 3 lshift ] literal ;
: bronze [ 1 4 lshift ] literal ;
: brown [ 1 5 lshift ] literal ;
: chartreuse [ 1 6 lshift ] literal ;
: coral [ 1 7 lshift ] literal ;
: crimson [ 1 8 lshift ] literal ;
: cyan [ 1 9 lshift ] literal ;
: fuchsia [ 1 10 lshift ] literal ;
: gold [ 1 11 lshift ] literal ;
: gray [ 1 12 lshift ] literal ;
: green [ 1 13 lshift ] literal ;
: indigo [ 1 14 lshift ] literal ;
: lavender [ 1 15 lshift ] literal ;
: lime [ 1 16 lshift ] literal ;
: magenta [ 1 17 lshift ] literal ;
: maroon [ 1 18 lshift ] literal ;
: olive [ 1 19 lshift ] literal ;
: orange [ 1 20 lshift ] literal ;
: plum [ 1 21 lshift ] literal ;
: purple [ 1 22 lshift ] literal ;
: red [ 1 23 lshift ] literal ;
: salmon [ 1 24 lshift ] literal ;
: silver [ 1 25 lshift ] literal ;
: tan [ 1 26 lshift ] literal ;
: teal [ 1 27 lshift ] literal ;
: tomato [ 1 28 lshift ] literal ;
: turquoise [ 1 29 lshift ] literal ;
: violet [ 1 30 lshift ] literal ;
: white [ 1 31 lshift ] literal ;
: yellow [ 1 32 lshift ] literal ;

struct
  cell% field bag-count
  cell% field bag-modifier
  cell% field bag-color
end-struct bag%

\ Simple square roots for powers of 2.
: sqrt ( n -- n )
  0 begin
    over 1 > while
    1+ swap 2/ swap
  repeat
  nip ;

: "color" c" other     aqua      beige     black     blue      bronze    brown     chartreusecoral     crimson   cyan      fuchsia   gold      gray      green     indigo    lavender  lime      magenta   maroon    olive     orange    plum      purple    red       salmon    silver    tan       teal      tomato    turquoise violet    white     yellow    " ;
10 constant "color"-width
: (color) ( color -- c-addr u )
  dup 0> if sqrt 1+ then
  "color"-width * "color" 1+ + "color"-width -trailing ;
: .color ( color -- ) (color) type space ;

: "modifier" c" no      bright  clear   dark    dim     dotted  drab    dull    faded   light   mirroredmuted   pale    plaid   posh    shiny   striped vibrant wavy    " ;
8 constant "modifier"-width
: (modifier) ( modifier -- c-addr u )
  dup 0> if sqrt 1+ then
  "modifier"-width * "modifier" 1+ + "modifier"-width -trailing ;
: .modifier ( modifier -- ) (modifier) type space ;

: .bag ( bag -- )
  dup bag-count @ ?dup if
    dup 1 = if s" bag" else s" bags" then rot
    .
    rot
  else
    s" bags" rot
  then
  dup bag-modifier @ .modifier bag-color @ .color
  type space ;

: bag ( modifier color -- bag )
  bag% %allot
  tuck bag-color !
  tuck bag-modifier !
  0 over bag-count ! ;
: bags ( modifier color -- bag ) bag ;

: bag@ ( bag%-addr -- modifier color )
  dup bag-modifier @
  swap bag-color @ ;
: next-bag ( -- offset ) bag% %size ;

: queue-set-dump-bags ( queue-set -- )
  queue-set-bounds u+do
    i 2@ swap .modifier .color ." , "
  2 cells +loop
  cr ;

: dump-bags ( addr n -- )
  0 u+do
    dup i bag% %size * +
    dup ." [dump-bag at " . ." ]"
    .bag ." , "
  loop
  drop cr ;

struct
  cell% field rule-bag
  cell% field rule-count
  array% field rule-body
end-struct rule%

\ Rules are stored sequentially on the heap, but the rule-array tracks
\ the start of each rule, basically as a pointer. The rule-count items after
\ it in the array are pointers to the bag% instances in the rule body.

variable rule-array
1024 2* array-from-capacity rule-array !
." rule-array: " rule-array @ . cr
." array-addr: " rule-array @ array-addr @ . cr
." array-length: " rule-array @ array-length @ . cr
." array-capacity: " rule-array @ array-capacity @ . cr

: rules-reset ( -- ) 0 rule-array @ array-length ! ;

: rule! ( rule -- ) rule-array @ swap array, ;

: dump-rule ( rule-addr -- )
  dup rule-bag @ .bag ." contains "
  dup rule-count @ dup . ." : "
  rule% %size under+
  dump-bags ;

: dump-rule-array
  rule-array @ over-array
    i @ dump-rule
  loop-array
  cr ;

: contain ( bag -- rule )
  0 rule% %allot ( bag count rule )
  tuck rule-count ! ( bag rule )
  tuck rule-bag ! ( rule )
  dup rule! ;

: rule-body@ ( rule -- addr ) rule% %size + ;

: bag, ( rule [ n ] modifier color -- rule )
  2dup 0= swap 0= or if
    2drop 
  else
    bag
    bag-count !
    1 over rule-count +!
  then ;

: bags, ( rule modifier color -- rule ) bag, ;
: bag. ( rule modifier color -- rule ) bag, ;
: bags. ( rule modifier color -- rule ) bag, ;

\ Helpers for directly-contains?
: add-flag-default ( s d -- flag s d ) false 3 roll 2swap ;
: pull-rule-body ( rule d -- d rule rule-body-addr ) rot dup rule-body@ ;
: pull-rule-count ( rule rule-body-addr -- rule-body-addr rule-body-addr rule-count )
  dup rot rule-count @ ;
: rule-bounds ( rule-body-addr rule-body-addr rule-count -- limit start ) bag% %size * under+ ;
: rule-contains-update ( flag inner-bag rule-bag -- flag' inner-bag ) 2over d= 3 roll or -rot ;

: directly-contains? ( outer-rule inner-bag-d -- f )
  add-flag-default ( return-value outer-rule inner-bag )
  pull-rule-body ( return-value inner-bag outer-rule rule-body )
  pull-rule-count ( return-value inner-bag rule-body rule-body rule-count )
  rule-bounds u+do ( return-value inner-bag )
    i bag@ rule-contains-update ( flag' inner-bag )
    2 pick if leave then
  bag% %size +loop
  2drop ;

\ Helpers for all-can-contain?
: init-all-can-contain? ( inner-bag -- queue-set cursor )
    queue-set-new dup 2swap queue-set-2add 0 ;
: current-bag@ ( queue-set cursor -- queue-set cursor current-bag ) 2dup queue-set2@ ;
: current-rule@ ( previous-rule rule-addr -- current-rule ) @ nip ;
: current-contains? ( bag rule -- bag rule f ) dup 2over directly-contains? ;
: current-bag! ( queue-set a d rule -- queue-set a d rule ) 4 pick over rule-bag @ bag@ queue-set-2add ;
: cursor1+ ( cursor -- cursor' ) 1+ ;
: cursor-done? ( queue-set cursor -- queue-set cursor f )
  2dup swap queue-set-2size >= ;

: contains-array-bounds ( addr u -- addr addr ) cells over + swap ;

: all-can-contain? ( rule-list rule-count inner-bag -- queue-set )
s" : all-can-contain? ( rule-list rule-count inner-bag -- queue-set )" debug
  { rule-list total-rule-count inner-bag }
  s" { rule-list total-rule-count inner-bag }" debug
  inner-bag bag@
  s" inner-bag bag@" debug
  init-all-can-contain? ( qset cursor )
  s" init-all-can-contain? ( qset cursor )" debug
  begin
  s" begin" debug
    current-bag@ 0 ( qset curser current-bag rule )
    s" current-bag@ 0 ( qset curser current-bag rule )" debug
    rule-list total-rule-count contains-array-bounds u+do
    s" rule-list total-rule-count contains-array-bounds u+do" debug
      i current-rule@
      s" i current-rule@" debug
      current-contains? if current-bag! then
      s" current-contains? if current-bag! then" debug
    cell +loop
    s" cell +loop" debug
    drop 2drop cursor1+ ( qset cursor' )
    s" drop 2drop cursor1+ ( qset cursor' )" debug
  cursor-done? until ( qset cursor )
  s" cursor-done? until ( qset cursor )" debug
  drop ( qset )
  s" drop ( qset )" debug
  ;

: parse-rule ( a u -- rule% ) evaluate ;

2 constant modifier-offset
1 constant color-offset

\ Searches in the input string for words offset before "bag*" and returns
\ an array of strings (doubles).
: find-bag-prefix ( a u offset -- a u )
  { offset }
  32 split-all ( a1 n1 )
  { split-addr split-count }
  here 0 ( a0 n0 )
  split-count 0 u+do ( a0 n0 )
    split-addr i 2* cells + 2@ s" bag" string-prefix? if
      split-addr i offset - 2* cells + 2@
      2,
      1+
    then
  loop ;

\ Searches in the input string for the modifiers and returns an array of strings (doubles).
: find-modifiers ( a u -- a u ) modifier-offset find-bag-prefix ;

\ Searches in the input string for the colors and returns an array of strings (doubles).
: find-colors ( a u -- a u ) color-offset find-bag-prefix ;

\ writes the terms at an offset found to fd-out.
: find-offset-file ( offset a u -- )
  over-lines ( offset a u )
    2 pick find-bag-prefix
    2* cells over + swap u+do
      i 2@ write-output-line
    2 cells +loop
  done-lines ;

\ helpers

: over-bags ( runtime: rule-addr -- bag-addr )
  ]] dup
  rule-body@
  swap rule-count @ bag% %size *
  over + swap
  u+do
  i [[
  ; immediate compile-only

: loop-bags ( -- )
  ]] next-bag +loop [[ ; immediate compile-only

: find-bag-rule ( bag-addr -- rule-addr|0 )
  0
  rule-array @ over-array
    i @ rule-bag @ bag@
    3 pick bag@
    d= if drop i @ leave then
  loop-array
  nip ;

: initialize-counter ( bag -- counter bag ) 0 swap ;
: initialize-buffer-array-capacity ( bag -- b-addr b-num b-capacity )
  find-bag-rule
  here 1024 cells allot
  swap over !
  1 1024 ;

: initialize-buffer-cursor ( -- cursor ) 0 ;
: bags-contained-current-rule@ ( b-addr b-last cursor -- b-addr b-last cursor rule-addr|0 )
  2dup >= if
    2 pick over cells + @
  else 0
  then ;
: +bag-counter ( counter buffer-addr buffer-num cursor rule-addr bag-addr bag-addr -- counter buffer-addr buffer-num cursor rule-addr bag-addr )
  bag-count @
  6 roll
  +
  2rot 2rot 5 roll ;

: buffer-append-rule ( counter buffer-addr buffer-num cursor rule-addr bag-rule-addr buffer-capacity -- counter buffer-addr buffer-num cursor rule-addr )
  2rot 1+ 2 roll over > if
    2dup 4 roll -rot 1- cells + ! 2swap
  then ;

: +cursor ( cursor rule-addr -- cursor' ) drop 1+ ;
: clean-up ( counter buffer-addr buffer-len cursor -- counter ) drop 2drop ;

\ todo it's counting each sub-bag once. eg, if a bag contains two other bags, which
\ contains 3 bags, that would be a total of 8 bags, not 5
: #bags-contained ( bag -- n )
  initialize-counter
  initialize-buffer-array-capacity { buffer-capacity }
  initialize-buffer-cursor
  begin
    bags-contained-current-rule@
    ?dup while
    dup ." CURRENT-RULE " rule-bag @ .bag cr
    dup over-bags
      dup +bag-counter
      dup 4 spaces ." CURRENT RULE BAG " .bag cr
      find-bag-rule
      buffer-capacity buffer-append-rule
    loop-bags
    +cursor
  repeat
  \ buffer-capacity -1 * cells allot
  clean-up ;

: (parta) ( bag a u -- n )
  over-lines
    \ 2dup ." parsing: " type cr
    parse-rule drop
  done-lines
  -1 rule-array @ array-length +!
  rule-array @ array-length @ rot
  all-can-contain?
  queue-set-2size 1- ;

: parta ( -- ) shiny gold bag s" d/day07.txt" (parta) . cr ;

: (partb) ( bag a u -- n )
  over-lines
    parse-rule drop
  done-lines
  #bags-contained ;

: partb ( -- ) s" d/day07.txt" (partb) . cr ;
