
s" files.fth" included
s" debug.fth" included
s" stack.fth" included

\ 
\ Some words defining the password-info structure.
\ 

struct
  cell% field password-policy-low
  cell% field password-policy-high
  char% field password-policy-char
  double% field password-policy-password
end-struct password-policy%

: parse>n ( addr1 n1 -- n addr2 n2 )
  \ Parses a single from a string starting at addr1 n1. Returns the single and the rest of the string.
  0. 2swap >number 2swap d>s -rot ;

: skip-prefix ( addr0 n0 addr1 n1 -- addr2 n2 )
  \ Skips forward in addr0 to remove the prefix in addr1, after asserting that it is there.
  2over 2over assert( string-prefix? ) nip /string ;

: >password-policy ( addr n -- addr )
  \ Parses and input string into a password-policy%.
  parse>n
  s" -" skip-prefix
  parse>n
  1 /string
  over c@ -rot 1 /string
  s" : " skip-prefix
  password-policy% %allot
  dup 2swap rot password-policy-password 2!
  tuck password-policy-char c!
  tuck password-policy-high !
  tuck password-policy-low !
  ;

: count-char ( a n c -- n )
  \ count the number of times c appears in the string at a n.
  \ s" count-char" debug
  { target }
  0 -rot
  bounds u+do
    i c@ target = if 1+ then
  loop ;

: count-passward-chars ( a -- n )
  \ count the number of times the char appears in the password
  dup password-policy-password 2@ rot password-policy-char @ count-char ;

: count-above-low? ( a n -- f )
  \ is the count above the low?
  swap password-policy-low @ >= ;

: count-below-high? ( a n -- f )
  \ is the count below the high?
  swap password-policy-high @ <= ;

: is-valid-a? ( addr -- flag )
  \ is the password valid according to the policy?
  dup count-passward-chars
  2dup count-above-low?
  -rot count-below-high?
  and ;

: day02a ( addr n -- n )
  \ Read the input and return how many passwords are valid according to their policies?
  open-input
  0 begin           ( n )
    read-input-line
  while             ( u-size f )
    line-buffer swap >password-policy
    is-valid-a? if    ( addr )
      1+
    then
  repeat
  close-input
  drop
  ;

: debug-policy-line ( a n -- )
  { line-addr line-len }
  line-addr line-len >password-policy dup
  is-valid-a? if
    line-addr line-len fd-out write-line throw
    count-passward-chars s>d <# #s #> fd-out write-line throw
    s" " fd-out write-line throw
  else
    drop
  then
  ;

: day02a-debug ( addr n -- )
  s" t/day02a.txt" open-output
  open-input
  begin           ( n )
    read-input-line
  while             ( u-size f )
    line-buffer swap debug-policy-line
  repeat
  close-input
  close-output
  drop
  ;

: char[] ( a n index -- c )
  tuck assert( >= )
  + c@ ;

: policy-low-char ( policy -- c )
  dup password-policy-password 2@
  rot password-policy-low @ 1-
  char[] ;

: policy-high-char ( policy -- c )
  dup password-policy-password 2@
  rot password-policy-high @ 1-
  char[] ;

: is-policy-char? ( c policy -- f ) password-policy-char @ = ;

: is-valid-b? ( password-policy -- f )
  { policy }
  policy policy-low-char policy is-policy-char?
  policy policy-high-char policy is-policy-char?
  xor
  ;

: day02b ( addr n -- n )
  \ Read the input and return how many passwords are valid according to their policies?
  open-input
  0 begin           ( n )
    read-input-line
  while             ( u-size f )
    line-buffer swap >password-policy
    is-valid-b? if    ( addr )
      1+
    then
  repeat
  close-input
  drop
  ;

\ 372 is too low


