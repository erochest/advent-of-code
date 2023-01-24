
: test-skip-prefix
  s" something" s" some" skip-prefix
  assert( s" thing" compare 0= )
  \ s" test-skip-prefix " debug
  ;

: test-parse>n
  s" 42-52" parse>n
  assert( s" -52" compare 0= )
  assert( 42 = )
  \ s" test-parse>n " debug
  ;

: test->password-policy
  s" 1-3 a: abcde" >password-policy
  assert( dup password-policy-low @ 1 = )
  assert( dup password-policy-high @ 3 = )
  assert( dup password-policy-char c@ [char] a = )
  assert( password-policy-password 2@ s" abcde" compare 0= )
  \ s" test->password-policy " debug
  ;

: test-third
  1 2 3 third
  assert( 1 = )
  assert( 3 = )
  assert( 2 = )
  assert( 1 = )
  \ s" test-third " debug
  ;

: test-count-char
  s" abcde" [char] a count-char assert( 1 = )
  s" cdefg" [char] b count-char assert( 0= )
  s" ccccccc" [char] c count-char assert( 7 = )
  \ s" test-count-char " debug
  ;

: test-is-valid-a?
  s" 1-3 a: abcde"   >password-policy assert( is-valid-a? )
  s" 1-3 b: cdefg"   >password-policy assert( is-valid-a? invert )
  s" 2-9 c: ccccccc" >password-policy assert( is-valid-a? )
  \ s" test-is-valid?" debug
  ;

: test-day02a
  s" f/day02.txt" day02a
  assert( 2 = )
  \ s" test-day02a" debug
  ;

: test-ad-hoc
  s" 2-4 r: rxmrd" >password-policy
  dup password-policy-char @ assert( [char] r =  )
  dup password-policy-low @ assert( 2 = )
  dup password-policy-high @ assert( 4 = )
  dup password-policy-password 2@ assert( s" rxmrd" compare 0= )
  dup password-policy-password 2@ rot password-policy-char @ count-char assert( 2 = )

  s" 7-15 h: thqthdphmbhhjphhh" >password-policy
  dup password-policy-char @ assert( [char] h =  )
  dup password-policy-low @ assert( 7 = )
  dup password-policy-high @ assert( 15 = )
  dup password-policy-password 2@ assert( s" thqthdphmbhhjphhh" compare 0= )
  dup password-policy-password 2@ rot password-policy-char @ count-char assert( 8 = )
  ;

: test-char[]
  s" abcde" 0 char[] assert( [char] a = )
  s" abcde" 2 char[] assert( [char] c = )
  ;

: test-policy-low-char
  s" 1-3 a: abcde" >password-policy policy-low-char assert( [char] a = )
  s" 1-3 b: cdefg" >password-policy policy-low-char assert( [char] c = )
  ;

: test-policy-high-char
  s" 1-3 a: abcde" >password-policy policy-high-char assert( [char] c = )
  s" 1-3 b: cdefg" >password-policy policy-high-char assert( [char] e = )
  ;

: test-is-policy-char?
  [char] a s" 1-3 a: abcde" >password-policy assert( is-policy-char? )
  [char] b s" 1-3 b: cdefg"   >password-policy assert( is-policy-char? )
  [char] z s" 2-9 c: ccccccc" >password-policy assert( is-policy-char? invert )
  ;

: test-is-valid-b?
  s" 1-3 a: abcde" >password-policy assert( is-valid-b? )
  s" 1-3 b: cdefg" >password-policy assert( is-valid-b? invert )
  s" 2-9 c: ccccccccc" >password-policy assert( is-valid-b? invert )
  s" 2-9 c: cbccccccc" >password-policy assert( is-valid-b? )
  ;

: test-day02b
  s" f/day02.txt" day02b
  assert( 1 = )
  \ s" test-day02a" debug
  ;

test-skip-prefix
test-parse>n
test->password-policy
test-third
test-count-char
test-is-valid-a?
test-day02a
test-ad-hoc

test-char[]
test-policy-low-char
test-policy-high-char
test-is-policy-char?
test-is-valid-b?
test-day02b
