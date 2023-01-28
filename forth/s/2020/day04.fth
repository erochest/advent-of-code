s" ../files.fth" included
s" ../debug.fth" included
s" ../stack.fth" included
s" ../paragraphs.fth" included
s" ../strings.fth" included

struct
    cell% field passport-birth-year
    cell% field passport-issue-year
    cell% field passport-expiration-year
    double% field passport-height
    double% field passport-hair-color
    double% field passport-eye-color
    double% field passport-id
    cell% field passport-country-id
end-struct passport%

: is-field? ( ka kn fa fn -- ka kn f ) 2over str= ;
: parse-number-value ( a n -- s ) s>number? drop d>s ;
: string-value ( va vn p ka kn -- va vn p ) 2drop -rot -trailing rot ;
: number-value ( va vn p ka kn -- s p ) 2drop -rot parse-number-value swap ;

: set-passport-field ( passport a n -- )
    [char] : split-on ( p ka kn va vn )
    2swap 4 roll -rot ( va vn p ka kn )
    s" hcl" is-field? if ( va vn p ka kn ) string-value passport-hair-color 2!
    else s" iyr" is-field? if number-value passport-issue-year !
    else s" eyr" is-field? if number-value passport-expiration-year !
    else s" ecl" is-field? if string-value passport-eye-color 2!
    else s" cid" is-field? if number-value passport-country-id !
    else s" pid" is-field? if string-value passport-id 2!
    else s" byr" is-field? if number-value passport-birth-year !
    else s" hgt" is-field? if string-value passport-height 2!
    else true abort" unrecognized field"
    then then then then then then then then
    ;

: parse-passport ( a n -- p )
    passport% %allot ( a n p )
    -rot 2dup + -rot ( p limit a n )
    begin
        32 split-on 2swap ( p limit a1 n1 a0 n0 )
        over 5 pick < ( p limit a1 n1 a0 n0 f )
    while ( p limit a1 n1 a0 n0 )
        5 pick -rot set-passport-field ( p limit a1 n1 )
    repeat
    2drop 2drop drop
    ;

: str-empty? ( a n -- f ) 0= nip ;

: passport-is-valid? ( p -- f )
    dup passport-birth-year @ 0<>
    over passport-issue-year @ 0<> and
    over passport-expiration-year @ 0<> and
    over passport-height 2@ str-empty? invert and
    over passport-hair-color 2@ str-empty? invert and
    over passport-eye-color 2@ str-empty? invert and
    over passport-id 2@ str-empty? invert and
    nip
    ;

: (parta) ( a n -- n )
    open-input ( )
    0 ( c )
    begin ( c )
        read-paragraph ( c n )
        dup 0<> while ( c n )
        paragraph-buffer swap parse-passport ( c p )
        passport-is-valid? if
            1+ ( c1 )
        then
    repeat ( c n )
    close-input ( c n )
    drop ( c )
    ;

: parta ( -- ) s" ../data/2020/day04.txt" (parta) . cr ;

: between ( a b c -- f )
    \ Returns true if a >= b and a <= c.
    2 pick >= -rot >= and ;

: valid-birth-year? ( n -- f ) 1920 2002 between ;
: valid-issue-year? ( n -- f ) 2010 2020 between ;
: valid-expiration-year? ( n -- f ) 2020 2030 between ;

: valid-height? ( a n -- f )
    0. 2swap >number ( d a n )
    2dup s" in" str= if 2drop d>s 59 76 between
    else s" cm" str= if d>s 150 193 between
    else 2drop false
    then then ;

: valid-hair-color? ( a n -- f )
    dup 0= if 2drop false
    else over c@ [char] # = if
        1 /string
        true -rot bounds u+do
            i c@ [char] 0 [char] 9 between
            i c@ [char] a [char] f between or
            and
        loop
    else 2drop false
    then then
    ;

: valid-eye-color? ( a n -- f )
    { a n }
    false
    a n s" amb" str= or
    a n s" blu" str= or
    a n s" brn" str= or
    a n s" gry" str= or
    a n s" grn" str= or
    a n s" hzl" str= or
    a n s" oth" str= or ;

: valid-id? ( a n -- f )
    dup 9 = if
        true -rot bounds u+do
            i c@ [char] 0 [char] 9 between and
        loop
    else 2drop false
    then
    ;

: passport-is-valid-b? ( p -- f )
    dup passport-birth-year @ valid-birth-year?
    over passport-issue-year @ valid-issue-year? and
    over passport-expiration-year @ valid-expiration-year? and
    over passport-height 2@ valid-height? and
    over passport-hair-color 2@ valid-hair-color? and
    over passport-eye-color 2@ valid-eye-color? and
    over passport-id 2@ valid-id? and
    nip
    ;

: (partb) ( a n -- n )
    open-input ( )
    0 ( c )
    begin ( c )
        read-paragraph ( c n )
        dup 0<> while ( c n )
        paragraph-buffer swap parse-passport ( c p )
        passport-is-valid-b? if
            1+ ( c1 )
        then
    repeat ( c n )
    close-input ( c n )
    drop ( c )
    ;

: partb ( -- ) s" ../data/2020/day04.txt" (partb) . cr ;
