
: fixture s" ../sample/2020/day04.txt" open-input ;

: test-skip-space
    s" a   something" 1 /string
    dup assert( 12 = )
    skip-space
    dup assert( 9 = )
    assert( s" something" str= )

    s" else"
    skip-space
    assert( s" else" str= )

    \ s" test-skip-space" debug
    ;

: test-read-to
    s" somthing" 32 read-to
    dup assert( 0= )
    assert( s" " str= )

    s" something else" 32 read-to
    assert( s" else" str= )

    \ s" test-read-to" debug
    ;

: test-split-on
    s" key:value" [char] : split-on

    assert( s" value" str= )
    assert( s" key" str= )

    s" key-only" [char] : split-on

    assert( s" " str= )
    assert( s" key-only" str= )

    \ s" test-split-on" debug
    ;

: test-set-passport-field
    passport% %allot

    dup s" hcl:#ae17e1" set-passport-field assert( dup passport-hair-color 2@ s" #ae17e1" str= )
    dup s" iyr:2013" set-passport-field assert( dup passport-issue-year @ 2013 = )
    dup s" eyr:2024" set-passport-field assert( dup passport-expiration-year @ 2024 = )
    dup s" ecl:brn" set-passport-field assert( dup passport-eye-color 2@ s" brn" str= )
    dup s" cid:174" set-passport-field assert( dup passport-country-id @ 174 = )
    dup s" pid:760753108" set-passport-field assert( dup passport-id 2@ s" 760753108" str= )
    dup s" byr:1931" set-passport-field assert( dup passport-birth-year @ 1931 = )
    dup s" hgt:179cm" set-passport-field assert( dup passport-height 2@ s" 179cm" str= )
    dup s" hgt:179cm " set-passport-field assert( dup passport-height 2@ s" 179cm" str= )

    drop \ drop the passport
    \ s" test-set-passport-field" debug
    ;

: test-parse-passport
    s" hcl:#ae17e1 iyr:2013 eyr:2024 ecl:brn cid:174 pid:760753108 byr:1931 hgt:179cm "
    parse-passport
    dup passport-birth-year @ assert( 1931 = )
    dup passport-issue-year @ assert( 2013 = )
    dup passport-expiration-year @ assert( 2024 = )
    dup passport-height 2@ assert( s" 179cm" str= )
    dup passport-hair-color 2@ assert( s" #ae17e1" str= )
    dup passport-eye-color 2@ assert( s" brn" str= )
    dup passport-id 2@ assert( s" 760753108" str= )
    dup passport-country-id @ assert( 174 = )
    drop
    s" hcl:#cfa07d eyr:2025 pid:166559648 iyr:2011 hgt:59in "
    parse-passport
    dup passport-birth-year @ assert( 0= )
    dup passport-eye-color 2@ assert( s" " str= )
    drop
    \ s" test-parse-passport" debug
    ;

: test-passport-is-valid?
    s" ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm "
    parse-passport assert( passport-is-valid? )

    s" iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884 hcl:#cfa07d byr:1929 "
    parse-passport assert( passport-is-valid? 0= )

    s" hcl:#ae17e1 iyr:2013 eyr:2024 ecl:brn pid:760753108 byr:1931 hgt:179cm "
    parse-passport assert( passport-is-valid? )

    s" hcl:#cfa07d eyr:2025 pid:166559648 iyr:2011 ecl:brn hgt:59in "
    parse-passport assert( passport-is-valid? 0= )

    \ s" test-passport-is-valid?" debug
    ;

: test-(parta)
    s" ../sample/2020/day04.txt" (parta)
    assert( 2 = )
    \ s" test-(parta)" debug
    ;

: test-(parta)-data s" ../data/2020/day04.txt" (parta) assert( 247 = ) ;

: test-valid-birth-year?
    2002 assert( valid-birth-year? )
    2003 assert( valid-birth-year? 0= )
    \ s" test-valid-birth-year?" debug
    ;

: test-valid-issue-year?
    2019 assert( valid-issue-year? )
    2009 assert( valid-issue-year? 0= )
    2021 assert( valid-issue-year? 0= )
    \ s" test-valid-issue-year?" debug
    ;

: test-valid-expiration-year?
    2019 assert( valid-expiration-year? 0= )
    2021 assert( valid-expiration-year? )
    2033 assert( valid-expiration-year? 0= )
    \ s" test-valid-exiration-date?" debug
    ;

: test-valid-height?
    s" 60in" assert( valid-height? )
    s" 190cm" assert( valid-height? )
    s" 190in" assert( valid-height? 0= )
    s" 190" assert( valid-height? 0= )
    \ s" valid-height?" debug
    ;

: test-valid-hair-color?
    s" #123abc" assert( valid-hair-color? )
    s" #123abz" assert( valid-hair-color? 0= )
    s" 123abc" assert( valid-hair-color? 0= )
    0 0 assert( valid-hair-color? 0= )
    \ s" test-valid-hair-color?" debug
    ;

: test-valid-eye-color?
    s" brn" assert( valid-eye-color? )
    s" wat" assert( valid-eye-color? 0= )
    \ s" test-valid-eye-color?" debug
    ;

: test-valid-id?
    s" 000000001" assert( valid-id? )
    s" 0123456789" assert( valid-id? 0= )
    \ s" test-valid-id?" debug
    ;

: test-passport-is-valid-b?
    s" eyr:1972 cid:100 hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926 "
    parse-passport assert( passport-is-valid-b? 0= )

    s" iyr:2019 hcl:#602927 eyr:1967 hgt:170cm ecl:grn pid:012533040 byr:1946 "
    parse-passport assert( passport-is-valid-b? 0= )

    s" hcl:dab227 iyr:2012 ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277 "
    parse-passport assert( passport-is-valid-b? 0= )

    s" hgt:59cm ecl:zzz eyr:2038 hcl:74454a iyr:2023 pid:3556412378 byr:2007 "
    parse-passport assert( passport-is-valid-b? 0= )

    s" pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980 hcl:#623a2f "
    parse-passport assert( passport-is-valid-b? )

    s" eyr:2029 ecl:blu cid:129 byr:1989 iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm "
    parse-passport assert( passport-is-valid-b? )

    s" hcl:#888785 hgt:164cm byr:2001 iyr:2015 cid:88 pid:545766238 ecl:hzl eyr:2022 "
    parse-passport assert( passport-is-valid-b? )

    s" iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719 "
    parse-passport assert( passport-is-valid-b? )

    \ s" test-passport-is-valid-b?" debug
    ;

: test-(partb) s" ../sample/2020/day04b.txt" (partb) assert( 4 = ) ;

: test-(partb)-data s" ../data/2020/day04.txt" (partb) assert( 145 = ) ;

: test-all
    test-set-passport-field
    test-parse-passport
    test-passport-is-valid?
    test-(parta)
    test-(parta)-data
    test-valid-birth-year?
    test-valid-issue-year?
    test-valid-expiration-year?
    test-valid-height?
    test-valid-hair-color?
    test-valid-eye-color?
    test-valid-id?
    test-passport-is-valid-b?
    test-(partb)
    test-(partb)-data
    ;