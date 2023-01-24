: fixture s" f/day04.txt" open-input ;

: test-is-end-of-paragraph?
    \ empty line (end of paragraph)
    true 0 assert( is-end-of-paragraph? )
    \ end of file
    false 0 assert( is-end-of-paragraph? )
    \ not end of paragraph
    true 42 assert( is-end-of-paragraph? invert )
    ;

: test-read-paragraph
    fixture
    paragraph-buffer
    read-paragraph
    s" ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm "
    assert( str= )
    close-input
    \ s" test-read-paragraph" debug
    ;

: test-read-paragraph-file
    fixture

    paragraph-buffer read-paragraph
    s" ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm "
    assert( str= )

    paragraph-buffer read-paragraph
    s" iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884 hcl:#cfa07d byr:1929 "
    assert( str= )

    paragraph-buffer read-paragraph
    s" hcl:#ae17e1 iyr:2013 eyr:2024 ecl:brn pid:760753108 byr:1931 hgt:179cm "
    assert( str= )

    paragraph-buffer read-paragraph
    s" hcl:#cfa07d eyr:2025 pid:166559648 iyr:2011 ecl:brn hgt:59in "
    assert( str= )

    read-paragraph assert( 0= )

    close-input
    \ s" test-read-paragraph-file" debug
    ;

: test-all
    test-is-end-of-paragraph?
    test-read-paragraph
    test-read-paragraph-file
    ;