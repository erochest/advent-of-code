! Copyright (C) 2023 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: tools.test advent.io advent.y2023.day03 kernel sorting ;
IN: advent.y2023.day03.tests

{
    {
        T{ number-region f 467 { { 0 0 } { 1 0 } { 2 0 } } }
        T{ number-region f 114 { { 5 0 } { 6 0 } { 7 0 } } }
        T{ number-region f  35 { { 2 2 } { 3 2 } } }
        T{ number-region f 633 { { 6 2 } { 7 2 } { 8 2 } } }
        T{ number-region f 617 { { 0 4 } { 1 4 } { 2 4 } } }
        T{ number-region f  58 { { 7 5 } { 8 5 } } }
        T{ number-region f 592 { { 2 6 } { 3 6 } { 4 6 } } }
        T{ number-region f 755 { { 6 7 } { 7 7 } { 8 7 } } }
        T{ number-region f 664 { { 1 9 } { 2 9 } { 3 9 } } }
        T{ number-region f 598 { { 5 9 } { 6 9 } { 7 9 } } }
    }
} [ 2023 3 fixture (file-lines) find-numbers ] unit-test

{
    {
        { 1 1 } { 2 1 } { 3 1 } { 4 1 }
        { 1 2 } { 4 2 }
        { 1 3 } { 2 3 } { 3 3 } { 4 3 }
    }
}
[ 35 { { 2 2 } { 3 2 } } <number-region> get-surrounding ]
unit-test

{ V{ { 1 4 } } }
[
    V{ } clone
    dup 1 4 append-point!
] unit-test

{ V{ { 1 2 } { 2 2 } { 3 2 } { 4 2 } } }
[
    V{ } clone
    2 over { 1 0 } { 4 4 } (rectangle-seq)
] unit-test

{
    {
        { 0 0 } { 0 1 } { 0 2 } { 0 3 }
        { 1 0 } { 1 1 } { 1 2 } { 1 3 }
        { 2 0 } { 2 1 } { 2 2 } { 2 3 }
        { 3 0 } { 3 1 } { 3 2 } { 3 3 }
        { 4 0 } { 4 1 } { 4 2 } { 4 3 }
    }
}
[ { 0 0 } { 4 3 } rectangle-seq sort ] unit-test

! { 4361 } [ 2023 3 fixture sum-part-numbers ] unit-test
