! Copyright (C) 2023 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: tools.test advent.io advent.y2023.day03 ;
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

{ 4361 } [ 2023 3 fixture sum-part-numbers ] unit-test
