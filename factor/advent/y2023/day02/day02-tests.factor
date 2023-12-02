! Copyright (C) 2023 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: advent.io tools.test advent.y2023.day02 ;
IN: advent.y2023.day02.tests

{ T{ color-grab f 7 "green" } }
[ "7 green" >color-grab ]
unit-test

{ f } [ "notcolor" >color-grab ] unit-test

{ {
    T{ color-grab f 3 "red" }
    T{ color-grab f 4 "yellow" }
    T{ color-grab f 5 "green" }
} }
[ "3 red, 4 yellow, 5 green" >color-grab-list ] unit-test

{
    T{ game f 42 {
        { T{ color-grab f 3 "red" } T{ color-grab f 4 "yellow" }
          T{ color-grab f 5 "blue" } }
        { T{ color-grab f 15 "pink" } }
        { T{ color-grab f 1 "green" } T{ color-grab f 4 "yellow" }
        }
        }
    }
}
[
    "Game 42: 3 red, 4 yellow, 5 blue; 15 pink; 1 green, 4 yellow"
    >game
] unit-test

{ t }
[
    H{ { "red" 12 } { "yellow" 13 } { "blue" 14 } }
    { T{ color-grab f 3 "red" } T{ color-grab f 4 "yellow" }
      T{ color-grab f 5 "blue" } }
    possible-given?
] unit-test
{ f }
[
    H{ { "red" 12 } { "green" 13 } { "blue" 14 } }
    { T{ color-grab f 3 "red" } T{ color-grab f 4 "yellow" }
      T{ color-grab f 15 "blue" } }
    possible-given?
] unit-test
{ f }
[
    H{ { "red" 12 } { "green" 13 } { "blue" 14 } }
    { T{ color-grab f 3 "red" } T{ color-grab f 4 "pink" }
      T{ color-grab f 15 "blue" } }
    possible-given?
] unit-test

{ 8 } [
    H{ { "red" 12 } { "green" 13 } { "blue" 14 } }
    2023 2 fixture
    sum-possible-ids
] unit-test
