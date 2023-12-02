! Copyright (C) 2023 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: accessors advent.io arrays tools.test advent.y2023.day02 ;
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

{ H{ { "red" 3 } } }
[
    H{ }
    T{ color-grab f 3 "red" } 
    merge-count
] unit-test

{ H{ { "red" 3 } { "green" 13 } } }
[ H{ { "red" 3 } } T{ color-grab f 13 "green" } merge-count ]
unit-test

{ H{ { "green" 13 } } }
[ H{ { "green" 13 } } 4 "green" <color-grab> merge-count ]
unit-test

{ H{ { "green" 13 } { "red" 3 } { "pink" 4 } } }
[
    13 "green" <color-grab>
    3 "red" <color-grab>
    4 "pink" <color-grab> 
    3array
    H{ }
    collect-max-counts
] unit-test

{ H{ { "green" 13 } { "blue" 6 } { "red" 20 } } }
[
    "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
    >game grabs>>
    collect-game-max-counts
] unit-test

{ 48 } [ H{ { "green" 2 } { "blue" 6 } { "red" 4 } } power-cubes ] unit-test

{ 8 } [ 2023 2 fixture sum-possible-ids ] unit-test

{ 2286 } [ 2023 2 fixture sum-power-cubes ] unit-test
{ 56322 } [ 2023 2 data-file sum-power-cubes ] unit-test

