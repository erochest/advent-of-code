{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Test.Tasty
import Test.Tasty.HUnit

import Advent
import Advent.Day7
import Advent.Day7 (findFuelCosts, Crab (Crab))

main :: IO ()
main = defaultMain suite

fixture :: [Crab]
fixture = map Crab [16,1,2,0,4,2,7,1,2,14]

suite :: TestTree
suite = testGroup "day 7"
  [ testGroup "part 1"
    [ testGroup "partOne"
      [ testCase "calculates the checksum for the fixture" $
        partOne fixture @?= 37
      ]
    , testGroup "findDistances"
      [ testCase "compares distances for every position" $ do
        let expected = [0..17]
        expected @=? map fst (findDistances 0 16 fixture)
      , testCase "finds the total distance for each position" $ do
        let actual = findDistances 0 16 fixture
        (2, 37) `elem` actual @? "has the distance for position 2"
        (10, 71) `elem` actual @? "has the distance for position 10"
        (16, 111) `elem` actual @? "has the distance for position 16"
      ]
      , testGroup "Parseable.parse"
        [ testCase "parses input" $
          Just fixture @=? parse "16,1,2,0,4,2,7,1,2,14"
        ]
    ]
  , testGroup "part 2"
     [ testGroup "partTwo"
      [ testCase "calculates the checksum for the fixture" $
        partTwo fixture @?= 168
      ]
    , testGroup "findFuelCosts"
      [ testCase "calculates the increasing costs between two points" $
        findFuelCosts 5 5 [Crab 16] @?= [(5, 66)]
      ]
    , testGroup "fuelCost"
      [ testCase "calculates the cost between the same square" $
        fuelCost 4 4 @?= 0
      , testCase "calculates the cost between adjacent squares" $ do
        fuelCost 4 5 @?= 1
        fuelCost 5 4 @?= 1
      , testCase "calculates the increasing cost between non-adjacent squares" $ do
        fuelCost 4 6 @?= 3
        fuelCost 4 7 @?= 6
        fuelCost 7 4 @?= 6
        fuelCost 16 5 @?= 66
        fuelCost 2 5 @?= 6
        fuelCost 1 5 @?= 10
        fuelCost 5 14 @?= 45
      ]
     ]
  ]