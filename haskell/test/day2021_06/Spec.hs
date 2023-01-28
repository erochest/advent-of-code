{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.Vector as V

import Test.Tasty
import Test.Tasty.HUnit

import Advent
import Advent.Day2021_06

import qualified Data.List as L
import Debug.Trace
import Test.Tasty (testGroup)
import Test.Tasty.HUnit (testCase)
import Advent.Day2021_06 (LanternFishSchool(LanternFishSchool, getSchool), lanternFishSchoolDay, newLanternFishSchool)

main :: IO ()
main = defaultMain suite

fixture :: LanternFishSchool
fixture = LanternFishSchool [0, 1, 1, 2, 1, 0, 0, 0, 0]
-- [3,4,3,1,2]

suite :: TestTree
suite = testGroup "2021, day 6"
  [ testGroup "part 1"
    [ testGroup "partOne"
      [ testCase "calculates the checksum for the fixture" $
        partOne fixture @?= 5934
      ]
    , testGroup "lanternFishSchoolDay"
      [ testCase "decreases the day until spawning" $ do
        let input    = LanternFishSchool [0, 4, 3, 2, 1, 0, 0, 0, 0]
            expected = LanternFishSchool [4, 3, 2, 1, 0, 0, 0, 0, 0]
        expected @=? lanternFishSchoolDay input undefined
      , testCase "resets the cycle day after spawning" $ do
        let input = LanternFishSchool [7, 4, 3, 2, 1, 0, 0, 0, 0]
        getSchool (lanternFishSchoolDay input undefined) V.! 6 @?= 7
      , testCase "includes aging new-born fish" $ do
        let input = LanternFishSchool [0, 4, 3, 2, 1, 0, 0, 9, 0]
        getSchool (lanternFishSchoolDay input undefined) V.! 6 @?= 9
      , testCase "adds aging new-born fish with resetting cycles" $ do
        let input = LanternFishSchool [7, 4, 3, 2, 1, 0, 0, 9, 0]
        getSchool (lanternFishSchoolDay input undefined) V.! 6 @?= 16
      , testCase "adds new fish at newborn cycle day when spawning" $ do
        let input = LanternFishSchool [7, 4, 3, 2, 1, 0, 0, 0, 0]
        getSchool (lanternFishSchoolDay input undefined) V.! 8 @?= 7
      ]
    , testGroup "newLanternFishSchool"
      [ testCase "creates a fish with the right number of spaces" $ do
        V.length (getSchool newLanternFishSchool) @?= 9
      , testCase "initializes the population to 0" $ do
        V.all (==0) (getSchool newLanternFishSchool) @? "all phases have population of zero"
      ]
    , testGroup "addLanternFish"
      [ testCase "adds a fish with the right phase to the school" $ do
        let expected = LanternFishSchool [0, 0, 0, 1, 0, 0, 0, 0, 0]
        addLanternFish newLanternFishSchool [3] @?= expected
      , testCase "adds all fish with the right phases to the school" $ do
        addLanternFish newLanternFishSchool [3,4,3,1,2] @?= fixture
      ]
    , testGroup "Parseable.parse"
      [ testCase "parses a list of lantern fish" $ do
        parse "3,4,3,1,2" @?= Just fixture
      ]
    ]
  , testGroup "part 2"
     [ testGroup "partTwo"
      [ testCase "calculates the checksum for the fixture" $
        partTwo fixture @?= 26984457539
      ]
     ]
  ]