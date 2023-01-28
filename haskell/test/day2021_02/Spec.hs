{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe (mapMaybe)

import Test.Tasty
import Test.Tasty.HUnit

import Data.Attoparsec.ByteString.Char8 (parseOnly)

import Advent
import Advent.Day2021_02

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite = testGroup "2021, day2"
    [ testGroup "movement"
      [ testCase "parses directions" $ do
        Right (Movement Forward 2) @=? parseOnly movement "forward 2"
        Right (Movement Up 42) @=? parseOnly movement "up 42"
        Right (Movement Down 13) @=? parseOnly movement "down 13"
      ]
    , testGroup "parseMovement"
      [ testCase "parses directions" $ do
        Just (Movement Forward 7) @=? parse "forward 7"
        Just (Movement Up 13) @=? parse "up 13"
        Just (Movement Down 19) @=? parse "down 19"
      , testCase "parses a list" $
        [ Movement Forward 5
        , Movement Down 5
        , Movement Forward 8
        , Movement Up 3
        , Movement Down 8
        , Movement Forward 2 ] @=? mapMaybe parse [ "forward 5", "down 5", "forward 8"
                                                  , "up 3", "down 8", "forward 2"
                                                  ]
      ]
    , testGroup "move1"
      [ testCase "moves forward" $
        (13, 7) @=? move1 (11, 7) (Movement Forward 2)
      , testCase "moves up" $
        (11, 5) @=? move1 (11, 7) (Movement Up 2)
      , testCase "move1 down" $
        (11, 9) @=? move1 (11, 7) (Movement Down 2)
      ]
    , testGroup "move2"
      [ testCase "moves forward" $
        Sub 15 60 10 @=? move2 (Sub 13 40 10) (Movement Forward 2)
      , testCase "moves up" $
        Sub 13 40 2 @=? move2 (Sub 13 40 5) (Movement Up 3)
      , testCase "moves down" $
        Sub 13 40 10 @=? move2 (Sub 13 40 2) (Movement Down 8)
      ]
    , testGroup "subChecksum"
      [ testCase "computes checksum" $
        900 @=? subChecksum (Sub 15 60 20)
      ]
    , testGroup "partTwo"
      [ testCase "does everything" $
        900 @=? partTwo [ Movement Forward 5
                        , Movement Down 5
                        , Movement Forward 8
                        , Movement Up 3
                        , Movement Down 8
                        , Movement Forward 2
                        ]
      ]
    ]

