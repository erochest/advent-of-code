{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy.Char8 as C
import Data.Monoid

import Test.Tasty
import Test.Tasty.HUnit

import Advent
import Advent.Day2021_03

main :: IO ()
main = defaultMain suite

testParseBinary :: C.ByteString -> Int -> Assertion
testParseBinary input expected = parseBinary input @?= Just expected

testParse :: C.ByteString -> BitCountList -> Assertion
testParse input expected = parse input @?= Just expected

fixture :: [BitCountList]
fixture = [ BCL [bc' 0, bc' 0, bc' 1, bc' 0, bc' 0]
          , BCL [bc' 1, bc' 1, bc' 1, bc' 1, bc' 0]
          , BCL [bc' 1, bc' 0, bc' 1, bc' 1, bc' 0]
          , BCL [bc' 1, bc' 0, bc' 1, bc' 1, bc' 1]
          , BCL [bc' 1, bc' 0, bc' 1, bc' 0, bc' 1]
          , BCL [bc' 0, bc' 1, bc' 1, bc' 1, bc' 1]
          , BCL [bc' 0, bc' 0, bc' 1, bc' 1, bc' 1]
          , BCL [bc' 1, bc' 1, bc' 1, bc' 0, bc' 0]
          , BCL [bc' 1, bc' 0, bc' 0, bc' 0, bc' 0]
          , BCL [bc' 1, bc' 1, bc' 0, bc' 0, bc' 1]
          , BCL [bc' 0, bc' 0, bc' 0, bc' 1, bc' 0]
          , BCL [bc' 0, bc' 1, bc' 0, bc' 1, bc' 0]
          ]

suite :: TestTree
suite = testGroup "2021, day 3"
    [ testGroup "partOne"
      [ testCase "produces the correct response on the fixture data" $ do
        198 @=? partOne fixture
      ]
    , testGroup "epsilon"
      [ testCase "returns the least common responses" $
        9 @=? epsilon (BCL [bc 5 7, bc 7 5, bc 4 8, bc 5 7, bc 7 5])
      , testCase "bases on the most common bit" $ do
        0 @=? epsilon (BCL [bc 3 9])
        1 @=? epsilon (BCL [bc 9 3])
      ]
    , testGroup "gamma"
      [ testCase "returns the most common responses" $
        22 @=? gamma (BCL [bc 5 7, bc 7 5, bc 4 8, bc 5 7, bc 7 5])
      , testCase "bases on the most common bit" $ do
        1 @=? gamma (BCL [bc 3 9])
        0 @=? gamma (BCL [bc 9 3])
      ]
    , testGroup "parse"
      [ testCase "parses 0" $ testParse "0" $ BCL [bc 1 0]
      , testCase "parses 1" $ testParse "1" $ BCL [bc 0 1]
      , testCase "parses sequence" $ testParse "1010" $ BCL [ bc 0 1, bc 1 0, bc 0 1, bc 1 0 ]
      ]
    , testGroup "parseBitCount"
      [ testCase "parses 0" $ parseBitCount '0' @?= Just (bc 1 0)
      , testCase "parses 1" $ parseBitCount '1' @?= Just (bc 0 1)
      , testCase "errors on anything else" $ parseBitCount 'a' @?= Nothing
      ]
    , testGroup "Semigroup"
      [ testCase "sums pairs of BitCounts" $ do
        let left     = BCL [bc 0 1, bc 1 0, bc 4 7]
            right    = BCL [bc 2 1, bc 7 7, bc 8 8]
            expected = BCL [bc 2 2, bc 8 7, bc 12 15]
        expected @=? left <> right
      , testCase "right-aligns lists of different lengths" $ do
        let left     = BCL [bc 7 4]
            right    = BCL [bc 1 2, bc 3 4, bc 5 6]
            expected = BCL [bc 1 2, bc 3 4, bc 12 10]
        expected @=? left <> right
      , testCase "passes through on empty lists" $ do
        let left     = BCL [bc 1 2, bc 3 4, bc 5 6]
            right    = BCL []
            expected = BCL [bc 1 2, bc 3 4, bc 5 6]
        expected @=? left <> right
      , testCase "passes through when combined with mempty" $ do
        let left     = BCL [bc 1 2, bc 3 4, bc 5 6]
        left @=? left <> mempty
      ]
    , testGroup "padLeft"
      [ testCase "leaves alone if the same length" $ do
        let left  = [Sum 1, Sum 2]
        let right = [Sum 3, Sum 4]
        (left, right) @=? padLeft left right
      , testCase "pads the first argument if it's short" $ do
        let left  = [Sum 1, Sum 2]
        let right = [Sum 3, Sum 4, Sum 5]
        (Sum 0:left, right) @=? padLeft left right
      , testCase "pads the second argument if it's short" $ do
        let left  = [Sum (-1), Sum 1, Sum 2]
        let right = [Sum 4, Sum 5]
        (left, Sum 0:right) @=? padLeft left right
      , testCase "handles empty lists" $ do
        let left  = [Sum (-1), Sum 1, Sum 2]
        let right = []
        (left, [Sum 0, Sum 0, Sum 0]) @=? padLeft left right
      ]
    , testGroup "parseBinary"
      [ testCase "parses binary numbers" $ do
        testParseBinary "00000" 0
        testParseBinary "00100" 4
      ]
    , testGroup "binary"
      [ testCase "parses 0" $ binary 0 '0' @?= 0
      , testCase "parses 1" $ binary 0 '1' @?= 1
      , testCase "shifts existing left" $ do
        binary 1 '0' @?= 2
        binary 1 '1' @?= 3
        binary 3 '1' @?= 7
        binary 7 '0' @?= 14
        binary 7 '1' @?= 15
      ]
    , testGroup "part 2"
      [ testGroup "partTwo"
        [ testCase "produces the correct response from the fixture data" $ do
          partTwo fixture @?= 230
        ]
      , testGroup "oxygenGenerator"
        [ testCase "returns if there is only one item" $ do
          let input = BCL [bc' 1, bc' 1, bc' 1, bc' 0, bc' 0]
          input @=? oxygenGenerator [input]
        , testCase "processes fixture data" $ do
          let expected = BCL [bc' 1, bc' 0, bc' 1, bc' 1, bc' 1]
          expected @=? oxygenGenerator fixture
        ]
      , testGroup "co2Scrubber"
        [ testCase "processes fixture data" $ do
          let expected = BCL [bc' 0, bc' 1, bc' 0, bc' 1, bc' 0]
          expected @=? co2Scrubber fixture
        ]
      , testGroup "filterCountList"
        [ testCase "returns if there is only one item" $ do
          let input = [(BCL [bc' 1, bc' 1, bc' 1, bc' 0, bc' 0], 42)]
          head input @=? filterCountList (uncurry (<=)) input
        , testCase "defaults to 1 on ties" $ do
          let input = [ (BCL [bc' 0, bc' 1, bc' 1, bc' 0, bc' 0], 42)
                      , (BCL [bc' 1, bc' 1, bc' 1, bc' 0, bc' 0], 99)
                      ]
          99 @=? snd (filterCountList (uncurry (<=)) input)
        , testCase "filters on multiple layers" $ do
          let input = [ (BCL [bc' 0, bc' 1, bc' 1, bc' 0, bc' 0], 42)
                      , (BCL [bc' 1, bc' 0, bc' 0, bc' 0, bc' 0], 13)
                      , (BCL [bc' 1, bc' 1, bc' 1, bc' 0, bc' 0], 99)
                      ]
          99 @=? snd (filterCountList (uncurry (<=)) input)
        ]
      , testGroup "filterPass"
        [ testCase "returns the items with the most significant digits" $ do
          let expected = [ (BCL [bc' 1, bc' 1, bc' 1, bc' 1, bc' 0], 1)
                         , (BCL [bc' 1, bc' 0, bc' 1, bc' 1, bc' 0], 2)
                         , (BCL [bc' 1, bc' 0, bc' 1, bc' 1, bc' 1], 3)
                         , (BCL [bc' 1, bc' 0, bc' 1, bc' 0, bc' 1], 4)
                         , (BCL [bc' 1, bc' 1, bc' 1, bc' 0, bc' 0], 7)
                         , (BCL [bc' 1, bc' 0, bc' 0, bc' 0, bc' 0], 8)
                         , (BCL [bc' 1, bc' 1, bc' 0, bc' 0, bc' 1], 9)
                         ]
          expected @=? filterPass (uncurry (<=)) (zip fixture [0..12])
        ]
      , testGroup "bclToInt"
        [ testCase "recognizes 1" $ 1 @=? bclToInt (BCL [bc' 1])
        , testCase "recognizes 0" $ 0 @=? bclToInt (BCL [bc' 0])
        , testCase "shifts values over" $ 28 @=? bclToInt (BCL [bc' 1, bc' 1, bc' 1, bc' 0, bc' 0])
        ]
      ]
    ]

