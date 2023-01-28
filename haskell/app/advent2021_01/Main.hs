module Main where

import Data.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Maybe
import Debug.Trace

import Advent

main :: IO ()
main = Advent.interactLines partTwo

traceValue :: Show a => String -> a -> a
traceValue tag value = trace (tag ++ ": " ++ show value) value

partOne :: [Int] -> Int
partOne = countIncreases
-- partOne = toLazyByteString . (<> char8 '\n') . intDec . countIncreases . parse

partTwo :: [Int] -> Int
partTwo = countIncreases . map sumTriple . toTriples

instance Advent.Parseable Int where
  parse = fmap fst . C.readInt

toTriples :: [Int] -> [(Int, Int, Int)]
toTriples (a : rest@(b : c : _)) = (a, b, c) : toTriples rest
toTriples [_, _] = []
toTriples [_] = []
toTriples [] = []

sumTriple :: (Int, Int, Int) -> Int
sumTriple (a, b, c) = a + b + c

countIncreases :: [Int] -> Int
countIncreases = snd . foldl increases (Nothing, 0)

increases :: (Maybe Int, Int) -> Int -> (Maybe Int, Int)
increases (Nothing, count) current = (Just current, count)
increases (Just prev, count) current
    | prev < current = (Just current, count + 1)
    | otherwise      = (Just current, count)
