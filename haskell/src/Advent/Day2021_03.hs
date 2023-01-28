{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Advent.Day2021_03 
  ( partOne
  , partTwo
  , epsilon
  , gamma
  , parseBitCount
  , parseBinary
  , binary
  , bc
  , bc'
  , bclToInt
  , padLeft
  , BitCount
  , BitCountList(..)
  , oxygenGenerator
  , filterCountList
  , filterPass
  , co2Scrubber
  ) where

import Control.Arrow ((***), (&&&))
import Data.Bits (complement)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Foldable (foldl')
import Data.Monoid
import Data.Traversable (sequenceA)

import Advent

partOne :: [BitCountList] -> Int
partOne = uncurry (*) . (gamma &&& epsilon) . mconcat

type BitCount = (Sum Int, Sum Int)
newtype BitCountList = BCL { unwrapList :: [BitCount] }
  deriving (Show, Eq)

bc' :: Int -> BitCount
bc' 0 = (Sum 1, Sum 0)
bc' 1 = (Sum 0, Sum 1)
bc' _ = undefined

bc :: Int -> Int -> BitCount
bc a b = (Sum a, Sum b)

instance Parseable BitCountList where
  parse = fmap BCL . sequenceA . C.foldr ((:) . parseBitCount) []

parseBitCount :: Char -> Maybe BitCount
parseBitCount '0' = Just (bc 1 0)
parseBitCount '1' = Just (bc 0 1)
parseBitCount _   = Nothing

bclToInt :: BitCountList -> Int
bclToInt = gamma

instance Semigroup BitCountList where
  (BCL as) <> (BCL bs) = BCL $ uncurry (zipWith (<>)) $ padLeft as bs

instance Monoid BitCountList where
  mempty = BCL mempty

padLeft :: Monoid a => [a] -> [a] -> ([a], [a])
padLeft a b = (padToLength padTo a, padToLength padTo b)
  where
    padTo = max (length a) (length b)
    padToLength padTo xs = replicate (padTo - length xs) mempty ++ xs

epsilon :: BitCountList -> Int
epsilon = keepBits epsilonBit
  where
    epsilonBit (a, b) | a < b     = 0
                      | otherwise = 1

gamma :: BitCountList -> Int
gamma = keepBits gammaBit
  where
    gammaBit (a, b) | a > b     = 0
                    | otherwise = 1

keepBits :: ((Sum Int, Sum Int) -> Int) -> BitCountList -> Int
keepBits bit = foldl' build 0 . map bit . unwrapList
  where
    build :: Int -> Int -> Int
    build accum n = n + 2 * accum

-- Maybe I'll need this stuff for part 2.

parseBinary :: C.ByteString -> Maybe Int
parseBinary = Just . C.foldl' binary 0

binary :: Int -> Char -> Int
binary n '0' = 0 + 2 * n
binary n '1' = 1 + 2 * n
binary _ _   = undefined

partTwo :: [BitCountList] -> Int
partTwo = uncurry (*) . (bclToInt *** bclToInt) . (oxygenGenerator &&& co2Scrubber)

oxygenGenerator :: [BitCountList] -> BitCountList
oxygenGenerator [bcl] = bcl
oxygenGenerator bcls = snd $ filterCountList (uncurry (<=)) $ map (id &&& id) bcls

filterCountList :: (BitCount -> Bool) -> [(BitCountList, a)] -> (BitCountList, a)
filterCountList _ [bcl] = bcl
filterCountList f bcls = filterCountList f $ peelLayer $ filterPass f bcls

peelLayer :: [(BitCountList, a)] -> [(BitCountList, a)]
peelLayer = map peelLayer'

peelLayer' :: (BitCountList, a) -> (BitCountList, a)
peelLayer' (BCL (_:rest), a) = (BCL rest, a)
peelLayer' p = p

filterPass :: (BitCount -> Bool) -> [(BitCountList, a)] -> [(BitCountList, a)]
filterPass f input = filter (filterFunc . head . unwrapList . fst) input
  where
    bitCount = head $ unwrapList $ mconcat $ map fst input
    filterFunc = if f bitCount
                 then (== 1) . snd
                 else (== 1) . fst

co2Scrubber :: [BitCountList] -> BitCountList
co2Scrubber [bcl] = bcl
co2Scrubber bcls = snd $ filterCountList (uncurry (>)) $ map (id &&& id) bcls
