{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Advent.Day6
  ( partOne
  , partTwo
  , LanternFishSchool(..)
  , newLanternFishSchool
  , addLanternFish
  , lanternFishSchoolDay
  ) where

import Control.Arrow (first)
import Data.Foldable
import Debug.Trace

import qualified Data.Vector as V

import Advent
import qualified Data.ByteString.Lazy.Char8 as C

partOne :: LanternFishSchool -> Int
partOne = sum . getSchool . lanternFishSchoolDays 80

partTwo :: LanternFishSchool -> Int
partTwo = sum . getSchool . lanternFishSchoolDays 256

spawnStart :: Int 
spawnStart = 6

newFishBonus :: Int 
newFishBonus = 2

newtype LanternFishSchool = LanternFishSchool { getSchool :: V.Vector Int }
  deriving (Eq, Show, Ord)

lanternFishSchoolDays :: Int -> LanternFishSchool -> LanternFishSchool
lanternFishSchoolDays days school = foldl' lanternFishSchoolDay school [1..days]

lanternFishSchoolDay :: LanternFishSchool -> Int -> LanternFishSchool
lanternFishSchoolDay (LanternFishSchool school) _
  = LanternFishSchool $ V.update school $ V.map (uncurry day) $ V.indexed school
  where
    day :: Int -> Int -> (Int, Int)
    day i _ = let i' = i + 1
              in  case i' of
                  9 -> (i, school V.! 0)
                  7 -> (i, school V.! 0 + school V.! i')
                  _ -> (i, school V.! i')

newLanternFishSchool :: LanternFishSchool
newLanternFishSchool = LanternFishSchool $ V.replicate 9 0

addLanternFish :: LanternFishSchool -> [Int] -> LanternFishSchool
addLanternFish (LanternFishSchool school) = LanternFishSchool . V.accum (+) school . map (,1)

instance Parseable LanternFishSchool where
  parse = fmap (addLanternFish newLanternFishSchool) . traverse (fmap fst . C.readInt) . C.split ','
