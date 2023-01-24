{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Advent.Day7
  ( partOne
  , partTwo
  , Crab(..)
  , findDistances
  , findFuelCosts
  , fuelCost
  ) where

import Control.Arrow ((&&&))
import Debug.Trace

import Advent
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Foldable

partOne :: [Crab] -> Int
partOne crabs = minimum $ map snd $ findDistances crabMin crabMax crabs
  where
    crabMin = getPosition $ minimum crabs
    crabMax = getPosition $ maximum crabs

partTwo :: [Crab] -> Int
partTwo crabs = minimum $ map snd $ findFuelCosts crabMin crabMax crabs
  where
    crabMin = getPosition $ minimum crabs
    crabMax = getPosition $ maximum crabs

newtype Crab = Crab { getPosition :: Int }
  deriving (Eq, Show, Ord)

instance Parseable [Crab] where
  parse = traverse (fmap (Crab . fst) . C.readInt) . C.split ','

findDistances :: Int -> Int -> [Crab] -> [(Int, Int)]
findDistances a b crabs = map (id &&& (sum . findDistancesFrom crabs)) [a..b+1]
  where
    findDistancesFrom :: [Crab] -> Int -> [Int]
    findDistancesFrom crabs x = map (abs . (x -) . getPosition) crabs

findFuelCosts :: Int -> Int -> [Crab] -> [(Int, Int)]
findFuelCosts a b crabs = map (id &&& sum . findFuelCostsFrom crabs) [a..b]
  where
    findFuelCostsFrom :: [Crab] -> Int -> [Int]
    findFuelCostsFrom crabs x = map (fuelCost x . getPosition) crabs

fuelCost :: Int -> Int -> Int 
fuelCost a b = sum $ [0 .. abs (a - b)]