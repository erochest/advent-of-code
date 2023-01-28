{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Advent.Day2021_05
  ( partOne
  , partTwo
  , mapDimension
  , Point
  , SegmentData
  , StraightSegment(..)
  , CrookedSegment(..)
  , Map
  , createMap
  , mapSegments
  , Expandable(..)
  , expandSegment
  , pointIndex
  , countDoubled
  ) where

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M

import Debug.Trace

import Advent

partOne :: [StraightSegment] -> Int
partOne = countDoubled . mapSegments createMap

partTwo :: [CrookedSegment] -> Int
partTwo = countDoubled . mapSegments createMap

mapDimension :: Int
mapDimension = 1000

type Point = (Int, Int)

type SegmentData = (Point, Point)
newtype StraightSegment = SS SegmentData
  deriving (Show, Eq)
newtype CrookedSegment = CS SegmentData
  deriving (Show, Eq)

type Map = V.Vector Int

instance Parseable StraightSegment where
  parse = either (const Nothing) (Just . SS) . parseOnly segment . C.toStrict

instance Parseable CrookedSegment where
  parse = either (const Nothing) (Just . CS) . parseOnly segment . C.toStrict

segment :: Parser SegmentData
segment = (,) <$> point <*> (" -> " *> point)

point :: Parser Point
point = (,) <$> decimal <*> (char8 ',' *> decimal)

createMap :: Map
createMap = V.replicate (mapDimension * mapDimension) 0

class Expandable a where
  expand :: a -> [Point]

mapSegments :: Expandable a => Map -> [a] -> Map
mapSegments m segments = V.accum (+) m $ map ((, 1) . pointIndex) (concatMap expand segments)

instance Expandable StraightSegment where
  expand (SS (start@(x0, y0), end@(x1, y1)))
    | x0 == x1 || y0 == y1 = expandSegment (offset x0 x1, offset y0 y1) start end
    | otherwise            = []

instance Expandable CrookedSegment where
  expand (CS (start@(x0, y0), end@(x1, y1)))
    = expandSegment (offset x0 x1, offset y0 y1) start end

offset :: Ord a => a -> a -> Int
offset a b = case compare a b of
              LT -> 1
              EQ -> 0
              GT -> (-1)

expandSegment :: (Int, Int) -> Point -> Point -> [Point]
expandSegment offset@(offsetX, offsetY) start@(x0, y0) end
  | start == end = [end]
  | otherwise    = let next = (x0 + offsetX, y0 + offsetY)
                   in  start:expandSegment offset next end

pointIndex :: Point -> Int
pointIndex (x, y) = x + y * mapDimension

countDoubled :: Map -> Int
countDoubled = V.length . V.filter (>1)
