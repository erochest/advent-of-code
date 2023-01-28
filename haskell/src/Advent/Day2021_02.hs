{-# LANGUAGE OverloadedStrings #-}

module Advent.Day2021_02
    ( Direction(..)
    , Movement(..)
    , Sub(..)
    , partOne
    , partTwo
    , movement
    , move1
    , move2
    , subChecksum
    ) where

import Control.Applicative ((<|>))
import Data.Foldable (foldl')
import Data.Maybe (mapMaybe)

import qualified Data.ByteString.Lazy.Char8 as C
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 (parseOnly, Parser, char, decimal, space)

import Advent

partOne :: [Movement] -> Int
partOne = uncurry (*) . foldl' move1 (0, 0)

partTwo :: [Movement] -> Int
partTwo = subChecksum . foldl' move2 (Sub 0 0 0)

data Direction = Forward | Down | Up
  deriving (Show, Eq, Enum)

data Movement = Movement !Direction !Int
  deriving (Show, Eq)

data Sub = Sub
         { subHorizontal :: !Int
         , subDepth      :: !Int
         , subAim        :: !Int
         } deriving (Show, Eq)

instance Parseable Movement where
  parse = either (const Nothing) Just . parseOnly movement . C.toStrict

movement :: Parser Movement
movement = Movement <$> direction <*> (space *> decimal)

direction :: Parser Direction
direction =   (Forward <$ "forward")
          <|> (Up      <$ "up")
          <|> (Down    <$ "down")

move1 :: (Int, Int) -> Movement -> (Int, Int)
move1 (horizontal, depth) (Movement Forward amount) = (horizontal + amount, depth)
move1 (horizontal, depth) (Movement Up amount)      = (horizontal, depth - amount)
move1 (horizontal, depth) (Movement Down amount)    = (horizontal, depth + amount)

move2 :: Sub -> Movement -> Sub
move2 (Sub h d a) (Movement Forward amount) = Sub (h + amount) (d + a * amount) a
move2 (Sub h d a) (Movement Up amount)      = Sub h d (a - amount)
move2 (Sub h d a) (Movement Down amount)    = Sub h d (a + amount)

subChecksum :: Sub -> Int
subChecksum (Sub horizontal depth _) = horizontal * depth
