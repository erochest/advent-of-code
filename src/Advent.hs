module Advent
    ( interactLines
    , interactFile
    , Parseable(..)
    , inspect
    ) where

import Data.Maybe (fromJust, mapMaybe)

import Data.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as C

import Debug.Trace

inspect :: Show a => String -> a -> a
inspect tag value = trace (tag ++ ": " ++ show value) value

class Parseable a where
  parse :: C.ByteString -> Maybe a

interactLines :: (Parseable a, Show b) => ([a] -> b) -> IO ()
interactLines f =
  C.interact (toLazyByteString . (<> char8 '\n') . string8 . show . f . mapMaybe parse . C.lines)

interactFile :: (Parseable a, Show b) => (a -> b) -> IO ()
interactFile f =
  C.interact (toLazyByteString . (<> char8 '\n') . string8 . show . f . fromJust . parse)
