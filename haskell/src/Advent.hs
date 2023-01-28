module Advent
    ( interact
    , interactAll
    , interactLines
    , interactFile
    , Parseable(..)
    , inspect
    ) where

import qualified Data.ByteString.Builder    as B
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Maybe (fromJust, mapMaybe)
import           Prelude                    hiding (interact)

import Debug.Trace


interact :: ([BS.ByteString] -> B.Builder) -> IO ()
interact f = BS.interact $ B.toLazyByteString . (<> B.charUtf8 '\n') . f . BS.lines

interactAll :: (BS.ByteString -> B.Builder) -> IO ()
interactAll f = BS.interact $ B.toLazyByteString . (<> B.charUtf8 '\n') . f

inspect :: Show a => String -> a -> a
inspect tag value = trace (tag ++ ": " ++ show value) value

class Parseable a where
  parse :: BS.ByteString -> Maybe a

interactLines :: (Parseable a, Show b) => ([a] -> b) -> IO ()
interactLines f =
  BS.interact (B.toLazyByteString . (<> B.char8 '\n') . B.string8 . show . f . mapMaybe parse . BS.lines)

interactFile :: (Parseable a, Show b) => (a -> b) -> IO ()
interactFile f =
  BS.interact (B.toLazyByteString . (<> B.char8 '\n') . B.string8 . show . f . fromJust . parse)
