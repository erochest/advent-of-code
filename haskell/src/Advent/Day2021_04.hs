{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day2021_04
  ( partOne
  , partTwo
  , BingoGame(..)
  , BingoBoard(..)
  , BingoBoardSession(..)
  , BingoSession(..)
  , play
  , playToLast
  , createSession
  , playStep
  , hasWon
  , sliceColumn
  , checksum
  , sumMarked
  ) where

import Data.Foldable
import Data.Maybe (fromMaybe, mapMaybe)
import Debug.Trace

import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Sequence as S
import Data.Sequence (Seq(..))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M

import Advent
import qualified Data.Vector as M

partOne :: BingoGame -> Int
partOne = uncurry checksum . play

partTwo :: BingoGame -> Int
partTwo = uncurry checksum . playToLast

instance Parseable () where
  parse _ = Nothing

data BingoGame
  = BingoGame
  { bingoGameDraws  :: ![Int]
  , bingoGameBoards :: ![BingoBoard]
  } deriving (Eq, Show)

newtype BingoBoard
  = BingoBoard
  { unwrapBoard :: V.Vector Int
  } deriving (Eq, Show, Semigroup)

data BingoBoardSession
  = BingoBoardSession
  { bingoBoardSessionBoard :: !BingoBoard
  , bingoBoardSessionMarks :: !(V.Vector Bool)
  -- going to need something else here. tbd what
  } deriving (Eq, Show)

data BingoSession
  = BingoSession
  { bingoSessionBoardSessions :: !(V.Vector BingoBoardSession)
  , bingoSessionLastDraw      :: !(Maybe Int)
  } deriving (Eq, Show)

play :: BingoGame -> (Int, BingoBoardSession)
play game = playLoop game $ createSession game
  where
    playLoop :: BingoGame -> BingoSession -> (Int, BingoBoardSession)
    playLoop game session
      = let (game', session') = playStep game session
            winner = V.filter hasWon (bingoSessionBoardSessions session') V.!? 0
        in  case winner of
              Nothing -> playLoop game' session'
              Just winner -> (fromMaybe 0 (bingoSessionLastDraw session'), winner)

playToLast :: BingoGame -> (Int, BingoBoardSession)
playToLast game = playLoop game $ createSession game
  where
    playLoop :: BingoGame -> BingoSession -> (Int, BingoBoardSession)
    playLoop game session
      = let (game', session') = playStep game session
            (winners, losers) = V.partition hasWon (bingoSessionBoardSessions session')
            winner = winners V.!? 0
        in  case winner of
              Nothing -> playLoop game' session'
              Just winner -> if V.length losers == 0
                             then (fromMaybe 0 (bingoSessionLastDraw session'), winner)
                             else playLoop game' session' { bingoSessionBoardSessions = losers }

createSession :: BingoGame -> BingoSession
createSession = (`BingoSession` Nothing) . V.fromList . map createBoardSession . bingoGameBoards

createBoardSession :: BingoBoard -> BingoBoardSession
createBoardSession board =
  BingoBoardSession board $ V.replicate (length $ unwrapBoard board) False

playStep :: BingoGame -> BingoSession -> (BingoGame, BingoSession)
playStep game@BingoGame { bingoGameDraws = (draw:draws) }
         BingoSession { bingoSessionBoardSessions } =
  (game { bingoGameDraws = draws }, session)
  where
    session = BingoSession
            { bingoSessionBoardSessions = sessions
            , bingoSessionLastDraw = Just draw
            }
    sessions = V.map (playBoardStep draw) bingoSessionBoardSessions
playStep game@BingoGame { bingoGameDraws = [] } session = (game, session)

playBoardStep :: Int -> BingoBoardSession -> BingoBoardSession
playBoardStep draw session@BingoBoardSession { bingoBoardSessionBoard, bingoBoardSessionMarks }
  = maybe session (update session bingoBoardSessionMarks) index
  where
    index = V.findIndex (==draw) $ unwrapBoard bingoBoardSessionBoard
    flagIndex i v = M.write v i True
    updateMarks session marks = session { bingoBoardSessionMarks = marks }
    update session marks i = updateMarks session $ V.modify (flagIndex i) marks

hasWon :: BingoBoardSession -> Bool
hasWon BingoBoardSession { bingoBoardSessionMarks }
  =  or [wonRow r | r <- indices]
  || or [wonColumn c | c <- indices]
  -- || wonDiagonal id indices
  -- || wonDiagonal (4 -) indices
  where
    indices = [0..4]
    wonRow i = V.and $ V.slice (i * 5) 5 bingoBoardSessionMarks
    wonColumn c = V.and $ sliceColumn c 5 bingoBoardSessionMarks
    wonDiagonal f = all (get bingoBoardSessionMarks f)
    get v f i = v V.! (5 * f i + i)

sliceColumn :: Int -> Int -> V.Vector a -> V.Vector a
sliceColumn i columnCount v = V.generate len $ \i -> v V.! (indices !! i)
  where
    indices = [i, i+columnCount .. V.length v - 1]
    len = length indices

data BingoGameParseState
  = Init
  | PreGame !BingoGame
  | InGame !BingoGame !BingoBoard
  deriving (Eq, Show)

unwrapParsedBoard :: BingoGameParseState -> Maybe BingoGame
unwrapParsedBoard Init = Nothing
unwrapParsedBoard (PreGame game) = Just $ reverseBoards game
unwrapParsedBoard (InGame game board) = Just $ reverseBoards $ appendBoard game board

instance Parseable BingoGame where
  parse = unwrapParsedBoard . foldl' parseStep Init . C.lines

parseStep :: BingoGameParseState -> C.ByteString -> BingoGameParseState
parseStep Init input = PreGame
                     $ (`BingoGame` [])
                     $ mapMaybe (fmap fst . C.readInt)
                     $ C.split ',' input
parseStep state@(PreGame game) input
  | C.null input = state
  | otherwise    = InGame game
                 $ BingoBoard
                 $ V.fromList
                 $ mapMaybe (fmap fst . C.readInt)
                 $ C.words input
parseStep state@(InGame game board) input
  | C.null input = PreGame $ appendBoard game board
  | otherwise    = InGame game
                 $ (board <>)
                 $ BingoBoard
                 $ V.fromList
                 $ mapMaybe (fmap fst . C.readInt)
                 $ C.words input

appendGameBoard :: BingoBoard -> [Int] -> BingoBoard
appendGameBoard (BingoBoard board) xs = BingoBoard $ board <> V.fromList xs

appendBoard :: BingoGame -> BingoBoard -> BingoGame
appendBoard game@BingoGame { bingoGameBoards } board
  = game { bingoGameBoards = board:bingoGameBoards }

reverseBoards :: BingoGame -> BingoGame
reverseBoards game@BingoGame { bingoGameBoards } = game { bingoGameBoards = reverse bingoGameBoards }

checksum :: Int -> BingoBoardSession -> Int
checksum draw = (draw *) . sumMarked

sumMarked :: BingoBoardSession -> Int
sumMarked BingoBoardSession { bingoBoardSessionBoard, bingoBoardSessionMarks }
  = V.sum $ V.map snd $ V.filter (not . fst) $ V.zip bingoBoardSessionMarks $ unwrapBoard bingoBoardSessionBoard
