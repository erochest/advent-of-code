{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Maybe (isNothing)

import qualified Data.Sequence as S
import qualified Data.Vector as V

import Test.Tasty
import Test.Tasty.HUnit

import Advent
import Advent.Day2021_04

main :: IO ()
main = defaultMain suite

fixture :: BingoGame
fixture
  = BingoGame
  { bingoGameDraws = [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1]
  , bingoGameBoards =
    [ BingoBoard [ 22, 13, 17, 11,  0
                 ,  8,  2, 23,  4, 24
                 , 21,  9, 14, 16,  7
                 ,  6, 10,  3, 18,  5
                 ,  1, 12, 20, 15, 19
                 ]
    , BingoBoard [  3, 15,  0,  2, 22
                 ,  9, 18, 13, 17,  5
                 , 19,  8,  7, 25, 23
                 , 20, 11, 10, 24,  4
                 , 14, 21, 16, 12,  6
                 ]
    , BingoBoard [ 14, 21, 17, 24,  4
                 , 10, 16, 15,  9, 19
                 , 18,  8, 23, 26, 20
                 , 22, 11, 13,  6,  5
                 ,  2,  0, 12,  3,  7
                 ]
    ]
  }

suite :: TestTree
suite = testGroup "2021, day 4"
    [ testGroup "part 1"
      [ testGroup "partOne"
        [ testCase "finds the winning board's checksum" $
          partOne fixture @?= 4512
        ]
      , testGroup "Parseable.parse"
        [ testCase "reads the list of draws" $ do
          let input = "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\n"
          fmap bingoGameDraws (parse input) @?= Just (bingoGameDraws fixture)
        , testCase "reads a board" $ do
          let input = "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\n\
                      \22 13 17 11  0\n\
                      \ 8  2 23  4 24\n\
                      \21  9 14 16  7\n\
                      \ 6 10  3 18  5\n\
                      \ 1 12 20 15 19\n\n"
          fmap (head . bingoGameBoards) (parse input) @?= Just (head $ bingoGameBoards fixture)
        ]
      , testGroup "play"
        [ testCase "plays through to the winning board and draw" $ do
          let board = BingoBoard [ 14, 21, 17, 24,  4
                                 , 10, 16, 15,  9, 19
                                 , 18,  8, 23, 26, 20
                                 , 22, 11, 13,  6,  5
                                 ,  2,  0, 12,  3,  7
                                 ]
          let marks = [  True,  True,  True,  True,  True
                      , False, False, False,  True, False
                      , False, False,  True, False, False
                      , False,  True, False, False,  True
                      ,  True,  True, False, False,  True
                      ]
          let expectedSession = BingoBoardSession board marks
          let expectedDraw = 24
          play fixture @?= (expectedDraw, expectedSession)
        ]
      , testGroup "hasWon"
        [ testCase "returns False on no winner" $ do
          let marks = [ False, False, False, False, False
                      , False, False, False, False, False
                      , False, False, False, False, False
                      , False, False, False, False, False
                      , False, False, False, False, False
                      ]
          let board = head $ bingoGameBoards fixture
          let input = BingoBoardSession board marks
          not (hasWon input) @? "has not won"
        , testCase "recognizes winning on a row" $ do
          let marks = [ False,  True, False, False, False
                      , False, False, False,  True, False
                      ,  True, False, False, False, False
                      ,  True,  True,  True,  True,  True
                      , False, False, False, False,  True
                      ]
          let board = head $ bingoGameBoards fixture
          let input = BingoBoardSession board marks
          hasWon input @? "has won a row"
        , testCase "recognizes winning on a column" $ do
          let marks = [ False, False,  True,  True,  True
                      ,  True, False, False,  True,  True
                      , False, False, False, False,  True
                      , False,  True, False,  True,  True
                      , False, False, False,  True,  True
                      ]
          let board = head $ bingoGameBoards fixture
          let input = BingoBoardSession board marks
          hasWon input @? "has won a row"
        ]
      , testGroup "sliceColumn"
        [ testCase "pulls the first column" $ do
          let input:: V.Vector Int = [  1,  2,  3,  4
                                     ,  5,  6,  7,  8
                                     ,  9, 10, 11, 12
                                     , 13, 14, 15, 16:: Int
                                     ]
          let expected :: V.Vector Int = [  1,  5,  9, 13]
          expected @=? sliceColumn 0 4 input
        , testCase "pulls a middle column" $ do
          let input:: V.Vector Int = [  1,  2,  3,  4
                                     ,  5,  6,  7,  8
                                     ,  9, 10, 11, 12
                                     , 13, 14, 15, 16:: Int
                                     ]
          let expected :: V.Vector Int = [  3,  7, 11, 15]
          expected @=? sliceColumn 2 4 input
        , testCase "pulls the last column" $ do
          let input:: V.Vector Int = [  1,  2,  3,  4
                                     ,  5,  6,  7,  8
                                     ,  9, 10, 11, 12
                                     , 13, 14, 15, 16:: Int
                                     ]
          let expected :: V.Vector Int = [  4,  8, 12, 16]
          expected @=? sliceColumn 3 4 input
        ]
      , testGroup "createSession"
        [ testCase "creates a session from a board" $ do
          let session = createSession fixture
          V.length (bingoSessionBoardSessions session) @?= 3
          isNothing (bingoSessionLastDraw session) @? "Session draw is not set"
        ]
      , testGroup "playStep"
        [ testCase "removes a draw from the game" $ do
          let (game', _) = playStep fixture $ createSession fixture
          (length (bingoGameDraws fixture) - 1) @=? length (bingoGameDraws game')
          4 @=? head (bingoGameDraws game')
        , testCase "returns the same session and game if no draws" $ do
          let game = fixture { bingoGameDraws = [] }
          let session = createSession game
          let (game', session') = playStep game session
          game @=? game'
          session @=? session'
        , testCase "marks the first draw on all boards" $ do
          let (game', session') = playStep fixture $ createSession fixture
          let session0 = bingoSessionBoardSessions session' V.! 0
          let session1 = bingoSessionBoardSessions session' V.! 1
          let session2 = bingoSessionBoardSessions session' V.! 2
          bingoBoardSessionMarks session0 @?= [ False, False, False, False, False
                                              , False, False, False, False, False
                                              , False, False, False, False,  True
                                              , False, False, False, False, False
                                              , False, False, False, False, False
                                              ]
          bingoBoardSessionMarks session1 @?= [ False, False, False, False, False
                                              , False, False, False, False, False
                                              , False, False,  True, False, False
                                              , False, False, False, False, False
                                              , False, False, False, False, False
                                              ]
          bingoBoardSessionMarks session2 @?= [ False, False, False, False, False
                                              , False, False, False, False, False
                                              , False, False, False, False, False
                                              , False, False, False, False, False
                                              , False, False, False, False,  True
                                              ]
        ]
      , testGroup "checksum"
        [ testCase "multiplies the last draw with the sum of unmarked squares" $ do
          let board = bingoGameBoards fixture !! 2
          let marks = [  True,  True,  True,  True,  True
                      , False, False, False,  True, False
                      , False, False,  True, False, False
                      , False,  True, False, False,  True
                      ,  True,  True, False, False,  True
                      ]
          let session = BingoBoardSession board marks
          let lastDraw = 24
          checksum lastDraw session @?= 4512
        ]
      , testGroup "sumUnmarked"
        [ testCase "sums all unmarked squares" $ do
          let board = bingoGameBoards fixture !! 2
          let marks = [ False, False, False, False, False
                      , False, False, False, False, False
                      , False, False, False, False, False
                      , False, False, False, False, False
                      , False, False, False, False, False
                      ]
          let session = BingoBoardSession board marks
          sumMarked session @?= 325
        , testCase "sums only unmarked squares" $ do
          let board = bingoGameBoards fixture !! 2
          let marks = [  True,  True,  True,  True,  True
                      , False, False, False,  True, False
                      , False, False,  True, False, False
                      , False,  True, False, False,  True
                      ,  True,  True, False, False,  True
                      ]
          let session = BingoBoardSession board marks
          sumMarked session @?= 188
        ]
      ]
    , testGroup "part 2"
      [ testGroup "partTwo"
        [ testCase "finds the losing board's checksum" $
          partTwo fixture @?= 1924
        ]
      , testGroup "playToLast"
        [ testCase "plays through to find the last board to win" $ do
          let board = BingoBoard [  3, 15,  0,  2, 22
                                 ,  9, 18, 13, 17,  5
                                 , 19,  8,  7, 25, 23
                                 , 20, 11, 10, 24,  4
                                 , 14, 21, 16, 12,  6
                                 ]
          let expectedDraw = 13
          let (actualDraw, actualSession) = playToLast fixture
          actualDraw @?= expectedDraw
          bingoBoardSessionBoard actualSession @?= board
        ]
      ]
    ]

