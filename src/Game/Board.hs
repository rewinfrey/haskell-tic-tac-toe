module Game.Board where

import Data.Matrix
import Data.Vector hiding (any)
import Data.List

type Board = Matrix Space

type Location = (Int, Int)

data Move = X | O | Blank deriving (Show, Eq)

data Space = Space { location :: Location, move :: Move } deriving (Show, Eq)

data State = Winner | Tie | Undecided deriving (Show, Eq)

newBoard :: Int -> Board
newBoard i = matrix i i $ \(i,j) -> Space { location = (i, j), move = Blank }

boardState :: Board -> State
boardState board
  | winner board = Winner
  | tie board    = Tie
  | otherwise    = Undecided

winner :: Board -> Bool
winner board
  | colWinner board 1 (maxCols board) = True
  | rowWinner board 1 (maxCols board) = True
  | diagWinner board                  = True
  | otherwise                         = False

tie :: Board -> Bool
tie board =
  not . any (\space -> move space == Blank) $ Data.Matrix.toList board

update :: Board -> Location -> Move -> Board
update board (i, j) move = Data.Matrix.fromList 3 3 updateBoard
  where space = Data.Matrix.getElem i j board
        updatedSpace = space { move = move }
        updateBoard = updateSpace <$> Data.Matrix.toList board
        updateSpace space = if open space then updatedSpace else space
          where open space@ Space { location = location, move = move } = location == (i,j) && move == Blank

-- Functions below are supporting functions for the main API above

rowWinner :: Board -> Int -> Int -> Bool
rowWinner board = colWinner (Game.Board.transpose board)

colWinner :: Board -> Int -> Int -> Bool
colWinner board colNum maxColNum
  | colNum == maxColNum                = False
  | vectorWinner $ getCol colNum board = True
  | otherwise                          = colWinner board (colNum + 1) maxColNum

diagWinner :: Board -> Bool
diagWinner board
  | vectorWinner $ getDiag board         = True
  | vectorWinner $ getOppositeDiag board = True
  | otherwise                            = False

vectorWinner :: Vector Space -> Bool
vectorWinner v =
  Data.List.all match column
  where
    match space = move space /= Blank && move space == move firstSpace
    column = Data.Vector.toList v
    firstSpace = Data.List.head column

transpose :: Board -> Board
transpose = Data.Matrix.transpose

maxCols :: Board -> Int
maxCols board = Data.Vector.length $ getRow 1 board

getOppositeDiag :: Board -> Data.Vector.Vector Space
getOppositeDiag board =
  Data.Vector.fromList $ Data.List.map (\(i, j) -> getElem i j board) (Data.List.zip [1..maxColNum] $ Data.List.reverse [1..maxColNum])
  where maxColNum = maxCols board
