module Game.Board where

import Data.Matrix
import Data.Vector hiding (any)
import Data.List

type Board = Matrix Space

data State = Winner | Tie | Undecided deriving (Show, Eq)

data Move = X | O | Blank deriving (Show, Eq)

data Space = Space { location :: (Int, Int), move :: Move } deriving (Show, Eq)

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
transpose (Board matrix) = Board $ Data.Matrix.transpose matrix
newBoard :: Int -> Board
newBoard i = matrix i i $ \(i,j) -> Space { location = (i, j), move = Blank }

maxCols :: Board -> Int
maxCols (Board matrix) = Data.Vector.length $ getRow 1 matrix

getOppositeDiag :: Board -> Data.Vector.Vector Char
getOppositeDiag (Board matrix) =
  Data.Vector.fromList $ Data.List.map (\(i, j) -> getElem i j matrix) (Data.List.zip [1..maxColNum] $ Data.List.reverse [1..maxColNum])
  where maxColNum = maxCols $ Board matrix
