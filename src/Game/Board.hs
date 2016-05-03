module Game.Board where

import Data.Matrix
import Data.Vector
import Data.List

newtype Board = Board (Matrix Char) deriving (Show, Eq)

data State = Winner | Tie | Undecided deriving (Show, Eq)

sentinel = ' '

newBoard :: Int -> Board
newBoard i = Board (matrix i i $ \(i,j) -> sentinel)

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
tie (Board matrix) =
  not $ sentinel `Data.List.elem` (Data.Matrix.toList matrix)

rowWinner :: Board -> Int -> Int -> Bool
rowWinner board colNum maxColNum =
  colWinner (Game.Board.transpose board) colNum maxColNum

colWinner :: Board -> Int -> Int -> Bool
colWinner (Board matrix) colNum maxColNum
  | colNum == maxColNum                 = False
  | vectorWinner $ getCol colNum matrix = True
  | otherwise                           = colWinner (Board matrix) (colNum + 1) maxColNum

diagWinner :: Board -> Bool
diagWinner (Board matrix)
  | vectorWinner $ getDiag matrix                 = True
  | vectorWinner $ getOppositeDiag $ Board matrix = True
  | otherwise                                     = False

vectorWinner :: Vector Char -> Bool
vectorWinner v =
  let col = Data.Vector.toList v
      result = Data.List.nub $ Data.List.map (\boardSquare -> (boardSquare /= sentinel) && (boardSquare == (Data.List.head col))) col
  in
    case result of
      [True] -> True
      _ -> False

transpose :: Board -> Board
transpose (Board matrix) = Board $ Data.Matrix.transpose matrix

maxCols :: Board -> Int
maxCols (Board matrix) = Data.Vector.length $ getRow 1 matrix

getOppositeDiag :: Board -> Data.Vector.Vector Char
getOppositeDiag (Board matrix) =
  Data.Vector.fromList $ Data.List.map (\(i, j) -> getElem i j matrix) (Data.List.zip [1..maxColNum] $ Data.List.reverse [1..maxColNum])
  where maxColNum = maxCols $ Board matrix
