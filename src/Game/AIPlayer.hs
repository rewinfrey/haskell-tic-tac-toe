module Game.AIPlayer where

import Data.Foldable (minimumBy, maximumBy)
import Game.Board( Board, Space(..), Move(X), Move(O), Space, Location, tempSpace, boardState, State( Winner) , State( Tie ), State( Undecided ), update, availableSpaces )

-- Ply refers to the depth of the game tree as it is evaluated.
type Ply = Int
type Score = Int

data MinMaxPlayer = Min | Max deriving (Eq, Show)

minimax :: Board -> MinMaxPlayer -> Move -> Ply -> (Space, Score, MinMaxPlayer)
minimax board minmaxPlayer move ply =
  findBestMove minmaxPlayer $ evaluateBoard board minmaxPlayer move ply <$> availableSpaces board

evaluateBoard :: Board -> MinMaxPlayer -> Move -> Ply -> Space -> (Space, Score, MinMaxPlayer)
evaluateBoard board minmaxPlayer move ply space@ Space { location = location } =
  case boardState updatedBoard of
    Undecided -> minimax updatedBoard (oppositeMinMaxPlayer minmaxPlayer) (oppositeMove move) (ply + 1)
    Winner    -> (space { move = move }, score minmaxPlayer ply, minmaxPlayer)
    Tie       -> (space { move = move }, 0, minmaxPlayer)
  where updatedBoard = update board location move

findBestMove :: MinMaxPlayer -> [(Space, Score, MinMaxPlayer)] -> (Space, Score, MinMaxPlayer)
findBestMove minmaxPlayer scores =
  case minmaxPlayer of
    Max -> maximumBy scoreComparison scores
    Min -> minimumBy scoreComparison scores
  where scoreComparison (_, score, _) (_, score', _) = compare (abs score) (abs score')

oppositeMove :: Move -> Move
oppositeMove X = O
oppositeMove O = X

oppositeMinMaxPlayer :: MinMaxPlayer -> MinMaxPlayer
oppositeMinMaxPlayer Max = Min
oppositeMinMaxPlayer Min = Max

score :: MinMaxPlayer -> Ply -> Score
score Max ply = 1000 - ply
score Min ply = (-1000) + ply
