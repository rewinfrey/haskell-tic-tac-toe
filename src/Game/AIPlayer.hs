module Game.AIPlayer where

import Data.Foldable (minimumBy, maximumBy)
import Game.Board( Board, Space(..), Location, boardState, State(..), update, availableSpaces )
import Game.Player( Token(..) )

-- Ply refers to the depth of the game tree as it is evaluated.
type Ply = Int
type Score = Int

data MinMaxPlayer = Min | Max deriving (Eq, Show)

minimax :: Board -> Token -> Location
minimax board token =
  location . fst $ maximumBy (\(_, score) (_, score') -> compare score score') bestScores
  where bestScores = evaluateBoardForSpace <$> availableSpaces board
        evaluateBoardForSpace space = (space, evaluateBoard board Max token 0 space)

evaluateBoard :: Board -> MinMaxPlayer -> Token -> Ply -> Space -> Score
evaluateBoard board minmaxPlayer token ply space@ Space { location = location } =
  case boardState updatedBoard of
    Undecided -> bestScore minmaxPlayer (evaluateBoard updatedBoard (oppositeMinMaxPlayer minmaxPlayer) (oppositeToken token) (ply + 1) <$> availableSpaces updatedBoard)
    Winner    -> if minmaxPlayer == Max then 1000 - ply else (-1000) + ply
    Tie       -> 0
  where updatedBoard = update board location token

bestScore :: MinMaxPlayer -> [Score] -> Score
bestScore minmaxPlayer scores =
  -- When a terminal condition is met (the board's state is either Win or Tie),
  -- the minmax player at that depth is not the same minmax player when
  -- evaluating the best score. To account for this discrepancy, and to make it
  -- more intuitive, we flip the minmaxPlayer so we compare scores according to
  -- their final states for the minmaxPlayer that resulted in a terminal condition.
  case oppositeMinMaxPlayer minmaxPlayer of
    Max -> maximumBy compareScores scores
    Min -> minimumBy compareScores scores
  where compareScores score score' = compare score score'

oppositeToken :: Token -> Token
oppositeToken X = O
oppositeToken O = X

oppositeMinMaxPlayer :: MinMaxPlayer -> MinMaxPlayer
oppositeMinMaxPlayer Max = Min
oppositeMinMaxPlayer Min = Max
