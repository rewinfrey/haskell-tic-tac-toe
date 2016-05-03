module Game.Context where

import Game.Player
import Game.Board

data GameContext = GameContext { board :: Board, player1 :: Player, player2 :: Player } deriving (Show)

newGameContext :: Player -> Player -> GameContext
newGameContext player1 player2 = GameContext
  { player1 = player1
  , player2 = player2
  , board = newBoard 3 3
  }
