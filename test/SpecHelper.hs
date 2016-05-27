module SpecHelper
  ( module Test.Hspec
  , module Game.AIPlayer
  , module Game.Context
  , module Game.Player
  , module Game.Board
  , newSpace
  , newBlankBoard
  ) where

import Test.Hspec
import Game.AIPlayer
import Game.Context
import Game.Player
import Game.Board

newSpace :: Location -> Move -> Space
newSpace location move = Space { location = location, move = move }

newBlankBoard :: Board
newBlankBoard = newBoard 3
