module SpecHelper
  ( module Test.Hspec
  , module Game.Context
  , module Game.Player
  , module Game.Board
  ) where

import Test.Hspec
import Game.Context
import Game.Player
import Game.Board

newSpace :: (Int, Int) -> Move -> Space
newSpace location move = Space { location = location, move = move }

newBlankBoard :: Board
newBlankBoard = newBoard 3
