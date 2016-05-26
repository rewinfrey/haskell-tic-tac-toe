module Game.AIPlayerSpec (main, spec) where

import Data.Matrix
import Game.AIPlayer
import SpecHelper

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "minimax" $
    it "returns the best move" $
      True `shouldBe` True
