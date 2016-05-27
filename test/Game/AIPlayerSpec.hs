module Game.AIPlayerSpec (main, spec) where

import Data.Matrix
import Game.AIPlayer
import SpecHelper

main :: IO ()
main = hspec spec

columnWinOpportunity :: Board
columnWinOpportunity =
  matrix 3 3 $ \(i,j) -> case (i,j) of
                          (1,1) -> newSpace (1,1) X; (1,2)     -> newSpace (1,2) Blank; (1,3) -> newSpace (1,3) Blank
                          (2,1) -> newSpace (2,1) X; (2,2)     -> newSpace (2,2) Blank; (2,3) -> newSpace (2,3) Blank
                          (3,1) -> newSpace (3,1) Blank; (3,2) -> newSpace (3,2) Blank; (3,3) -> newSpace (3,3) Blank

rowWinOpportunity :: Board
rowWinOpportunity =
  matrix 3 3 $ \(i,j) -> case (i,j) of
                          (1,1) -> newSpace (1,1) X; (1,2)     -> newSpace (1,2) X; (1,3)     -> newSpace (1,3) Blank
                          (2,1) -> newSpace (2,1) X; (2,2)     -> newSpace (2,2) O; (2,3)     -> newSpace (2,3) Blank
                          (3,1) -> newSpace (3,1) O; (3,2)     -> newSpace (3,2) Blank; (3,3) -> newSpace (3,3) Blank

winAndOpponentWinOpportunity :: Board
winAndOpponentWinOpportunity =
  matrix 3 3 $ \(i,j) -> case (i,j) of
                          (1,1) -> newSpace (1,1) X; (1,2)     -> newSpace (1,2) X; (1,3)     -> newSpace (1,3) Blank
                          (2,1) -> newSpace (2,1) X; (2,2)     -> newSpace (2,2) O; (2,3)     -> newSpace (2,3) Blank
                          (3,1) -> newSpace (3,1) O; (3,2)     -> newSpace (3,2) O; (3,3)     -> newSpace (3,3) Blank

blockOpponentWinOpportunity :: Board
blockOpponentWinOpportunity =
  matrix 3 3 $ \(i,j) -> case (i,j) of
                          (1,1) -> newSpace (1,1) X; (1,2)     -> newSpace (1,2) Blank; (1,3) -> newSpace (1,3) Blank
                          (2,1) -> newSpace (2,1) X; (2,2)     -> newSpace (2,2) Blank; (2,3) -> newSpace (2,3) Blank
                          (3,1) -> newSpace (3,1) O; (3,2)     -> newSpace (3,2) O; (3,3)     -> newSpace (3,3) Blank

blockOpponentForkOpportunity :: Board
blockOpponentForkOpportunity =
  matrix 3 3 $ \(i,j) -> case (i,j) of
                          (1,1) -> newSpace (1,1) O; (1,2)     -> newSpace (1,2) Blank; (1,3) -> newSpace (1,3) Blank
                          (2,1) -> newSpace (2,1) Blank; (2,2) -> newSpace (2,2) Blank; (2,3) -> newSpace (2,3) Blank
                          (3,1) -> newSpace (3,1) Blank; (3,2) -> newSpace (3,2) Blank; (3,3) -> newSpace (3,3) Blank

blockOpponentForkOpportunity2 :: Board
blockOpponentForkOpportunity2 =
  matrix 3 3 $ \(i,j) -> case (i,j) of
                          (1,1) -> newSpace (1,1) O;     (1,2) -> newSpace (1,2) X;     (1,3) -> newSpace (1,3) Blank
                          (2,1) -> newSpace (2,1) X;     (2,2) -> newSpace (2,2) Blank; (2,3) -> newSpace (2,3) O
                          (3,1) -> newSpace (3,1) Blank; (3,2) -> newSpace (3,2) O;     (3,3) -> newSpace (3,3) Blank

spec :: Spec
spec =
  describe "minimax" $ do
    it "finds immediate win simple case" $
      minimax columnWinOpportunity Max X 0 `shouldBe` (Space { location = (3,1), move = X }, 1000, Max)

    it "finds immediate win more complex case" $
      minimax rowWinOpportunity Max X 0 `shouldBe` (Space { location = (1,3), move = X }, 1000, Max)

    it "prioritizes wins over blocking opponent's win" $
      minimax winAndOpponentWinOpportunity Max X 0 `shouldBe` (Space { location = (1,3), move = X }, 1000, Max)

    it "blocks an opponent from winning" $
      minimax blockOpponentWinOpportunity Max X 0 `shouldBe` (Space { location = (3,3), move = O }, -999, Min)

    it "blocks an opponent from forking" $
      minimax blockOpponentForkOpportunity Max X 0 `shouldBe` (Space { location = (1,3), move = O }, -995, Min)

    it "blocks an opponent from forking two" $
      minimax blockOpponentForkOpportunity2 Max X 0 `shouldBe` (Space { location = (3,3), move = O }, -997, Min)
