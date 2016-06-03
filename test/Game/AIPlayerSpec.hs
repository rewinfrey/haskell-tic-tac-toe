module Game.AIPlayerSpec (main, spec) where

import Data.Matrix
import Game.AIPlayer
import SpecHelper

main :: IO ()
main = hspec spec

columnWin :: Board
columnWin =
  matrix 3 3 $ \(i,j) -> case (i,j) of
                          (1,1) -> newSpace (1,1) X; (1,2)     -> newSpace (1,2) Blank; (1,3) -> newSpace (1,3) Blank
                          (2,1) -> newSpace (2,1) X; (2,2)     -> newSpace (2,2) Blank; (2,3) -> newSpace (2,3) Blank
                          (3,1) -> newSpace (3,1) Blank; (3,2) -> newSpace (3,2) Blank; (3,3) -> newSpace (3,3) Blank

rowWin :: Board
rowWin =
  matrix 3 3 $ \(i,j) -> case (i,j) of
                          (1,1) -> newSpace (1,1) X; (1,2)     -> newSpace (1,2) X; (1,3)     -> newSpace (1,3) Blank
                          (2,1) -> newSpace (2,1) X; (2,2)     -> newSpace (2,2) O; (2,3)     -> newSpace (2,3) Blank
                          (3,1) -> newSpace (3,1) O; (3,2)     -> newSpace (3,2) Blank; (3,3) -> newSpace (3,3) Blank

winAndOpponentWin :: Board
winAndOpponentWin =
  matrix 3 3 $ \(i,j) -> case (i,j) of
                          (1,1) -> newSpace (1,1) X; (1,2)     -> newSpace (1,2) X; (1,3)     -> newSpace (1,3) Blank
                          (2,1) -> newSpace (2,1) X; (2,2)     -> newSpace (2,2) O; (2,3)     -> newSpace (2,3) Blank
                          (3,1) -> newSpace (3,1) O; (3,2)     -> newSpace (3,2) O; (3,3)     -> newSpace (3,3) Blank

blockOpponentWin :: Board
blockOpponentWin =
  matrix 3 3 $ \(i,j) -> case (i,j) of
                          (1,1) -> newSpace (1,1) X; (1,2)     -> newSpace (1,2) Blank; (1,3) -> newSpace (1,3) Blank
                          (2,1) -> newSpace (2,1) X; (2,2)     -> newSpace (2,2) Blank; (2,3) -> newSpace (2,3) Blank
                          (3,1) -> newSpace (3,1) O; (3,2)     -> newSpace (3,2) O; (3,3)     -> newSpace (3,3) Blank

blockOpponentWin2 :: Board
blockOpponentWin2 =
  matrix 3 3 $ \(i,j) -> case (i,j) of
                          (1,1) -> newSpace (1,1) Blank; (1,2) -> newSpace (1,2) Blank; (1,3) -> newSpace (1,3) O
                          (2,1) -> newSpace (2,1) Blank; (2,2) -> newSpace (2,2) X;     (2,3) -> newSpace (2,3) X
                          (3,1) -> newSpace (3,1) Blank; (3,2) -> newSpace (3,2) Blank; (3,3) -> newSpace (3,3) Blank

blockOpponentFork :: Board
blockOpponentFork =
  matrix 3 3 $ \(i,j) -> case (i,j) of
                          (1,1) -> newSpace (1,1) O; (1,2)     -> newSpace (1,2) Blank; (1,3) -> newSpace (1,3) Blank
                          (2,1) -> newSpace (2,1) Blank; (2,2) -> newSpace (2,2) Blank; (2,3) -> newSpace (2,3) Blank
                          (3,1) -> newSpace (3,1) Blank; (3,2) -> newSpace (3,2) Blank; (3,3) -> newSpace (3,3) Blank

blockOpponentFork2 :: Board
blockOpponentFork2 =
  matrix 3 3 $ \(i,j) -> case (i,j) of
                          (1,1) -> newSpace (1,1) O;     (1,2) -> newSpace (1,2) X;     (1,3) -> newSpace (1,3) Blank
                          (2,1) -> newSpace (2,1) X;     (2,2) -> newSpace (2,2) Blank; (2,3) -> newSpace (2,3) O
                          (3,1) -> newSpace (3,1) Blank; (3,2) -> newSpace (3,2) O;     (3,3) -> newSpace (3,3) Blank

blockFutureWin :: Board
blockFutureWin =
  matrix 3 3 $ \(i,j) -> case (i,j) of
                          (1,1) -> newSpace (1,1) Blank; (1,2) -> newSpace (1,2) Blank; (1,3) -> newSpace (1,3) Blank
                          (2,1) -> newSpace (2,1) Blank; (2,2) -> newSpace (2,2) X;     (2,3) -> newSpace (2,3) Blank
                          (3,1) -> newSpace (3,1) Blank; (3,2) -> newSpace (3,2) Blank; (3,3) -> newSpace (3,3) Blank
spec :: Spec
spec =
  describe "minimax" $ do
    it "finds immediate win simple case" $
      minimax columnWin X `shouldBe` (3,1)

    it "finds immediate win more complex case" $
      minimax rowWin X `shouldBe` (1,3)

    it "prioritizes wins over blocking opponent's win" $
      minimax winAndOpponentWin X `shouldBe` (1,3)

    it "blocks an opponent from winning" $
      minimax blockOpponentWin X `shouldBe` (3,3)

    it "blocks an opponent from forking" $
      minimax blockOpponentFork X `shouldBe` (2,2)

    it "blocks an opponent from forking two" $
      minimax blockOpponentFork2 X `shouldBe` (3,3)

    it "blocks an opponent from winning two" $
      minimax blockOpponentWin2 O `shouldBe` (2,1)

    it "chooses corner when opponent's first move is in the center" $
      minimax blockFutureWin O `shouldBe` (3,3)
