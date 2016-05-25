module Game.BoardSpec (main, spec) where

import Data.Matrix
import Data.List hiding (toList)
import SpecHelper
import Game.Board

main :: IO ()
main = hspec spec

newSpace :: (Int, Int) -> Move -> Space
newSpace location move = Space { location = location, move = move }

tieBoard :: Board
tieBoard =
  matrix 3 3 $ \(i,j) -> case (i,j) of
                          (1,1) -> newSpace (1,1) O; (1,2) -> newSpace (1,2) X; (1,3) -> newSpace (1,3) O
                          (2,1) -> newSpace (2,1) O; (2,2) -> newSpace (2,2) X; (2,3) -> newSpace (2,3) X
                          (3,1) -> newSpace (3,1) X; (3,2) -> newSpace (3,2) O; (3,3) -> newSpace (3,3) X

columnWin :: Board
columnWin =
  matrix 3 3 $ \(i,j) -> case (i,j) of
                          (1,1) -> newSpace (1,1) X; (1,2) -> newSpace (1,2) Blank; (1,3) -> newSpace (1,3) Blank
                          (2,1) -> newSpace (2,1) X; (2,2) -> newSpace (2,2) Blank; (2,3) -> newSpace (2,3) Blank
                          (3,1) -> newSpace (3,1) X; (3,2) -> newSpace (3,2) Blank; (3,3) -> newSpace (3,3) Blank

rowWin :: Board
rowWin =
  matrix 3 3 $ \(i,j) -> case (i,j) of
                          (1,1) -> newSpace (1,1) X;     (1,2) -> newSpace (1,2) X;     (1,3) -> newSpace (1,3) X
                          (2,1) -> newSpace (2,1) Blank; (2,2) -> newSpace (2,2) Blank; (2,3) -> newSpace (2,3) Blank
                          (3,1) -> newSpace (3,1) Blank; (3,2) -> newSpace (3,2) Blank; (3,3) -> newSpace (3,3) Blank

diagonalWin :: Board
diagonalWin =
  matrix 3 3 $ \(i,j) -> case (i,j) of
                          (1,1) -> newSpace (1,1) X;     (1,2) -> newSpace (1,2) Blank; (1,3) -> newSpace (1,3) Blank
                          (2,1) -> newSpace (2,1) Blank; (2,2) -> newSpace (2,2) X;     (2,3) -> newSpace (2,3) Blank
                          (3,1) -> newSpace (3,1) Blank; (3,2) -> newSpace (3,2) Blank; (3,3) -> newSpace (3,3) X

oppositeDiagonalWin :: Board
oppositeDiagonalWin =
  matrix 3 3 $ \(i,j) -> case (i,j) of
                          (1,1) -> newSpace (1,1) Blank; (1,2) -> newSpace (1,2) Blank; (1,3) -> newSpace (1,3) X
                          (2,1) -> newSpace (2,1) Blank; (2,2) -> newSpace (2,2) X;     (2,3) -> newSpace (2,3) Blank
                          (3,1) -> newSpace (3,1) X;     (3,2) -> newSpace (3,2) Blank; (3,3) -> newSpace (3,3) Blank
spec :: Spec
spec = do

  describe "newBoard" $ do
    it "returns a new Board with all Blank moves" $
      move <$> toList (newBoard 3) `shouldBe` Data.List.replicate 9 Blank

    it "returns Board with all possible locations represented" $
      location <$> toList (newBoard 3) `shouldBe` [ (1,1), (1,2), (1,3)
                                                  , (2,1), (2,2), (2,3)
                                                  , (3,1), (3,2), (3,3)]

  describe "winner" $ do
    it "returns true if the given board contains a column win" $
      winner columnWin `shouldBe` True

    it "returns true if the given board contains a row win" $
      winner rowWin `shouldBe` True

    it "returns true if the given board contains a diagonal win" $
      winner diagonalWin `shouldBe` True

    it "returns true if the given board contains an opposite diagonal win" $
      winner oppositeDiagonalWin `shouldBe` True

    it "returns false if the given board does not contain a win" $
      winner tieBoard `shouldBe` False

  describe "tie" $ do
    it "returns true if the given board is a tie" $
      tie tieBoard `shouldBe` True

    it "returns false if the given board is a blank board" $
      tie (newBoard 3) `shouldBe` False

  describe "boardState" $ do
    it "returns Winner when winner is present" $
      boardState columnWin `shouldBe` Winner

    it "returns Tie when tie is present" $
      boardState tieBoard `shouldBe` Tie

    it "returns Undecided when game is ongoing" $
      boardState (newBoard 3) `shouldBe` Undecided
