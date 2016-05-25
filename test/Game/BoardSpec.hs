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

spec :: Spec
spec = do
  describe "newBoard" $ do
    it "returns a new Board" $ do
      newBoard 3 `shouldBe` (Board $ matrix 3 3 $ \(i,j) -> ' ')

  describe "winner" $ do
    it "returns true if the given board contains a column win" $ do
      let winBoard = Board $ matrix 3 3 $ \(i,j) -> case (i,j) of (1,1) -> 'x'; (2,1) -> 'x'; (3,1) -> 'x'; (_,_) -> Game.Board.sentinel;
      winner winBoard `shouldBe` True

    it "returns true if the given board contains a row win" $ do
      let winBoard = Board $ Data.Matrix.transpose $ matrix 3 3 $ \(i,j) -> case (i,j) of (1,1) -> 'x'; (1,2) -> 'x'; (1,3) -> 'x'; (_,_) -> Game.Board.sentinel;
      winner winBoard `shouldBe` True

    it "returns true if the given board contains a diagonal win" $ do
      let winBoard = Board $ Data.Matrix.transpose $ matrix 3 3 $ \(i,j) -> case (i,j) of (1,1) -> 'x'; (2,2) -> 'x'; (3,3) -> 'x'; (_,_) -> Game.Board.sentinel;
      winner winBoard `shouldBe` True

    it "returns true if the given board contains an opposite diagonal win" $ do
      let winBoard = Board $ Data.Matrix.transpose $ matrix 3 3 $ \(i,j) -> case (i,j) of (1,3) -> 'x'; (2,2) -> 'x'; (3,1) -> 'x'; (_,_) -> Game.Board.sentinel;
      winner winBoard `shouldBe` True

    it "returns false if the given board does not contain a win" $ do
      let board = Board $ matrix 3 3 $ \(i,j) -> Game.Board.sentinel
      winner board `shouldBe` False

  describe "boardState" $ do
    it "returns Winner when winner is present" $ do
      let board = Board $ matrix 3 3 $ \(i,j) -> case i of 1 -> 'x'; 2 -> ' '; 3 -> Game.Board.sentinel
      boardState board `shouldBe` Winner

    it "returns Tie when tie is present" $ do
      boardState tieBoard `shouldBe` Tie

    it "returns Undecided when game is ongoing" $ do
      boardState (newBoard 3) `shouldBe` Undecided
    it "returns a new Board with all Blank moves" $
      move <$> toList (newBoard 3) `shouldBe` Data.List.replicate 9 Blank

    it "returns Board with all possible locations represented" $
      location <$> toList (newBoard 3) `shouldBe` [ (1,1), (1,2), (1,3)
                                                  , (2,1), (2,2), (2,3)
                                                  , (3,1), (3,2), (3,3)]
