module Game.BoardSpec (main, spec) where

import Data.Matrix
import SpecHelper

main :: IO ()
main = hspec spec

tieBoard :: Board
tieBoard =
  Board $ matrix 3 3 $ \(i,j) -> case (i,j) of
                                   (1,1) -> 'o'; (1,2) -> 'x'; (1,3) -> 'o'
                                   (2,1) -> 'o'; (2,2) -> 'x'; (2,3) -> 'x'
                                   (3,1) -> 'x'; (3,2) -> 'o'; (3,3) -> 'x'

spec :: Spec
spec = do
  describe "newBoard" $ do
    it "returns a new Board" $ do
      newBoard 3 3 `shouldBe` (Board $ matrix 3 3 $ \(i,j) -> ' ')

  describe "winner" $ do
    it "returns true if the given board contains a column win" $ do
      let winBoard = Board $ matrix 3 3 $ \(i,j) -> case (i,j) of (1,1) -> 'x'; (2,1) -> 'x'; (3,1) -> 'x'; (_,_) -> ' ';
      winner winBoard `shouldBe` True

    it "returns true if the given board contains a row win" $ do
      let winBoard = Board $ Data.Matrix.transpose $ matrix 3 3 $ \(i,j) -> case (i,j) of (1,1) -> 'x'; (1,2) -> 'x'; (1,3) -> 'x'; (_,_) -> ' ';
      winner winBoard `shouldBe` True

    it "returns true if the given board contains a diagonal win" $ do
      let winBoard = Board $ Data.Matrix.transpose $ matrix 3 3 $ \(i,j) -> case (i,j) of (1,1) -> 'x'; (2,2) -> 'x'; (3,3) -> 'x'; (_,_) -> ' ';
      winner winBoard `shouldBe` True

    it "returns true if the given board contains an opposite diagonal win" $ do
      let winBoard = Board $ Data.Matrix.transpose $ matrix 3 3 $ \(i,j) -> case (i,j) of (1,3) -> 'x'; (2,2) -> 'x'; (3,1) -> 'x'; (_,_) -> ' ';
      winner winBoard `shouldBe` True

    it "returns false if the given board does not contain a win" $ do
      let board = Board $ matrix 3 3 $ \(i,j) -> ' '
      winner board `shouldBe` False

  describe "boardState" $ do
    it "returns Winner when winner is present" $ do
      let board = Board $ matrix 3 3 $ \(i,j) -> case i of 1 -> 'x'; 2 -> ' '; 3 -> ' '
      boardState board `shouldBe` Winner

    it "returns Tie when tie is present" $ do
      boardState tieBoard `shouldBe` Tie
