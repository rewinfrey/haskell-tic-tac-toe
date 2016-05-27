module Game.BoardSpec (main, spec) where

import Data.List hiding (toList)
import Data.Matrix
import Data.Monoid
import Game.Board
import SpecHelper

main :: IO ()
main = hspec spec

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

edgeColumnWin :: Board
edgeColumnWin =
  matrix 3 3 $ \(i,j) -> case (i,j) of
                          (1,1) -> newSpace (1,1) Blank; (1,2) -> newSpace (1,2) Blank; (1,3) -> newSpace (1,3) X
                          (2,1) -> newSpace (2,1) Blank; (2,2) -> newSpace (2,2) Blank; (2,3) -> newSpace (2,3) X
                          (3,1) -> newSpace (3,1) Blank; (3,2) -> newSpace (3,2) Blank; (3,3) -> newSpace (3,3) X

middleColumnWin :: Board
middleColumnWin =
  matrix 3 3 $ \(i,j) -> case (i,j) of
                          (1,1) -> newSpace (1,1) Blank; (1,2) -> newSpace (1,2) X; (1,3) -> newSpace (1,3) Blank
                          (2,1) -> newSpace (2,1) Blank; (2,2) -> newSpace (2,2) X; (2,3) -> newSpace (2,3) Blank
                          (3,1) -> newSpace (3,1) Blank; (3,2) -> newSpace (3,2) X; (3,3) -> newSpace (3,3) Blank

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

middleRowWinner :: Board
middleRowWinner =
  matrix 3 3 $ \(i,j) -> case (i,j) of
                          (1,1) -> newSpace (1,1) Blank; (1,2) -> newSpace (1,2) X; (1,3) -> newSpace (1,3) Blank
                          (2,1) -> newSpace (2,1) Blank; (2,2) -> newSpace (2,2) X; (2,3) -> newSpace (2,3) Blank
                          (3,1) -> newSpace (3,1) Blank; (3,2) -> newSpace (3,2) X; (3,3) -> newSpace (3,3) Blank
bottomRowWinner :: Board
bottomRowWinner =
  matrix 3 3 $ \(i,j) -> case (i,j) of
                          (1,1) -> newSpace (1,1) Blank; (1,2) -> newSpace (1,2) Blank; (1,3) -> newSpace (1,3) Blank
                          (2,1) -> newSpace (2,1) Blank; (2,2) -> newSpace (2,2) Blank; (2,3) -> newSpace (2,3) Blank
                          (3,1) -> newSpace (3,1) X;     (3,2) -> newSpace (3,2) X; (3,3)     -> newSpace (3,3) X

spec :: Spec
spec = do

  describe "newBoard" $ do
    it "returns a new Board with all Blank moves" $
      move <$> toList (newBoard 3) `shouldBe` Data.List.replicate 9 Blank

    it "returns Board with all possible locations represented" $
      location <$> toList (newBoard 3) `shouldBe` [ (1,1), (1,2), (1,3)
                                                  , (2,1), (2,2), (2,3)
                                                  , (3,1), (3,2), (3,3) ]

  describe "winner" $ do
    it "returns true if the given board contains a column win" $
      winner columnWin `shouldBe` True

    it "returns true if the given board contains an edge column win" $
      winner edgeColumnWin `shouldBe` True

    it "returns true if the given board contains a middle column win" $
      winner middleColumnWin `shouldBe` True

    it "returns true if the given board contains a row win" $
      winner rowWin `shouldBe` True

    it "returns true if the given board contains a middle row win" $
      winner middleRowWinner `shouldBe` True

    it "returns true if the given board contains a winner along the bottom edge" $
      winner bottomRowWinner `shouldBe` True

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
      tie newBlankBoard `shouldBe` False

  describe "boardState" $ do
    it "returns Winner when winner is present" $
      boardState columnWin `shouldBe` Winner

    it "returns Tie when tie is present" $
      boardState tieBoard `shouldBe` Tie

    it "returns Undecided when game is ongoing" $
      boardState newBlankBoard `shouldBe` Undecided

  describe "update" $ do
    it "updates a board for a given location and move" $ do
      let board = update newBlankBoard (1, 1) X
      move <$> Data.Matrix.toList board `shouldBe` [X] <> Data.List.replicate 8 Blank

    it "only updates a board for which a given location's Space is Blank" $ do
      let board = update (update newBlankBoard (1, 1) X) (1, 1) O
      move <$> Data.Matrix.toList board `shouldBe` [X] <> Data.List.replicate 8 Blank

  describe "availableSpaces" $ do
    it "returns all Spaces when given a new Board" $
      location <$> availableSpaces newBlankBoard `shouldBe` [ (1,1), (1,2), (1,3)
                                                            , (2,1), (2,2), (2,3)
                                                            , (3,1), (3,2), (3,3) ]

    it "returns only Blank Spaces when given an undecided Board" $
      location <$> availableSpaces (update newBlankBoard (1,1) X) `shouldBe` [        (1,2), (1,3)
                                                                             , (2,1), (2,2), (2,3)
                                                                             , (3,1), (3,2), (3,3) ]
