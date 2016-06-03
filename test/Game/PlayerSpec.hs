module Game.PlayerSpec (main, spec) where

import SpecHelper

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "newToken" $ do
    it "returns O when given a string O" $
      newToken "O" `shouldBe` O

    it "returns X when given a string X" $
      newToken "X" `shouldBe` X

  describe "newPlayer" $
    it "returns a new Player" $
      do let newPlayerType  = "Human"
         let newPlayerToken = "x"
         let newPlayerTest  = newPlayer newPlayerToken newPlayerType
         token newPlayerTest `shouldBe` X
         playerType newPlayerTest `shouldBe` Human

  describe "newPlayerType" $
    it "returns a new PlayerType" $
      do let human    = "Human"
         let computer = "Computer"
         let unknown  = ""
         let humanPlayer    = newPlayerType human
         let computerPlayer = newPlayerType computer
         let unknownPlayer  = newPlayerType unknown
         humanPlayer `shouldBe` Human
         computerPlayer `shouldBe` Computer
         unknownPlayer `shouldBe` Human
