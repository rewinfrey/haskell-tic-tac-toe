module Game.PlayerSpec (main, spec) where

import SpecHelper

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "newPlayer" $ do
    it "returns a new Player" $ do
      let newPlayerType  = Human
      let newPlayerToken = 'x'
      let newPlayerTest  = newPlayer newPlayerToken newPlayerType
      token newPlayerTest `shouldBe` newPlayerToken
      playerType newPlayerTest `shouldBe` newPlayerType

  describe "newPlayerType" $ do
    it "returns a new PlayerType" $ do
      let human    = "human"
      let computer = "computer"
      let unknown  = ""
      let humanPlayer    = newPlayerType human
      let computerPlayer = newPlayerType computer
      let unknownPlayer  = newPlayerType unknown
      humanPlayer `shouldBe` Human
      computerPlayer `shouldBe` Computer
      unknownPlayer `shouldBe` Human

