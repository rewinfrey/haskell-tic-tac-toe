module Data.String.StripSpec (main, spec) where

import SpecHelper

import Test.QuickCheck

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "strip" $ do
    it "removes leading and trailing whitespace" $ do
      strip "\t  foo bar\n" `shouldBe` "foo bar"

    it "is idempotent" $ property $
      \str -> strip str == strip (strip str)

    it "is falsey" $ do
      True `shouldBe` False


