module MainSpec where

import Test.Hspec

spec :: Spec
spec = do
  describe "hello test" $ do
    it "test" $ 3 `shouldBe` (3 :: Int)

  describe "hello test2" $ do
    it "hogehoge" $ "foo" `shouldBe` "foo"

