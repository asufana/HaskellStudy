{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Redundant list comprehension" #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Avoid lambda" #-}

module MainSpec where
import Test.Hspec

spec :: Spec
spec = do
  describe "一般操作" $ do
    it "integer" $ 3     `shouldBe` (3 :: Int)
    it "string"  $ "foo" `shouldBe` "foo"
  describe "リスト操作" $ do
    it "length"             $ length [1,2,3]      `shouldBe` (3 :: Int)
    it "empty"              $ null [1,2,3]        `shouldBe` False
    it "append last"        $ [1,2] ++ [3]        `shouldBe` ([1,2,3] :: [Int])
    it "append first(cons)" $ 1 : [2,3]           `shouldBe` ([1,2,3] :: [Int])
    it "pickup head"        $ head [1,2,3]        `shouldBe` (1::Int)
    it "pickup any"         $ [1,2,3] !! 1        `shouldBe` (2 :: Int)
    it "pickup last"        $ last [1,2,3]        `shouldBe` (3 :: Int)
    it "exclude head"       $ tail [1,2,3]        `shouldBe` ([2,3] :: [Int])
    it "exclude last"       $ init [1,2,3]        `shouldBe` ([1,2] :: [Int])
    it "reverse"            $ reverse [1,2,3]     `shouldBe` ([3,2,1] :: [Int])
    it "take"               $ take 2 [1,2,3]      `shouldBe` ([1,2] :: [Int])
    it "drop"               $ drop 2 [1,2,3]      `shouldBe` ([3] :: [Int])
    it "contains"           $ elem 1 [1::Int,2,3] `shouldBe` True
    it "range"              $ [1..3]              `shouldBe` ([1,2,3] :: [Int])
  describe "リスト内包表記" $ do
    -- Pyton, Erlang にもあるリストの中でリストを生成する構文
    it "show"          $ [x | x <- [1..3]]                     `shouldBe` ([1,2,3] :: [Int])
    it "foreach"       $ [x * 2 | x <- [1..3]]                 `shouldBe` ([2,4,6] :: [Int])
    it "filter"        $ [x * 2 | x <- [1..3], x >= 2]         `shouldBe` ([4,6] :: [Int])
    it "filter multi"  $ [x * 2 | x <- [1..3], x >= 2, x >= 3] `shouldBe` ([6] :: [Int])
  describe "リスト操作" $ do
    it "map"           $ map (+1) [1,2,3]                      `shouldBe` ([2,3,4] :: [Int])
    it "map lambda"    $ map (\x -> x+1) [1,2,3]               `shouldBe` ([2,3,4] :: [Int])
    it "filter"        $ filter (>=3) [1,2,3]                  `shouldBe` ([3] :: [Int])
    it "filter lambda" $ filter (\x -> x>=3) [1,2,3]           `shouldBe` ([3] :: [Int])
    it "filter map"    $ map (+1) (filter (>=3) [1,2,3])       `shouldBe` ([4] :: [Int])
  describe "リスト操作モナド" $ do
    it "functor"       $ fmap (+1) (Just 1)                    `shouldBe` (Just 2)
    it "functor"       $ fmap (+1) Nothing                     `shouldBe` (Nothing)
    it "applicative"   $ (<*>) (Just (+1)) (Just 1)            `shouldBe` (Just 2)
    it "applicative"   $ (<*>) (Just (+1)) (Nothing)           `shouldBe` (Nothing)
  --it "applicative"   $ (<*>) (Nothing) (Just 1)              `shouldBe` (Nothing) なぜかエラーとなる
    it "monado"        $ (>>=) (Just 1) (\x -> Just (x + 1))   `shouldBe` (Just 2)
    it "monado"        $ (>>=) (Nothing) (\x -> Just (x + 1))  `shouldBe` (Nothing)
  --it "monado"        $ (>>=) (Nothing) (\x -> Nothing)       `shouldBe` (Nothing)
