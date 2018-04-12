{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- GHC 7.10 seems to require KindSignatures for the polymorph folds defined
-- below.
#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE KindSignatures #-}
#endif

module Data.Morphism.CataSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Morphism.Cata

import Data.Bool (bool)
import Data.Maybe (maybe)
import Data.Either (either)

data Unit = Unit
data Binary = Zero | One
data PolymorphSum a = PolymorphSum a
data PolymorphProduct a b = PolymorphProduct a b
data RegularRecursive a = Cons a (RegularRecursive a) | Empty
data RoseTree a = Node a [RoseTree a]

$(makeCata defaultOptions ''Unit)
$(makeCata defaultOptions ''Binary)
$(makeCata defaultOptions ''PolymorphSum)
$(makeCata defaultOptions ''PolymorphProduct)
$(makeCata defaultOptions ''RegularRecursive)
$(makeCata defaultOptions { cataName = "binaryFold" } ''Binary)
$(makeCata defaultOptions ''RoseTree)

$(makeCata defaultOptions { cataName = "bool'" } ''Bool)
$(makeCata defaultOptions { cataName = "maybe'" } ''Maybe)
$(makeCata defaultOptions { cataName = "either'" } ''Either)
$(makeCata defaultOptions { cataName = "foldr'" } ''[])

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "type support" $ do
    it "handles Unit" $ do
      unit True Unit `shouldBe` True
      unit "foo" Unit `shouldBe` "foo"

    it "handles simple sum types" $ do
      binary 'z' 'o' Zero `shouldBe` 'z'
      binary 'z' 'o' One `shouldBe` 'o'

    it "handles polymorph sum types" $ do
      polymorphSum show (PolymorphSum True) `shouldBe` "True"
      polymorphSum length (PolymorphSum "Frerich") `shouldBe` 7

    it "handles polymorph product types" $ do
      let fn = (\b x -> show (if b then x + 1 else x - 1)) :: Bool -> Int -> String
      polymorphProduct fn (PolymorphProduct True 99) `shouldBe` "100"
      polymorphProduct fn (PolymorphProduct False 88) `shouldBe` "87"

    it "handles regular recursive types" $ do
      let length' = regularRecursive (\_ acc -> acc + 1) 0 :: RegularRecursive a -> Int
      length' Empty `shouldBe` 0
      length' (Cons () (Cons () (Cons () Empty))) `shouldBe` 3
      length' (Cons 'a' (Cons 'b' Empty)) `shouldBe` 2

    it "handles rose trees" $ do
      let treeSum = roseTree (\x xs -> sum (x:xs)) :: RoseTree Int -> Int
      treeSum (Node 3 []) `shouldBe` 3
      treeSum (Node 0 [Node 1 [Node 2 [], Node 3 []], Node 4 [Node 5 [], Node 6 []], Node 7 [Node 8 [], Node 9 []]]) `shouldBe` 45

  describe "custom options" $
    it "allows customizing the function name" $ do
      binaryFold 'z' 'o' Zero `shouldBe` 'z'
      binaryFold 'z' 'o' One `shouldBe` 'o'

  describe "equivalence" $ do
    let checkBinaryEquiv f g a b = property (\x -> f a b x == g a b x)

    it "can be used to define bool" $
      checkBinaryEquiv bool bool' "false" "true"

    it "can be used to define maybe" $
      checkBinaryEquiv maybe maybe' "<empty>" (++ "!!!")

    it "can be used to define either" $
      checkBinaryEquiv either either' (show :: Bool -> String) (++ "!!!")

    it "can be used to define foldr" $
      -- Well, we can get 'foldr', but flipped.
      checkBinaryEquiv foldr (flip foldr') (\(_ :: Int) (acc :: Int) -> acc + 1) 0
