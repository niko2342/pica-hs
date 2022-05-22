{-# LANGUAGE OverloadedStrings #-}

module Data.Pica.FieldSpec (spec) where

import Control.Exception (evaluate)
import Data.Pica.Field
import qualified Data.Text as T
import Test.Hspec

spec :: Spec
spec = do
  describe "level" $ do
    it "throws an exception if used with an invalid tag" $ do
      evaluate (level (Field "345A" Nothing [])) `shouldThrow` anyException
      evaluate (level (Field "!45A" Nothing [])) `shouldThrow` anyException

    it "returns the level of a field" $ do
      level (Field "003@" Nothing []) `shouldBe` Main
      level (Field "123A" Nothing []) `shouldBe` Local
      level (Field "234A" Nothing []) `shouldBe` Copy
