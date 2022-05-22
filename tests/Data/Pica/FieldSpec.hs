{-# LANGUAGE OverloadedStrings #-}

module Data.Pica.FieldSpec (spec) where

import Control.Exception (evaluate)
import Data.Pica.Field
import qualified Data.Text as T
import Test.Hspec

spec :: Spec
spec = do
  describe "level" $ do
    it "returns the level of a field" $ do
      level (Field "003@" Nothing []) `shouldBe` Main
      level (Field "123A" Nothing []) `shouldBe` Local
      level (Field "234A" Nothing []) `shouldBe` Copy

    it "throws an exception if used with an invalid tag" $ do
      evaluate (level (Field "345A" Nothing [])) `shouldThrow` anyException
      evaluate (level (Field "!45A" Nothing [])) `shouldThrow` anyException

  describe "range" $ do
    it "returns the range of a field" $ do
      range (Field "003@" Nothing []) `shouldBe` "03"
      range (Field "123A" Nothing []) `shouldBe` "23"
      range (Field "234A" Nothing []) `shouldBe` "34"

    it "throws an exception if used with an invalid tag" $ do
      evaluate (range (Field "35A" Nothing [])) `shouldThrow` anyException
