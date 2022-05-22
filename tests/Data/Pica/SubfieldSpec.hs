{-# LANGUAGE OverloadedStrings #-}

module Data.Pica.SubfieldSpec (spec) where

import Control.Exception (evaluate)
import Control.Lens
import Data.Pica.Subfield
import qualified Data.Text as T
import Test.Hspec

spec :: Spec
spec = do
  describe "lenses" $ do
    it "returns the code of a subfield" $ do
      Subfield '0' "abc" ^. code `shouldBe` '0'

    it "returns the value of a subfield" $ do
      Subfield '0' "abc" ^. value `shouldBe` "abc"
