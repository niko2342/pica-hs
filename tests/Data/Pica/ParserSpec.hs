{-# LANGUAGE OverloadedStrings #-}

module Data.Pica.ParserSpec (spec) where

import Data.Pica.Arbitrary
import Data.Pica.Field (Field (..), parseField)
import Data.Pica.Record (Record (..), parseRecord)
import Data.Pica.Subfield (Subfield (..), parseSubfield)
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.Attoparsec
import Test.Hspec.QuickCheck

subfieldToText :: Subfield -> T.Text
subfieldToText (Subfield code value) = T.cons '\US' (T.cons code value)

subfieldsToText :: [Subfield] -> T.Text
subfieldsToText subfields = T.concat $ fmap subfieldToText subfields

fieldToText :: Field -> T.Text
fieldToText (Field tag (Just occurrence) subfields) =
  tag <> "/" <> occurrence <> " " <> subfieldsToText subfields <> "\RS"
fieldToText (Field tag Nothing subfields) =
  tag <> " " <> subfieldsToText subfields <> "\RS"

recordToText :: Record -> T.Text
recordToText (Record fields) = T.concat $ fmap fieldToText fields

spec :: Spec
spec = do
  describe "parseSubfield" $ do
    it "successfully parses subfields" $ do
      T.pack "\USa123" ~> parseSubfield `shouldParse` Subfield 'a' "123"

    prop "successfully parses arbitrary subfields" $
      \s -> subfieldToText s ~> parseSubfield `shouldParse` s

    it "successfully stops on record separator" $ do
      T.pack "\USa123\RSb" ~> parseSubfield `shouldParse` Subfield 'a' "123"

    it "successfully stops on unit separator" $ do
      T.pack "\USa123\USb" ~> parseSubfield `shouldParse` Subfield 'a' "123"

    it "should fail on invalid subfield codes '!' and '@'" $ do
      parseSubfield `shouldFailOn` T.pack "\US!123"
      parseSubfield `shouldFailOn` T.pack "\US@123"

  describe "parseField" $ do
    it "successfully parses fields" $ do
      T.pack "012A/01 \US0abc\RS" ~> parseField `shouldParse` Field "012A" (Just "01") [Subfield '0' "abc"]
      T.pack "003@ \US0123\RS" ~> parseField `shouldParse` Field "003@" Nothing [Subfield '0' "123"]

    prop "successfully parse arbitrary fields" $
      \f -> fieldToText f ~> parseField `shouldParse` f

    it "should fail on invalid field tags" $ do
      parseField `shouldFailOn` T.pack "345@ \US0123456789\RS"
      parseField `shouldFailOn` T.pack "0A2A \US0123456789\RS"
      parseField `shouldFailOn` T.pack "00AA \US0123456789\RS"
      parseField `shouldFailOn` T.pack "001! \US0123456789\RS"

    it "should fail on invalid occurrences" $ do
      parseField `shouldFailOn` T.pack "012A01 \US0123456789\RS"
      parseField `shouldFailOn` T.pack "012A/0A \US0123456789\RS"

  describe "parseRecord" $ do
    it "successfully parses records" $ do
      T.pack "003@ \US0123\RS" ~> parseRecord `shouldParse` Record [Field "003@" Nothing [Subfield '0' "123"]]

    prop "successfully parse an arbitrary record" $
      \r -> recordToText r ~> parseRecord `shouldParse` r
