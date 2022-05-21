{-# LANGUAGE OverloadedStrings #-}

module Data.Pica.ParserSpec (spec) where

import Data.Pica.Arbitrary
import Data.Pica.Parser
import Data.Pica.Types
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.Attoparsec
import Test.Hspec.QuickCheck

codeToChar :: SubfieldCode -> Char
codeToChar (SubfieldCode x) = x

valueToText :: SubfieldValue -> T.Text
valueToText (SubfieldValue x) = x

subfieldToText :: Subfield -> T.Text
subfieldToText (Subfield code value) = T.pack $ '\US' : scode : svalue
  where
    svalue = T.unpack $ valueToText value
    scode = codeToChar code

tagToText :: Tag -> T.Text
tagToText (Tag x) = x

occurrenceToText :: Occurrence -> T.Text
occurrenceToText (Occurrence o) = T.pack $ '/' : T.unpack o

subfieldsToText :: [Subfield] -> T.Text
subfieldsToText subfields = T.concat $ fmap subfieldToText subfields

fieldToText :: Field -> T.Text
fieldToText (Field (Tag tag) (Just occurrence) subfields) =
  tag <> occurrenceToText occurrence <> " " <> subfieldsToText subfields <> "\RS"
fieldToText (Field (Tag tag) Nothing subfields) =
  tag <> " " <> subfieldsToText subfields <> "\RS"

recordToText :: Record -> T.Text
recordToText (Record fields) = T.concat $ fmap fieldToText fields

spec :: Spec
spec = do
  describe "parseSubfieldCode" $ do
    it "successfully parses subield codes '0', '9', 'A', 'Z', 'a', 'z'" $ do
      T.pack "0" ~> parseSubfieldCode `shouldParse` SubfieldCode '0'
      T.pack "9" ~> parseSubfieldCode `shouldParse` SubfieldCode '9'
      T.pack "A" ~> parseSubfieldCode `shouldParse` SubfieldCode 'A'
      T.pack "Z" ~> parseSubfieldCode `shouldParse` SubfieldCode 'Z'
      T.pack "a" ~> parseSubfieldCode `shouldParse` SubfieldCode 'a'
      T.pack "z" ~> parseSubfieldCode `shouldParse` SubfieldCode 'z'

    it "should fail on invalid subfield codes '!' and '@'" $ do
      parseSubfieldCode `shouldFailOn` T.pack "!"
      parseSubfieldCode `shouldFailOn` T.pack "@"

    prop "successfully parse an arbitrary subield code" $
      \c -> T.pack [codeToChar c] ~> parseSubfieldCode `shouldParse` c

    describe "parseSubfieldValue" $ do
      it "successfully parses subfield value 'foo'" $ do
        T.pack "foo" ~> parseSubfieldValue `shouldParse` SubfieldValue "foo"

      it "successfully stops on record separator" $ do
        T.pack "foo\RSb" ~> parseSubfieldValue `shouldParse` SubfieldValue "foo"

      it "successfully stops on unit separator" $ do
        T.pack "foo\USb" ~> parseSubfieldValue `shouldParse` SubfieldValue "foo"

      prop "successfully parse an arbitrary subield value" $
        \s -> valueToText s ~> parseSubfieldValue `shouldParse` s

  describe "parseSubfield" $ do
    it "successfully parses subfields" $ do
      T.pack "\USa123" ~> parseSubfield `shouldParse` Subfield (SubfieldCode 'a') (SubfieldValue "123")

    prop "successfully parse an arbitrary subfield" $
      \s -> subfieldToText s ~> parseSubfield `shouldParse` s

  describe "parseFieldTag" $ do
    it "successfully parses field tag '003@'" $
      T.pack "003@" ~> parseTag `shouldParse` Tag (T.pack "003@")

    it "should fail on invalid field tags" $ do
      parseTag `shouldFailOn` T.pack "345@"
      parseTag `shouldFailOn` T.pack "0A2A"
      parseTag `shouldFailOn` T.pack "00AA"
      parseTag `shouldFailOn` T.pack "001!"

    prop "successfully parse an arbitrary field tag" $
      \t -> tagToText t ~> parseTag `shouldParse` t

  describe "parseOccurrence" $ do
    it "successfully parse occurrence '/01'" $
      T.pack "/01" ~> parseOccurrence `shouldParse` Occurrence "01"

    it "successfully parse occurrence '/001'" $
      T.pack "/001" ~> parseOccurrence `shouldParse` Occurrence "001"

    it "should fail on invalid occurrences" $ do
      parseOccurrence `shouldFailOn` T.pack "01"
      parseOccurrence `shouldFailOn` T.pack "/0A"

    prop "successfully parse an arbitrary field tag" $
      \o -> occurrenceToText o ~> parseOccurrence `shouldParse` o

  describe "parseField" $ do
    it "successfully parses fields" $ do
      T.pack "003@ \US0123456789\RS" ~> parseField `shouldParse` Field {_tag = Tag "003@", _occurrence = Nothing, _subfields = [Subfield {_code = SubfieldCode '0', _value = SubfieldValue "123456789"}]}

    prop "successfully parse an arbitrary field" $
      \f -> fieldToText f ~> parseField `shouldParse` f

  describe "parseRecord" $ do
    it "successfully parses records" $ do
      T.pack "003@ \US0123456789\RS" ~> parseRecord `shouldParse` Record [Field {_tag = Tag "003@", _occurrence = Nothing, _subfields = [Subfield {_code = SubfieldCode '0', _value = SubfieldValue "123456789"}]}]

    prop "successfully parse an arbitrary record" $
      \r -> recordToText r ~> parseRecord `shouldParse` r
