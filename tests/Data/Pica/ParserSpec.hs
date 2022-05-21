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

tagToText :: FieldTag -> T.Text
tagToText (FieldTag x) = x

occurrenceToText :: Occurrence -> T.Text
occurrenceToText (Occurrence o) = T.pack $ '/' : T.unpack o

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
        T.pack "foo" ~> parseSubfieldValue `shouldParse` SubfieldValue (T.pack "foo")

      it "successfully stops on record separator" $ do
        T.pack "foo\RSb" ~> parseSubfieldValue `shouldParse` SubfieldValue (T.pack "foo")

      it "successfully stops on unit separator" $ do
        T.pack "foo\USb" ~> parseSubfieldValue `shouldParse` SubfieldValue (T.pack "foo")

      prop "successfully parse an arbitrary subield value" $
        \s -> valueToText s ~> parseSubfieldValue `shouldParse` s

  describe "parseSubfield" $ do
    it "successfully parses subfields" $ do
      T.pack "\USa123" ~> parseSubfield `shouldParse` Subfield (SubfieldCode 'a') (SubfieldValue (T.pack "123"))

    prop "successfully parse an arbitrary subfield" $
      \s -> subfieldToText s ~> parseSubfield `shouldParse` s

  describe "parseFieldTag" $ do
    it "successfully parses field tag '003@'" $
      T.pack "003@" ~> parseFieldTag `shouldParse` FieldTag (T.pack "003@")

    it "should fail on invalid field tags" $ do
      parseFieldTag `shouldFailOn` T.pack "345@"
      parseFieldTag `shouldFailOn` T.pack "0A2A"
      parseFieldTag `shouldFailOn` T.pack "00AA"
      parseFieldTag `shouldFailOn` T.pack "001!"

    prop "successfully parse an arbitrary field tag" $
      \t -> tagToText t ~> parseFieldTag `shouldParse` t

  describe "parseOccurrence" $ do
    it "successfully parse occurrence '/01'" $
      T.pack "/01" ~> parseOccurrence `shouldParse` Occurrence (T.pack "01")

    it "successfully parse occurrence '/001'" $
      T.pack "/001" ~> parseOccurrence `shouldParse` Occurrence (T.pack "001")

    it "should fail on invalid occurrences" $ do
      parseOccurrence `shouldFailOn` T.pack "01"
      parseOccurrence `shouldFailOn` T.pack "/0A"

    prop "successfully parse an arbitrary field tag" $
      \o -> occurrenceToText o ~> parseOccurrence `shouldParse` o
