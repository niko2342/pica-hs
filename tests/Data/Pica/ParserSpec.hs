module Data.Pica.ParserSpec (spec) where

import Data.Pica (SubfieldCode (..), SubfieldValue (..))
import Data.Pica.Arbitrary
import Data.Pica.Parser
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.Attoparsec
import Test.Hspec.QuickCheck

codeToChar :: SubfieldCode -> Char
codeToChar (SubfieldCode x) = x

valueToText :: SubfieldValue -> T.Text
valueToText (SubfieldValue x) = x

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
