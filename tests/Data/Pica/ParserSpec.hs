module Data.Pica.ParserSpec (spec) where

import Data.Pica.Arbitrary
import Data.Pica.Parser (parseSubfieldCode)
import Data.Pica.Types (SubfieldCode (..))
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.Attoparsec
import Test.Hspec.QuickCheck

codeToChar :: SubfieldCode -> Char
codeToChar (SubfieldCode x) = x

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

    prop "successfully parses an arbitrary subield code" $
      \c -> T.pack [codeToChar c] ~> parseSubfieldCode `shouldParse` c
