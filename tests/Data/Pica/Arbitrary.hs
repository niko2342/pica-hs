module Data.Pica.Arbitrary () where

import Data.Char (isAlphaNum, isAscii)
import Data.Pica.Types (SubfieldCode (..))
import Test.QuickCheck

instance Arbitrary SubfieldCode where
  arbitrary = do
    c <- arbitrary `suchThat` (\c -> isAscii c && isAlphaNum c)
    return $ SubfieldCode c
