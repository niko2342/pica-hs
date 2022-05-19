module Data.Pica.Parser
  ( -- * Parser functions
    parseSubfieldCode,
    parseSubfieldValue,
  )
where

import Data.Attoparsec.Text
import Data.Char (isAlphaNum, isAscii)
import Data.Pica.Types (SubfieldCode (..), SubfieldValue (..))

parseSubfieldCode :: Parser SubfieldCode
parseSubfieldCode = do
  code <- satisfy (\c -> isAscii c && isAlphaNum c)
  return $ SubfieldCode code

parseSubfieldValue :: Parser SubfieldValue
parseSubfieldValue = do
  t <- takeTill (\c -> c == '\US' || c == '\RS')
  return $ SubfieldValue t
