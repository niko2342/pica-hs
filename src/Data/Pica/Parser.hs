module Data.Pica.Parser
  ( -- * Parser functions
    parseSubfieldCode,
    parseSubfieldValue,
    parseSubfield,
  )
where

import Data.Attoparsec.Text
import Data.Char (isAlphaNum, isAscii)
import Data.Pica.Types (Subfield (..), SubfieldCode (..), SubfieldValue (..))

parseSubfieldCode :: Parser SubfieldCode
parseSubfieldCode = SubfieldCode <$> satisfy (\c -> isAscii c && isAlphaNum c)

parseSubfieldValue :: Parser SubfieldValue
parseSubfieldValue = SubfieldValue <$> takeTill (\c -> c == '\US' || c == '\RS')

parseSubfield :: Parser Subfield
parseSubfield = Subfield <$ char '\US' <*> parseSubfieldCode <*> parseSubfieldValue
