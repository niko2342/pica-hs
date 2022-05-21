module Data.Pica.Parser
  ( -- * Parser functions
    parseSubfieldCode,
    parseSubfieldValue,
    parseSubfield,
    parseFieldTag,
  )
where

import Data.Attoparsec.Text
import Data.Char (isAlphaNum, isAscii, isAsciiUpper, isDigit)
import Data.Pica.Types
import qualified Data.Text as T

parseSubfieldCode :: Parser SubfieldCode
parseSubfieldCode = SubfieldCode <$> satisfy (\c -> isAscii c && isAlphaNum c)

parseSubfieldValue :: Parser SubfieldValue
parseSubfieldValue = SubfieldValue <$> takeTill (\c -> c == '\US' || c == '\RS')

parseSubfield :: Parser Subfield
parseSubfield = Subfield <$ char '\US' <*> parseSubfieldCode <*> parseSubfieldValue

parseFieldTag :: Parser FieldTag
parseFieldTag = do
  p0 <- satisfy (\c -> c >= '0' && c <= '2')
  p1 <- satisfy isDigit
  p2 <- satisfy isDigit
  p3 <- satisfy (\c -> isAsciiUpper c || c == '@')
  return $ FieldTag $ T.pack [p0, p1, p2, p3]
