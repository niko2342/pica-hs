module Data.Pica.Parser
  ( -- * Parser functions
    parseSubfieldCode,
    parseSubfieldValue,
    parseSubfield,
    parseTag,
    parseOccurrence,
    parseField,
    parseRecord,
  )
where

import Control.Applicative (optional)
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

parseTag :: Parser Tag
parseTag = do
  p0 <- satisfy (\c -> c >= '0' && c <= '2')
  p1 <- satisfy isDigit -- TODO: use digit
  p2 <- satisfy isDigit -- TODO: use digit
  p3 <- satisfy (\c -> isAsciiUpper c || c == '@')
  return $ Tag $ T.pack [p0, p1, p2, p3]

parseOccurrence :: Parser Occurrence
parseOccurrence = do
  char '/'
  digits <- choice [count 3 (satisfy isDigit), count 2 (satisfy isDigit)]
  return $ Occurrence $ T.pack digits

parseField :: Parser Field
parseField =
  Field
    <$> parseTag
    <*> optional parseOccurrence
    <* char ' '
    <*> many' parseSubfield
    <* char '\RS'

parseRecord :: Parser Record
parseRecord = Record <$> many1 parseField <* atEnd
