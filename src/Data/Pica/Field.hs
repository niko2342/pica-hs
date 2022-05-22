module Data.Pica.Field
  ( -- * Types
    Field (..),
    Level (..),

    -- * Functions
    parseField,
    level,
    range,
  )
where

import Control.Applicative (optional)
import Control.Exception (assert)
import Data.Attoparsec.Text
import Data.Char (isAsciiUpper)
import Data.Pica.Subfield
import qualified Data.Text as T

-- | A PICA+ field
--
-- @since 0.1.0
data Field = Field
  { _tag :: T.Text,
    _occurrence :: Maybe T.Text,
    _subfields :: [Subfield]
  }
  deriving (Show, Eq)

-- | Parse Tag
--
-- @since 0.1.0
parseTag :: Parser T.Text
parseTag = do
  p0 <- satisfy (\c -> c >= '0' && c <= '2')
  p1 <- digit
  p2 <- digit
  p3 <- satisfy (\c -> isAsciiUpper c || c == '@')
  return $ T.pack [p0, p1, p2, p3]

-- | Parse an occurrence
--
-- @since 0.1.0
parseOccurrence :: Parser T.Text
parseOccurrence = T.pack <$ char '/' <*> choice [count 3 digit, count 2 digit]

-- | Parse a field
--
-- @since 0.1.0
parseField :: Parser Field
parseField =
  Field
    <$> parseTag
    <*> optional parseOccurrence
    <* char ' '
    <*> many' parseSubfield
    <* char '\RS'

-- | The Level of a field.
--
-- @since 0.1.0
data Level = Main | Local | Copy
  deriving (Eq, Show)

-- | Returns the level of a field.
--
-- @since 0.1.0
level :: Field -> Level
level (Field tag _ _) = case T.uncons tag of
  Just ('0', _) -> Main
  Just ('1', _) -> Local
  Just ('2', _) -> Copy
  _ -> error "invalid tag"

-- | Returns the range of a field.
--
-- @since 0.1.0
range :: Field -> T.Text
range (Field tag _ _) = case T.length tag of
  4 -> T.take 2 $ T.drop 1 tag
  _ -> error "invalid tag"
