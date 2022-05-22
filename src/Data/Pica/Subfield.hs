module Data.Pica.Subfield
  ( -- * Types
    Subfield (..),

    -- * Functions
    parseSubfield,
  )
where

import Data.Attoparsec.Text
import Data.Char (isAlphaNum, isAscii)
import qualified Data.Text as T

-- | A PICA+ Subfield
--
-- @since 0.10
data Subfield = Subfield {_code :: Char, _value :: T.Text}
  deriving (Show, Eq)

-- | Parse a subfield
--
-- @since 0.1.0
parseSubfield :: Parser Subfield
parseSubfield =
  Subfield <$ char '\US'
    <*> satisfy (\c -> isAscii c && isAlphaNum c)
    <*> takeTill (\c -> c == '\US' || c == '\RS')
