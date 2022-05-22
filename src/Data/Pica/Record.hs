module Data.Pica.Record
  ( -- * Types
    Record (..),

    -- * Functions
    parseRecord,
  )
where

import Data.Attoparsec.Text
import Data.Pica.Field (Field, parseField)

-- | A PICA+ Record
--
-- @since 0.1.0
newtype Record = Record [Field]
  deriving (Show, Eq)

-- | Parse a record
--
-- @since 0.1.0
parseRecord :: Parser Record
parseRecord = Record <$> many1 parseField <* atEnd
