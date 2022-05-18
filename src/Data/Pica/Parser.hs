module Data.Pica.Parser
  ( -- * Parser functions
    parseSubfieldCode,
  )
where

import Data.Attoparsec.Text
import Data.Char (isAlphaNum, isAscii)
import Data.Pica.Types (SubfieldCode (..))

parseSubfieldCode :: Parser SubfieldCode
parseSubfieldCode = do
  code <- satisfy (\c -> isAscii c && isAlphaNum c)
  return $ SubfieldCode code
