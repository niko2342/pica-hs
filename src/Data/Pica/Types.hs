module Data.Pica.Types
  ( SubfieldCode (..),
  )
where

import Data.Text (Text)

newtype SubfieldCode = SubfieldCode Char
  deriving (Show, Eq)
