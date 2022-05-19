module Data.Pica.Types
  ( SubfieldCode (..),
    SubfieldValue (..),
  )
where

import Data.Text (Text)

newtype SubfieldCode = SubfieldCode Char
  deriving (Show, Eq)

newtype SubfieldValue = SubfieldValue Text
  deriving (Show, Eq)
