module Data.Pica.Types
  ( SubfieldCode (..),
    SubfieldValue (..),
    Subfield (..),
    FieldTag (..),
  )
where

import Data.Text (Text)

newtype SubfieldCode = SubfieldCode Char
  deriving (Show, Eq)

newtype SubfieldValue = SubfieldValue Text
  deriving (Show, Eq)

data Subfield = Subfield {_code :: SubfieldCode, _value :: SubfieldValue}
  deriving (Show, Eq)

newtype FieldTag = FieldTag Text
  deriving (Show, Eq)
