module Data.Pica.Types
  ( SubfieldCode (..),
    SubfieldValue (..),
    Subfield (..),
    FieldTag (..),
    Occurrence (..),
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

newtype Occurrence = Occurrence Text
  deriving (Show, Eq)
