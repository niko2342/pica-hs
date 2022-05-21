module Data.Pica.Types
  ( SubfieldCode (..),
    SubfieldValue (..),
    Subfield (..),
    Tag (..),
    Occurrence (..),
    Field (..),
    Record (..),
  )
where

import Data.Text (Text)

newtype SubfieldCode = SubfieldCode Char
  deriving (Show, Eq)

newtype SubfieldValue = SubfieldValue Text
  deriving (Show, Eq)

data Subfield = Subfield {_code :: SubfieldCode, _value :: SubfieldValue}
  deriving (Show, Eq)

newtype Tag = Tag Text
  deriving (Show, Eq)

newtype Occurrence = Occurrence Text
  deriving (Show, Eq)

data Field = Field
  { _tag :: Tag,
    _occurrence :: Maybe Occurrence,
    _subfields :: [Subfield]
  }
  deriving (Show, Eq)

newtype Record = Record [Field]
  deriving (Show, Eq)
