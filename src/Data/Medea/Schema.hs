module Data.Medea.Schema where

import Data.Vector (Vector)

import Data.Medea.Identifier (Identifier)

newtype Schema = Schema {
  typeInfo :: Vector Identifier
}
  deriving (Eq)
