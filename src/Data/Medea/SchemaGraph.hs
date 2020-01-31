module Data.Medea.SchemaGraph where

import Data.HashMap.Strict (HashMap)

import Data.Medea.Identifier (Identifier)
import Data.Medea.Schema (Schema)

newtype SchemaGraph = SchemaGraph (HashMap Identifier Schema)
  deriving (Eq)
