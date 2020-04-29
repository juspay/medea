module Data.Medea.Schema (Schema (..)) where

import Data.Map.Strict (Map)
import Data.Medea.Analysis (CompiledSchema)
import Data.Medea.Parser.Primitive (Identifier)

-- placeholder
newtype Schema
  = Schema
      { compiledSchemata :: Map Identifier CompiledSchema
      }
  deriving (Show)
