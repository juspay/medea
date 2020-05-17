{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Medea.Schema (Schema (..)) where

import Data.Map.Strict (Map)
import Data.Medea.Analysis (CompiledSchema)
import Data.Medea.Parser.Primitive (Identifier)

-- | A compiled Medea schema.
newtype Schema = Schema
  { compiledSchemata :: Map Identifier CompiledSchema
  }
  deriving newtype (Eq, Show)
