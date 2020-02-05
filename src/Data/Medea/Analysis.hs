{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Data.Medea.Analysis where

import Control.Monad.Except (MonadError(..))
import Control.Monad (foldM)
import Data.Map.Strict (Map)
import Data.Medea.Parser.Identifier (Identifier, startIdentifier)

import qualified Data.Map.Strict as M

import qualified Data.Medea.Parser.Spec.Schemata as Schemata
import qualified Data.Medea.Parser.Spec.Schema as Schema

data AnalysisError = 
  DuplicateSchemaName Identifier |
  NoStartSchema 

intoMap :: (MonadError AnalysisError m) => 
  Schemata.Specification -> m (Map Identifier Schema.Specification)
intoMap (Schemata.Specification v) = foldM go M.empty v
  where go acc spec = M.alterF (checkedInsert spec) (Schema.name spec) acc
        checkedInsert spec = \case 
          Nothing -> pure . Just $ spec
          Just _ -> throwError . DuplicateSchemaName . Schema.name $ spec

checkStartSchema :: (MonadError AnalysisError m) => 
  Map Identifier Schema.Specification -> m Schema.Specification
checkStartSchema m = case M.lookup startIdentifier m of
  Nothing -> throwError NoStartSchema
  Just scm -> pure scm
