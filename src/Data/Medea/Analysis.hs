{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Data.Medea.Analysis where

import Control.Monad.Except (MonadError(..))
import Control.Monad.State.Strict (evalStateT, gets, modify)
import Control.Monad (foldM)
import Data.Map.Strict (Map)
import Data.Medea.Parser.Identifier (Identifier, PrimTypeIdentifier(..), 
                                     startIdentifier, tryPrimType)
import Algebra.Graph.Acyclic.AdjacencyMap (AdjacencyMap, toAcyclic)

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Algebra.Graph.AdjacencyMap as Cyclic

import Data.Medea.Parser.Spec.Type (getReferences)

import qualified Data.Medea.Parser.Spec.Schemata as Schemata
import qualified Data.Medea.Parser.Spec.Schema as Schema

data AnalysisError = 
  DuplicateSchemaName Identifier |
  NoStartSchema |
  DanglingTypeReference Identifier |
  TypeRelationIsCyclic 

data TypeNode = 
  NullNode |
  BooleanNode |
  ObjectNode |
  ArrayNode |
  NumberNode |
  StringNode |
  AnyNode |
  CustomNode Identifier
  deriving (Eq, Ord, Show) 

intoAcyclic :: (MonadError AnalysisError m) => 
  [(TypeNode, TypeNode)] -> m (AdjacencyMap TypeNode)
intoAcyclic = maybe (throwError TypeRelationIsCyclic) pure . toAcyclic . Cyclic.edges

intoEdges :: (MonadError AnalysisError m) => 
  Map Identifier Schema.Specification -> Schema.Specification -> m [(TypeNode, TypeNode)]
intoEdges m spec = evalStateT (go [] spec startIdentifier) S.empty
  where go acc scm ident = do
          let newNode = CustomNode ident
          alreadySeen <- gets (S.member newNode)
          if alreadySeen
          then pure acc
          else do
            modify (S.insert newNode)
            case getReferences . Schema.types $ scm of
              [] -> pure ((newNode, NullNode) : acc)
              ell@(_ : _) -> (acc <>) . concat <$> traverse (resolveLinks newNode) ell
        resolveLinks u t = case tryPrimType t of
          Nothing -> case M.lookup t m of
            Nothing -> throwError . DanglingTypeReference $ t
            Just scm -> (:) <$> pure (u, CustomNode t) <*> go [] scm t
          Just prim -> pure . pure . (u,) . intoTypeNode $ prim
        intoTypeNode = \case
          NullIdentifier -> NullNode
          BooleanIdentifier -> BooleanNode
          ObjectIdentifier -> ObjectNode
          ArrayIdentifier -> ArrayNode
          NumberIdentifier -> NumberNode
          StringIdentifier -> StringNode

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