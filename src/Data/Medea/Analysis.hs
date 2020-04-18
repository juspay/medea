{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Data.Medea.Analysis where

import Prelude
import Algebra.Graph.Acyclic.AdjacencyMap (AdjacencyMap, toAcyclic)
import qualified Algebra.Graph.AdjacencyMap as Cyclic
import Control.Monad (foldM, when)
import Control.Monad.Except (MonadError (..))
import Control.Monad.State.Strict (evalStateT, gets, modify)
import Data.Coerce (coerce)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (isNothing)
import qualified Data.Map.Strict as M
import Data.Medea.JSONType (JSONType (..))
import Data.Medea.Parser.Primitive
  ( Identifier,
    PrimTypeIdentifier (..),
    MedeaString(..),
    Natural,
    isReserved,
    isStartIdent,
    startIdentifier,
    tryPrimType,
    typeOf,
  )
import qualified Data.Medea.Parser.Spec.Schema as Schema
import qualified Data.Medea.Parser.Spec.Schemata as Schemata
import qualified Data.Medea.Parser.Spec.Type as Type
import Data.Medea.Parser.Spec.Array (minLength, maxLength)
import Data.Medea.Parser.Spec.Object (properties, additionalAllowed)
import Data.Medea.Parser.Spec.Property (propSchema, propName, propOptional)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Vector as V

data AnalysisError
  = DuplicateSchemaName Identifier
  | NoStartSchema
  | DanglingTypeReference Identifier
  | TypeRelationIsCyclic
  | ReservedDefined Identifier
  | UnreachableSchemata
  | MinMoreThanMax Identifier
  | DanglingTypeRefProp Identifier
  | DuplicatePropName Identifier MedeaString

data TypeNode
  = AnyNode
  | PrimitiveNode JSONType
  | CustomNode Identifier
  deriving (Eq, Ord, Show)

data CompiledSchema = CompiledSchema {
  typesAs :: V.Vector TypeNode,
  minListLen :: Maybe Natural,
  maxListLen :: Maybe Natural,
  props :: HM.HashMap Text (TypeNode, Bool),
  additionalProps :: Bool
} deriving (Show)

intoAcyclic ::
  (MonadError AnalysisError m) =>
  [(TypeNode, TypeNode)] ->
  m (AdjacencyMap TypeNode)
intoAcyclic = maybe (throwError TypeRelationIsCyclic) pure . toAcyclic . Cyclic.edges

intoEdges ::
  (MonadError AnalysisError m) =>
  M.Map Identifier CompiledSchema ->
  CompiledSchema ->
  m [(TypeNode, TypeNode)]
intoEdges m compiledScm =
  evalStateT (go [] compiledScm startNode <* checkUnusedSchema <* checkUndefinedPropSchema) S.empty
  where
    startNode = CustomNode startIdentifier
    checkUnusedSchema = do
      reachableSchemas <- gets S.size 
      when (reachableSchemas < M.size m) $ throwError UnreachableSchemata
    checkUndefinedPropSchema =
      case filter isUndefinedNode . map fst . HM.elems . props $ compiledScm of
        (CustomNode ident):_ -> throwError $ DanglingTypeRefProp ident
        _ -> pure ()
      where
        isUndefinedNode (CustomNode ident) = isNothing . M.lookup ident $ m
        isUndefinedNode _                  = False
    go acc scm node = do
      alreadySeen <- gets (S.member node)
      if alreadySeen
      then pure acc
      else do
        modify (S.insert node)
        traverseRefs acc node $ V.toList . typesAs $ scm
    traverseRefs acc node refs =
      (acc <>) . concat <$> traverse (resolveLinks node) refs
    resolveLinks u t = case t of
      CustomNode ident -> case M.lookup ident m of
        Nothing -> throwError . DanglingTypeReference $ ident
        Just scm -> (:) <$> pure (u, t) <*> go [] scm t
      _  -> pure . pure $ (u,t)

intoMap ::
  (MonadError AnalysisError m) =>
  Schemata.Specification ->
  m (M.Map Identifier CompiledSchema)
intoMap (Schemata.Specification v) = foldM go M.empty v
  where
    go acc spec = M.alterF (checkedInsert spec) (Schema.name spec) acc
    checkedInsert spec = \case
      Nothing -> do
        when (isReserved ident && (not . isStartIdent) ident)
          $ throwError . ReservedDefined
          $ ident
        Just <$> compileSchema spec
      Just _ -> throwError . DuplicateSchemaName $ ident
      where
        ident = Schema.name spec

compileSchema ::
  (MonadError AnalysisError m) =>
  Schema.Specification ->
  m CompiledSchema
compileSchema scm = do
  when (minLength arraySpec > maxLength arraySpec) $
    throwError $ MinMoreThanMax schemaName
  propMap <- foldM go HM.empty (properties objSpec)
  pure $ CompiledSchema {
           typesAs         = defaultToAny $ fmap (identToNode . Just) types,
           minListLen      = coerce $ minLength arraySpec,
           maxListLen      = coerce $ maxLength arraySpec,
           props           = propMap,
           additionalProps = additionalAllowed objSpec
         }
    where
      Schema.Specification schemaName (Type.Specification types) arraySpec objSpec
        = scm
      go acc prop = HM.alterF (checkedInsert prop) (coerce $ propName prop) acc
      checkedInsert prop = \case
        Nothing -> pure . Just $ (identToNode (propSchema prop), propOptional prop)
        Just _  -> throwError $ DuplicatePropName schemaName (propName prop)
      defaultToAny vec | V.null vec = V.singleton AnyNode
                       | otherwise  = vec

identToNode :: Maybe Identifier -> TypeNode
identToNode ident = case ident of
  Nothing -> AnyNode
  Just t -> maybe (CustomNode t) (PrimitiveNode . typeOf) $ tryPrimType t

checkStartSchema ::
  (MonadError AnalysisError m) =>
  M.Map Identifier CompiledSchema ->
  m CompiledSchema
checkStartSchema m = case M.lookup startIdentifier m of
  Nothing -> throwError NoStartSchema
  Just scm -> pure scm
