{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Data.Medea.Analysis where

import Prelude
import Algebra.Graph.Acyclic.AdjacencyMap (toAcyclic)
import qualified Algebra.Graph.AdjacencyMap as Cyclic
import Control.Monad (foldM, when)
import Control.Monad.Except (MonadError (..))
import Data.Coerce (coerce)
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NEList
import Data.Maybe (isNothing, mapMaybe)
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
import qualified Data.Medea.Parser.Spec.String as String
import Data.Medea.Parser.Spec.Array (minLength, maxLength)
import Data.Medea.Parser.Spec.Object (properties, additionalAllowed)
import Data.Medea.Parser.Spec.Property (propSchema, propName, propOptional)
import qualified Data.Set as S
import qualified Data.Set.NonEmpty as NESet
import Data.Text (Text)
import qualified Data.Vector as V

data AnalysisError
  = DuplicateSchemaName Identifier
  | NoStartSchema
  | DanglingTypeReference Identifier Identifier
  | TypeRelationIsCyclic
  | ReservedDefined Identifier
  | DefinedButNotUsed Identifier 
  | MinMoreThanMax Identifier
  | DanglingTypeRefProp Identifier Identifier
  | DuplicatePropName Identifier MedeaString

data TypeNode
  = AnyNode
  | PrimitiveNode JSONType
  | CustomNode Identifier
  deriving (Eq, Ord, Show)

data CompiledSchema = CompiledSchema {
  schemaNode :: TypeNode,
  typesAs :: NESet.NESet TypeNode,
  minListLen :: Maybe Natural,
  maxListLen :: Maybe Natural,
  props :: HM.HashMap Text (TypeNode, Bool),
  additionalProps :: Bool,
  stringVals :: V.Vector Text
} deriving (Show)

checkAcyclic ::
  (MonadError AnalysisError m) =>
  M.Map Identifier CompiledSchema ->
  m ()
checkAcyclic m = when (isNothing . toAcyclic . getTypesAsGraph $ m)
    $ throwError TypeRelationIsCyclic

compileSchemata ::
  (MonadError AnalysisError m) =>
  Schemata.Specification ->
  m (M.Map Identifier CompiledSchema)
compileSchemata (Schemata.Specification v) = do
  m <- foldM go M.empty v
  checkStartSchema m
  checkDanglingReferences getTypeRefs DanglingTypeReference m
  checkDanglingReferences getPropertyTypeRefs DanglingTypeRefProp m
  checkUnusedSchemata m
  checkAcyclic m
  pure m
  where
    go acc spec = M.alterF (checkedInsert spec) (Schema.name spec) acc
    checkedInsert spec = \case
      Nothing -> Just <$> compileSchema spec
      Just _ -> throwError . DuplicateSchemaName $ ident
      where
        ident = Schema.name spec

compileSchema ::
  (MonadError AnalysisError m) =>
  Schema.Specification ->
  m CompiledSchema
compileSchema scm = do
  when (isReserved schemaName && (not . isStartIdent) schemaName)
    $ throwError . ReservedDefined
    $ schemaName 
  when (minLength arraySpec > maxLength arraySpec) $
    throwError $ MinMoreThanMax schemaName
  propMap <- foldM go HM.empty (properties objSpec)
  pure $ CompiledSchema {
      schemaNode      = identToNode . Just $ schemaName,
      typesAs         = NESet.fromList . defaultToAny . V.toList . fmap (identToNode . Just) $ types,
      minListLen      = coerce $ minLength arraySpec,
      maxListLen      = coerce $ maxLength arraySpec,
      props           = propMap,
      additionalProps = additionalAllowed objSpec,
      stringVals      = String.toReducedSpec stringValsSpec
    }
    where
      Schema.Specification schemaName (Type.Specification types)  stringValsSpec arraySpec objSpec
        = scm
      go acc prop = HM.alterF (checkedInsert prop) (coerce $ propName prop) acc
      checkedInsert prop = \case
        Nothing -> pure . Just $ (identToNode (propSchema prop), propOptional prop)
        Just _  -> throwError $ DuplicatePropName schemaName (propName prop)
      defaultToAny :: [TypeNode] -> NEList.NonEmpty TypeNode
      defaultToAny xs = case NEList.nonEmpty xs of
        Nothing  -> (NEList.:|) AnyNode []
        Just xs' -> xs'

checkStartSchema ::
  (MonadError AnalysisError m) =>
  M.Map Identifier CompiledSchema ->
  m ()
checkStartSchema m = case M.lookup startIdentifier m of
  Nothing -> throwError NoStartSchema
  Just _ -> pure ()

-- We need a 'getRefs' argument here so that we can differentiate between
-- different kinds of Dangling references(type/property/list/tuple).
checkDanglingReferences ::
  (MonadError AnalysisError m) =>
  (CompiledSchema -> [TypeNode]) ->
  (Identifier -> Identifier -> AnalysisError) ->
  M.Map Identifier CompiledSchema ->
  m ()
checkDanglingReferences getRefs err m = mapM_ go . M.toList $ m
  where
    go (schemaName, scm) = case getDanglingRefs scm of
      danglingRef:_  -> throwError $ err danglingRef schemaName
      []                 -> pure ()
    getDanglingRefs = filter isUndefined . mapMaybe fromCustomNode . getRefs
    isUndefined ident = isNothing . M.lookup ident $ m
    fromCustomNode (CustomNode ident) = Just ident
    fromCustomNode _                  = Nothing

checkUnusedSchemata ::
  (MonadError AnalysisError m) =>
  M.Map Identifier CompiledSchema ->
  m ()
checkUnusedSchemata m = mapM_ checkUnused . M.keys $ m
  where
    checkUnused ident 
      | S.member (CustomNode ident) allReferences = pure ()
      | isStartIdent ident = pure ()
      | otherwise = throwError $ DefinedButNotUsed ident
    allReferences = S.unions . fmap getReferences . M.elems $ m
    getReferences scm = S.fromList $ getTypeRefs scm ++ getPropertyTypeRefs scm

-- Helpers
identToNode :: Maybe Identifier -> TypeNode
identToNode ident = case ident of
  Nothing -> AnyNode
  Just t -> maybe (CustomNode t) (PrimitiveNode . typeOf) $ tryPrimType t

getTypeRefs :: CompiledSchema -> [TypeNode]
getTypeRefs = NEList.toList . NESet.toList . typesAs

getPropertyTypeRefs :: CompiledSchema -> [TypeNode]
getPropertyTypeRefs = fmap fst . HM.elems . props

getTypesAsGraph :: M.Map Identifier CompiledSchema -> Cyclic.AdjacencyMap TypeNode
getTypesAsGraph = Cyclic.edges . concatMap intoTypesAsEdges . M.elems

intoTypesAsEdges :: CompiledSchema -> [(TypeNode, TypeNode)]
intoTypesAsEdges scm = fmap (schemaNode scm,) . NEList.toList . NESet.toList . typesAs $ scm
