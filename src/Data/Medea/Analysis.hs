{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Data.Medea.Analysis
  ( AnalysisError (..),
    ArrayType (..),
    CompiledSchema (..),
    TypeNode (..),
    compileSchemata,
    arrayBounds,
  )
where

import Algebra.Graph.Acyclic.AdjacencyMap (toAcyclic)
import qualified Algebra.Graph.AdjacencyMap as Cyclic
import Control.Applicative ((<|>))
import Control.Monad (foldM, when)
import Control.Monad.Except (MonadError (..))
import Data.Can (Can (..))
import Data.Coerce (coerce)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NEList
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust, isNothing, mapMaybe)
import Data.Medea.JSONType (JSONType (..))
import Data.Medea.Parser.Primitive
  ( Identifier,
    MedeaString (..),
    Natural,
    PrimTypeIdentifier (..),
    ReservedIdentifier (..),
    identFromReserved,
    isReserved,
    isStartIdent,
    tryPrimType,
    typeOf,
  )
import Data.Medea.Parser.Spec.Array
  ( elementType,
    maxLength,
    minLength,
    tupleSpec,
  )
import Data.Medea.Parser.Spec.Object
  ( additionalAllowed,
    additionalSchema,
    properties,
  )
import Data.Medea.Parser.Spec.Property (propName, propOptional, propSchema)
import qualified Data.Medea.Parser.Spec.Schema as Schema
import qualified Data.Medea.Parser.Spec.Schemata as Schemata
import qualified Data.Medea.Parser.Spec.String as String
import qualified Data.Medea.Parser.Spec.Type as Type
import qualified Data.Set as S
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NESet
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Prelude

data AnalysisError
  = DuplicateSchemaName !Identifier
  | NoStartSchema
  | DanglingTypeReference !Identifier !Identifier
  | TypeRelationIsCyclic
  | ReservedDefined !Identifier
  | DefinedButNotUsed !Identifier
  | MinMoreThanMax !Identifier
  | DanglingTypeRefProp !Identifier !Identifier
  | DanglingTypeRefList !Identifier !Identifier
  | DanglingTypeRefTuple !Identifier !Identifier
  | DuplicatePropName !Identifier !MedeaString
  | PropertyWithoutObject !Identifier
  | ListWithoutArray !Identifier
  | TupleWithoutArray !Identifier
  | StringValsWithoutString !Identifier
  deriving stock (Eq, Show)

data TypeNode
  = AnyNode
  | PrimitiveNode !JSONType
  | CustomNode !Identifier
  deriving stock (Eq, Ord, Show)

data CompiledSchema = CompiledSchema
  { schemaNode :: !TypeNode,
    typesAs :: {-# UNPACK #-} !(NESet TypeNode),
    minArrayLen :: !(Maybe Natural),
    maxArrayLen :: !(Maybe Natural),
    arrayTypes :: !(Maybe ArrayType),
    props :: !(HashMap Text (TypeNode, Bool)),
    additionalProps :: !Bool,
    additionalPropSchema :: !TypeNode,
    stringVals :: {-# UNPACK #-} !(Vector Text)
  }
  deriving stock (Eq, Show)

arrayBounds :: CompiledSchema -> Can Natural Natural
arrayBounds scm = case (minArrayLen scm, maxArrayLen scm) of
  (Nothing, Nothing) -> Non
  (Just lo, Nothing) -> One lo
  (Nothing, Just hi) -> Eno hi
  (Just lo, Just hi) -> Two lo hi

data ArrayType
  = ListType !TypeNode
  | TupleType {-# UNPACK #-} !(Vector TypeNode)
  deriving stock (Eq, Show)

checkAcyclic ::
  (MonadError AnalysisError m) =>
  Map Identifier CompiledSchema ->
  m ()
checkAcyclic m =
  when (isNothing . toAcyclic . getTypesAsGraph $ m) $
    throwError TypeRelationIsCyclic

compileSchemata ::
  (MonadError AnalysisError m) =>
  Schemata.Specification ->
  m (Map Identifier CompiledSchema)
compileSchemata (Schemata.Specification v) = do
  m <- foldM go M.empty v
  checkStartSchema m
  checkDanglingReferences getTypeRefs DanglingTypeReference m
  checkDanglingReferences getPropertyTypeRefs DanglingTypeRefProp m
  checkDanglingReferences getListTypeRefs DanglingTypeRefList m
  checkDanglingReferences getTupleTypeRefs DanglingTypeRefTuple m
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
  let minListLen = minLength arraySpec
      maxListLen = maxLength arraySpec
  when (isJust minListLen && isJust maxListLen && minListLen > maxListLen)
    $ throwError
    $ MinMoreThanMax schemaName
  propMap <- foldM go HM.empty (maybe V.empty properties objSpec)
  let arrType = getArrayTypes (elementType arraySpec) (tupleSpec arraySpec)
      tupleLen = getTupleTypeLen arrType
      hasPropSpec = isJust objSpec
      compiledScm =
        CompiledSchema
          { schemaNode = identToNode . Just $ schemaName,
            typesAs = NESet.fromList . defaultToAny . V.toList . fmap (identToNode . Just) $ types,
            minArrayLen = minListLen <|> tupleLen,
            maxArrayLen = maxListLen <|> tupleLen,
            arrayTypes = arrType,
            props = propMap,
            additionalProps = maybe True additionalAllowed objSpec,
            additionalPropSchema = identToNode $ objSpec >>= additionalSchema,
            stringVals = String.toReducedSpec stringValsSpec
          }
  when (shouldNotHavePropertySpec compiledScm hasPropSpec)
    $ throwError . PropertyWithoutObject
    $ schemaName
  when (shouldNotHaveListSpec compiledScm)
    $ throwError . ListWithoutArray
    $ schemaName
  when (shouldNotHaveTupleSpec compiledScm)
    $ throwError . TupleWithoutArray
    $ schemaName
  when (shouldNotHaveStringSpec compiledScm)
    $ throwError . StringValsWithoutString
    $ schemaName
  pure compiledScm
  where
    Schema.Specification schemaName (Type.Specification types) stringValsSpec arraySpec objSpec =
      scm
    go acc prop = HM.alterF (checkedInsert prop) (coerce $ propName prop) acc
    checkedInsert prop = \case
      Nothing -> pure . Just $ (identToNode (propSchema prop), propOptional prop)
      Just _ -> throwError $ DuplicatePropName schemaName (propName prop)
    defaultToAny :: [TypeNode] -> NEList.NonEmpty TypeNode
    defaultToAny xs = case NEList.nonEmpty xs of
      Nothing -> (NEList.:|) AnyNode []
      Just xs' -> xs'

checkStartSchema ::
  (MonadError AnalysisError m) =>
  Map Identifier CompiledSchema ->
  m ()
checkStartSchema m = case M.lookup (identFromReserved RStart) m of
  Nothing -> throwError NoStartSchema
  Just _ -> pure ()

-- We need a 'getRefs' argument here so that we can differentiate between
-- different kinds of Dangling references(type/property/list/tuple).
checkDanglingReferences ::
  (MonadError AnalysisError m) =>
  (CompiledSchema -> [TypeNode]) ->
  (Identifier -> Identifier -> AnalysisError) ->
  Map Identifier CompiledSchema ->
  m ()
checkDanglingReferences getRefs err m = mapM_ go . M.toList $ m
  where
    go (schemaName, scm) = case getDanglingRefs scm of
      danglingRef : _ -> throwError $ err danglingRef schemaName
      [] -> pure ()
    getDanglingRefs = filter isUndefined . mapMaybe fromCustomNode . getRefs
    isUndefined ident = isNothing . M.lookup ident $ m
    fromCustomNode (CustomNode ident) = Just ident
    fromCustomNode _ = Nothing

checkUnusedSchemata ::
  (MonadError AnalysisError m) =>
  Map Identifier CompiledSchema ->
  m ()
checkUnusedSchemata m = mapM_ checkUnused . M.keys $ m
  where
    checkUnused ident
      | S.member (CustomNode ident) allReferences = pure ()
      | isStartIdent ident = pure ()
      | otherwise = throwError $ DefinedButNotUsed ident
    allReferences = S.unions . fmap getReferences . M.elems $ m
    getReferences scm =
      S.fromList $
        getTypeRefs scm ++ getPropertyTypeRefs scm ++ getListTypeRefs scm ++ getTupleTypeRefs scm

-- Helpers
identToNode :: Maybe Identifier -> TypeNode
identToNode ident = case ident of
  Nothing -> AnyNode
  Just t -> maybe (CustomNode t) (PrimitiveNode . typeOf) $ tryPrimType t

getTypeRefs :: CompiledSchema -> [TypeNode]
getTypeRefs = NEList.toList . NESet.toList . typesAs

getPropertyTypeRefs :: CompiledSchema -> [TypeNode]
getPropertyTypeRefs scm = (fmap fst . HM.elems . props $ scm) ++ [additionalPropSchema scm]

getListTypeRefs :: CompiledSchema -> [TypeNode]
getListTypeRefs scm = case arrayTypes scm of
  Just (ListType typeNode) -> [typeNode]
  _ -> []

getTupleTypeRefs :: CompiledSchema -> [TypeNode]
getTupleTypeRefs scm = case arrayTypes scm of
  Just (TupleType typeNodes) -> V.toList typeNodes
  _ -> []

getArrayTypes :: Maybe Identifier -> Maybe [Identifier] -> Maybe ArrayType
getArrayTypes Nothing Nothing = Nothing
getArrayTypes (Just ident) _ = Just . ListType . identToNode . Just $ ident
getArrayTypes _ (Just idents) =
  Just . TupleType . V.fromList $ identToNode . Just <$> idents

getTupleTypeLen :: Maybe ArrayType -> Maybe Natural
getTupleTypeLen (Just (TupleType types)) = Just . fromIntegral . V.length $ types
getTupleTypeLen _ = Nothing

getTypesAsGraph :: Map Identifier CompiledSchema -> Cyclic.AdjacencyMap TypeNode
getTypesAsGraph = Cyclic.edges . concatMap intoTypesAsEdges . M.elems

intoTypesAsEdges :: CompiledSchema -> [(TypeNode, TypeNode)]
intoTypesAsEdges scm = fmap (schemaNode scm,) . NEList.toList . NESet.toList . typesAs $ scm

arrayNode :: TypeNode
arrayNode = PrimitiveNode JSONArray

objectNode :: TypeNode
objectNode = PrimitiveNode JSONObject

stringNode :: TypeNode
stringNode = PrimitiveNode JSONString

hasListSpec :: CompiledSchema -> Bool
hasListSpec scm = case arrayTypes scm of
  Just (ListType _) -> True
  Just (TupleType _) -> False
  _ -> isJust $ minArrayLen scm <|> maxArrayLen scm

hasTupleSpec :: CompiledSchema -> Bool
hasTupleSpec scm = case arrayTypes scm of
  Just (TupleType _) -> True
  _ -> False

hasStringSpec :: CompiledSchema -> Bool
hasStringSpec = not . V.null . stringVals

shouldNotHavePropertySpec :: CompiledSchema -> Bool -> Bool
shouldNotHavePropertySpec scm hasPropSpec = hasPropSpec && (not . NESet.member objectNode . typesAs $ scm)

shouldNotHaveListSpec :: CompiledSchema -> Bool
shouldNotHaveListSpec scm = hasListSpec scm && (not . NESet.member arrayNode . typesAs $ scm)

shouldNotHaveTupleSpec :: CompiledSchema -> Bool
shouldNotHaveTupleSpec scm = hasTupleSpec scm && (not . NESet.member arrayNode . typesAs $ scm)

shouldNotHaveStringSpec :: CompiledSchema -> Bool
shouldNotHaveStringSpec scm = hasStringSpec scm && (not . NESet.member stringNode . typesAs $ scm)
