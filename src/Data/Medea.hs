{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module: Data.Medea
-- Description: A JSON schema language validator.
-- Copyright: (C) Juspay Technologies Pvt Ltd, 2020
-- License: MIT
-- Maintainer: koz.ross@retro-freedom.nz
-- Stability: Experimental
-- Portability: GHC only
--
-- This module contains the reference Haskell implementation of a Medea
-- validator, providing both schema graph file loading and validation, with some
-- convenience functions.
--
-- A minimal example of use follows. This example first attempts to load a Medea
-- schema graph file from @\/path\/to\/schema.medea@, and, if successful, attempts
-- to validate the JSON file at @\/path\/to\/my.json@ against the schemata so
-- loaded.
--
-- > import Data.Medea (loadSchemaFromFile, validateFromFile)
-- >
-- > main :: IO ()
-- > main = do
-- >   -- try to load the schema graph file
-- >   loaded <- loadSchemaFromFile "/path/to/schema.medea"
-- >   case loaded of
-- >      Left err -> print err -- or some other handling
-- >      Right scm -> do
-- >        -- try to validate
-- >        validated <- validateFromFile scm "/path/to/my.json"
-- >        case validated of
-- >          Left err -> print err -- or some other handling
-- >          Right validJson -> print validJson -- or some other useful thing
--
-- For more details about how to create Medea schema graph files, see
-- @TUTORIAL.md@ and @SPEC.md@.
module Data.Medea
  ( -- * Schema loading
    Schema,
    LoaderError (..),
    ParseError (..),
    buildSchema,
    loadSchemaFromFile,
    loadSchemaFromHandle,

    -- * Schema validation
    JSONType (..),
    SchemaInformation (..),
    ValidationError (..),
    ValidatedJSON,
    toValue,
    validAgainst,
    validate,
    validateFromFile,
    validateFromHandle,
  )
where

import Control.Applicative (Alternative (..))
import Control.Comonad.Cofree (Cofree (..))
import Control.DeepSeq (NFData (..))
import Control.Monad (unless)
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.RWS.Strict (RWST (..), evalRWST)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State.Strict (MonadState (..), gets)
import Data.Aeson (Array, Object, Value (..), decodeStrict)
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Can (Can (..))
import Data.Coerce (coerce)
import Data.Data (Data)
import Data.Foldable (asum, traverse_)
import Data.Functor (($>))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable (..))
import qualified Data.Map.Strict as M
import Data.Medea.Analysis (ArrayType (..), CompiledSchema (..), TypeNode (..), arrayBounds)
import Data.Medea.JSONType (JSONType (..), typeOf)
import Data.Medea.Loader
  ( LoaderError (..),
    buildSchema,
    loadSchemaFromFile,
    loadSchemaFromHandle,
  )
import Data.Medea.Parser.Primitive (Identifier (..), ReservedIdentifier (..), identFromReserved)
import Data.Medea.Parser.Types (ParseError (..))
import Data.Medea.Schema (Schema (..))
import Data.Medea.ValidJSON (ValidJSONF (..))
import qualified Data.Set as S
import Data.Set.NonEmpty
  ( NESet,
    dropWhileAntitone,
    findMin,
    member,
    singleton,
  )
import Data.Text (Text)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import System.IO (Handle)

-- | An annotation, describing which schema a given chunk of JSON was deemed to
-- be valid against.
data SchemaInformation
  = -- | No requirements were placed on this chunk.
    AnySchema
  | -- | Validated as JSON @null@.
    NullSchema
  | -- | Validated as JSON boolean.
    BooleanSchema
  | -- | Validated as JSON number.
    NumberSchema
  | -- | Validated as JSON string.
    StringSchema
  | -- | Validated as JSON array.
    ArraySchema
  | -- | Validated as JSON object.
    ObjectSchema
  | -- | Validated against the start schema.
    StartSchema
  | -- | Validated against the schema with the given name.
    UserDefined {-# UNPACK #-} !Text
  deriving stock (Eq, Data, Show, Generic)
  deriving anyclass (Hashable, NFData)

-- | JSON, annotated with what schemata it was deemed valid against.
newtype ValidatedJSON = ValidatedJSON (Cofree ValidJSONF SchemaInformation)
  deriving stock (Data)
  deriving newtype (Eq, Show)

-- Can't coerce-erase the constructor fmap, sigh
instance NFData ValidatedJSON where
  {-# INLINE rnf #-}
  rnf (ValidatedJSON (x :< f)) =
    rnf x `seq` (rnf . fmap ValidatedJSON $ f)

-- Nor here
instance Hashable ValidatedJSON where
  {-# INLINE hashWithSalt #-}
  hashWithSalt salt (ValidatedJSON (x :< f)) =
    salt `hashWithSalt` x `hashWithSalt` fmap ValidatedJSON f

-- | Convert to an Aeson 'Value', throwing away all schema information.
toValue :: ValidatedJSON -> Value
toValue (ValidatedJSON (_ :< f)) = case f of
  AnythingF v -> v
  NullF -> Null
  BooleanF b -> Bool b
  NumberF n -> Number n
  StringF s -> String s
  ArrayF v -> Array . fmap (toValue . coerce) $ v
  ObjectF hm -> Object . fmap (toValue . coerce) $ AKM.fromHashMapText hm

-- | What schema did this validate against?
validAgainst :: ValidatedJSON -> SchemaInformation
validAgainst (ValidatedJSON (label :< _)) = label -- TODO: This is a bit useless right now.

-- | All possible validation errors.
data ValidationError
  = EmptyError
  | -- | We could not parse JSON out of what we were provided.
    NotJSON
  | -- | We got a type different to what we expected.
    WrongType
      !Value
      -- ^ The chunk of JSON.
      !JSONType
      -- ^ What we expected the type to be.
  | -- | We expected one of several possibilities, but got something that fits
    -- none.
    NotOneOfOptions !Value
  | -- | We found a JSON object with a property that wasn't specified in its
    -- schema, and additional properties are forbidden.
    AdditionalPropFoundButBanned
      {-# UNPACK #-} !Text
      -- ^ The property in question.
      {-# UNPACK #-} !Text
      -- ^ The name of the specifying schema.
  | -- | We found a JSON object which is missing a property its schema requires.
    RequiredPropertyIsMissing
      {-# UNPACK #-} !Text
      -- ^ The property in question.
      {-# UNPACK #-} !Text
      -- ^ The name of the specifying schema.
  | -- | We found a JSON array which falls outside of the minimum or maximum
    -- length constraints its corresponding schema demands.
    OutOfBoundsArrayLength
      {-# UNPACK #-} !Text
      -- ^ The name of the specifying schema.
      !Value
      -- ^ The JSON chunk corresponding to the invalid array.
  | -- | This is a bug - please report it to us!
    ImplementationError
      {-# UNPACK #-} !Text -- some descriptive text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

instance Semigroup ValidationError where
  EmptyError <> x = x
  x <> _ = x

instance Monoid ValidationError where
  mempty = EmptyError

-- | Attempt to construct validated JSON from a strict bytestring.
-- This will attempt to decode using Aeson before validating.
--
-- If this fails, it will return the first failure condition; that is, the one
-- caused by the first node in a depth-first, right-to-left, document-order
-- traversal of the input JSON.
validate :: Schema -> ByteString -> Either ValidationError ValidatedJSON
validate scm bs = case decodeStrict bs of
  Nothing -> throwError NotJSON
  Just v -> ValidatedJSON <$> go v
  where
    go v =
      fmap fst . evalRWST (runValidationM . checkTypes $ v) scm $ (initialSet, Nothing)
    initialSet = singleton . CustomNode . identFromReserved $ RStart

-- | Helper for construction of validated JSON from a JSON file.
-- This will attempt to decode using Aeson before validating. This will return
-- errors on failure in the same way as 'validate' does.
--
-- This will clean up any file handle(s) if any exceptions are thrown.
validateFromFile ::
  (MonadIO m) =>
  Schema ->
  FilePath ->
  m (Either ValidationError ValidatedJSON)
validateFromFile scm = fmap (validate scm) . liftIO . BS.readFile

-- | Helper for construction of validated JSON from a 'Handle'. This will
-- attempt to decode using Aeson before validating. This will return errors on
-- failure in the same way as 'validate' does.
--
-- This will close the 'Handle' upon finding EOF, or if an exception is thrown.
validateFromHandle ::
  (MonadIO m) =>
  Schema ->
  Handle ->
  m (Either ValidationError ValidatedJSON)
validateFromHandle scm = fmap (validate scm) . liftIO . BS.hGetContents

-- Helpers

newtype ValidationM a = ValidationM
  { runValidationM ::
      RWST
        Schema
        ()
        (NESet TypeNode, Maybe Identifier)
        (Either ValidationError)
        a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader Schema,
      MonadState (NESet TypeNode, Maybe Identifier),
      MonadError ValidationError
    )

instance Alternative ValidationM where
  empty = ValidationM . RWST $ \_ _ -> Left EmptyError
  ValidationM comp1 <|> ValidationM comp2 = ValidationM . RWST $ go
    where
      go r s = case runRWST comp1 r s of
        Left err -> case runRWST comp2 r s of
          Left _ -> Left err
          Right res -> Right res
        Right res -> Right res

failWith :: ValidationError -> ValidationM a
failWith err = ValidationM . RWST $ \_ _ -> Left err

-- We have 3 different cases:
-- 1. If we are checking against AnyNode, we ALWAYS succeed.
-- 2. If we are checking against PrimitiveNode, we can match with EXACTLY ONE
--    kind of PrimitiveNode.
-- 3. If we are checking against CustomNode, we can match against ANY CustomNode.
--    Thus, we must try all of them.
checkTypes :: Value -> ValidationM (Cofree ValidJSONF SchemaInformation)
checkTypes v = checkAny v <|> checkPrim v <|> checkCustoms v

-- checkAny throws EmptyError if AnyNode is not found. This lets checkTypes
-- use the error thrown by checkPrim/checkCustoms if checkAny fails.
checkAny :: Value -> ValidationM (Cofree ValidJSONF SchemaInformation)
checkAny v = do
  minNode <- gets (findMin . fst)
  case minNode of
    AnyNode -> pure (AnySchema :< AnythingF v)
    _ -> failWith EmptyError

-- checkPrim searches the NESet for the PrimitiveNode corresponding to the Value, otherwise throws an error.
checkPrim :: Value -> ValidationM (Cofree ValidJSONF SchemaInformation)
checkPrim v = do
  (nodes, par) <- get
  unless (member (PrimitiveNode . typeOf $ v) nodes) (failWith . NotOneOfOptions $ v)
  case v of
    Null -> pure (NullSchema :< NullF)
    Bool b -> pure (BooleanSchema :< BooleanF b)
    Number n -> pure (NumberSchema :< NumberF n)
    String s -> case par of
      -- if we are checking a dependent string, we match against the supplied
      -- values
      Nothing -> pure (StringSchema :< StringF s)
      Just parIdent -> do
        scm <- lookupSchema parIdent
        let validVals = stringVals scm
        if
            | V.length validVals == 0 -> pure (StringSchema :< StringF s)
            | s `V.elem` validVals -> pure (StringSchema :< StringF s)
            | otherwise -> failWith . NotOneOfOptions $ v
    Array arr -> case par of
      Nothing -> put (anySet, Nothing) >> (ArraySchema :<) . ArrayF <$> traverse checkTypes arr
      Just parIdent -> checkArray arr parIdent
    Object obj -> case par of
      -- Fast path (no object spec)
      Nothing ->
        put (anySet, Nothing) >> (ObjectSchema :<) . ObjectF <$> traverse checkTypes (AKM.toHashMapText obj)
      Just parIdent -> checkObject obj parIdent

-- check if the array satisfies the corresponding specification.
checkArray :: Array -> Identifier -> ValidationM (Cofree ValidJSONF SchemaInformation)
checkArray arr parIdent = do
  scm <- lookupSchema parIdent
  let arrLen = fromIntegral . V.length $ arr
  maybe (failWith outOfBounds) pure $ case arrayBounds scm of
    Non -> Just () -- no bounds, so any array will do
    One lo -> unless (arrLen >= lo) Nothing
    Eno hi -> unless (arrLen <= hi) Nothing
    Two lo hi -> unless (arrLen >= lo && arrLen <= hi) Nothing
  let valsAndTypes = pairValsWithTypes . arrayTypes $ scm
  checkedArray <- traverse go valsAndTypes
  pure (ArraySchema :< ArrayF checkedArray)
  where
    outOfBounds = OutOfBoundsArrayLength (textify parIdent) . Array $ arr
    pairValsWithTypes = \case
      Nothing -> (,AnyNode) <$> arr
      Just (ListType node) -> (,node) <$> arr
      Just (TupleType nodes) -> V.zip arr nodes
    go (val, typeNode) = do
      put (singleton typeNode, Nothing)
      checkTypes val

-- check if object properties satisfy the corresponding specification.
checkObject :: Object -> Identifier -> ValidationM (Cofree ValidJSONF SchemaInformation)
checkObject obj parIdent = do
  valsAndTypes <- pairPropertySchemaAndVal (AKM.toHashMapText obj) parIdent
  checkedObj <- traverse go valsAndTypes
  pure (ObjectSchema :< ObjectF checkedObj)
  where
    go (val, typeNode) = do
      put (singleton typeNode, Nothing)
      checkTypes val

pairPropertySchemaAndVal ::
  HashMap Text Value -> Identifier -> ValidationM (HashMap Text (Value, TypeNode))
pairPropertySchemaAndVal obj parIdent = do
  scm <- lookupSchema parIdent
  mappedObj <- traverse (pairProperty scm) . HM.mapWithKey (,) $ obj
  traverse_ isMatched . HM.mapWithKey (,) . props $ scm
  pure mappedObj
  where
    -- maps each property value with the schema it should validate against
    pairProperty scm (propName, v) = case HM.lookup propName . props $ scm of
      Just (typeNode, _) -> pure (v, typeNode)
      Nothing ->
        if additionalProps scm
          then pure (v, additionalPropSchema scm)
          else failWith . AdditionalPropFoundButBanned (textify parIdent) $ propName
    -- throws an error if a non-optional property was not found in the object
    isMatched (propName, (_, optional)) = case HM.lookup propName obj of
      Nothing ->
        unless optional . failWith . RequiredPropertyIsMissing (textify parIdent) $ propName
      Just _ -> pure ()

-- checkCustoms removes all non custom nodes from the typeNode set and
-- checks the Value against each until one succeeds.
checkCustoms :: Value -> ValidationM (Cofree ValidJSONF SchemaInformation)
checkCustoms v = do
  -- Here we drop all non-custom nodes
  customNodes <- gets (dropWhileAntitone (not . isCustom) . fst)
  asum . fmap checkCustom . S.toList $ customNodes
  where
    checkCustom = \case
      CustomNode ident -> do
        neighbourhood <- typesAs <$> lookupSchema ident
        put (neighbourhood, Just ident)
        ($> (UserDefined . textify $ ident)) <$> checkTypes v
      -- TODO: Implement a safer filter to avoid having this.
      _ -> failWith . ImplementationError $ "Unreachable code: these nodes must be custom."

lookupSchema ::
  (MonadReader Schema m, MonadError ValidationError m) => Identifier -> m CompiledSchema
lookupSchema ident = do
  x <- asks $ M.lookup ident . compiledSchemata
  case x of
    Just scm -> pure scm
    Nothing -> throwError . ImplementationError $ "Unreachable state: We should be able to find this schema"

anySet :: NESet TypeNode
anySet = singleton AnyNode

textify :: Identifier -> Text
textify (Identifier t) = t

isCustom :: TypeNode -> Bool
isCustom (CustomNode _) = True
isCustom _ = False
