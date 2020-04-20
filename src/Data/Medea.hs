{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Medea
  ( SchemaInformation (..),
    LoaderError (..),
    JSONType (..),
    ValidationError (..),
    ValidatedJSON,
    Schema,
    toValue,
    validAgainst,
    buildSchema,
    loadSchemaFromFile,
    loadSchemaFromHandle,
    validate,
    validateFromFile,
    validateFromHandle
  )
where

import Control.Applicative (Alternative, (<|>))
import Control.Comonad.Cofree (Cofree (..))
import Control.DeepSeq (NFData (..))
import Control.Monad (MonadPlus, unless, when)
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader, asks, runReaderT)
import Control.Monad.State.Strict (MonadState (..), evalStateT, gets)
import Data.Aeson (Array, Object, Value (..), decode)
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)
import Data.Coerce (coerce)
import Data.Data (Data)
import Data.Foldable (asum)
import Data.Functor (($>))
import Data.Hashable (Hashable (..))
import qualified Data.Map.Strict as M
import Data.Maybe (isJust, fromJust)
import Data.Medea.Analysis (TypeNode (..), CompiledSchema(..), ArrayType(..))
import qualified Data.HashMap.Strict as HM
import Data.Medea.JSONType (JSONType (..), typeOf)
import Data.Medea.Loader
  ( LoaderError (..),
    buildSchema,
    loadSchemaFromFile,
    loadSchemaFromHandle,
  )
import Data.Medea.Parser.Primitive (Identifier (..), startIdentifier)
import Data.Medea.Schema (Schema (..))
import Data.Medea.ValidJSON (ValidJSONF (..))
import qualified Data.Set as S
import Data.Set.NonEmpty
  ( NESet,
    singleton,
    findMin,
    member,
    dropWhileAntitone
  )
import Data.Text (Text)
import Data.These (These (..))
import Data.Traversable (mapM)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import System.IO (Handle, hSetBinaryMode)

-- | The schema-derived information attached to the current node.
data SchemaInformation
  = AnySchema
  | NullSchema
  | BooleanSchema
  | NumberSchema
  | StringSchema
  | ArraySchema
  | ObjectSchema
  | StartSchema
  | UserDefined Text
  deriving (Eq, Data, Show, Generic)

instance NFData SchemaInformation

instance Hashable SchemaInformation

-- | JSON, with additional schema-derived information as an annotation.
newtype ValidatedJSON = ValidatedJSON (Cofree ValidJSONF SchemaInformation)
  deriving (Eq, Data, Show)

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

-- | Convert to an Aeson representation (throwing away all schema information).
toValue :: ValidatedJSON -> Value
toValue (ValidatedJSON (_ :< f)) = case f of
  AnythingF v -> v
  NullF -> Null
  BooleanF b -> Bool b
  NumberF n -> Number n
  StringF s -> String s
  ArrayF v -> Array . fmap (toValue . coerce) $ v
  ObjectF hm -> Object . fmap (toValue . coerce) $ hm

-- | Get the name of the schema that this validated against.
validAgainst :: ValidatedJSON -> SchemaInformation
validAgainst (ValidatedJSON (label :< _)) = label

-- | All possible validation errors.
data ValidationError
  = EmptyError
  |  -- | We could not parse JSON out of what we were provided.
    NotJSON
  | -- | We got a type different to what we expected.
    WrongType Value JSONType
  | -- | We expected one of several possibilities, but got something that fits
    -- none.
    NotOneOfOptions Value
  | AdditionalPropFoundButBanned Text Text
  | RequiredPropertyIsMissing Text Text
  | OutOfBoundsArrayLength Text Value
  | ImplementationError Text
  deriving (Eq, Show)

instance Semigroup ValidationError where
  EmptyError <> x = x
  x <> _ = x

instance Monoid ValidationError where
  mempty = EmptyError

-- | Attempt to construct validated JSON from a bytestring.
validate ::
  (MonadPlus m, MonadError ValidationError m) =>
  Schema ->
  ByteString ->
  m ValidatedJSON
validate scm bs = case decode bs of
  Nothing -> throwError NotJSON
  Just v -> ValidatedJSON <$> go v
  where
    go v = runReaderT (evalStateT (checkTypes v) (initialSet, Nothing)) scm
    initialSet = singleton . CustomNode $ startIdentifier

-- | Helper for construction of validated JSON from a JSON file.
validateFromFile ::
  (MonadPlus m, MonadError ValidationError m, MonadIO m) =>
  Schema ->
  FilePath ->
  m ValidatedJSON
validateFromFile scm fp = do
  bs <- liftIO (BS.readFile fp)
  validate scm bs

-- | Helper for construction of validated JSON from a 'Handle'.
-- This will set the argument 'Handle' to binary mode, as this function won't
-- work otherwise. This function will close the 'Handle' once it finds EOF.
validateFromHandle ::
  (MonadPlus m, MonadError ValidationError m, MonadIO m) =>
  Schema ->
  Handle ->
  m ValidatedJSON
validateFromHandle scm h = do
  liftIO (hSetBinaryMode h True)
  bs <- liftIO (BS.hGetContents h)
  validate scm bs

-- Helpers

-- We have 3 different cases:
-- 1. If we are checking against AnyNode, we ALWAYS succeed.
-- 2. If we are checking against PrimitiveNode, we can match with EXACTLY ONE
--    kind of PrimitiveNode.
-- 3. If we are checking against CustomNode, we can match against ANY CustomNode.
--    Thus, we must try all of them.
checkTypes
  :: (Alternative m, MonadReader Schema m, MonadState (NESet TypeNode, Maybe Identifier) m, MonadError ValidationError m)
  => Value
  -> m (Cofree ValidJSONF SchemaInformation)
checkTypes v = checkAny v <|> checkPrim v <|> checkCustoms v

-- checkAny throws EmptyError if AnyNode is not found. This lets checkTypes
-- use the error thrown by checkPrim/checkCustoms if checkAny fails.
checkAny
  :: (Alternative m, MonadState (NESet TypeNode, Maybe Identifier) m, MonadError ValidationError m)
  => Value
  -> m (Cofree ValidJSONF SchemaInformation)
checkAny v = do
  minNode <- gets $ findMin . fst -- AnyNode is the smallest possible TypeNode.
  case minNode of 
    AnyNode -> pure $ AnySchema :< AnythingF v
    _       -> throwError EmptyError

-- checkPrim searches the NESet for the PrimitiveNode corresponding to the Value, otherwise throws an error.
checkPrim
  :: (Alternative m, MonadReader Schema m, MonadState (NESet TypeNode, Maybe Identifier) m, MonadError ValidationError m)
  => Value
  -> m (Cofree ValidJSONF SchemaInformation)
checkPrim v = do
  (nodes, par) <- gets id
  unless (member (PrimitiveNode . typeOf $ v) nodes) $ throwError . NotOneOfOptions $ v
  case v of
    Null -> pure $ NullSchema :< NullF
    Bool b -> pure $ BooleanSchema :< BooleanF b
    Number n -> pure $ NumberSchema :< NumberF n
    String s -> case par of
      -- if we are checking against a dependant string, we match against the supplied values
      Nothing -> pure $ StringSchema :< StringF s
      Just parIdent -> do
        scm <- asks $ lookupSchema parIdent
        let validVals = stringVals scm
        if s `V.elem` validVals || null validVals
           then pure $ StringSchema :< StringF s
           else throwError $ NotOneOfOptions v
    Array arr -> case par of
      Nothing -> put (anySet, Nothing) >> (ArraySchema :<) . ArrayF <$> mapM checkTypes arr
      Just parIdent -> checkArray arr parIdent
    Object obj -> case par of
      -- Fast Path (no object spec)
      Nothing -> put (anySet, Nothing) >> (ObjectSchema :<) . ObjectF <$> mapM checkTypes obj
      Just parIdent -> checkObject obj parIdent

-- check if the array length is within the specification range.
checkArray
  :: (Alternative m, MonadReader Schema m, MonadState (NESet TypeNode, Maybe Identifier) m, MonadError ValidationError m)
  => Array
  -> Identifier
  -> m (Cofree ValidJSONF SchemaInformation)
checkArray arr parIdent = do
  scm <- asks $ lookupSchema parIdent
  let arrLen = fromIntegral $ V.length arr
  when (maybe False (arrLen <) (minArrayLen scm)
    || maybe False (arrLen >) (maxArrayLen scm)) $
    throwError . OutOfBoundsArrayLength (textify parIdent) . Array $ arr
  let valsAndTypes = pairValsWithTypes $ arrayTypes scm
  checkedArray <- mapM (\(val, typeNode) -> put (singleton typeNode, Nothing) >> checkTypes val) valsAndTypes
  pure $ ArraySchema :< ArrayF checkedArray
    where
      pairValsWithTypes Nothing = fmap (,AnyNode) arr
      pairValsWithTypes (Just (ListType node)) = fmap (,node) arr
      pairValsWithTypes (Just (TupleType nodes)) = V.zip arr nodes


-- check if object properties satisfy the corresponding specification.
checkObject
  :: (Alternative m, MonadReader Schema m, MonadState (NESet TypeNode, Maybe Identifier) m, MonadError ValidationError m)
  => Object
  -> Identifier
  -> m (Cofree ValidJSONF SchemaInformation)
checkObject obj parIdent = do
  scm <- asks $ lookupSchema parIdent
  valsAndTypes <- fmap fromJust . HM.filter isJust
    <$> mergeHashMapsWithKeyM (combine $ additionalProps scm) (props scm) obj
  checkedObj <- mapM (\(val, typeNode) -> put (singleton typeNode, Nothing) >> checkTypes val) valsAndTypes
  pure $ ObjectSchema :< ObjectF checkedObj
    where
      -- combine is used to merge propertySpec with the actual object's property in a monadic context.
      -- It returns Just (value, the type it must match against) inside the monadic context.
      -- Returns Nothing when the property must be removed.
      -- 1. Only property spec found
      combine _ propName (This (_, isOptional)) = do
        unless isOptional $
          throwError . RequiredPropertyIsMissing (textify parIdent) $ propName
        pure Nothing
      -- 2. No property spec found i.e. this is an additional property
      combine additionalAllowed propName (That val) = do
        unless additionalAllowed $
          throwError . AdditionalPropFoundButBanned (textify parIdent) $ propName
        pure $ Just (val, AnyNode)
      -- 3. We found a property spec to match against.
      combine _ _ (These (typeNode, _) val) =
        pure $ Just (val, typeNode)
      mergeHashMapsWithKeyM
        :: (Monad m, Eq k, Hashable k)
        => (k -> These v1 v2 -> m v3)
        -> HM.HashMap k v1 -> HM.HashMap k v2 -> m (HM.HashMap k v3)
      mergeHashMapsWithKeyM f hm1 hm2 = mapM (uncurry f) $ pairKeyVal merged
        where
          merged = HM.unionWith joinThisThat (This <$> hm1) (That <$> hm2)
          pairKeyVal :: HM.HashMap k v -> HM.HashMap k (k, v)
          pairKeyVal = HM.mapWithKey (,)
          joinThisThat (This x) (That y) = These x y
          joinThisThat _         _       = error "These cases are not possible"

-- checkCustoms removes all non custom nodes from the typeNode set and
-- checks the Value against each until one succeeds.
checkCustoms
  :: (Alternative m, MonadReader Schema m, MonadState (NESet TypeNode, Maybe Identifier) m, MonadError ValidationError m)
  => Value
  -> m (Cofree ValidJSONF SchemaInformation)
checkCustoms v = do
  -- Here we drop all non custom nodes.
  customNodes <- gets $ dropWhileAntitone (not . isCustom) . fst
  asum . fmap checkCustom . S.toList $ customNodes
  where
    isCustom (CustomNode _) = True
    isCustom _              = False
    -- Check value against successfors of a custom node.
    checkCustom (CustomNode ident)= do
      neighbourhood <- asks $ typesAs . lookupSchema ident
      put (neighbourhood, Just ident)
      ($> (UserDefined . textify $ ident)) <$> checkTypes v
    checkCustom _ = throwError $ ImplementationError "Unreachable code: All these nodes MUST be custom."

anySet :: NESet TypeNode
anySet = singleton AnyNode

textify :: Identifier -> Text
textify (Identifier t) = t

-- Unsafe function
lookupSchema :: Identifier -> Schema -> CompiledSchema
lookupSchema ident = fromJust . M.lookup ident . compiledSchemata
