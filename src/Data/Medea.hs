{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

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
    validateFromHandle,
  )
where

import Algebra.Graph.Acyclic.AdjacencyMap (AdjacencyMap, postSet)
import Control.Comonad.Cofree (Cofree (..), unfoldM)
import Control.DeepSeq (NFData (..))
import Control.Monad.Except (MonadError (..))
import Control.Monad.Extra (firstJustM)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader, asks, local, runReaderT)
import Data.Aeson (Value (..), decode)
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)
import Data.Coerce (coerce)
import Data.Data (Data)
import Data.Hashable (Hashable (..))
import Data.Medea.Analysis (TypeNode (..))
import Data.Medea.JSONType (JSONType (..))
import Data.Medea.Loader
  ( LoaderError (..),
    buildSchema,
    loadSchemaFromFile,
    loadSchemaFromHandle,
  )
import Data.Medea.Parser.Identifier (Identifier (..), startIdentifier)
import Data.Medea.Schema (Schema (..))
import Data.Medea.ValidJSON (ValidJSONF (..))
import qualified Data.Set as S
import Data.Text (Text)
import GHC.Generics (Generic)
import Lens.Micro.GHC (Lens', lens, set)
import Lens.Micro.Mtl (view)
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
  = -- | We could not parse JSON out of what we were provided.
    NotJSON
  | -- | We got a type different to what we expected.
    WrongType Value JSONType
  | -- | We expected one of several possibilities, but got something that fits
    -- none.
    NotOneOfOptions Value
  deriving (Eq)

-- | Attempt to construct validated JSON from a bytestring.
validate ::
  (MonadError ValidationError m) =>
  Schema ->
  ByteString ->
  m ValidatedJSON
validate (Schema tg) bs = case decode bs of
  Nothing -> throwError NotJSON
  Just v -> ValidatedJSON <$> go v
  where
    go v = runReaderT (unfoldM checkTypes v) (envFromGraph tg)

-- | Helper for construction of validated JSON from a JSON file.
validateFromFile ::
  (MonadError ValidationError m, MonadIO m) =>
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
  (MonadError ValidationError m, MonadIO m) =>
  Schema ->
  Handle ->
  m ValidatedJSON
validateFromHandle scm h = do
  liftIO (hSetBinaryMode h True)
  bs <- liftIO (BS.hGetContents h)
  validate scm bs

-- Helpers

data TypeCheckEnv
  = TypeCheckEnv
      { graph :: AdjacencyMap TypeNode,
        _current :: TypeNode
      }

envFromGraph :: AdjacencyMap TypeNode -> TypeCheckEnv
envFromGraph g = TypeCheckEnv g . CustomNode $ startIdentifier

current :: Lens' TypeCheckEnv TypeNode
current = lens _current (\tce c -> tce {_current = c})

checkTypes ::
  (MonadReader TypeCheckEnv m, MonadError ValidationError m) =>
  Value ->
  m (SchemaInformation, ValidJSONF Value)
checkTypes v = do
  currentNode <- view current
  case currentNode of
    AnyNode -> pure (AnySchema, AnythingF v)
    PrimitiveNode t -> case (t, v) of
      (JSONNull, Null) -> pure (NullSchema, NullF)
      (JSONBoolean, Bool b) -> pure (BooleanSchema, BooleanF b)
      (JSONNumber, Number n) -> pure (NumberSchema, NumberF n)
      (JSONString, String s) -> pure (StringSchema, StringF s)
      -- We currently don't check array or object contents
      -- therefore, we punt to AnyNode before we carry on
      (JSONArray, Array arr) -> local (set current AnyNode) (pure (ArraySchema, ArrayF arr))
      (JSONObject, Object obj) -> local (set current AnyNode) (pure (ObjectSchema, ObjectF obj))
      _ -> throwError . WrongType v $ t
    CustomNode ident -> do
      neighbourhood <- asks (postSet currentNode . graph)
      success <- firstJustM go . S.toList $ neighbourhood
      case success of
        Nothing -> throwError . NotOneOfOptions $ v
        Just (_, more) -> pure (UserDefined . textify $ ident, more)
  where
    -- Not ideal, as this _should_ be handled by unfoldM
    -- Maybe something else in my stack would fix it?
    go t = catchError (Just <$> local (set current t) (checkTypes v)) (\_ -> pure Nothing)

textify :: Identifier -> Text
textify (Identifier t) = t
