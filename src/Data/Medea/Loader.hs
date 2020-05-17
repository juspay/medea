{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Medea.Loader
  ( LoaderError (..),
    buildSchema,
    loadSchemaFromFile,
    loadSchemaFromHandle,
  )
where

import Control.Monad.Except (MonadError (..), runExcept)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString, hGetContents, readFile)
import qualified Data.List.NonEmpty as NE
import Data.Medea.Analysis
  ( AnalysisError (..),
    compileSchemata,
  )
import Data.Medea.Parser.Primitive (toText, unwrap)
import qualified Data.Medea.Parser.Spec.Schemata as Schemata
import Data.Medea.Schema (Schema (..))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')
import Data.Void (Void)
import System.IO (Handle)
import Text.Megaparsec (ParseError (..), bundleErrors, parse)
import Prelude hiding (readFile)

-- | Possible errors from loading Medea schemata.
data LoaderError
  = -- | The data provided wasn't UTF-8.
    NotUtf8
  | -- | An identifier was longer than allowed.
    IdentifierTooLong
  | -- | A length specification had no minimum/maximum specification.
    EmptyLengthSpec
  | -- | Parsing failed.
    ParserError 
      !(ParseError Text Void) -- ^ The error we got. 
  | -- | No schema labelled @$start@ was provided.
    StartSchemaMissing
  | -- | A schema was typed in terms of itself.
    SelfTypingSchema
  | -- | A schema was defined more than once.
    MultipleSchemaDefinition 
      {-# UNPACK #-} !Text -- ^ The multiply-defined schema name.
  | -- | We expected a schema, but couldn't find it. 
    MissingSchemaDefinition 
      {-# UNPACK #-} !Text -- ^ Name of the schema we were expecting. 
      {-# UNPACK #-} !Text -- ^ Name of the schema that referenced it.
  | -- | A schema was named with a reserved identifier (other than @start@). 
    SchemaNameReserved 
      {-# UNPACK #-} !Text -- ^ The schema name.
  | -- | An isolated schema was found.
    IsolatedSchemata 
      {-# UNPACK #-} !Text -- ^ The schema name.
  | -- | A property schema refers to a non-existent schema.
    MissingPropSchemaDefinition 
      {-# UNPACK #-} !Text -- ^ Name of the non-existent schema being referenced.
      {-# UNPACK #-} !Text -- ^ Name of the referencing schema.
  | -- | A minimum length specification was more than its corresponding 
    -- maximum length specification.
    MinimumLengthGreaterThanMaximum 
      {-# UNPACK #-} !Text -- ^ The name of the schema with the faulty specification.
  | -- | A property was specified more than once. 
    MultiplePropSchemaDefinition 
      {-# UNPACK #-} !Text -- ^ Name of the parent schema.
      {-# UNPACK #-} !Text -- ^ Name of the property that was defined more than once.
  | -- | A list specification did not provide an element type. 
    MissingListSchemaDefinition 
      {-# UNPACK #-} !Text -- ^ Name of the missing list element type schema. 
      {-# UNPACK #-} !Text -- ^ Name of the parent schema.
  | -- | A tuple specification does not provide a positional schema. 
    MissingTupleSchemaDefinition 
      {-# UNPACK #-} !Text -- ^ Name of the missing tuple positional schema. 
      {-# UNPACK #-} !Text -- ^ Name of the parent schema.
  | -- | Schema had a property specification, but no @$object@ type.
    PropertySpecWithoutObjectType 
      {-# UNPACK #-} !Text -- ^ Schema name.
  | -- | Schema had a list specification, but no @$array@ type.
    ListSpecWithoutArrayType 
      {-# UNPACK #-} !Text -- ^ Schema name.
  | -- | Schema had a tuple specification, but no @$array@ type.
    TupleSpecWithoutArrayType 
      {-# UNPACK #-} !Text -- ^ Schema name.
  | -- | Schema had a string specification, but no @$string@ type.
    StringSpecWithoutStringType 
      {-# UNPACK #-} !Text -- ^ Schema name.
  deriving stock (Eq, Show)

-- | Attempt to produce a schema from UTF-8 data in memory.
buildSchema ::
  (MonadError LoaderError m) =>
  ByteString ->
  m Schema
buildSchema bs = do
  utf8 <- parseUtf8 bs
  spec <- fromUtf8 ":memory:" utf8
  analyze spec

-- | Parse and process a Medea schema graph file.
loadSchemaFromFile ::
  (MonadIO m, MonadError LoaderError m) =>
  FilePath ->
  m Schema
loadSchemaFromFile fp = do
  contents <- liftIO . readFile $ fp
  utf8 <- parseUtf8 contents
  spec <- fromUtf8 fp utf8
  analyze spec

-- | Load data corresponding to a Medea schema graph file from a 'Handle'.
loadSchemaFromHandle ::
  (MonadIO m, MonadError LoaderError m) =>
  Handle ->
  m Schema
loadSchemaFromHandle h = do
  contents <- liftIO . hGetContents $ h
  utf8 <- parseUtf8 contents
  spec <- fromUtf8 (show h) utf8
  analyze spec

-- Helper

parseUtf8 ::
  (MonadError LoaderError m) =>
  ByteString ->
  m Text
parseUtf8 = either (const (throwError NotUtf8)) pure . decodeUtf8'

fromUtf8 ::
  (MonadError LoaderError m) =>
  String ->
  Text ->
  m Schemata.Specification
fromUtf8 sourceName utf8 =
  case parse Schemata.parseSpecification sourceName utf8 of
    Left err -> case NE.head . bundleErrors $ err of
      TrivialError o u e ->
        throwError . ParserError . TrivialError o u $ e
      -- TODO: Handle all kinds of ParseError
      FancyError {} -> throwError IdentifierTooLong
    Right scm -> pure scm

analyze ::
  (MonadError LoaderError m) =>
  Schemata.Specification ->
  m Schema
analyze scm = case runExcept $ compileSchemata scm of
  Left (DuplicateSchemaName ident) ->
    throwError $ MultipleSchemaDefinition (toText ident)
  Left NoStartSchema -> throwError StartSchemaMissing
  Left (DanglingTypeReference danglingRef parSchema) ->
    throwError $ MissingSchemaDefinition (toText danglingRef) (toText parSchema)
  Left TypeRelationIsCyclic -> throwError SelfTypingSchema
  Left (ReservedDefined ident) ->
    throwError $ SchemaNameReserved (toText ident)
  Left (DefinedButNotUsed ident) ->
    throwError $ IsolatedSchemata (toText ident)
  Left (DanglingTypeRefProp danglingRef parSchema) ->
    throwError $ MissingPropSchemaDefinition (toText danglingRef) (toText parSchema)
  Left (MinMoreThanMax ident) ->
    throwError $ MinimumLengthGreaterThanMaximum (toText ident)
  Left (DuplicatePropName ident prop) ->
    throwError $
      MultiplePropSchemaDefinition (toText ident) (unwrap prop)
  Left (DanglingTypeRefList danglingRef parSchema) ->
    throwError $ MissingListSchemaDefinition (toText danglingRef) (toText parSchema)
  Left (DanglingTypeRefTuple danglingRef parSchema) ->
    throwError $ MissingTupleSchemaDefinition (toText danglingRef) (toText parSchema)
  Left (PropertyWithoutObject schema) ->
    throwError $ PropertySpecWithoutObjectType (toText schema)
  Left (ListWithoutArray schema) ->
    throwError $ ListSpecWithoutArrayType (toText schema)
  Left (TupleWithoutArray schema) ->
    throwError $ TupleSpecWithoutArrayType (toText schema)
  Left (StringValsWithoutString schema) ->
    throwError $ StringSpecWithoutStringType (toText schema)
  Right g -> pure . Schema $ g
