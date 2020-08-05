{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Data.Medea.Loader
  ( LoaderError (..),
    buildSchema,
    loadSchemaFromFile,
    loadSchemaFromHandle,
  )
where

import Control.Monad.Except (runExcept)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString, hGetContents, readFile)
import Data.Medea.Analysis
  ( AnalysisError (..),
    compileSchemata,
  )
import Data.Medea.Parser.Primitive (toText, unwrap)
import qualified Data.Medea.Parser.Spec.Schemata as Schemata
import Data.Medea.Parser.Types (ParseError)
import Data.Medea.Schema (Schema (..))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')
import System.IO (Handle)
import Text.Megaparsec (ParseErrorBundle, parse)
import Prelude hiding (readFile)

-- | Possible errors from loading Medea schemata.
data LoaderError
  = -- | The data provided wasn't UTF-8.
    NotUtf8
  | -- | Parsing failed.
    ParsingFailed
      !(ParseErrorBundle Text ParseError)
  | -- | No schema labelled @$start@ was provided.
    StartSchemaMissing
  | -- | A schema was typed in terms of itself.
    SelfTypingSchema
  | -- | A schema was defined more than once.
    MultipleSchemaDefinition
      {-# UNPACK #-} !Text
  | -- | We expected a schema, but couldn't find it.
    MissingSchemaDefinition
      {-# UNPACK #-} !Text
      -- ^ Name of the schema we were expecting.
      {-# UNPACK #-} !Text
      -- ^ Name of the schema that referenced it.
  | -- | A schema was named with a reserved identifier (other than @start@).
    SchemaNameReserved
      {-# UNPACK #-} !Text
  | -- | An isolated schema was found.
    IsolatedSchemata
      {-# UNPACK #-} !Text
  | -- | A property schema refers to a non-existent schema.
    MissingPropSchemaDefinition
      {-# UNPACK #-} !Text
      -- ^ Name of the non-existent schema being referenced.
      {-# UNPACK #-} !Text
      -- ^ Name of the referencing schema.
  | -- | A minimum length specification was more than its corresponding
    -- maximum length specification.
    MinimumLengthGreaterThanMaximum
      {-# UNPACK #-} !Text
  | -- | A property was specified more than once.
    MultiplePropSchemaDefinition
      {-# UNPACK #-} !Text
      -- ^ Name of the parent schema.
      {-# UNPACK #-} !Text
      -- ^ Name of the property that was defined more than once.
  | -- | A list specification did not provide an element type.
    MissingListSchemaDefinition
      {-# UNPACK #-} !Text
      -- ^ Name of the missing list element type schema.
      {-# UNPACK #-} !Text
      -- ^ Name of the parent schema.
  | -- | A tuple specification does not provide a positional schema.
    MissingTupleSchemaDefinition
      {-# UNPACK #-} !Text
      -- ^ Name of the missing tuple positional schema.
      {-# UNPACK #-} !Text
      -- ^ Name of the parent schema.
  | -- | Schema had a property specification, but no @$object@ type.
    PropertySpecWithoutObjectType
      {-# UNPACK #-} !Text
  | -- | Schema had a list specification, but no @$array@ type.
    ListSpecWithoutArrayType
      {-# UNPACK #-} !Text
  | -- | Schema had a tuple specification, but no @$array@ type.
    TupleSpecWithoutArrayType
      {-# UNPACK #-} !Text
  | -- | Schema had a string specification, but no @$string@ type.
    StringSpecWithoutStringType
      {-# UNPACK #-} !Text
  deriving stock (Eq, Show)

-- | Attempt to produce a schema from UTF-8 data in memory.
buildSchema :: ByteString -> Either LoaderError Schema
buildSchema bs = do
  utf8 <- parseUtf8 bs
  spec <- fromUtf8 ":memory:" utf8
  analyze spec

-- | Parse and process a Medea schema graph file.
--
-- Any file handle(s) will be closed if an exception is thrown.
loadSchemaFromFile :: (MonadIO m) => FilePath -> m (Either LoaderError Schema)
loadSchemaFromFile fp = do
  contents <- liftIO . readFile $ fp
  pure (parseUtf8 contents >>= fromUtf8 fp >>= analyze)

-- | Load data corresponding to a Medea schema graph file from a 'Handle'.
--
-- This relies on 'hGetContents' to do its work, and all caveats about the state
-- a 'Handle' can be left in afterwards apply here.
loadSchemaFromHandle :: (MonadIO m) => Handle -> m (Either LoaderError Schema)
loadSchemaFromHandle h = do
  contents <- liftIO . hGetContents $ h
  pure (parseUtf8 contents >>= fromUtf8 (show h) >>= analyze)

-- Helper

parseUtf8 :: ByteString -> Either LoaderError Text
parseUtf8 = either (const (Left NotUtf8)) pure . decodeUtf8'

fromUtf8 :: String -> Text -> Either LoaderError Schemata.Specification
fromUtf8 sourceName utf8 =
  case parse Schemata.parseSpecification sourceName utf8 of
    Left err -> Left . ParsingFailed $ err
    Right scm -> pure scm

analyze :: Schemata.Specification -> Either LoaderError Schema
analyze = bimap translateError Schema . runExcept . compileSchemata
  where
    translateError = \case
      DuplicateSchemaName ident -> MultipleSchemaDefinition . toText $ ident
      NoStartSchema -> StartSchemaMissing
      DanglingTypeReference danglingRef parSchema ->
        MissingSchemaDefinition (toText danglingRef) (toText parSchema)
      TypeRelationIsCyclic -> SelfTypingSchema
      ReservedDefined ident -> SchemaNameReserved . toText $ ident
      DefinedButNotUsed ident -> IsolatedSchemata . toText $ ident
      MinMoreThanMax ident -> MinimumLengthGreaterThanMaximum . toText $ ident
      DanglingTypeRefProp danglingRef parSchema ->
        MissingPropSchemaDefinition (toText danglingRef) (toText parSchema)
      DanglingTypeRefList danglingRef parSchema ->
        MissingListSchemaDefinition (toText danglingRef) (toText parSchema)
      DanglingTypeRefTuple danglingRef parSchema ->
        MissingTupleSchemaDefinition (toText danglingRef) (toText parSchema)
      DuplicatePropName ident prop ->
        MultiplePropSchemaDefinition (toText ident) (unwrap prop)
      PropertyWithoutObject schema ->
        PropertySpecWithoutObjectType . toText $ schema
      ListWithoutArray schema -> ListSpecWithoutArrayType . toText $ schema
      TupleWithoutArray schema -> TupleSpecWithoutArrayType . toText $ schema
      StringValsWithoutString schema ->
        StringSpecWithoutStringType . toText $ schema
