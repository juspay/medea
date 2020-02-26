{-# LANGUAGE FlexibleContexts #-}

module Data.Medea.Loader 
(
  LoaderError(..),
  Schema,
  buildSchema,
  loadSchemaFromFile,
  loadSchemaFromHandle
) where

import Control.Monad.Except (MonadError(..), runExcept)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')
import Data.Void (Void)
import Text.Megaparsec (ParseError(..), bundleErrors, parse)
import Prelude hiding (readFile)
import System.IO (Handle)
import Control.Monad.IO.Class (MonadIO(..))
import Data.ByteString (ByteString, readFile, hGetContents)
import Algebra.Graph.Acyclic.AdjacencyMap (AdjacencyMap)

import qualified Data.List.NonEmpty as NE

import Data.Medea.Parser.Identifier (toText)
import Data.Medea.Analysis (TypeNode, AnalysisError(..),
                            intoAcyclic, intoEdges, intoMap, checkStartSchema)
import Data.Medea.Schema (Schema(..))

import qualified Data.Medea.Parser.Spec.Schemata as Schemata

-- | Possible errors that can be produced by the loader
data LoaderError = 
  -- | The data provided wasn't UTF-8.
  NotUtf8 |
  -- | An identifier was longer than allowed.
  IdentifierTooLong | 
  -- | Parsing failed.
  ParserError (ParseError Text Void) |
  -- | No schema labelled $start was provided.
  StartSchemaMissing |
  -- | A schema was typed in terms of itself.
  SelfTypingSchema |
  -- | A schema was defined more than once.
  MultipleSchemaDefinition Text | -- ^ re-used name
  -- | A schema contains a type specifier aiming at an undefined schema.
  MissingSchemaDefinition Text   | -- ^ name of the undefined schema
  -- | A schema with non-start reserved naming identifier.
  SchemaNameReserved Text | -- name of the reserved identifier
  -- | 
  UnusedSchemata

  deriving (Show)

-- | Attempt to produce a schema from binary data in memory. 
buildSchema :: (MonadError LoaderError m) => 
  ByteString -> m Schema
buildSchema bs = do
  utf8 <- parseUtf8 bs
  spec <- fromUtf8 ":memory:" utf8
  Schema <$> analyze spec

-- | Parse and process a Medea schema graph file.
loadSchemaFromFile :: (MonadIO m, MonadError LoaderError m) => 
  FilePath -> m Schema
loadSchemaFromFile fp = do
  contents <- liftIO . readFile $ fp
  utf8 <- parseUtf8 contents
  spec <- fromUtf8 fp utf8
  Schema <$> analyze spec

-- | Load data corresponding to a Medea schema graph file from a 'Handle'.
loadSchemaFromHandle :: (MonadIO m, MonadError LoaderError m) => 
  Handle -> m Schema
loadSchemaFromHandle h = do
  contents <- liftIO . hGetContents $ h
  utf8 <- parseUtf8 contents
  spec <- fromUtf8 (show h) utf8
  Schema <$> analyze spec

-- Helper

parseUtf8 :: (MonadError LoaderError m) => 
  ByteString -> m Text
parseUtf8 = either (const (throwError NotUtf8)) pure . decodeUtf8'

fromUtf8 :: (MonadError LoaderError m) => 
  String -> Text -> m Schemata.Specification
fromUtf8 sourceName utf8 = 
  case parse Schemata.parseSpecification sourceName utf8 of
    Left err -> case NE.head . bundleErrors $ err of
      TrivialError o u e -> throwError . ParserError . TrivialError o u $ e
      FancyError{} -> throwError IdentifierTooLong
    Right scm -> pure scm 

analyze :: (MonadError LoaderError m) => 
  Schemata.Specification -> m (AdjacencyMap TypeNode)
analyze scm = case runExcept go of
  Left (DuplicateSchemaName ident) -> throwError . MultipleSchemaDefinition . toText $ ident
  Left NoStartSchema -> throwError StartSchemaMissing
  Left (DanglingTypeReference ident) -> throwError . MissingSchemaDefinition . toText $ ident
  Left TypeRelationIsCyclic -> throwError SelfTypingSchema
  Left (ReservedDefined ident) -> throwError . SchemaNameReserved . toText $ ident
  Left UnreachableSchemata -> throwError UnusedSchemata
  Right g -> pure g
  where go = do
          m <- intoMap scm
          startSchema <- checkStartSchema m
          edges <- intoEdges m startSchema
          intoAcyclic edges
