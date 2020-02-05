{-# LANGUAGE FlexibleContexts #-}

module Data.Medea.Loader 
(
  LoaderError(..),
  Schema,
  buildSchema,
  loadSchemaFromFile,
  loadSchemaFromHandle
) where

import Control.Monad.Except (MonadError(..))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')
import Data.Void (Void)
import Text.Megaparsec (ParseError(..), bundleErrors, parse)
import Prelude hiding (readFile)
import System.IO (Handle)
import Control.Monad.IO.Class (MonadIO(..))
import Data.ByteString (ByteString, readFile, hGetContents)

import qualified Data.List.NonEmpty as NE

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
  NoStartSchema
  deriving (Show)

-- | Attempt to produce a schema from binary data in memory. 
buildSchema :: (MonadError LoaderError m) => 
  ByteString -> m Schema
buildSchema bs = do
  utf8 <- parseUtf8 bs
  fromUtf8 ":memory:" utf8

-- | Parse and process a Medea schema graph file.
loadSchemaFromFile :: (MonadIO m, MonadError LoaderError m) => 
  FilePath -> m Schema
loadSchemaFromFile fp = do
  contents <- liftIO . readFile $ fp
  utf8 <- parseUtf8 contents
  fromUtf8 fp utf8

-- | Load data corresponding to a Medea schema graph file from a 'Handle'.
loadSchemaFromHandle :: (MonadIO m, MonadError LoaderError m) => 
  Handle -> m Schema
loadSchemaFromHandle h = do
  contents <- liftIO . hGetContents $ h
  utf8 <- parseUtf8 contents
  fromUtf8 (show h) utf8

-- Helper

parseUtf8 :: (MonadError LoaderError m) => 
  ByteString -> m Text
parseUtf8 = either (const (throwError NotUtf8)) pure . decodeUtf8'

fromUtf8 :: (MonadError LoaderError m) => 
  String -> Text -> m Schema
fromUtf8 sourceName utf8 = 
  case parse Schemata.parseSpecification sourceName utf8 of
    Left err -> case NE.head . bundleErrors $ err of
      TrivialError o u e -> throwError . ParserError . TrivialError o u $ e
      FancyError{} -> throwError IdentifierTooLong
    Right _ -> pure Schema -- TODO: analysis pass!

