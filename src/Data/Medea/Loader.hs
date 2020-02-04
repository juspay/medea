module Data.Medea.Loader 
(
  LoaderError(..),
  Schema,
  buildSchema,
  loadSchemaFromFile,
  loadSchemaFromHandle
) where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (ParseError(..), bundleErrors, parse)
import Prelude hiding (readFile)
import System.IO (Handle)
import Control.Monad.IO.Class (MonadIO(..))
import Data.ByteString (ByteString, readFile, hGetContents)

import qualified Data.List.NonEmpty as NE

import Data.Medea.Schema (Schema(..))
import Data.Text.Utf8 (Utf8String, fromByteString)

import qualified Data.Medea.Parser.Spec.Schemata as Schemata

-- | Possible errors that can be produced by the loader
data LoaderError = 
  -- | The data provided wasn't UTF-8.
  NotUtf8 |
  -- | An identifier was longer than allowed.
  IdentifierTooLong | 
  -- | Parsing failed.
  ParserError (ParseError Text Void)
  deriving (Show)

-- | Attempt to produce a schema from binary data in memory. 
buildSchema :: ByteString -> Either LoaderError Schema
buildSchema bs = case fromByteString bs of
  Nothing -> Left NotUtf8
  Just utf8 -> fromUtf8 ":memory:" utf8

-- | Parse and process a Medea schema graph file.
loadSchemaFromFile :: (MonadIO m) => 
  FilePath -> m (Either LoaderError Schema)
loadSchemaFromFile fp = do
  contents <- liftIO . readFile $ fp
  case fromByteString contents of
    Nothing -> pure . Left $ NotUtf8
    Just utf8 -> pure . fromUtf8 fp $ utf8

-- | Load data corresponding to a Medea schema graph file from a 'Handle'.
loadSchemaFromHandle :: (MonadIO m) => 
  Handle -> m (Either LoaderError Schema)
loadSchemaFromHandle h = do
  contents <- liftIO . hGetContents $ h
  case fromByteString contents of
    Nothing -> pure . Left $ NotUtf8
    Just utf8 -> pure . fromUtf8 (show h) $ utf8

-- Helper

fromUtf8 :: String -> Utf8String -> Either LoaderError Schema
fromUtf8 sourceName utf8 = 
  case parse Schemata.parseSpecification sourceName utf8 of
    Left err -> case NE.head . bundleErrors $ err of
      TrivialError o u e -> Left . ParserError . TrivialError o u $ e
      FancyError{} -> Left IdentifierTooLong
    Right _ -> Right Schema -- TODO: analysis pass!
