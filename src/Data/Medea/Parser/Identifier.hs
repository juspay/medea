{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Data.Medea.Parser.Identifier where 

import Prelude hiding (head)
import Data.Text (Text, cons, head)
import Data.Text.Encoding (encodeUtf8)
import Data.Functor (($>))
import Data.Char (isSeparator)
import Text.Megaparsec (MonadParsec(..), 
                        chunk, customFailure, single, (<|>))

import qualified Data.ByteString as BS

import Data.Medea.Parser.Error (ParseError(..))

newtype Identifier = Identifier { toText :: Text }
  deriving (Eq, Ord, Show)

parseIdentifier :: (MonadParsec ParseError Text m) => 
  m Identifier
parseIdentifier = do
  ident <- takeWhile1P (Just "Non-separator") (not . isSeparator)
  checkedConstruct Identifier ident

startIdentifier :: Identifier
startIdentifier = Identifier "$start"

newtype ReservedIdentifier = ReservedIdentifier Text
  deriving (Eq)

parseReserved :: (MonadParsec ParseError Text m) => 
  m ReservedIdentifier
parseReserved = do
  lead <- single '$'
  rest <- takeWhile1P Nothing (not . isSeparator)
  let ident = cons lead rest
  checkedConstruct ReservedIdentifier ident

parseSchemaHeader :: (MonadParsec ParseError Text m) => 
  m ReservedIdentifier
parseSchemaHeader = do
  ident <- chunk "$schema"
  pure . ReservedIdentifier $ ident

parseTypeHeader :: (MonadParsec ParseError Text m) => 
  m ReservedIdentifier
parseTypeHeader = do
  ident <- chunk "$type"
  pure . ReservedIdentifier $ ident

tryReserved :: Identifier -> Maybe ReservedIdentifier
tryReserved (Identifier ident) = 
  if head ident == '$'
  then Just (ReservedIdentifier ident)
  else Nothing

forgetReserved :: ReservedIdentifier -> Identifier
forgetReserved (ReservedIdentifier ident) = Identifier ident

data PrimTypeIdentifier =
  NullIdentifier |
  BooleanIdentifier |
  ObjectIdentifier |
  ArrayIdentifier |
  NumberIdentifier |
  StringIdentifier
  deriving (Eq)

parsePrimType :: (MonadParsec ParseError Text m) => 
  m PrimTypeIdentifier
parsePrimType = chunk "$null" $> NullIdentifier <|> 
                chunk "$boolean" $> BooleanIdentifier <|>
                chunk "$object" $> ObjectIdentifier <|>
                chunk "$array" $> ArrayIdentifier <|>
                chunk "$number" $> NumberIdentifier <|>
                chunk "$string" $> StringIdentifier

tryPrimType :: Identifier -> Maybe PrimTypeIdentifier
tryPrimType (Identifier ident) = case ident of
  "$null" -> Just NullIdentifier
  "$boolean" -> Just BooleanIdentifier
  "$object" -> Just ObjectIdentifier
  "$array" -> Just ArrayIdentifier
  "$number" -> Just NumberIdentifier
  "$string" -> Just StringIdentifier
  _ -> Nothing

forgetPrimType :: PrimTypeIdentifier -> Identifier
forgetPrimType = \case
  NullIdentifier -> Identifier "$null"
  BooleanIdentifier -> Identifier "$boolean"
  ObjectIdentifier -> Identifier "$object"
  ArrayIdentifier -> Identifier "$array"
  NumberIdentifier -> Identifier "$number"
  StringIdentifier -> Identifier "$string"

-- Helpers
checkedConstruct :: (MonadParsec ParseError Text m) => 
  (Text -> a) -> Text -> m a
checkedConstruct f t = 
  if (> 32) . BS.length . encodeUtf8 $ t
  then customFailure . IdentifierTooLong $ t
  else pure . f $ t
