{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Data.Medea.Identifier 
(
  Identifier, ReservedIdentifier, PrimTypeIdentifier,
  parseIdentifier, 
  tryReserved, parseReserved, parseTypeHeader, parseSchemaHeader, 
  startIdentifier, forgetReserved,
  tryPrimType, parsePrimType, forgetPrimType
) where

import Data.Functor (($>))
import Data.Char (isSeparator)
import Control.Monad (when)
import Text.Megaparsec (MonadParsec(..), 
                        chunk, customFailure, single, (<|>))
import Data.Hashable (Hashable)

import Data.Text.Utf8 (Utf8String, byteWidth, isPrefixOf, cons)
import Data.Medea.SchemaError (SchemaError(..))

newtype Identifier = Identifier Utf8String
  deriving (Eq, Hashable)

parseIdentifier :: (MonadParsec SchemaError Utf8String m) => 
  m Identifier
parseIdentifier = do
  ident <- takeWhile1P Nothing (not . isSeparator)
  when (byteWidth ident > 32) (customFailure IdentifierTooLong)
  pure . Identifier $ ident

newtype ReservedIdentifier = ReservedIdentifier Utf8String
  deriving (Eq)

startIdentifier :: ReservedIdentifier
startIdentifier = ReservedIdentifier "$start"

parseReserved :: (MonadParsec SchemaError Utf8String m) => 
  m ReservedIdentifier
parseReserved = do
  lead <- single '$'
  rest <- takeWhile1P Nothing (not . isSeparator)
  let ident = cons lead rest
  when (byteWidth ident > 32) (customFailure IdentifierTooLong)
  pure . ReservedIdentifier $ ident

parseSchemaHeader :: (MonadParsec SchemaError Utf8String m) => 
  m ReservedIdentifier
parseSchemaHeader = do
  ident <- chunk "$schema"
  pure . ReservedIdentifier $ ident

parseTypeHeader :: (MonadParsec SchemaError Utf8String m) => 
  m ReservedIdentifier
parseTypeHeader = do
  ident <- chunk "$type"
  pure . ReservedIdentifier $ ident

tryReserved :: Identifier -> Maybe ReservedIdentifier
tryReserved (Identifier ident) = 
  if "$" `isPrefixOf` ident
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

parsePrimType :: (MonadParsec SchemaError Utf8String m) => 
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
