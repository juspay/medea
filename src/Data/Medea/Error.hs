{-# LANGUAGE FlexibleContexts #-}

module Data.Medea.Error where

import Control.Monad.Except (MonadError(..))
import Data.Medea.Types.Lines (LineNumber)

data LoaderError = 
  ParseFailed ParseError | 
  InvalidSemantics SemanticError
  deriving (Show)

data ParseError = 
  -- Invalid amount of spaces
  InvalidSpaces Int -- How many spaces
                LineNumber | -- Where
  -- Schema header wasn't well-formed
  InvalidSchemaHeader LineNumber | -- where
  -- We got something which isn't UTF-8
  NotUTF8 LineNumber | -- where
  -- Should have a $schema tag on a schema header, but didn't
  MissingSchemaTag LineNumber | -- where
  -- Should have a $type tag on a type specifier, but didn't
  MissingTypeTag LineNumber | -- where
  -- No schemata provided at all
  NoSchemata
  deriving (Show)

throwParseError :: (MonadError LoaderError m) => 
  ParseError -> m a
throwParseError = throwError . ParseFailed

data SemanticError = 
  -- Given an identifier which is too long
  IdentifierTooLong LineNumber | -- where
  -- A reserved identifier (other than $start) is naming a schema
  ReservedIdentifierNamingSchema LineNumber | -- where
  -- A reserved identifier is naming a type
  ReservedIdentifierNamingType LineNumber | -- where
  -- The same identifier was used to name two different schemata
  SameSchemaName LineNumber | -- where
  -- Schema labelled with $start isn't in the graph
  StartingSchemaMissing
  deriving (Show)

throwSemanticError :: (MonadError LoaderError m) => 
  SemanticError -> m a
throwSemanticError = throwError . InvalidSemantics
