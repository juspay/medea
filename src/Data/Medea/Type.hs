{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Medea.Type 
(
  TypeSpecification,
  parseTypeSpecification,
  getReferences
) where

import Control.Monad (replicateM_)
import Text.Megaparsec (MonadParsec(..), some)
import Text.Megaparsec.Char (eol, spaceChar)
import Data.Vector (Vector)   

import qualified Data.Vector as V

import Data.Text.Utf8 (Utf8String)
import Data.Medea.Identifier (Identifier, 
                              parseIdentifier, parseTypeHeader)
import Data.Medea.SchemaError (SchemaError)

newtype TypeSpecification = TypeSpecification (Vector Identifier)
  deriving (Eq)

getReferences :: TypeSpecification -> [Identifier]
getReferences (TypeSpecification v) = V.toList v

parseTypeSpecification :: (MonadParsec SchemaError Utf8String m) => 
  m TypeSpecification
parseTypeSpecification = do
  replicateM_ 4 spaceChar
  _ <- parseTypeHeader
  _ <- eol
  TypeSpecification . V.fromList <$> some parseTypeSpec
  where parseTypeSpec = do
          replicateM_ 8 spaceChar
          ident <- parseIdentifier
          _ <- eol
          pure ident
  
