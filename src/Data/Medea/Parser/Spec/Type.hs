{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Medea.Parser.Spec.Type where

import Control.Monad (replicateM_)
import Text.Megaparsec (MonadParsec(..), some)
import Text.Megaparsec.Char (eol, spaceChar)
import Data.Vector (Vector)   

import qualified Data.Vector as V

import Data.Text.Utf8 (Utf8String)
import Data.Medea.Parser.Identifier (Identifier, 
                                     parseIdentifier, parseTypeHeader)
import Data.Medea.Parser.Error (ParseError)

newtype Specification = Specification (Vector Identifier)
  deriving (Eq)

getReferences :: Specification -> [Identifier]
getReferences (Specification v) = V.toList v

parseSpecification :: (MonadParsec ParseError Utf8String m) => 
  m Specification
parseSpecification = do
  replicateM_ 4 spaceChar
  _ <- parseTypeHeader
  _ <- eol
  Specification . V.fromList <$> some parseTypeSpec
  where parseTypeSpec = do
          replicateM_ 8 spaceChar
          ident <- parseIdentifier
          _ <- eol
          pure ident
  
