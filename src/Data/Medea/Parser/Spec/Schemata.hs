{-# LANGUAGE FlexibleContexts #-}

module Data.Medea.Parser.Spec.Schemata where

import Text.Megaparsec (MonadParsec(..), many)
import Text.Megaparsec.Char (eol)
import Data.Vector (Vector)

import qualified Data.Vector as V

import Data.Text.Utf8 (Utf8String)
import Data.Medea.Parser.Error (ParseError)

import qualified Data.Medea.Parser.Spec.Schema as Schema

newtype Specification = Specification (Vector Schema.Specification)

parseSpecification :: (MonadParsec ParseError Utf8String m) => 
  m Specification
parseSpecification = do
  spec <- Schema.parseSpecification
  specs <- many (eol *> Schema.parseSpecification)
  pure . Specification . V.fromList $ (spec : specs)
