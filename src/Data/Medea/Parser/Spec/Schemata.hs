{-# LANGUAGE FlexibleContexts #-}

module Data.Medea.Parser.Spec.Schemata where

import Data.Text (Text)
import Text.Megaparsec (MonadParsec(..), many)
import Text.Megaparsec.Char (eol)
import Data.Vector (Vector)

import qualified Data.Vector as V

import Data.Medea.Parser.Error (ParseError)

import qualified Data.Medea.Parser.Spec.Schema as Schema

newtype Specification = Specification (Vector Schema.Specification)

parseSpecification :: (MonadParsec ParseError Text m) => 
  m Specification
parseSpecification = do
  spec <- Schema.parseSpecification
  specs <- many (eol *> Schema.parseSpecification)
  pure . Specification . V.fromList $ (spec : specs)
