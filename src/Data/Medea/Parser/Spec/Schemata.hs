{-# LANGUAGE FlexibleContexts #-}

module Data.Medea.Parser.Spec.Schemata where

import Text.Megaparsec (MonadParsec(..), sepBy1)
import Text.Megaparsec.Char (eol)
import Data.Vector (Vector)

import qualified Data.Vector as V

import Data.Medea.Parser.Types (MedeaParser)

import qualified Data.Medea.Parser.Spec.Schema as Schema

newtype Specification = Specification (Vector Schema.Specification)

parseSpecification :: MedeaParser Specification
parseSpecification = do
  specs <- Schema.parseSpecification `sepBy1` eol
  eof
  pure . Specification . V.fromList $ specs
