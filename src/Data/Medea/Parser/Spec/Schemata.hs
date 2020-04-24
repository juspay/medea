{-# LANGUAGE FlexibleContexts #-}

module Data.Medea.Parser.Spec.Schemata where

import qualified Data.Medea.Parser.Spec.Schema as Schema
import Data.Medea.Parser.Types (MedeaParser)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.Megaparsec (MonadParsec (..), sepBy1)
import Text.Megaparsec.Char (eol)

newtype Specification = Specification (Vector Schema.Specification)

parseSpecification :: MedeaParser Specification
parseSpecification = do
  specs <- Schema.parseSpecification `sepBy1` eol
  eof
  pure . Specification . V.fromList $ specs
