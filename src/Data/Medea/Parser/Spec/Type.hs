{-# LANGUAGE FlexibleContexts #-}

module Data.Medea.Parser.Spec.Type where

import Data.Text (Text)
import Control.Monad (replicateM_)
import Text.Megaparsec (MonadParsec(..), some)
import Text.Megaparsec.Char (eol, char)
import Data.Vector (Vector)   

import qualified Data.Vector as V

import Data.Medea.Parser.Primitive (Identifier, 
                                     parseIdentifier, parseTypeHeader, parseLine)
import Data.Medea.Parser.Types (MedeaParser, ParseError)

newtype Specification = Specification (Vector Identifier)
  deriving (Eq)

getReferences :: Specification -> [Identifier]
getReferences (Specification v) = V.toList v

parseSpecification :: MedeaParser Specification
parseSpecification = do
  parseLine 4 parseTypeHeader
  types <- some . try $ parseLine 8 parseIdentifier
  pure . Specification . V.fromList $ types
