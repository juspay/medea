{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Medea.Parser.Spec.Object where

import Data.Functor (($>))
import Data.Medea.Parser.Primitive (parseLine, parseReservedChunk)
import qualified Data.Medea.Parser.Spec.Property as Property
import Data.Medea.Parser.Types (MedeaParser)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.Megaparsec
  ( MonadParsec (..),
    many,
    option,
    try,
  )

data Specification
  = Specification
      { properties :: Vector Property.Specification,
        additionalAllowed :: Bool
      }
  deriving (Eq)

parseSpecification :: MedeaParser Specification
parseSpecification = do
  _ <- parseLine 4 (parseReservedChunk "properties")
  Specification <$> parseProperties <*> parseAdditionalAllowed
  where
    parseProperties = V.fromList <$> many (try Property.parseSpecification)
    parseAdditionalAllowed =
      option False . try . parseLine 8 $
        parseReservedChunk "additional-properties-allowed" $> True
