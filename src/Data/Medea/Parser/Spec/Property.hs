{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Medea.Parser.Spec.Property
  (Specification(..), parseSpecification) where

import           Data.Functor                (($>))
import           Text.Megaparsec             (MonadParsec (..), option, try)
import           Data.Medea.Parser.Primitive (Identifier, MedeaString,
                                              parseIdentifier, parseLine,
                                              parseReservedChunk, parseString,
                                              parseKeyVal)
import           Data.Medea.Parser.Types     (MedeaParser)

data Specification = Specification {
  propName :: MedeaString,
  propSchema :: Maybe Identifier,
  propOptional :: Bool
} deriving (Eq)

parseSpecification :: MedeaParser Specification
parseSpecification = Specification
  <$> parsePropName
  <*> parsePropSchema
  <*> parsePropOptional
    where 
      parsePropName = parseLine 8 $
        parseKeyVal "property-name" parseString
      parsePropSchema = option Nothing . try . parseLine 8 $
        Just <$> parseKeyVal "property-schema" parseIdentifier
      parsePropOptional = option False . try . parseLine 8 $
        parseReservedChunk "optional-property" $> True
