{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Medea.Parser.Spec.Property
  ( Specification (..),
    parseSpecification,
  )
where

import Data.Functor (($>))
import Data.Medea.Parser.Primitive
  ( Identifier,
    MedeaString,
    ReservedIdentifier (..),
    parseIdentifier,
    parseKeyVal,
    parseLine,
    parseReserved,
    parseString,
  )
import Data.Medea.Parser.Types (MedeaParser)
import Text.Megaparsec (MonadParsec (..), option, try)

data Specification = Specification
  { propName :: !MedeaString,
    propSchema :: !(Maybe Identifier),
    propOptional :: !Bool
  }
  deriving stock (Eq)

parseSpecification :: MedeaParser Specification
parseSpecification =
  Specification
    <$> parsePropName
    <*> parsePropSchema
    <*> parsePropOptional
  where
    parsePropName =
      parseLine 8 $
        parseKeyVal RPropertyName parseString
    parsePropSchema =
      option Nothing . try . parseLine 8 $
        Just <$> parseKeyVal RPropertySchema parseIdentifier
    parsePropOptional =
      option False . try . parseLine 8 $
        parseReserved ROptionalProperty $> True
