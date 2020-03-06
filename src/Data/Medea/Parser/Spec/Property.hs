{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Medea.Parser.Spec.Property
  (Specification(..), parseSpecification) where

import           Control.Applicative         ((<|>))
import           Control.Monad               (replicateM_)
import           Data.Functor                (($>))
import           Data.Text                   (Text)
import           Data.Vector                 (Vector)
import           Text.Megaparsec             (MonadParsec (..), many, option,
                                              some, try)
import           Text.Megaparsec.Char        (char, eol)
import qualified Data.Vector                 as V
import           Data.Medea.Parser.Primitive (Identifier, MedeaString,
                                              parseIdentifier, parseLine,
                                              parseReservedChunk, parseString)
import           Data.Medea.Parser.Types     (MedeaParser, ParseError)

data Specification = Specification {
  propName :: MedeaString,
  propSchema :: Maybe Identifier,
  propOptional :: Bool
} deriving (Eq)

parseSpecification :: MedeaParser Specification
parseSpecification = Specification
  <$> parsePropName
  <*> try parsePropSchema
  <*> try parsePropOptional
    where 
      parsePropName = parseLine 8 $
        parseReservedChunk "property-name" *> char ' ' *> parseString
      parsePropSchema = option Nothing . parseLine 8 $
        parseReservedChunk "property-schema" *> fmap Just parseIdentifier
      parsePropOptional = option False . parseLine 8 $
        parseReservedChunk "optional-property" $> True
