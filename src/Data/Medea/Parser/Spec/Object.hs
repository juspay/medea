{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Medea.Parser.Spec.Object where

import           Control.Applicative              (Alternative, (<|>))
import           Control.Applicative.Permutations (Permutation, runPermutation,
                                                   toPermutation,
                                                   toPermutationWithDefault)
import           Control.Monad                    (replicateM_)
import           Data.Default                     (Default(..))
import           Data.Functor                     (($>))
import           Data.Medea.Parser.Primitive      (Identifier, MedeaString,
                                                   parseIdentifier, parseLine,
                                                   parseReservedChunk,
                                                   parseString, permute)
import qualified Data.Medea.Parser.Spec.Property  as Property
import           Data.Medea.Parser.Types          (MedeaParser, ParseError)
import           Data.Text                        (Text)
import           Data.Vector                      (Vector)
import qualified Data.Vector                      as V
import           Text.Megaparsec                  (MonadParsec (..), many,
                                                   option, some, try)
import           Text.Megaparsec.Char             (char, eol)

data Specification = Specification {
  properties :: Vector Property.Specification,
  additionalAllowed :: Bool
} deriving (Eq)

getPropReferences :: Specification -> [Maybe Identifier]
getPropReferences = V.toList . fmap Property.propSchema . properties

instance Default Specification where
  def = Specification V.empty False

parseSpecification :: MedeaParser Specification
parseSpecification = do
  parseLine 4 (parseReservedChunk "properties")
  Specification <$> parseProperties <*> try parseAdditionalAllowed
  where
    parseProperties = V.fromList <$> many (try Property.parseSpecification)
    parseAdditionalAllowed = option False . parseLine 8 $
      parseReservedChunk "additional-properties-allowed" $> True
