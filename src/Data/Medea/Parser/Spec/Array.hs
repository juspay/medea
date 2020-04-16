{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Medea.Parser.Spec.Array where

import Control.Applicative ((<|>))
import Control.Applicative.Permutations (runPermutation, toPermutationWithDefault)
import Text.Megaparsec (MonadParsec(..), try, customFailure)

import Data.Medea.Parser.Primitive (Natural, parseKeyVal,
                                    parseReservedChunk, parseNatural, parseLine)
import Data.Medea.Parser.Types (MedeaParser, ParseError(..))

data Specification = Specification {
  minLength :: Maybe Natural,
  maxLength :: Maybe Natural
} deriving (Eq)

defaultSpec :: Specification
defaultSpec = Specification Nothing Nothing

combineSpec :: Specification -> Specification -> Specification
combineSpec (Specification a1 b1) (Specification a2 b2) = Specification (a1 <|> a2) (b1 <|> b2)

parseSpecification :: MedeaParser Specification
parseSpecification = do
     _ <- parseLine 4 $ parseReservedChunk "length"
     spec <- try permute
     case spec of
       Specification Nothing Nothing -> customFailure EmptyLengthSpec
       _                             -> pure spec
  where
    permute = runPermutation $ Specification
      <$> toPermutationWithDefault Nothing (try parseMinSpec)
      <*> toPermutationWithDefault Nothing (try parseMaxSpec)
    parseMinSpec = parseLine 8 $
      Just <$> parseKeyVal "minimum" parseNatural
    parseMaxSpec = parseLine 8 $
      Just <$> parseKeyVal "maximum" parseNatural
