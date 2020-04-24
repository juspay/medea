{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Medea.Parser.Spec.Array where

import Control.Applicative ((<|>))
import Control.Applicative.Permutations (runPermutation, toPermutationWithDefault)
import Data.Medea.Parser.Primitive
  ( Identifier,
    Natural,
    parseIdentifier,
    parseKeyVal,
    parseLine,
    parseNatural,
    parseReservedChunk,
  )
import Data.Medea.Parser.Types (MedeaParser, ParseError (..))
import Text.Megaparsec (MonadParsec (..), customFailure, many, try)

data Specification
  = Specification
      { minLength :: Maybe Natural,
        maxLength :: Maybe Natural,
        elementType :: Maybe Identifier,
        tupleSpec :: Maybe [Identifier]
      }
  deriving (Eq, Show)

-- tupleSpec with an empty list indicates an empty tuple/encoding of unit
-- tupleSpec of Nothing indicates that there is no tuple spec at all

defaultSpec :: Specification
defaultSpec = Specification Nothing Nothing Nothing Nothing

combineSpec :: Specification -> Specification -> Specification
combineSpec (Specification a1 b1 c1 d1) (Specification a2 b2 c2 d2) = Specification (a1 <|> a2) (b1 <|> b2) (c1 <|> c2) (d1 <|> d2)

parseSpecification :: MedeaParser Specification
parseSpecification = do
  spec <- try permute
  case spec of
    Specification Nothing Nothing Nothing Nothing ->
      -- the user must specify length, or a type, or a tuple spec
      customFailure EmptyLengthArraySpec
    Specification _ _ (Just _) (Just _) ->
      -- the user has defined both element type and tuple.
      -- this is illegal behaviour
      customFailure ConflictingSpecRequirements
    Specification (Just _) _ _ (Just _) ->
      -- the user cannot specify length and tuples
      customFailure ConflictingSpecRequirements
    Specification _ (Just _) _ (Just _) ->
      customFailure ConflictingSpecRequirements
    _ -> pure spec
  where
    permute =
      runPermutation $
        Specification
          <$> toPermutationWithDefault Nothing (try parseMinSpec)
          <*> toPermutationWithDefault Nothing (try parseMaxSpec)
          <*> toPermutationWithDefault Nothing (try parseElementType)
          <*> toPermutationWithDefault Nothing (try parseTupleSpec)

parseMinSpec :: MedeaParser (Maybe Natural)
parseMinSpec =
  parseLine 4 $ Just <$> parseKeyVal "min_length" parseNatural

parseMaxSpec :: MedeaParser (Maybe Natural)
parseMaxSpec =
  parseLine 4 $ Just <$> parseKeyVal "max_length" parseNatural

parseElementType :: MedeaParser (Maybe Identifier)
parseElementType = do
  _ <- parseLine 4 $ parseReservedChunk "element_type"
  element <- parseLine 8 parseIdentifier <|> customFailure EmptyArrayElements
  pure $ Just element

parseTupleSpec :: MedeaParser (Maybe [Identifier])
parseTupleSpec = do
  _ <- parseLine 4 $ parseReservedChunk "tuple"
  elemList <- many $ try $ parseLine 8 parseIdentifier
  pure $ Just elemList
