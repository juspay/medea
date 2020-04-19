{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Medea.Parser.Spec.Array where

import Control.Applicative ((<|>))
import Control.Applicative.Permutations (runPermutation, toPermutationWithDefault)
import Text.Megaparsec (MonadParsec(..), try, customFailure, many)
import Data.Medea.Parser.Primitive (Natural, parseKeyVal,
                                    parseReservedChunk, parseNatural, parseLine, Identifier, parseIdentifier)
import Data.Medea.Parser.Types (MedeaParser, ParseError(..))

data Specification = Specification {
  minLength :: Maybe Natural,
  maxLength :: Maybe Natural,
  elementType :: Maybe Identifier,
  tupleSpec :: Maybe [Identifier]
} deriving (Eq, Show)

-- tupleSpec with an empty list indicates an empty tuple/encoding of unit
-- tupleSpec of Nothing indicates that there is no tuple spec at all


defaultSpec :: Specification
defaultSpec = Specification Nothing Nothing Nothing Nothing

combineSpec :: Specification -> Specification -> Specification
combineSpec (Specification a1 b1 c1 d1) (Specification a2 b2 c2 d2) = Specification (a1 <|> a2) (b1 <|> b2) (c1 <|> c2) (d1 <|> d2)

parseSpecification :: MedeaParser Specification
parseSpecification = do
    (minL, maxL) <- parseLength <|> pure (Nothing, Nothing)
    spec <- try $ permute minL maxL
    case spec of
      Specification Nothing Nothing Nothing Nothing -> do
        -- the user must specify length, or a type, or a tuple spec
        customFailure EmptyLengthArraySpec
      Specification _ _ (Just _) (Just _) -> do
        -- the user has defined both element type and tuple. 
        -- this is illegal behaviour
        customFailure ConflictingSpecRequirements
      Specification (Just ml) _ _ (Just ts) -> do
        -- the user cannot specify length and tuples
        customFailure ConflictingSpecRequirements
      Specification _ (Just ml) _ (Just ts) -> do
        customFailure ConflictingSpecRequirements
      _                             -> pure spec
  where
    permute minL' maxL'= runPermutation $ Specification minL' maxL'
      <$> toPermutationWithDefault Nothing (try parseElementType)
      <*> toPermutationWithDefault Nothing (try parseTupleSpec)

parseLength :: MedeaParser (Maybe Natural, Maybe Natural)
parseLength = do
  _ <-  try $ parseLine 4 $ try $ parseReservedChunk "length"
  (minL, maxL) <- permute
  pure (minL, maxL)
  where
    permute = runPermutation $ (,)
      <$> toPermutationWithDefault Nothing (try parseMinSpec)
      <*> toPermutationWithDefault Nothing (try parseMaxSpec)

parseMinSpec :: MedeaParser (Maybe Natural)
parseMinSpec = do
  parseLine 8 $ Just <$> parseKeyVal "minimum" parseNatural

parseMaxSpec :: MedeaParser (Maybe Natural)
parseMaxSpec = do
  parseLine 8 $ Just <$> parseKeyVal "maximum" parseNatural

parseElementType :: MedeaParser (Maybe Identifier)
parseElementType = do
  _ <- parseLine 4 $ parseReservedChunk "element_type"
  element <-  (parseLine 8 parseIdentifier) <|> customFailure EmptyArrayElements
  pure $ Just element

parseTupleSpec :: MedeaParser (Maybe [Identifier])
parseTupleSpec = do
  _ <- parseLine 4 $ parseReservedChunk "tuple"
  elemList <- many $ try $ parseLine 8 parseIdentifier
  pure $ Just elemList
