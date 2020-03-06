{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Data.Medea.Parser.Spec.Array where

import Data.Default (Default(..))
import Data.Text (Text)
import Control.Applicative ((<|>))
import Control.Applicative.Permutations (runPermutation, toPermutation, toPermutationWithDefault)
import Control.Monad (replicateM_)
import Text.Megaparsec (MonadParsec(..), some, option, try)
import Text.Megaparsec.Debug (dbg)
import Text.Megaparsec.Char (eol, char)
import Data.Vector (Vector)

import qualified Data.Vector as V

import Data.Medea.Parser.Primitive (Identifier, parseKeyVal,
                                    parseReservedChunk, parseIdentifier, parseLine)
import Data.Medea.Parser.Types (MedeaParser, ParseError)

data Specification = Specification {
  minLength :: Maybe Identifier,
  maxLength :: Maybe Identifier
} deriving (Eq)

instance Default Specification where
  def = Specification Nothing Nothing

combineSpec (Specification a1 b1) (Specification a2 b2) = Specification (a1 <|> a2) (b1 <|> b2)

parseSpecification :: MedeaParser Specification
parseSpecification = do
     parseLine 4 $ parseReservedChunk "length"
     oneOrBoth parseMinSpec parseMaxSpec
  where
    oneOrBoth p1 p2 = try (bothOrFirst p1 p2) <|> bothOrFirst p2 p1
    bothOrFirst p1 p2 = combineSpec <$> try p1 <*> try (option def p2)
    parseMinSpec = parseLine 8 $ do
      ident <- parseKeyVal "minimum" parseIdentifier
      pure . uncurry Specification . (,Nothing) . Just $ ident
    parseMaxSpec = parseLine 8 $ do
      ident <- parseKeyVal "maximum" parseIdentifier
      pure . uncurry Specification . (Nothing,) . Just $ ident
