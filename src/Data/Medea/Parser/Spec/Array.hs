{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Data.Medea.Parser.Spec.Array where

import Data.Text (Text)
import Control.Applicative ((<|>))
import Control.Applicative.Permutations (runPermutation, toPermutation, toPermutationWithDefault)
import Control.Monad (replicateM_)
import Text.Megaparsec (MonadParsec(..), some, option, try)
import Text.Megaparsec.Debug (dbg)
import Text.Megaparsec.Char (eol, char)
import Data.Vector (Vector)

import qualified Data.Vector as V

import Data.Medea.Parser.Primitive (Identifier,
                                     parseIdentifier, parseLengthHeader, parseMinimumHeader, parseMaximumHeader)
import Data.Medea.Parser.Types (MedeaParser, ParseError)

data Specification = Specification {
  minLength :: Maybe Identifier,
  maxLength :: Maybe Identifier
} deriving (Eq)

defaultSpec :: Specification
defaultSpec = Specification Nothing Nothing

combineSpec (Specification a1 b1) (Specification a2 b2) = Specification (a1 <|> a2) (b1 <|> b2)

parseSpecification :: MedeaParser Specification
parseSpecification = replicateM_ 4 (char ' ')
  *> parseLengthHeader
  *> eol
  *> oneOrBoth parseMinSpec parseMaxSpec
  where
    oneOrBoth p1 p2 = try (bothOrFirst p1 p2) <|> try (bothOrFirst p2 p1)
    -- Returns either the composition of both spec modifiers or only the first one
    bothOrFirst p1 p2 = combineSpec <$> try p1 <*> try (option defaultSpec p2)
    -- Returns the specification modifier for minLength
    parseMinSpec = replicateM_ 8 (char ' ')
      *> parseMinimumHeader
      *> char ' '
      *> fmap (uncurry Specification . (,Nothing) . Just) parseIdentifier
      <* eol
    -- Returns the specification modifier for maxLength
    parseMaxSpec = replicateM_ 8 (char ' ')
      *> parseMaximumHeader
      *> char ' '
      *> fmap (uncurry Specification . (Nothing,) . Just) parseIdentifier
      <* eol
