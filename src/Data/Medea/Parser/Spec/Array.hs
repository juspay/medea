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
                                     parseIdentifier, parseLengthHeader, parseMinimumHeader, parseMaximumHeader, parseLine)
import Data.Medea.Parser.Types (MedeaParser, ParseError)

data Specification = Specification {
  minLength :: Maybe Identifier,
  maxLength :: Maybe Identifier
} deriving (Eq)

defaultSpec :: Specification
defaultSpec = Specification Nothing Nothing

combineSpec (Specification a1 b1) (Specification a2 b2) = Specification (a1 <|> a2) (b1 <|> b2)

parseSpecification :: MedeaParser Specification
parseSpecification =
     parseLine 4 parseLengthHeader
  *> oneOrBoth parseMinSpec parseMaxSpec
  where
    oneOrBoth p1 p2 = try (bothOrFirst p1 p2) <|> bothOrFirst p2 p1
    bothOrFirst p1 p2 = combineSpec <$> try p1 <*> try (option defaultSpec p2)
    parseMinSpec = parseLine 8 $
         parseMinimumHeader
      *> char ' '
      *> fmap (uncurry Specification . (,Nothing) . Just) parseIdentifier
    parseMaxSpec = parseLine 8 $
         parseMaximumHeader
      *> char ' '
      *> fmap (uncurry Specification . (Nothing,) . Just) parseIdentifier
