{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Medea.Parser.Spec.String where

import Data.Coerce (coerce)
import Data.Medea.Parser.Primitive (MedeaString, parseLine, parseReservedChunk, parseString, unwrap)
import Data.Medea.Parser.Types (MedeaParser, ParseError (..))
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Text.Megaparsec (MonadParsec (..), customFailure, many)

newtype Specification = Specification (Vector MedeaString)
  deriving (Eq, Show)

toReducedSpec :: Specification -> Vector Text
toReducedSpec spec = fmap unwrap (coerce spec :: Vector MedeaString)

defaultSpec :: Specification
defaultSpec = Specification Vec.empty

parseSpecification :: MedeaParser Specification
parseSpecification = do
  _ <- parseLine 4 $ parseReservedChunk "string_values"
  items <- many $ try $ parseLine 8 parseString
  if null items
    then customFailure EmptyStringValuesSpec
    else pure $ Specification $ Vec.fromList items
