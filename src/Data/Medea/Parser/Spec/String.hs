{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Medea.Parser.Spec.String where

import Data.Text (Text)
import Data.Coerce (coerce)
import Text.Megaparsec (MonadParsec(..), many)
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Data.Medea.Parser.Types (MedeaParser)
import Data.Medea.Parser.Primitive (MedeaString, unwrap)

import Data.Medea.Parser.Primitive (parseLine, parseReservedChunk, parseString)

newtype Specification = Specification (Vector MedeaString)
  deriving (Eq, Show)

toReducedSpec :: Specification -> Vector Text
toReducedSpec spec = fmap unwrap $ (coerce spec :: Vector MedeaString)


defaultSpec :: Specification
defaultSpec = Specification $ Vec.empty

parseSpecification :: MedeaParser Specification
parseSpecification = do
  _ <- parseLine 4 $ parseReservedChunk "string_values"
  items <- many $ try $ parseLine 8 parseString
  pure $ Specification $ Vec.fromList items
  
  
