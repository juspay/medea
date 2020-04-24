{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Medea.Parser.Spec.Type where

import Data.Medea.Parser.Primitive
  ( Identifier,
    parseIdentifier,
    parseLine,
    parseReservedChunk,
  )
import Data.Medea.Parser.Types (MedeaParser)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.Megaparsec (MonadParsec (..), some)

newtype Specification = Specification (Vector Identifier)
  deriving (Eq)

defaultSpec :: Specification
defaultSpec = Specification V.empty

getReferences :: Specification -> [Identifier]
getReferences (Specification v) = V.toList v

parseSpecification :: MedeaParser Specification
parseSpecification = do
  _ <- parseLine 4 $ parseReservedChunk "type"
  types <- some . try $ parseLine 8 parseIdentifier
  pure . Specification . V.fromList $ types
