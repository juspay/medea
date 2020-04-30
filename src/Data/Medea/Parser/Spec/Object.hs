{-# LANGUAGE FlexibleContexts #-}

module Data.Medea.Parser.Spec.Object
  ( Specification (..),
    parseSpecification,
  )
where

import Control.Monad (when)
import Data.Functor (($>))
import Data.Maybe (isJust)
import Data.Medea.Parser.Primitive (Identifier, ReservedIdentifier (..), parseIdentifier, parseKeyVal, parseLine, parseReserved)
import qualified Data.Medea.Parser.Spec.Property as Property
import Data.Medea.Parser.Types (MedeaParser, ParseError (..))
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.Megaparsec
  ( MonadParsec (..),
    customFailure,
    many,
    option,
    try,
  )

data Specification
  = Specification
      { properties :: Vector Property.Specification,
        additionalAllowed :: Bool,
        additionalSchema :: Maybe Identifier
      }
  deriving (Eq)

parseSpecification :: MedeaParser Specification
parseSpecification = do
  _ <- parseLine 4 (parseReserved RProperties)
  props <- parseProperties
  additionalAllowed' <- parseAdditionalAllowed
  additionalSchema' <- parseAdditionalSchema
  when (not additionalAllowed' && isJust additionalSchema') $
    customFailure ConflictingSpecRequirements
  pure $ Specification props additionalAllowed' additionalSchema'

parseProperties :: MedeaParser (Vector Property.Specification)
parseProperties = V.fromList <$> many (try Property.parseSpecification)

parseAdditionalAllowed :: MedeaParser Bool
parseAdditionalAllowed =
  option False . try . parseLine 8 $
    parseReserved RAdditionalPropertiesAllowed $> True

parseAdditionalSchema :: MedeaParser (Maybe Identifier)
parseAdditionalSchema =
  option Nothing . fmap Just . try . parseLine 8 $
    parseKeyVal RAdditionalPropertySchema parseIdentifier
