{-# LANGUAGE DerivingStrategies #-}

module Data.Medea.Parser.Types (MedeaParser, ParseError (..)) where

import Data.Text (Text)
import Text.Megaparsec (Parsec, ShowErrorComponent, showErrorComponent)

data ParseError
  = IdentifierTooLong {-# UNPACK #-} !Text
  | ExpectedReservedIdentifier {-# UNPACK #-} !Text
  | LeadingZero {-# UNPACK #-} !Text
  | ConflictingSpecRequirements
  | EmptyLengthArraySpec
  | EmptyArrayElements
  | EmptyStringValuesSpec
  deriving stock (Eq, Ord, Show)

instance ShowErrorComponent ParseError where
  showErrorComponent = show

type MedeaParser = Parsec ParseError Text
