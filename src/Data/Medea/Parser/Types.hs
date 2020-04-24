module Data.Medea.Parser.Types where

import Data.Text (Text)
import Text.Megaparsec (Parsec, ShowErrorComponent, showErrorComponent)

data ParseError
  = IdentifierTooLong Text
  | ExpectedReservedIdentifier Text
  | LeadingZero Text
  | ConflictingSpecRequirements
  | EmptyLengthArraySpec
  | EmptyArrayElements
  | EmptyStringValuesSpec
  deriving (Eq, Ord, Show)

instance ShowErrorComponent ParseError where
  showErrorComponent = show

type MedeaParser = Parsec ParseError Text
