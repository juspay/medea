{-# LANGUAGE DerivingStrategies #-}

module Data.Medea.Parser.Types (MedeaParser, ParseError (..)) where

import Data.Text (Text)
import Text.Megaparsec (Parsec, ShowErrorComponent, showErrorComponent)

-- | All possible errors from the Medea parser.
data ParseError
  = -- | An identifier exceeded 32 bytes.
    IdentifierTooLong {-# UNPACK #-} !Text
  | -- | We saw a non-reserved identifier where we wanted a reserved one.
    ExpectedReservedIdentifier {-# UNPACK #-} !Text
  | -- | A Medea natural number had literal zeroes.
    LeadingZero {-# UNPACK #-} !Text
  | -- | We were given incompatible requirements within a specification.
    ConflictingSpecRequirements
  | -- | We were not given a length in an array specification.
    EmptyLengthArraySpec
  | -- | We were not given an element specification in an array specification.
    EmptyArrayElements
  | -- | We were given no string values in a string specification.
    EmptyStringValuesSpec
  deriving stock (Eq, Ord, Show)

instance ShowErrorComponent ParseError where
  showErrorComponent = show

type MedeaParser = Parsec ParseError Text
