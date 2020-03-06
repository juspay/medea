module Data.Medea.Parser.Types where 

import Data.Text (Text)
import Text.Megaparsec (Parsec)

data ParseError =
  IdentifierTooLong Text |
  LeadingZero Text |
  EmptyLengthSpec
  deriving (Eq, Ord)

type MedeaParser = Parsec ParseError Text
