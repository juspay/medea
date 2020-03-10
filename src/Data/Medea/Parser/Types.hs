module Data.Medea.Parser.Types where 

import Data.Text (Text)
import Text.Megaparsec (Parsec, ShowErrorComponent, showErrorComponent)

data ParseError =
  IdentifierTooLong Text |
  LeadingZero Text |
  EmptyLengthSpec
  deriving (Eq, Ord, Show)

instance ShowErrorComponent ParseError where
  showErrorComponent = show

type MedeaParser = Parsec ParseError Text
