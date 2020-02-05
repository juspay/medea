module Data.Medea.Parser.Error where 

import Data.Text (Text)

newtype ParseError = IdentifierTooLong Text
  deriving (Eq, Ord)
