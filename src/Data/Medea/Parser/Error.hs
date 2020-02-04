module Data.Medea.Parser.Error where 

import Data.Text.Utf8 (Utf8String)

newtype ParseError = IdentifierTooLong Utf8String
  deriving (Eq, Ord)
