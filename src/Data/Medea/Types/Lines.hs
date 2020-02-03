module Data.Medea.Types.Lines where

import Streamly.Prelude (Enumerable(..))

newtype LineNumber = LineNumber { expose :: Word }
  deriving (Eq, Ord, Bounded, Show)

-- gah
instance Enum LineNumber where
  toEnum = LineNumber . toEnum
  fromEnum = fromEnum . expose

instance Enumerable LineNumber where
  enumerateFrom = fmap LineNumber . enumerateFrom . expose
  enumerateFromTo (LineNumber n) = fmap LineNumber . enumerateFromTo n . expose
  enumerateFromThen (LineNumber n) = fmap LineNumber . enumerateFromThen n . expose
  enumerateFromThenTo (LineNumber from) (LineNumber n) = fmap LineNumber . enumerateFromThenTo from n . expose

data LineToken a =
  -- A newline
  NewLine |
  -- A '$schema' line starting a schema def
  SchemaHeader a |
  -- $type - start of a type spec section
  TypeSpecHeader |
  -- A single type specifier
  TypeSpecifier a

data Line a = Line { 
  line :: LineNumber,
  tok :: LineToken a
}

