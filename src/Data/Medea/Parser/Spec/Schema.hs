{-# LANGUAGE FlexibleContexts #-}

module Data.Medea.Parser.Spec.Schema where 

import Data.Text (Text)
import Text.Megaparsec (MonadParsec(..), (<|>))
import Text.Megaparsec.Char (eol, char)

import Data.Medea.Parser.Error (ParseError)
import Data.Medea.Parser.Identifier (Identifier, 
                                     parseIdentifier, parseSchemaHeader)

import qualified Data.Vector as V
import qualified Data.Medea.Parser.Spec.Type as Type

data Specification = Specification {
  name :: !Identifier,
  types :: !Type.Specification
}
  deriving (Eq)

parseSpecification :: (MonadParsec ParseError Text m) => 
  m Specification
parseSpecification = do
  _ <- parseSchemaHeader
  _ <- char ' '
  schemaName <- parseIdentifier
  _ <- eol
  ts <- Type.parseSpecification <|> pure (Type.Specification V.empty)
  pure . Specification schemaName $ ts

