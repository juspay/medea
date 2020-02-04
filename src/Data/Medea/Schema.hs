{-# LANGUAGE FlexibleContexts #-}

module Data.Medea.Schema 
(
  Schema,
  name, getTypes, parseSchema
) where

import Text.Megaparsec (MonadParsec(..))
import Text.Megaparsec.Char (eol, spaceChar)

import Data.Text.Utf8 (Utf8String)
import Data.Medea.Identifier (Identifier, 
                              parseIdentifier, parseSchemaHeader)
import Data.Medea.Type (TypeSpecification, 
                        parseTypeSpecification, getReferences)
import Data.Medea.SchemaError (SchemaError(..))

data Schema = Schema {
  name :: !Identifier,
  types :: !TypeSpecification
}
  deriving (Eq)

parseSchema :: (MonadParsec SchemaError Utf8String m) => 
  m Schema
parseSchema = do
  _ <- parseSchemaHeader
  _ <- spaceChar
  schemaName <- parseIdentifier
  _ <- eol
  ts <- parseTypeSpecification
  _ <- eol 
  pure . Schema schemaName $ ts

getTypes :: Schema -> [Identifier]
getTypes = getReferences . types
