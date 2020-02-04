module Data.Medea.SchemaError 
(
  SchemaError(..)
) where

data SchemaError = 
  IdentifierTooLong |
  DuplicateSchemaNames |
  NoStartingSchema
