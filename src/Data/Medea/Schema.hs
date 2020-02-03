{-# LANGUAGE FlexibleContexts #-}

module Data.Medea.Schema where

import Data.Vector (Vector)
import Control.Monad.Except (MonadError(..))

import Data.Medea.Types.Lines (LineNumber)
import Data.Medea.Error (LoaderError, SemanticError(..), throwSemanticError)
import Data.Medea.Primitive (Primitive)
import Data.Medea.Identifier (Identifier, 
                              isReserved, isPrimitive, primFromIdentifier)

data TypeInformation = 
  PrimType Primitive | 
  SchemaType Identifier 

makeTypeInformation :: (MonadError LoaderError m) => 
  LineNumber -> Identifier -> m TypeInformation
makeTypeInformation lineNum ident
  | isReserved ident && isPrimitive ident = pure . PrimType . primFromIdentifier $ ident
  | isReserved ident = throwSemanticError . ReservedNonPrimitiveType $ lineNum 
  | otherwise = pure . SchemaType $ ident

newtype Schema = Schema {
  typeInfo :: Vector TypeInformation
}
