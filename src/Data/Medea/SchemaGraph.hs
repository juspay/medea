{-# LANGUAGE FlexibleContexts #-}

module Data.Medea.SchemaGraph where

import Control.Monad (unless)
import Data.Vector (Vector)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Control.Monad.State.Strict (MonadState, gets, modify, evalStateT)
import Control.Monad.Except (MonadError(..), runExcept)
import Data.Text.Short (ShortText)

import qualified Data.HashSet as HS

import Data.Medea.Types.Lines (Line(..), LineToken(..), LineNumber)
import Data.Medea.Identifier (Identifier, 
                              makeIdentifier, isReserved, isStarting, isPrimitive, start)
import Data.Medea.Schema (Schema)
import Data.Medea.Error (LoaderError, SemanticError(..), throwSemanticError)

newtype SchemaGraph = SchemaGraph (HashMap Identifier Schema)
  deriving (Eq)

-- Semantic analysis

analyze :: Vector (Line ShortText) -> Either LoaderError (Vector (Line Identifier))
analyze v = runExcept (evalStateT comp HS.empty)
  where comp = do
          v' <- makeIdentifiers v
          verifyStart
          pure v'

makeIdentifiers :: (MonadError LoaderError m, MonadState (HashSet Identifier) m) => 
  Vector (Line ShortText) -> m (Vector (Line Identifier))
makeIdentifiers = traverse go
  where go (Line lineNum (SchemaHeader st)) = do
          ident <- makeIdentifier lineNum st 
          if (not . isReserved $ ident) || isStarting ident
          then do
            checkAndNote ident lineNum
            pure (Line lineNum (SchemaHeader ident))
          else throwSemanticError . ReservedIdentifierNamingSchema $ lineNum
        go (Line lineNum (TypeSpecifier st)) = do
          ident <- makeIdentifier lineNum st
          if (not . isReserved $ ident) || isPrimitive ident
          then pure (Line lineNum (TypeSpecifier ident))
          else throwSemanticError . ReservedIdentifierNamingType $ lineNum
        go (Line n NewLine) = pure (Line n NewLine)
        go (Line n TypeSpecHeader) = pure (Line n TypeSpecHeader)

checkAndNote :: (MonadState (HashSet Identifier) m, MonadError LoaderError m) => 
  Identifier -> LineNumber -> m ()
checkAndNote ident lineNum = do
  seenBefore <- gets (HS.member ident)
  if seenBefore
  then throwSemanticError . SameSchemaName $ lineNum
  else modify (HS.insert ident)

verifyStart :: (MonadState (HashSet Identifier) m, MonadError LoaderError m) => 
  m ()
verifyStart = do
  found <- gets (HS.member start)
  unless found (throwSemanticError StartingSchemaMissing)
