{-# LANGUAGE FlexibleContexts #-}

module Data.Medea.SchemaGraph where

import Data.Bifunctor (first)
import Control.Monad (unless, when)
import Data.Vector (Vector)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Control.Monad.State.Strict (MonadState, gets, modify, evalStateT)
import Control.Monad.Except (MonadError(..), runExcept)
import Data.Text.Short (ShortText)

import qualified Data.Vector as V
import qualified Data.HashSet as HS

import Data.Medea.Types.Lines (Line(..), LineToken(..), LineNumber)
import Data.Medea.Identifier (Identifier, 
                              makeIdentifier, isReserved, isStarting, isPrimitive, start)
import Data.Medea.Schema (Schema(..), TypeInformation, makeTypeInformation)
import Data.Medea.Error (LoaderError, SemanticError(..), throwSemanticError)

newtype SchemaGraph = SchemaGraph (HashMap Identifier Schema)

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

uncons :: (MonadError LoaderError m) => 
  Vector a -> m (a, Vector a)
uncons v = do
  when (V.null v) (throwSemanticError UnexpectedEndOfFile)
  (,) <$> V.unsafeHeadM v <*> pure (V.unsafeTail v)

acceptNewline :: (MonadError LoaderError m) => 
  Vector (Line a) -> m (Vector (Line a))
acceptNewline v = do
  (h, t) <- uncons v
  case h of
    (Line _ NewLine) -> pure t
    (Line lineNum _) -> throwSemanticError (ExpectedNewLine lineNum)

intoSchema :: (MonadError LoaderError m) => 
  Vector (Line Identifier) -> m ((Identifier, Schema), Vector (Line Identifier))
intoSchema v = do
  (h, t) <- uncons v
  case h of
    (Line _ (SchemaHeader name)) -> do
      (typeSpecs, rest) <- intoTypeSpecifiers t
      pure ((name, Schema typeSpecs), rest)
    (Line lineNum _) -> throwSemanticError . ExpectedSchemaHeader $ lineNum

schematize :: (MonadError LoaderError m) => 
  Vector (Line Identifier) -> m (Vector (Identifier, Schema))
schematize v = do
  (firstScm, rest) <- intoSchema v
  fst . first V.fromList <$> go ([firstScm], rest)
    where go (acc, vec) =
            if V.null vec
            then pure (acc, vec)
            else do
              vec' <- acceptNewline vec
              (scm, vec'') <- intoSchema vec'
              go (scm : acc, vec'')

intoTypeSpecifiers :: (MonadError LoaderError m) => 
  Vector (Line Identifier) -> m (Vector TypeInformation, Vector (Line Identifier))
intoTypeSpecifiers v = 
  if V.null v
  then pure (V.empty, v)
  else do
    (h, t) <- (,) <$> V.unsafeHeadM v <*> pure (V.tail v)
    case h of
      (Line _ TypeSpecHeader) -> getTypeSpecs t
      (Line lineNum _) -> throwSemanticError . ExpectedTypeSpecHeader $ lineNum

getTypeSpecs :: (MonadError LoaderError m) => 
  Vector (Line Identifier) -> m (Vector TypeInformation, Vector (Line Identifier))
getTypeSpecs v = first V.fromList <$> go ([], v)
  where go (acc, vec) = 
          if V.null vec
          then pure (acc, vec)
          else do
            (h, t) <- (,) <$> V.unsafeHeadM v <*> pure (V.tail v)
            case h of
              (Line lineNum (TypeSpecifier ident)) -> do
                ts <- makeTypeInformation lineNum ident
                go (ts : acc, t)
              (Line _ NewLine) -> pure (acc, vec) -- leave the newline for someone else
              (Line lineNum _) -> throwSemanticError . ExpectedTypeSpec $ lineNum 
