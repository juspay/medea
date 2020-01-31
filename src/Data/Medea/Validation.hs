{-# LANGUAGE FlexibleContexts #-}

module Data.Medea.Validation where

import Control.Monad (when)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.Except (MonadError(..))
import Data.Aeson (Value)

import Data.Medea.Primitive (Primitive)

data ValidatorState = ValidatorState { 
  maxDepth :: Word, 
  currentDepth :: Word 
}
  deriving (Eq)

data ValidationError = ValidationError { 
  context :: Value, 
  err :: ValidationErrorType 
}
  deriving (Eq)

data ValidationErrorType = 
  -- exceeded our depth-first limit
  TraversalDepthExceeded Word | -- limit
  -- we didn't get the type we expected
  ExpectedPrimitiveType Primitive -- what we expected
  deriving (Eq)

checkRecursionDepth :: (MonadReader ValidatorState m, MonadError ValidationError m) =>
  Value -> m ()
checkRecursionDepth v = do
  limit <- asks maxDepth
  current <- asks currentDepth
  when (current > limit) (throwError . ValidationError v . TraversalDepthExceeded $ limit)

valid :: (Applicative m) => m ()
valid = pure () 
