{-# LANGUAGE FlexibleContexts #-}

module Data.Medea.Validation where

import Control.Monad (when)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.Except (MonadError(..))
import Data.Aeson (Value)

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

newtype ValidationErrorType = TraversalDepthExceeded Word -- limit
  deriving (Eq)

checkRecursionDepth :: (MonadReader ValidatorState m, MonadError ValidationError m) =>
  Value -> m ()
checkRecursionDepth v = do
  limit <- asks maxDepth
  current <- asks currentDepth
  when (current > limit) (throwError . ValidationError v . TraversalDepthExceeded $ limit) 
